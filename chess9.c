//copyright Cristian Mocanu 27 july 2014 all rights reserved
//gcc chess9.c -Ofast -lm -pthread -Wall

#include <fcntl.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/sysinfo.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define NDEBUG
#include <assert.h>

typedef unsigned long long U64;

#define MAX_DEPTH 64
#define MAX_GAME 300
#define MAX_MOVES 128
#define MAX_BESTMOVES 4096

#include "data7.c"

struct POSITION_EXTRA {
  unsigned char flags;
  unsigned char en_passent_target_square;
  unsigned char halfmove_clock;
  short static_score;
  U64 hash;
};

struct POSITION {
  int board[64];
  U64 sides[2];
  U64 pieces[13];
  struct POSITION_EXTRA extra;
};

struct MOVE {
  unsigned short data;
  short score;
};

struct HASH_TABLE_ENTRY {
  U64 hash;
  unsigned char depth;
  unsigned char flag;
  short score;
  unsigned short best_move;
};

struct HASH_TABLE_ENTRY_PACKED {
  U64 word1;
  U64 word2;
};

struct STACK {
  unsigned char other_piece;
  unsigned char other_piece_square;
  struct POSITION_EXTRA extra;
  struct MOVE *moves;
  struct MOVE first_move[MAX_MOVES];
  unsigned char number_of_moves;
  struct MOVE best_line[MAX_DEPTH];
  unsigned char best_line_length;
  short initial_lower_bound;
  short lower_bound;
  short upper_bound;
  short score;
  unsigned short hash_move;
  unsigned short killer_move[3];
};

struct LOOP_ARGUMENTS {
  struct POSITION position;
  unsigned int side;
  struct STACK base_stack[MAX_GAME];
  struct STACK *stack;
};

struct BESTMOVE {
  struct MOVE best_line[MAX_DEPTH];
  unsigned int best_line_length;
  unsigned int nodes;
  unsigned char depth;
};

#define WHITE 0
#define BLACK 1
#define NONE 0
#define WHITE_PAWN 1
#define WHITE_KNIGHT 2
#define WHITE_BISHOP 3
#define WHITE_ROOK 4
#define WHITE_QUEEN 5
#define WHITE_KING 6
#define BLACK_PAWN 7
#define BLACK_KNIGHT 8
#define BLACK_BISHOP 9
#define BLACK_ROOK 10
#define BLACK_QUEEN 11
#define BLACK_KING 12
#define HASH_TABLE_ENTRY_PACKED_SIZE sizeof(struct HASH_TABLE_ENTRY_PACKED)
#define MOVE_SIZE sizeof(struct MOVE)
#define FLAG_CASTLE_KING_WHITE 1
#define FLAG_CASTLE_KING_BLACK 2
#define FLAG_CASTLE_QUEEN_WHITE 4
#define FLAG_CASTLE_QUEEN_BLACK 8
#define FLAG_CHECK 16
#define FLAG_EN_PASSENT 9
#define FLAG_PAWN_DOUBLE_MOVE 10
#define FLAG_TRANSFORMATION_KNIGHT 11
#define FLAG_TRANSFORMATION_BISHOP 12
#define FLAG_TRANSFORMATION_ROOK 13
#define FLAG_TRANSFORMATION_QUEEN 14
#define FLAG_TRANSPOSITION_UPPER_BOUND 1
#define FLAG_TRANSPOSITION_LOWER_BOUND 2
#define FLAG_TRANSPOSITION_EXACT 0
#define MATE_SCORE 14100
#define MIN_SCORE (-evals[WHITE_KING] - MAX_DEPTH)
#define MAX_SCORE (evals[WHITE_KING] + MAX_DEPTH)
#define NO_EN_PASSENT_TARGET_SQUARE 255

U64 zobrist_board[64][14], zobrist_side[2], zobrist_king_rocade[2], zobrist_queen_rocade[2];
unsigned long nodes, start_time, move_time, max_time, iteration_start, iteration_duration;
bool can_run, debug = false, ponder = true;
short reference_score, cpus, first = 0;
struct MOVE *pondermove = NULL;
struct BESTMOVE bestmove[MAX_BESTMOVES];
struct HASH_TABLE_ENTRY_PACKED *transposition_table = NULL;
unsigned int bestmove_read, bestmove_write, transposition_size = (1 << 27) - 1;
FILE *uci_out;
pthread_mutex_t parallel_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t parallel_cond = PTHREAD_COND_INITIALIZER;

#define max(X, Y) (((X) > (Y)) ? (X) : (Y))
#define min(X, Y) (((X) < (Y)) ? (X) : (Y))
#define long_rand() (((U64) rand() << 32) | rand())
#define is_mate_score(SCORE) ((SCORE) < -MATE_SCORE)
#define is_opponent_mate_score(SCORE) ((SCORE) > MATE_SCORE)
#define get_piece_color(PIECE) ((PIECE) > 6)
#define get_piece_on_board(POSITION, SQUARE) ((POSITION)->board[SQUARE])
#define get_single_square(PIECES) (__builtin_ffsll(PIECES) - 1)
#define is_any_on_board(POSITION, SQUARE) ((POSITION)->board[SQUARE] != NONE)
#define is_color_on_board(POSITION, SQUARE, SIDE) ((POSITION)->sides[SIDE] & (1ULL << SQUARE))
#define is_piece_on_board(POSITION, SQUARE, PIECE) ((POSITION)->board[SQUARE] == (PIECE))
#define set_flag(EXTRA, FLAG) (EXTRA.flags |= (FLAG))
#define clear_flag(EXTRA, FLAG) (EXTRA.flags &= ~(FLAG))
#define read_flag(EXTRA, FLAG) (EXTRA.flags & (FLAG))
#define is_en_passent_target_square(POSITION) ((POSITION)->extra.en_passent_target_square != NO_EN_PASSENT_TARGET_SQUARE)
#define square_behind_the_pawn(SIDE, SQUARE) (SQUARE + (SIDE ? 8 : -8))
#define square_in_front_of_the_pawn(SIDE, SQUARE) (SQUARE + (SIDE ? -8 : 8))
#define notation_to_location(NOTATION) ((*NOTATION - 'a') + (*(NOTATION + 1) - '1') * 8)
#define all_pieces(POSITION) ((POSITION)->sides[WHITE] | (POSITION)->sides[BLACK])
#define all_pawns(POSITION) ((POSITION)->pieces[WHITE_PAWN] | (POSITION)->pieces[BLACK_PAWN])
#define sliding_pieces(POSITION, SIDE) ((SIDE) ? POSITION->pieces[BLACK_BISHOP] | POSITION->pieces[BLACK_ROOK] | POSITION->pieces[BLACK_QUEEN] : POSITION->pieces[WHITE_BISHOP] | POSITION->pieces[WHITE_ROOK] | POSITION->pieces[WHITE_QUEEN])
#define pawn_piece(SIDE) (SIDE ? BLACK_PAWN : WHITE_PAWN)
#define knight_piece(SIDE) (SIDE ? BLACK_KNIGHT : WHITE_KNIGHT)
#define bishop_piece(SIDE) (SIDE ? BLACK_BISHOP : WHITE_BISHOP)
#define rook_piece(SIDE) (SIDE ? BLACK_ROOK : WHITE_ROOK)
#define queen_piece(SIDE) (SIDE ? BLACK_QUEEN : WHITE_QUEEN)
#define king_piece(SIDE) (SIDE ? BLACK_KING : WHITE_KING)
#define get_square(LINE, COLUMN) ((LINE) * 8 + (COLUMN))
#define bishop_mobility(PIECES, SIDE, SQUARE) (bishop_mobility_index[SIDE][SQUARE][(((PIECES) & attacks[WHITE_BISHOP][SQUARE]) * bishop_magic_mask[SQUARE]) >> (64 - bishop_magic_shift[SQUARE])])
#define rook_mobility(PIECES, SIDE, SQUARE) (rook_mobility_index[SIDE][SQUARE][(((PIECES) & attacks[WHITE_ROOK][SQUARE]) * rook_magic_mask[SQUARE]) >> (64 - rook_magic_shift[SQUARE])])
#define queen_mobility(PIECES, SIDE, SQUARE) (bishop_mobility(PIECES, SIDE, SQUARE) + rook_mobility(PIECES, SIDE, SQUARE))
#define bishop_attacks(PIECES, SQUARE) (bishop_magic_index[SQUARE][(((PIECES) & attacks[WHITE_BISHOP][SQUARE]) * bishop_magic_mask[SQUARE]) >> (64 - bishop_magic_shift[SQUARE])])
#define rook_attacks(PIECES, SQUARE) (rook_magic_index[SQUARE][(((PIECES) & attacks[WHITE_ROOK][SQUARE]) * rook_magic_mask[SQUARE]) >> (64 - rook_magic_shift[SQUARE])])
#define queen_attacks(PIECES, SQUARE) (bishop_attacks(PIECES, SQUARE) | rook_attacks(PIECES, SQUARE))
#define knight_attacks(SQUARE) (attacks[WHITE_KNIGHT][SQUARE])
#define pawn_attacks(SQUARE, PIECE) (attacks[PIECE][SQUARE])
#define king_attacks(SQUARE) (attacks[WHITE_KING][SQUARE])
#define king_attackers(POSITION, SIDE, PIECES) (attack_to(POSITION, !SIDE, PIECES, get_single_square((POSITION)->pieces[king_piece(SIDE)])))
#define is_check_from_piece(POSITION, SIDE, SQUARE, PIECE) (attack_from(all_pieces(POSITION), SQUARE, PIECE) & (POSITION)->pieces[king_piece(!SIDE)])
#define pack_move(BEGIN_SQUARE, END_SQUARE, FLAG) ((BEGIN_SQUARE) | (((unsigned short) (END_SQUARE)) << 6) | (((unsigned short) (FLAG)) << 12))
#define get_move_begin_square(MOVE) ((MOVE)->data & 63)
#define get_move_end_square(MOVE) (((MOVE)->data >> 6) & 63)
#define get_move_flag(MOVE) ((MOVE)->data >> 12)
#define is_transformation_move(MOVE) ((get_move_flag(MOVE)) == FLAG_TRANSFORMATION_QUEEN || (get_move_flag(MOVE)) == FLAG_TRANSFORMATION_ROOK || (get_move_flag(MOVE)) == FLAG_TRANSFORMATION_BISHOP || (get_move_flag(MOVE)) == FLAG_TRANSFORMATION_KNIGHT)
#define is_killer_move(STACK, MOVE) ((MOVE)->data == (STACK)->killer_move[0] || (MOVE)->data == (STACK)->killer_move[1] || (MOVE)->data == (STACK)->killer_move[2])
#define is_capture_move(POSITION, MOVE) (get_piece_on_board((POSITION), get_move_end_square(MOVE)))
#define material_for_attack(POSITION, SIDE) ((POSITION->pieces[queen_piece(SIDE)] && (more_than_one(position->pieces[knight_piece(SIDE)] | position->pieces[bishop_piece(SIDE)]) || position->pieces[rook_piece(SIDE)])) || (more_than_one(POSITION->pieces[rook_piece(SIDE)]) && more_than_one(position->pieces[knight_piece(SIDE)] | position->pieces[bishop_piece(SIDE)])))
#define has_bishop_pair(POSITION, SIDE) (more_than_one(POSITION->pieces[bishop_piece(SIDE)]))
#define king_in_pawn_transformation_square(POSITION, SIDE, SQUARE) (pawn_transformation_square[SIDE][SQUARE] & POSITION->pieces[king_piece(!SIDE)])
#define pieces_not_pawns(POSITION, SIDE) (POSITION->sides[SIDE] ^ POSITION->pieces[pawn_piece(SIDE)] ^ POSITION->pieces[king_piece(SIDE)])
#define heavy_pieces(POSITION, SIDE) (POSITION->pieces[rook_piece(SIDE)] | POSITION->pieces[queen_piece(SIDE)])
#define diagonal_pieces(POSITION, SIDE) (POSITION->pieces[bishop_piece(SIDE)] | POSITION->pieces[queen_piece(SIDE)])
#define connected_rooks(POSITION, SIDE) (POSITION->pieces[rook_piece(SIDE)] && (POSITION->pieces[rook_piece(SIDE)] & attack_from(all_pieces(POSITION), get_single_square((POSITION)->pieces[rook_piece(SIDE)]), rook_piece(SIDE))))
#define pawns_defending_king(POSITION, SIDE, SQUARE) ((POSITION)->pieces[pawn_piece(SIDE)] & king_attacks(SQUARE))
#define semiopen_file(POSITION, SIDE, COLUMN) (!(file[COLUMN] & (POSITION)->pieces[pawn_piece(SIDE)]))
#define open_diagonal(POSITION, SIDE, DIAGONAL) (!(diagonals[DIAGONAL] & (SIDE ? ~SEVENTH_RANK & ~SIXTH_RANK & ~file[0] & ~file[7] & all_pawns(POSITION) | (~SECOND_RANK & POSITION->pieces[BLACK_PAWN]) : ~SECOND_RANK & ~THIRD_RANK & ~file[0] & ~file[7] & all_pawns(POSITION) | (~SEVENTH_RANK & POSITION->pieces[WHITE_PAWN]))))
#define pawn_is_in_chain(POSITION, SQUARE, PAWNS) (PAWNS & king_attacks(SQUARE) & ~(1ULL << square_behind_the_pawn(WHITE, square)) & ~(1ULL << square_behind_the_pawn(BLACK, square)))
#define isolated_pawn(POSITION, SIDE, SQUARE) (!(((SQUARE % 8 == 0 ? 0 : file[SQUARE % 8 - 1]) | (SQUARE % 8 == 7 ? 0 : file[SQUARE % 8 + 1])) & POSITION->pieces[pawn_piece(SIDE)]))
#define backwards_pawn(POSITION, SIDE, SQUARE) (!(free_pawn_index[!SIDE][square_in_front_of_the_pawn(SIDE, SQUARE)] & ~file[SQUARE % 8] & (POSITION)->pieces[pawn_piece(SIDE)]))
#define passed_pawn(POSITION, SIDE, SQUARE) (!(free_pawn_index[SIDE][SQUARE] & POSITION->pieces[pawn_piece(!SIDE)]))
#define move_number(STACK) ((STACK)->moves - (STACK)->first_move)
#define no_more_moves(STACK) (move_number(STACK) >= (STACK)->number_of_moves)
#define is_first_move(STACK) ((STACK)->moves == (STACK)->first_move)
#define is_capture(STACK) (STACK->other_piece)
#define discard_moves(STACK) ((STACK)->moves = (STACK)->first_move + (STACK)->number_of_moves)
#define reset_moves(STACK) ((STACK)->moves = (STACK)->first_move)
#define initialize_moves(STACK) ((STACK)->number_of_moves = 0, reset_moves(STACK))
#define change_color(PIECE) (PIECE ? (get_piece_color(PIECE) ? PIECE - 6 : PIECE + 6) : 0)
#define chebyshev_distance(SQUARE1, SQUARE2) (max(abs((SQUARE1) / 8 - (SQUARE2) / 8), abs((SQUARE1) % 8 - (SQUARE2) % 8)))
#define more_than_one(PIECES) ((PIECES) && ((PIECES) & ((PIECES) - 1)))
#define candidate_passer(POSITION, SIDE, SQUARE) (!(file_in_front[SIDE][SQUARE] & all_pawns(POSITION)))
#define pawn_is_blocked(POSITION, SIDE, SQUARE) ((1ULL << square_in_front_of_the_pawn(SIDE, SQUARE)) & all_pieces(POSITION))
#define significant_change(SCORE) (abs(SCORE) < 200 ? 50 : abs(SCORE) / 4)
#define is_odd(N) ((N) & 1)

//TODO: get time in nanoseconds
unsigned long get_time_in_milliseconds() {
  struct timespec temp;
  clock_gettime(CLOCK_MONOTONIC, &temp);
  return temp.tv_sec * 1000 + temp.tv_nsec / 1000000;
}

unsigned char pop_last_square(U64 *const squares) {
  assert(squares);
  const unsigned char i = get_single_square(*squares);
  assert(i < 64);
  *squares &= *squares - 1;
  return i;
}

unsigned char pop_first_square(U64 *const squares) {
  assert(squares);
  const unsigned char i = 63 - __builtin_clzll(*squares);
  assert(i < 64);
  *squares ^= 1ULL << i;
  return i;
}

U64 same_valuable_piece(const struct POSITION *const position,
						const unsigned char piece) {
  assert(piece < 13);
  switch (piece) {
  case WHITE_KNIGHT: return position->pieces[WHITE_KNIGHT] | position->pieces[WHITE_BISHOP];
  case BLACK_KNIGHT: return position->pieces[BLACK_KNIGHT] | position->pieces[BLACK_BISHOP];
  default: return position->pieces[piece];
  }
}

U64 more_valuable_piece(const struct POSITION *const position,
						const unsigned char piece) {
  assert(piece < 13);
  switch (piece) {
  case WHITE_PAWN: return position->pieces[WHITE_KNIGHT] | position->pieces[WHITE_BISHOP] | position->pieces[WHITE_ROOK] | position->pieces[WHITE_QUEEN] | position->pieces[WHITE_KING];
  case BLACK_PAWN: return position->pieces[BLACK_KNIGHT] | position->pieces[BLACK_BISHOP] | position->pieces[BLACK_ROOK] | position->pieces[BLACK_QUEEN] | position->pieces[BLACK_KING];
  case WHITE_KNIGHT: return position->pieces[WHITE_ROOK] | position->pieces[WHITE_QUEEN] | position->pieces[WHITE_KING];
  case BLACK_KNIGHT: return position->pieces[BLACK_ROOK] | position->pieces[BLACK_QUEEN] | position->pieces[BLACK_KING];
  case WHITE_BISHOP: return position->pieces[WHITE_ROOK] | position->pieces[WHITE_QUEEN] | position->pieces[WHITE_KING];
  case BLACK_BISHOP: return position->pieces[BLACK_ROOK] | position->pieces[BLACK_QUEEN] | position->pieces[BLACK_KING];
  case WHITE_ROOK: return position->pieces[WHITE_QUEEN] | position->pieces[WHITE_KING];
  case BLACK_ROOK: return position->pieces[BLACK_QUEEN] | position->pieces[BLACK_KING];
  case WHITE_QUEEN: return position->pieces[WHITE_KING];
  case BLACK_QUEEN: return position->pieces[BLACK_KING];
  default: return 0;
  }
}

U64 attack_from(const U64 pieces,
				const unsigned char square,
				const unsigned char piece) {
  assert(square < 64);
  assert(piece < 13);
  switch(piece) {
  case WHITE_PAWN: return pawn_attacks(square, WHITE_PAWN);
  case BLACK_PAWN: return pawn_attacks(square, BLACK_PAWN);
  case WHITE_KNIGHT:
  case BLACK_KNIGHT: return knight_attacks(square);
  case WHITE_BISHOP:
  case BLACK_BISHOP: return bishop_attacks(pieces, square);
  case WHITE_ROOK:
  case BLACK_ROOK: return rook_attacks(pieces, square);
  case WHITE_QUEEN:
  case BLACK_QUEEN: return queen_attacks(pieces, square);
  case WHITE_KING:
  case BLACK_KING: return king_attacks(square);
  default: return NONE;
  }
}

U64 attack_to(const struct POSITION *const position,
			  const unsigned int side,
			  const U64 pieces,
			  const unsigned char square) {
  assert(side < 2);
  assert(square < 64);
  return (position->pieces[pawn_piece(side)] & pawn_attacks(square, pawn_piece(!side)))
	| (position->pieces[knight_piece(side)] & knight_attacks(square))
	| ((position->pieces[bishop_piece(side)] | position->pieces[queen_piece(side)]) & bishop_attacks(pieces, square))
	| ((position->pieces[rook_piece(side)] | position->pieces[queen_piece(side)]) & rook_attacks(pieces, square));
}

U64 smallest_attack_to(const struct POSITION *const position,
					   const unsigned int side,
					   const U64 pieces,
					   const unsigned char square) {
  assert(side < 2);
  assert(square < 64);
  U64 squares;
  squares = pieces & position->pieces[pawn_piece(side)] & pawn_attacks(square, pawn_piece(!side));
  if (squares) {
	return squares;
  } else {
	squares = pieces & position->pieces[knight_piece(side)] & knight_attacks(square);
	if (squares) {
	  return squares;
	} else {
	  squares = pieces & position->pieces[bishop_piece(side)] & bishop_attacks(pieces, square);
	  if (squares) {
		return squares;
	  } else {
		squares = pieces & position->pieces[rook_piece(side)] & rook_attacks(pieces, square);
		if (squares) {
		  return squares;
		} else {
		  squares = pieces & position->pieces[queen_piece(side)] & queen_attacks(pieces, square);
		  if (squares) {
			return squares;
		  } else {
			return pieces & king_attacks(square) & position->pieces[king_piece(side)];
		  }
		}
	  }
	}
  }
}

void move_piece(struct POSITION *const position,
				const unsigned char begin_square,
				const unsigned char end_square,
				const unsigned char piece,
				const unsigned int side) {
  assert(begin_square < 64);
  assert(end_square < 64);
  assert(piece < 13);
  assert(side < 2);
  assert(is_piece_on_board(position, begin_square, piece));
  assert(!is_piece_on_board(position, end_square, piece));
  assert(get_piece_color(piece) == side);
  position->pieces[piece] ^= (1ULL << begin_square) | (1ULL << end_square);
  position->sides[side] ^= (1ULL << begin_square) | (1ULL << end_square);
  assert((position->sides[WHITE] & position->sides[BLACK]) == 0);
  position->board[begin_square] = NONE;
  position->board[end_square] = piece;
}

void change_piece(struct POSITION *const position,
				  const unsigned char square,
				  const unsigned char from_piece,
				  const unsigned char to_piece,
				  const unsigned int side) {
  assert(square < 64);
  assert(from_piece < 13);
  assert(to_piece < 13);
  assert(side < 2);
  assert(is_piece_on_board(position, square, from_piece));
  assert(get_piece_color(from_piece) == side);
  assert(!is_piece_on_board(position, square, to_piece));
  assert(get_piece_color(to_piece) == side);
  position->pieces[from_piece] ^= (1ULL << square);
  position->pieces[to_piece] ^= (1ULL << square);
  position->board[square] = to_piece;
}

void set_piece_on_board(struct POSITION *const position,
						const unsigned char square,
						const unsigned char piece,
						const unsigned int side) {
  assert(square < 64);
  assert(piece < 13);
  assert(side < 2);
  assert(!is_piece_on_board(position, square, piece));
  assert(get_piece_color(piece) == side);
  position->pieces[piece] ^= (1ULL << square);
  position->sides[side] ^= (1ULL << square);
  assert((position->sides[WHITE] & position->sides[BLACK]) == 0);
  position->board[square] = piece;
}

void remove_piece_from_board(struct POSITION *const position,
							 const unsigned char square,
							 const unsigned char piece,
							 const unsigned int side) {
  assert(square < 64);
  assert(piece < 13);
  assert(side < 2);
  assert(is_piece_on_board(position, square, piece));
  assert(get_piece_color(piece) == side);
  assert(piece != king_piece(side));
  position->pieces[piece] ^= (1ULL << square);
  position->sides[side] ^= (1ULL << square);
  assert((position->sides[WHITE] & position->sides[BLACK]) == 0);
  position->board[square] = NONE;
}

void sort_moves(struct STACK *const stack) {
  struct MOVE temp;
  int i, l, r;
  int sort_begin[MAX_MOVES], sort_end[MAX_MOVES];
  if (stack->number_of_moves < 20) {
	for (i = 1; i < stack->number_of_moves; i++) {
	  temp = stack->moves[i];
	  for (l = i; l > 0 && stack->moves[l - 1].score < temp.score; l--) {
		stack->moves[l] = stack->moves[l - 1];
	  }
	  stack->moves[l] = temp;
	}
  } else {
	i = 0;
	sort_begin[0] = 0;
	sort_end[0] = stack->number_of_moves;
	while (i >= 0) {
	  l = sort_begin[i];
	  r = sort_end[i] - 1;
	  if (l < r) {
		temp = stack->moves[l];
		while (l < r) {
		  while (stack->moves[r].score <= temp.score && l < r) {
			r--;
		  }
		  if (l < r) {
			stack->moves[l++] = stack->moves[r];
		  }
		  while (stack->moves[l].score >= temp.score && l < r) {
			l++;
		  }
		  if (l < r) {
			stack->moves[r--] = stack->moves[l];
		  }
		}
		stack->moves[l] = temp;
		sort_begin[i + 1] = l + 1;
		sort_end[i + 1] = sort_end[i];
		sort_end[i++] = l;
	  }
	  else {
		i--;
	  }
	}
  }
}

void take_best_score(struct STACK *const stack1,
					 struct STACK *const stack2) {
  assert(-stack2->score > stack1->score);
  stack1->score = -stack2->score;
  if (is_mate_score(stack1->score)) {
	stack1->score += 1;
  } else if (is_opponent_mate_score(stack1->score)) {
	stack1->score -= 1;
  }
  stack1->best_line_length = min(stack2->best_line_length, MAX_MOVES - 2);
  stack1->best_line[0] = *(stack1->moves - 1);
  memcpy(&stack1->best_line[1], stack2->best_line, stack1->best_line_length * MOVE_SIZE);
  stack1->best_line_length++;
}

void init_hash() {
  unsigned int i, j;
  zobrist_side[WHITE] = long_rand();
  zobrist_side[BLACK] = long_rand();
  zobrist_king_rocade[WHITE] = long_rand();
  zobrist_king_rocade[BLACK] = long_rand();
  zobrist_queen_rocade[WHITE] = long_rand();
  zobrist_queen_rocade[BLACK] = long_rand();
  for (i = 0; i < 64; i++) {
	for (j = 0; j < 14; j++) {
	  zobrist_board[i][j] = long_rand();
	}
  }
}

U64 hash_position(struct POSITION *const position,
				  const unsigned int side) {
  assert(side < 2);
  unsigned int i;
  U64 hash = zobrist_side[side];
  for (i = 0; i < 64; i++) {
	hash ^= zobrist_board[i][get_piece_on_board(position, i)];
  }
  if (is_en_passent_target_square(position)) {
	hash ^= zobrist_board[position->extra.en_passent_target_square][13];
  }
  if (read_flag(position->extra, FLAG_CASTLE_KING_WHITE)) {
	hash ^= zobrist_king_rocade[WHITE];
  }
  if (read_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE)) {
	hash ^= zobrist_queen_rocade[WHITE];
  }
  if (read_flag(position->extra, FLAG_CASTLE_KING_BLACK)) {
	hash ^= zobrist_king_rocade[BLACK];
  }
  if (read_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK)) {
	hash ^= zobrist_queen_rocade[BLACK];
  }
  return hash;
}

unsigned char letter_to_piece(const char letter) {
  switch(letter) {
  case 'P': return WHITE_PAWN;
  case 'N': return WHITE_KNIGHT;
  case 'B': return WHITE_BISHOP;
  case 'R': return WHITE_ROOK;
  case 'Q': return WHITE_QUEEN;
  case 'K': return WHITE_KING;
  case 'p': return BLACK_PAWN;
  case 'n': return BLACK_KNIGHT;
  case 'b': return BLACK_BISHOP;
  case 'r': return BLACK_ROOK;
  case 'q': return BLACK_QUEEN;
  case 'k': return BLACK_KING;
  default: return NONE;
  }
}

unsigned int parse_fen_position(struct POSITION *const position,
								const char *const string) {
  unsigned int i = 0;
  unsigned char line = 7;
  unsigned char column, piece;
  const unsigned int l = strlen(string);
  while (i < l) {
    column = 0;
    while (column < 8 && i < l) {
      if (isdigit(string[i])) {
		column += string[i] - '0';
      } else {
		piece = letter_to_piece(string[i]);
		set_piece_on_board(position, get_square(line, column), piece, get_piece_color(piece));
		position->extra.static_score += evals[piece] * (get_piece_color(piece) ? -1 : 1);
		column++;
      }
      i++;
    }
	i++;
	if (line == 0) {
	  break;
	}
    line--;
  }
  return i;
}

unsigned int parse_fen_flags(struct POSITION *const position,
							 unsigned int *side,
							 const char *const string) {
  unsigned int i = 0;
  char *temp;
  const unsigned int l = strlen(string);
  position->extra.en_passent_target_square = NO_EN_PASSENT_TARGET_SQUARE;
  *side = 0;
  if (i < l) {
	if (string[i] == 'b') {
	  *side = 1;
	}
    i += 2;
    if (i < l && string[i] == 'K') {
	  set_flag(position->extra, FLAG_CASTLE_KING_WHITE);
      i++;
    }
    if (i < l && string[i] == 'Q') {
	  set_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE);
      i++;
    }
    if (i < l && string[i] == 'k') {
	  set_flag(position->extra, FLAG_CASTLE_KING_BLACK);
      i++;
    }
    if (i < l && string[i] == 'q') {
	  set_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK);
      i++;
    }
    if (i < l && string[i] == '-') {
      i++;
    }
    i++;
    if (i < l && string[i] != '-')  {
	  position->extra.en_passent_target_square = (string[i + 1] - '1') * 8 + string[i] - 'a';
	  assert(position->extra.en_passent_target_square < 64);
	  i++;
    }
	i++;
	if (i < l) {
	  position->extra.halfmove_clock = strtol(&string[i], &temp, 10);
	  i += temp - &string[i];
	  if (i < l) {
		strtol(&string[i], &temp, 10);
		i += temp - &string[i];
	  }
	}
  }
  return i;
}

unsigned int parse_fen(struct POSITION *const position,
					   unsigned int *side,
					   const char *const string) {
  unsigned int i = parse_fen_position(position, string);
  i += parse_fen_flags(position, side, string + i);
  if (*side) {
	position->extra.static_score = -position->extra.static_score;
  }
  if (king_attackers(position, side, all_pieces(position))) {
	set_flag(position->extra, FLAG_CHECK);
  } else {
	clear_flag(position->extra, FLAG_CHECK);
  }
  position->extra.hash = hash_position(position, *side);
  return i;
}

unsigned int get_fen_position(const struct POSITION *const position,
							  char *const string) {
  unsigned int k = 0;
  int empty;
  unsigned char piece, line = 7, column, square;
  while (1) {
    empty = 0;
    for (column = 0; column < 8; column++) {
	  square = get_square(line, column);
	  if (is_any_on_board(position, square)) {
		piece = get_piece_on_board(position, square);
		assert(piece < 13);
		assert(piece > 0);
		if (empty) {
		  string[k++] = '0' + empty;
		  empty = 0;
		}
		string[k++] = piece_to_letter[piece];
	  } else {
		empty++;
	  }
    }
    if (empty) {
      string[k++] = '0' + empty;
    }
    if (line != 0) {
      string[k++] = '/';
    }
	if (line == 0) {
	  break;
	} else {
	  line--;
	}
  }
  return k;
}

unsigned int get_fen_flags(const struct POSITION *const position,
						   const unsigned int side,
						   const unsigned char ply,
						   char *const string) {
  assert(side < 2);
  unsigned int k = 0;
  string[k++] = side_to_letter[side];
  string[k++] = ' ';
  const int aux = k;
  if (read_flag(position->extra, FLAG_CASTLE_KING_WHITE)) {
    string[k++] = 'K';
  }
  if (read_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE)) {
    string[k++] = 'Q';
  }
  if (read_flag(position->extra, FLAG_CASTLE_KING_BLACK)) {
    string[k++] = 'k';
  }
  if (read_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK)) {
    string[k++] = 'q';
  }
  if (aux == k) {
    string[k++] = '-';
  }
  string[k++] = ' ';
  if (is_en_passent_target_square(position)) {
	string[k++] = 'a' + position->extra.en_passent_target_square % 8;
	string[k++] = '1' + position->extra.en_passent_target_square / 8;
  } else {
    string[k++] = '-';
  }
  string[k++] = ' ';
  sprintf(&string[k], "%d %d", position->extra.halfmove_clock, 1 + ply / 2);
  return k;
}

unsigned int get_fen(const struct POSITION *const position,
					 const unsigned int side,
					 const unsigned char ply,
					 char *const string) {
  assert(side < 2);
  unsigned int i = get_fen_position(position, string);
  string[i++] = ' ';
  return i + get_fen_flags(position, side, ply, string + i);
}

void print_fen(const struct POSITION *const position,
			   const unsigned int side,
			   const unsigned char ply) {
  assert(side < 2);
  char string[88];
  get_fen(position, side, ply, string);
  printf("%s\n", string);
}

void get_from_transposition_table(const U64 hash,
								  struct HASH_TABLE_ENTRY *const entry) {
  const struct HASH_TABLE_ENTRY_PACKED packed = transposition_table[hash & transposition_size];
  entry->hash = packed.word1 ^ packed.word2;
  if (entry->hash == hash) {
	entry->depth = (unsigned char) packed.word2;
	entry->flag = (unsigned char) (packed.word2 >> 8);
	entry->score = (short) (packed.word2 >> 16);
	entry->best_move = (unsigned short) (packed.word2 >> 32);
  }
}

void put_in_transposition_table(const U64 hash,
								const struct STACK *const stack,
								const unsigned char depth) {
  U64 flag, word2;
  if (can_run && stack->best_line_length > 0) {
	const struct HASH_TABLE_ENTRY_PACKED packed = transposition_table[hash & transposition_size];
	if (packed.word1 ^ packed.word2 != hash || (unsigned char) packed.word2 <= depth) {
	  if (stack->initial_lower_bound >= stack->score) {
		flag = FLAG_TRANSPOSITION_UPPER_BOUND;
	  } else if (stack->score >= stack->upper_bound) {
		flag = FLAG_TRANSPOSITION_LOWER_BOUND;
	  } else {
		flag = FLAG_TRANSPOSITION_EXACT;
	  }
	  word2 = ((U64) depth)
		| (flag << 8)
		| ((U64) (unsigned short) stack->score << 16)
		| ((U64) stack->best_line->data << 32);
	  transposition_table[hash & transposition_size].word1 = hash ^ word2;
	  transposition_table[hash & transposition_size].word2 = word2;
	}
  }
}

unsigned char get_end_piece(const unsigned int side,
							const unsigned char flag) {
  assert(side < 2);
  switch (flag) {
  case FLAG_TRANSFORMATION_QUEEN: return queen_piece(side);
  case FLAG_TRANSFORMATION_ROOK: return rook_piece(side);
  case FLAG_TRANSFORMATION_BISHOP: return bishop_piece(side);
  case FLAG_TRANSFORMATION_KNIGHT: return knight_piece(side);
  default: return NONE;
  }
}

void unmove(struct POSITION *const position,
			const unsigned int side,
			struct STACK *const stack,
			const unsigned char begin_square,
			const unsigned char end_square,
			const unsigned char flag) {
  assert(side < 2);
  assert(begin_square < 64);
  assert(end_square < 64);
  position->extra = stack->extra;
  const unsigned char end_piece = get_end_piece(side, flag);
  if (end_piece) {
	change_piece(position, end_square, end_piece, pawn_piece(side), side);
  }
  move_piece(position, end_square, begin_square, get_piece_on_board(position, end_square), side);
  switch (flag) {
  case FLAG_CASTLE_KING_WHITE: move_piece(position, 5, 7, WHITE_ROOK, WHITE); break;
  case FLAG_CASTLE_QUEEN_WHITE: move_piece(position, 3, 0, WHITE_ROOK, WHITE); break;
  case FLAG_CASTLE_KING_BLACK: move_piece(position, 61, 63, BLACK_ROOK, BLACK); break;
  case FLAG_CASTLE_QUEEN_BLACK: move_piece(position, 59, 56, BLACK_ROOK, BLACK);
  }
  if (is_capture(stack)) {
	set_piece_on_board(position, stack->other_piece_square, stack->other_piece, !side);
  }
  stack->moves++;
}

bool is_pinned(const struct POSITION *const position,
			   const unsigned int side,
			   const unsigned char begin_square,
			   const unsigned char end_square,
			   U64 pieces) {
  assert(side < 2);
  assert(begin_square < 64);
  assert(end_square < 64);
  pieces = pieces ^ (1ULL << begin_square) | (1ULL << end_square);
  return ((get_piece_on_board(position, begin_square) != king_piece(side))
		  && (((position->pieces[bishop_piece(!side)] | position->pieces[queen_piece(!side)])
			  & (pieces ^ (1ULL << end_square))
			  & bishop_attacks(pieces, get_single_square(position->pieces[king_piece(side)])))
			 || ((position->pieces[rook_piece(!side)] | position->pieces[queen_piece(!side)])
				 & (pieces ^ (1ULL << end_square))
				 & rook_attacks(pieces, get_single_square(position->pieces[king_piece(side)])))));
}

short static_exchange(const struct POSITION *const position,
					  unsigned int side,
					  unsigned char begin_square,
					  const unsigned char begin_piece,
					  const unsigned char end_square,
					  const unsigned char flag) {
  assert(side < 2);
  assert(begin_square < 64);
  assert(begin_piece < 13);
  assert(end_square < 64);
  assert(is_color_on_board(position, begin_square, side));
  short gain[32];
  int d = 0;
  unsigned char capture_square;
  U64 temp, pieces = all_pieces(position) ^ (1ULL << end_square);
  if (flag == FLAG_EN_PASSENT) {
	pieces ^= square_behind_the_pawn(side, end_square);
	capture_square = square_behind_the_pawn(side, end_square);
  } else {
	capture_square = end_square;
  }
  gain[d] = evals[get_piece_on_board(position, capture_square)];
  while (true) {
	d++;
	gain[d] = evals[get_piece_on_board(position, begin_square)] - gain[d - 1];
	side = !side;
	pieces ^= (1ULL << begin_square);
	temp = smallest_attack_to(position, side, pieces, end_square);
	if (!temp) {
	  while (--d) {
		gain[d - 1] = -max(-gain[d - 1], gain[d]);
	  }
	  return gain[0];
	} else {
	  begin_square = get_single_square(temp);
	}
  }
}

short evaluate_piece(const U64 pieces,
					 const unsigned int side,
					 const unsigned char square,
					 const unsigned char piece) {
  assert(side < 2);
  assert(square < 64);
  assert(piece < 13);
  switch(piece) {
  case WHITE_KNIGHT:
  case BLACK_KNIGHT: return knight_mobility_index[side][square];
  case WHITE_BISHOP:
  case BLACK_BISHOP: return bishop_mobility(pieces, side, square);
  case WHITE_ROOK:
  case BLACK_ROOK: return rook_mobility(pieces, side, square);
  case WHITE_QUEEN:
  case BLACK_QUEEN: return queen_mobility(pieces, side, square);
  default: return 0;
  }
}

//TODO: bonus for attacking undefended pieces
short evaluate_move(const struct POSITION *const position,
					const unsigned int side,
					const unsigned char begin_square,
					const unsigned char end_square,
					const unsigned char flag) {
  assert(side < 2);
  assert(begin_square < 64);
  assert(end_square < 64);
  const unsigned char piece = get_piece_on_board(position, begin_square);
  short score = 0, bonus = 0;
  switch (flag) {
  case FLAG_CASTLE_KING_WHITE:
  case FLAG_CASTLE_QUEEN_WHITE:
  case FLAG_CASTLE_KING_BLACK:
  case FLAG_CASTLE_QUEEN_BLACK: return score + 20;
  default:
	if (flag == FLAG_PAWN_DOUBLE_MOVE
		&& (pawn_attacks(square_behind_the_pawn(side, end_square), piece) & position->pieces[pawn_piece(!side)])) {
	  score += min(static_exchange(position,
								   side,
								   begin_square,
								   piece,
								   square_behind_the_pawn(side, end_square),
								   NONE),
				   static_exchange(position,
								   side,
								   begin_square,
								   piece,
								   end_square,
								   NONE));
	} else {
	  score += static_exchange(position,
							   side,
							   begin_square,
							   piece,
							   end_square,
							   flag);
	}
	if (is_check_from_piece(position, side, end_square, piece)) {
	  //return max(score + 10, 10);
	  return score + 200;
	} else if (score >= 0) {
	  if ((read_flag(position->extra, FLAG_CASTLE_KING_WHITE) && (piece == WHITE_KING || (begin_square == 7 && piece == WHITE_ROOK)))
		  || (read_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE) && (piece == WHITE_KING || (begin_square == 0 && piece == WHITE_ROOK)))
		  || (read_flag(position->extra, FLAG_CASTLE_KING_BLACK) && (piece == BLACK_KING || (begin_square == 63 && piece == BLACK_ROOK)))
		  || (read_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK) && (piece == BLACK_KING || (begin_square == 56 && piece == BLACK_ROOK)))) {
		bonus -= 20;
	  } else {
		if (piece != pawn_piece(side)) {
		  bonus += 30 + evaluate_piece(all_pieces(position) ^ (1ULL << begin_square), side, end_square, piece)
			- evaluate_piece(all_pieces(position), side, begin_square, piece);
		}
		if (attack_from(all_pawns(position) & ~(1ULL << begin_square), end_square, piece)
			& king_attacks(get_single_square(position->pieces[king_piece(!side)]))) {
		  bonus += 20;
		}
	  }
	  score = max(score + bonus, 0);
	}
  }
  return score;
}

//TODO: evaluate trapped pieces (bishop and rook)
//TODO: put pawn evaluation in a hash table and reuse
//TODO: evaluate basic endings pawn/knight/bishop/rook/queen
//TODO: pawn ending: key squares for the king, opposition
short evaluate_position(struct POSITION *const position,
						const unsigned int side_to_move) {
  assert(side_to_move < 2);
  short max_score[2], score[2] = {0, 0};
  short all_score = position->extra.static_score, bonus;
  unsigned int min_col = 7, max_col = 0, min_line = 7, max_line = 0, i, opened, side;
  U64 temp, passed_pawns, pawn_center_square;
  U64 control[2] = {0, 0};
  unsigned char square, aux;
  bool in_island, king_sideways_bonus;
  if (!all_pawns(position)
	  && __builtin_popcountll(all_pieces(position)) < 7
	  && abs(position->extra.static_score) <= 300) {
	return 0;
  } else {
	temp = all_pieces(position);
	while (temp) {
	  square = pop_last_square(&temp);
	  i = get_piece_on_board(position, square);
	  control[get_piece_color(i)] |= attack_from(all_pieces(position), square, i);
	  if (i == WHITE_PAWN || i == BLACK_PAWN) {
		min_col = min(min_col, square % 8);
		max_col = max(max_col, square % 8);
		min_line = min(min_line, square / 8);
		max_line = max(max_line, square / 8);
	  }
	}
	if (all_pawns(position)) {
	  square = ((min_line + max_line) / 2) * 8 + (min_col + max_col) / 2;
	  pawn_center_square = (1ULL << square);
	  if (is_odd(min_line + max_line)) {
		pawn_center_square |= (1ULL << (square + 8));
		if (is_odd(min_col + max_col)) {
		  pawn_center_square |= (1ULL << (square + 1)) | (1ULL << (square + 9));
		}
	  } else if (is_odd(min_col + max_col)) {
		pawn_center_square |= (1ULL << (square + 1));
	  }
	} else {
	  pawn_center_square = CENTER;
	}
	temp = all_pieces(position) ^ all_pawns(position) ^ position->pieces[WHITE_KING] ^ position->pieces[BLACK_KING];
	while (temp) {
	  square = pop_last_square(&temp);
	  i = get_piece_on_board(position, square);
	  side = get_piece_color(i);
	  score[side] += 2 * evaluate_piece(all_pieces(position), side, square, i);
	  if (!(attack_from(all_pieces(position), square, i)
			& (more_valuable_piece(position, change_color(i)) | same_valuable_piece(position, change_color(i))))
		  && !(attack_from(all_pieces(position), square, i)
			   & ~position->sides[side]
			   & ~control[!side])) {
		score[side] -= 30;
		if ((1ULL << square) & control[!side]) {
		  score[side] -= 10;
		}
	  }
	  if (i != knight_piece(side)
		  && (attack_from(NONE, square, i)
			  & (~file[square % 8] | attack_from(position->pieces[pawn_piece(side)], square, i))
			  & (more_valuable_piece(position, change_color(i)) | (position->sides[!side] & ~control[!side])))) {
		score[side] += 5;
	  }
	}
	for (side = 0; side < 2; side++) {
	  if (has_bishop_pair(position, side)) {
		score[side] += 10;
	  }
	  if (connected_rooks(position, side)) {
		score[side] += 5;
	  }
	  score[side] += 20 * __builtin_popcountll(pieces_not_pawns(position, side));
	  aux = get_single_square(position->pieces[king_piece(!side)]);
	  if (material_for_attack(position, side)) {
		if (position->pieces[knight_piece(side)]) {
		  temp = knight_attacks(aux) & ~all_pieces(position);
		  while (temp) {
			score[side] += 10 * __builtin_popcountll(knight_attacks(pop_last_square(&temp))
													 & position->pieces[knight_piece(side)]);
		  }
		}
		if (diagonal_pieces(position, side)) {
		  temp = bishop_attacks(all_pieces(position), aux) & ~all_pieces(position);
		  while (temp) {
			square = pop_last_square(&temp);
			score[side] += 10 * __builtin_popcountll(bishop_attacks(all_pieces(position), square)
													 & diagonal_pieces(position, side));
		  }
		}
		if (heavy_pieces(position, side)) {
		  temp = rook_attacks(all_pieces(position), aux) & ~all_pieces(position);
		  while (temp) {
			square = pop_last_square(&temp);
			score[side] += 10 * __builtin_popcountll(rook_attacks(all_pieces(position), square)
													 & heavy_pieces(position, side));
		  }
		}
		temp = king_attacks(aux);
		while (temp) {
		  square = pop_last_square(&temp);
		  score[side] += 10 * max((1 + __builtin_popcountll(attack_to(position, side, all_pieces(position), square))
								   - __builtin_popcountll(attack_to(position, !side, all_pieces(position), square))), 0);
		}
	  } else {
		temp = pawn_center_square;
		assert(temp);
		i = 7;
		while (temp) {
		  square = pop_last_square(&temp);
		  i = min(i, chebyshev_distance(square, aux));
		}
		score[side] += 20 * i;
	  }
	  score[side] -= 5 * __builtin_popcountll((position->sides[side] ^ position->pieces[king_piece(side)]) & ~control[side]);
	  if (position->pieces[pawn_piece(side)]) {
		score[side] += 5;
		in_island = false;
	  }
	  opened = bonus = 0;
	  king_sideways_bonus = false;
	  for (i = 0; i < 8; i++) {
		if (heavy_pieces(position, side)) {
		  if (!king_sideways_bonus
			  && material_for_attack(position, side)
			  && semiopen_file(position, side, i)
			  && semiopen_file(position, !side, i)
			  && (rook_attacks(all_pawns(position), get_single_square(file[i] & rank[aux / 8]))
				  & position->pieces[king_piece(!side)])
			  && !(attack_to(position, !side, all_pieces(position), get_single_square(file[i] & rank[aux / 8]))
				   | (position->pieces[king_piece(!side)] & king_attacks(get_single_square(file[i] & rank[aux / 8]))))) {
			score[side] += 10;
			king_sideways_bonus = true;
			if (!(~rank[aux / 8] & king_attacks(aux) & ~position->pieces[!side] & ~control[side])) {
			  score[side] += 10;
			}
		  }
		  if (material_for_attack(position, side) && (file[i] & king_attacks(aux))) {
			if (semiopen_file(position, side, i)) {
			  score[side] += 5;
			  if (file[i] & position->pieces[king_piece(!side)]) {
				score[side] += 5;
			  }
			}
			if (semiopen_file(position, !side, i)) {
			  score[side] += 10;
			  if (file[i] & position->pieces[king_piece(!side)]) {
				score[side] += 10;
			  }
			  if (file[i] & heavy_pieces(position, side)) {
				score[side] += 5;
				if (more_than_one(file[i] & heavy_pieces(position, side))) {
				  score[side] += 20;
				}
			  }
			}
		  } else if (semiopen_file(position, side, i) && semiopen_file(position, !side, i)) {
			bonus += 5;
			if (file[i] & heavy_pieces(position, side)) {
			  bonus += 5;
			  if (more_than_one(file[i] & heavy_pieces(position, side))) {
				bonus += 10;
			  }
			}
			opened++;
		  }
		}
		if (file[i] & position->pieces[pawn_piece(side)]) {
		  if (!in_island) {
			score[side] -= 5;
			in_island = true;
		  }
		  if (more_than_one(file[i] & position->pieces[pawn_piece(side)])) {
			score[side] -= 10 * __builtin_popcountll(file[i] & position->pieces[pawn_piece(side)]);
			if (i > 1 || i < 6) {
			  score[side] -= 5;
			}
		  }
		} else {
		  in_island = false;
		}
	  }
	  if (opened < 3) {
		score[side] += bonus;
	  }
	  if (position->pieces[queen_piece(side)] || (position->pieces[bishop_piece(side)] & squares[WHITE])) {
		opened = 0;
		bonus = 0;
		for (i = 5; i <= 9; i += 2) {
		  if (open_diagonal(position, side, i)) {
			if (material_for_attack(position, side)
				&& (diagonals[i] & king_attacks(aux))
				&& (!(diagonals[i] & position->pieces[bishop_piece(!side)]))) {
			  score[side] += 20;
			  if (diagonals[i] & position->pieces[king_piece(!side)]) {
				score[side] += 5;
			  }
			} else {
			  opened++;
			  bonus += 5;
			  if (diagonals[i] & position->pieces[bishop_piece(side)]) {
				bonus += 5;
			  }
			}
		  }
		}
		for (i = 19; i <= 25; i += 2) {
		  if (open_diagonal(position, side, i)) {
			if (material_for_attack(position, side)
				&& (diagonals[i] & king_attacks(aux))
				&& (!(diagonals[i] & position->pieces[bishop_piece(!side)]))) {
			  score[side] += 20;
			  if (diagonals[i] & position->pieces[king_piece(!side)]) {
				score[side] += 5;
			  }
			} else {
			  opened++;
			  bonus += 5;
			  if (diagonals[i] & position->pieces[bishop_piece(side)]) {
				bonus += 5;
			  }
			}
		  }
		}
		if (opened < 4) {
		  score[side] += bonus;
		}
	  }
	  if (position->pieces[queen_piece(side)] || (position->pieces[bishop_piece(side)] & squares[BLACK])) {
		opened = 0;
		bonus = 0;
		for (i = 4; i <= 10; i += 2) {
		  if (open_diagonal(position, side, i)) {
			if (material_for_attack(position, side)
				&& (diagonals[i] & king_attacks(aux))
				&& (!(diagonals[i] & position->pieces[bishop_piece(!side)]))) {
			  score[side] += 20;
			  if (diagonals[i] & position->pieces[king_piece(!side)]) {
				score[side] += 5;
			  }
			} else {
			  opened++;
			  bonus += 5;
			  if (diagonals[i] & position->pieces[bishop_piece(side)]) {
				bonus += 5;
			  }
			}
		  }
		}
		for (i = 20; i <= 24; i += 2) {
		  if (open_diagonal(position, side, i)) {
			if (material_for_attack(position, side)
				&& (diagonals[i] & king_attacks(aux))
				&& (!(diagonals[i] & position->pieces[bishop_piece(!side)]))) {
			  score[side] += 20;
			  if (diagonals[i] & position->pieces[king_piece(!side)]) {
				score[side] += 5;
			  }
			} else {
			  opened++;
			  bonus += 5;
			  if (diagonals[i] & position->pieces[bishop_piece(side)]) {
				bonus += 5;
			  }
			}
		  }
		}
		if (opened < 4) {
		  score[side] += bonus;
		}
	  }
	  score[side] += 10 * __builtin_popcountll(position->pieces[pawn_piece(side)] & CENTER);
	  passed_pawns = 0;
	  temp = position->pieces[pawn_piece(side)];
	  while (temp) {
		square = (side ? pop_last_square(&temp) : pop_first_square(&temp));
		score[side] += pawn_square_table[side][square];
		if (backwards_pawn(position, side, square)) {
		  if (isolated_pawn(position, side, square)) {
			if (square % 8 > 1 && square % 8 < 6) {
			  score[side] -= 5;
			}
			if (__builtin_popcountll(pieces_not_pawns(position, !side)) - __builtin_popcountll(pieces_not_pawns(position, side)) > 0) {
			  score[side] -= 10;
			}
		  }
		  if (heavy_pieces(position, !side)) {
			if (semiopen_file(position, !side, square % 8)
				&& ((1ULL << square_in_front_of_the_pawn(side, square)) & control[!side])
				&& (side ? (square < 48 || ((1ULL << square - 16) & control[!side]))
					: (square > 15 || ((1ULL << square + 16) & control[!side])))) {
				score[side] -= 20;
			}
		  }
		}
		if (passed_pawn(position, side, square)) {
		  passed_pawns |= (1ULL << square);
		  if (!(file_in_front[side][square] & position->pieces[pawn_piece(side)])) {
			score[side] += passed_pawn_square_table[side][square];
			if (pawn_is_blocked(position, side, square)) {
			  score[side] -= 10;
			} else if (file_in_front[side][square] & (all_pieces(position) ^ position->pieces[king_piece(side)])) {
			  score[side] -= 5;
			}
			if (pawn_is_in_chain(position, square, passed_pawns)) {
			  score[side] += 20;
			}
			if (file_in_front[!side][square] & position->pieces[rook_piece(side)]) {
			  score[side] += 5;
			}
			if (!pieces_not_pawns(position, !side)
				&& !king_in_pawn_transformation_square(position, side, side_to_move == side ? square : square_behind_the_pawn(side, square))) {
			  score[side] += evals[WHITE_QUEEN] - evals[WHITE_PAWN];
			}
		  }
		} else {
		  if (candidate_passer(position, side, square)) {
			if (__builtin_popcountll(free_pawn_index[!side][square_in_front_of_the_pawn(side, square)]
									 & position->pieces[pawn_piece(side)])
				> __builtin_popcountll(free_pawn_index[side][square]
									   & position->pieces[pawn_piece(!side)])) {
			  score[side] += 10;
			}
		  }
		}
	  }
	}
	all_score += score[side_to_move] - score[!side_to_move];
	if (!(position->pieces[WHITE_PAWN] | position->pieces[WHITE_ROOK] | position->pieces[WHITE_QUEEN])
		&& ((!position->pieces[WHITE_BISHOP] && __builtin_popcountll(position->pieces[WHITE_KNIGHT]) < 3)
			|| (!position->pieces[WHITE_KNIGHT] && __builtin_popcountll(position->pieces[WHITE_BISHOP]) < 2))) {
	  max_score[WHITE] = 0;
	} else {
	  max_score[WHITE] = evals[BLACK_KING];
	}
	if (!(position->pieces[BLACK_PAWN] | position->pieces[BLACK_ROOK] | position->pieces[BLACK_QUEEN])
		&& ((!position->pieces[BLACK_BISHOP] && __builtin_popcountll(position->pieces[BLACK_KNIGHT]) < 3)
			|| (!position->pieces[BLACK_KNIGHT] && __builtin_popcountll(position->pieces[BLACK_BISHOP]) < 2))) {
	  max_score[BLACK] = 0;
	} else {
	  max_score[BLACK] = evals[WHITE_KING];
	}
	return all_score >= 0 ? min(all_score, max_score[side_to_move]) : max(all_score, -max_score[!side_to_move]);
  }
}

void add_move(const struct POSITION *const position,
			  const unsigned int side,
			  const unsigned char begin_square,
			  const unsigned char end_square,
			  const unsigned char flag,
			  struct STACK *const stack) {
  assert(side < 2);
  assert(begin_square < 64);
  assert(end_square < 64);
  assert(stack->number_of_moves >= 0);
  stack->moves->data = pack_move(begin_square, end_square, flag);
  if (stack->moves->data == stack->hash_move) {
	stack->moves->score = 1000;
  } else {
	stack->moves->score = evaluate_move(position,
										side,
										begin_square,
										end_square,
										flag);
	if (is_killer_move(stack, stack->moves)) {
	  stack->moves->score += 60;
	}
  }
  stack->number_of_moves++;
  stack->moves++;
}

void add_move_with_transformation(const struct POSITION *const position,
								  const unsigned int side,
								  const unsigned char begin_square,
								  const unsigned char end_square,
								  struct STACK *const stack) {
  assert(side < 2);
  assert(begin_square < 64);
  assert(end_square < 64);
  assert(stack->number_of_moves >= 0);
  assert(is_piece_on_board(position, begin_square, pawn_piece(side)));
  if (end_square < 8 || end_square > 55) {
	assert((end_square < 8 && side == BLACK) || (end_square > 55 && side == WHITE));
	stack->moves->data = pack_move(begin_square, end_square, FLAG_TRANSFORMATION_QUEEN);
	stack->moves->score = evals[WHITE_QUEEN] - evals[WHITE_PAWN] + evals[get_piece_on_board(position, end_square)];
	stack->moves++;
	stack->moves->data = pack_move(begin_square, end_square, FLAG_TRANSFORMATION_ROOK);
	stack->moves->score = 0;
	stack->moves++;
	stack->moves->data = pack_move(begin_square, end_square, FLAG_TRANSFORMATION_BISHOP);
	stack->moves->score = 0;
	stack->moves++;
	stack->moves->data = pack_move(begin_square, end_square, FLAG_TRANSFORMATION_KNIGHT);
	stack->moves->score = 0;
	stack->moves++;
	stack->number_of_moves += 4;
  } else {
	add_move(position, side, begin_square, end_square, NONE, stack);
  }
}

void get_pawn_attacks(const struct POSITION *const position,
					  struct STACK *const stack,
					  const unsigned int side) {
  assert(side < 2);
  unsigned char square;
  U64 attacked, temp;
  temp = position->pieces[pawn_piece(side)];
  while (temp) {
	square = pop_last_square(&temp);
	attacked = pawn_attacks(square, pawn_piece(side)) & position->sides[!side];
	while (attacked) {
	  add_move_with_transformation(position, side, square, pop_last_square(&attacked), stack);
	}
  }
  if (is_en_passent_target_square(position)) {
	attacked = pawn_attacks(position->extra.en_passent_target_square, pawn_piece(!side))
	  & position->pieces[pawn_piece(side)];
	while (attacked) {
	  add_move(position, side, pop_last_square(&attacked), position->extra.en_passent_target_square, FLAG_EN_PASSENT, stack);
	}
  }
}

void get_pawn_transformation_moves(const struct POSITION *const position,
								   struct STACK *const stack,
								   const unsigned int side) {
  assert(side < 2);
  unsigned char square;
  U64 temp;
  temp = (side ? (position->pieces[BLACK_PAWN] & SECOND_RANK) >> 8 : (position->pieces[WHITE_PAWN] & SEVENTH_RANK) << 8) & ~all_pieces(position);
  while (temp) {
	square = pop_last_square(&temp);
	add_move_with_transformation(position, side, square + (side ? 8 : -8), square, stack);
  }
}

void get_pawn_moves(const struct POSITION *const position,
					struct STACK *const stack,
					const unsigned int side) {
  assert(side < 2);
  unsigned char square;
  U64 attacked, temp;
  attacked = temp = (side ? (position->pieces[BLACK_PAWN] & ~SECOND_RANK) >> 8 : (position->pieces[WHITE_PAWN] & ~SEVENTH_RANK) << 8) & ~all_pieces(position);
  while (temp) {
	square = pop_last_square(&temp);
	add_move(position, side, square + (side ? 8 : -8), square, NONE, stack);
  }
  temp = (side ? (attacked & SIXTH_RANK) >> 8 : (attacked & THIRD_RANK) << 8) & ~all_pieces(position);
  while (temp) {
	square = pop_last_square(&temp);
	add_move(position, side, square + (side ? 16 : -16), square, FLAG_PAWN_DOUBLE_MOVE, stack);
  }
}

void get_moves_to_square(const struct POSITION *const position,
						 struct STACK *const stack,
						 const unsigned int side,
						 const unsigned char square,
						 U64 attackers) {
  assert(side < 2);
  assert(square < 64);
  while (attackers) {
	add_move(position, side, pop_last_square(&attackers), square, NONE, stack);
  }
}

void get_king_castle_moves(const struct POSITION *const position,
						   struct STACK *const stack,
						   const unsigned int side) {
  assert(side < 2);
  const unsigned char square = get_single_square(position->pieces[king_piece(side)]);
  switch (square) {
  case 4:
	if (read_flag(position->extra, FLAG_CASTLE_KING_WHITE)
		&& !(all_pieces(position) & 96ULL)
		&& !(position->pieces[BLACK_KING] & (1ULL << 14))
		&& !(attack_to(position, BLACK, all_pieces(position), 4)
			 | attack_to(position, BLACK, all_pieces(position), 5)
			 | attack_to(position, BLACK, all_pieces(position), 6))) {
	  add_move(position, WHITE, 4, 6, FLAG_CASTLE_KING_WHITE, stack);
	}
	if (read_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE)
		&& !(all_pieces(position) & 14ULL)
		&& !(position->pieces[BLACK_KING] & ((1ULL << 9) | (1ULL << 10)))
		&& !(attack_to(position, BLACK, all_pieces(position), 1)
			 | attack_to(position, BLACK, all_pieces(position), 2)
			 | attack_to(position, BLACK, all_pieces(position), 3)
			 | attack_to(position, BLACK, all_pieces(position), 4))) {
	  add_move(position, WHITE, 4, 2, FLAG_CASTLE_QUEEN_WHITE, stack);
	}
	break;
  case 60:
	if (read_flag(position->extra, FLAG_CASTLE_KING_BLACK)
		&& !(all_pieces(position) & 6917529027641081856ULL)
		&& !(position->pieces[WHITE_KING] & (1ULL << 54))
		&& !(attack_to(position, WHITE, all_pieces(position), 60)
			 | attack_to(position, WHITE, all_pieces(position), 61)
			 | attack_to(position, WHITE, all_pieces(position), 62))) {
	  add_move(position, BLACK, 60, 62, FLAG_CASTLE_KING_BLACK, stack);
	}
	if (read_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK)
		&& !(all_pieces(position) & 1008806316530991104ULL)
		&& !(position->pieces[WHITE_KING] & ((1ULL << 49) | (1ULL << 50)))
		&& !(attack_to(position, WHITE, all_pieces(position), 57)
			 | attack_to(position, WHITE, all_pieces(position), 58)
			 | attack_to(position, WHITE, all_pieces(position), 59)
			 | attack_to(position, WHITE, all_pieces(position), 60))) {
	  add_move(position, BLACK, 60, 58, FLAG_CASTLE_QUEEN_BLACK, stack);
	}
  }
}

void get_king_moves(const struct POSITION *const position,
					struct STACK *const stack,
					const unsigned int side) {
  assert(side < 2);
  unsigned char i;
  const unsigned char square = get_single_square(position->pieces[king_piece(side)]);
  U64 attacked = king_attacks(square) & ~position->sides[side];
  while (attacked) {
	i = pop_last_square(&attacked);
	if (!attack_to(position, !side, all_pieces(position) ^ position->pieces[king_piece(side)], i)
		&& !(position->pieces[king_piece(!side)] & king_attacks(i))) {
	  add_move(position, side, square, i, NONE, stack);
	}
  }
}

void get_moves_in_check(const struct POSITION *const position,
						struct STACK *const stack,
						const unsigned int side) {
  assert(side < 2);
  U64 temp;
  unsigned char king, square, piece, j, i, k;
  const U64 attackers = king_attackers(position, side, all_pieces(position));
  get_king_moves(position, stack, side);
  if (!more_than_one(attackers)) {
	square = get_single_square(attackers);
	get_moves_to_square(position, stack, side, square, attack_to(position, side, all_pieces(position), square)
						& ~position->pieces[pawn_piece(side)]);
	temp = pawn_attacks(square, pawn_piece(!side)) & position->pieces[pawn_piece(side)];
	while (temp) {
	  add_move_with_transformation(position, side, pop_last_square(&temp), square, stack);
	}
	if (is_en_passent_target_square(position)
		&& square == square_behind_the_pawn(side, position->extra.en_passent_target_square)) {
	  temp = pawn_attacks(position->extra.en_passent_target_square, pawn_piece(!side))
		& position->pieces[pawn_piece(side)];
	  while (temp) {
		add_move(position,
				 side,
				 pop_last_square(&temp),
				 position->extra.en_passent_target_square,
				 FLAG_EN_PASSENT,
				 stack);
	  }
	}
	piece = get_piece_on_board(position, square);
	king = get_single_square(position->pieces[king_piece(side)]);
	assert(piece != king_piece(!side));
	if (piece == bishop_piece(!side)
		|| (piece == queen_piece(!side)
			&& (attack_from(all_pieces(position), square, WHITE_BISHOP)
				& (1ULL << king)))) {
	  temp = attack_from(all_pieces(position), square, WHITE_BISHOP)
		& attack_from(all_pieces(position), king, WHITE_BISHOP);
	} else if (piece == rook_piece(!side)
			   || (piece == queen_piece(!side)
				   && (attack_from(all_pieces(position), square, WHITE_ROOK)
					   & (1ULL << king)))) {
	  temp = attack_from(all_pieces(position), square, WHITE_ROOK)
		& attack_from(all_pieces(position), king, WHITE_ROOK);
	}
	while (temp) {
	  j = pop_last_square(&temp);
	  get_moves_to_square(position, stack, side, j, attack_to(position, side, all_pieces(position), j)
						  & ~position->pieces[pawn_piece(side)]);
	  i = j + (side ? 8 : -8);
	  if ((1ULL << i) & position->pieces[pawn_piece(side)]) {
		add_move_with_transformation(position, side, i, j, stack);
	  } else {
		k = i + (side ? 8 : -8);
		if (((1ULL << k) & position->pieces[pawn_piece(side)])
			&& !is_any_on_board(position, i)
			&& j / 8 == (side ? 6 : 1)) {
		  add_move(position, side, k, j, FLAG_PAWN_DOUBLE_MOVE, stack);
		}
	  }
	}
  }
}

void generate_all_moves(const struct POSITION *const position,
						struct STACK *const stack,
						const unsigned int side) {
  assert(side < 2);
  assert(!stack->number_of_moves);
  unsigned char i, j;
  U64 temp, pieces;
  if (read_flag(position->extra, FLAG_CHECK)) {
	get_moves_in_check(position, stack, side);
  } else {
	for (j = knight_piece(side); j < king_piece(side); j++) {
	  temp = position->pieces[j];
	  while (temp) {
		i = pop_last_square(&temp);
		pieces = attack_from(all_pieces(position), i, j) & ~position->sides[side];
		while (pieces) {
		  add_move(position, side, i, pop_last_square(&pieces), NONE, stack);
		}
	  }
	}
	get_pawn_attacks(position, stack, side);
	get_pawn_transformation_moves(position, stack, side);
	get_pawn_moves(position, stack, side);
	get_king_castle_moves(position, stack, side);
	get_king_moves(position, stack, side);
  }
}

void get_attack_moves(const struct POSITION *const position,
					  struct STACK *const stack,
					  const unsigned int side) {
  assert(side < 2);
  unsigned char i, j;
  const unsigned char square = get_single_square(position->pieces[king_piece(side)]);
  U64 temp, pieces, attacked = king_attacks(square) & position->sides[!side];
  get_pawn_attacks(position, stack, side);
  for (j = knight_piece(side); j < king_piece(side); j++) {
	temp = position->pieces[j];
	while (temp) {
	  i = pop_last_square(&temp);
	  pieces = attack_from(all_pieces(position), i, j) & position->sides[!side];
	  while (pieces) {
		add_move(position, side, i, pop_last_square(&pieces), NONE, stack);
	  }
	}
  }
  while (attacked) {
	i = pop_last_square(&attacked);
	if (!attack_to(position, !side, all_pieces(position) ^ position->pieces[king_piece(side)], i)
		&& !(position->pieces[king_piece(!side)] & king_attacks(i))) {
	  add_move(position, side, square, i, NONE, stack);
	}
  }
}

void get_check_moves_without_capture(const struct POSITION *const position,
									 struct STACK *const stack,
									 const unsigned int side) {
  assert(side < 2);
  const unsigned char square = get_single_square(position->pieces[king_piece(!side)]);
  unsigned char target;
  U64 temp, aux;
  if (position->pieces[knight_piece(side)]) {
	temp = knight_attacks(square) & ~all_pieces(position);
	while (temp) {
	  target = pop_last_square(&temp);
	  aux = knight_attacks(target) & position->pieces[knight_piece(side)];
	  while (aux) {
		add_move(position, side, pop_last_square(&aux), target, NONE, stack);
	  }
	}
  }
  if (diagonal_pieces(position, side)) {
	temp = bishop_attacks(all_pieces(position), square) & ~all_pieces(position);
	while (temp) {
	  target = pop_last_square(&temp);
	  aux = bishop_attacks(all_pieces(position), target) & diagonal_pieces(position, side);
	  while (aux) {
		add_move(position, side, pop_last_square(&aux), target, NONE, stack);
	  }
	}
  }
  if (heavy_pieces(position, side)) {
	temp = rook_attacks(all_pieces(position), square) & ~all_pieces(position);
	while (temp) {
	  target = pop_last_square(&temp);
	  aux = rook_attacks(all_pieces(position), target) & heavy_pieces(position, side);
	  while (aux) {
		add_move(position, side, pop_last_square(&aux), target, NONE, stack);
	  }
	}
  }
  if (side ? square < 40 : square > 23) {
	aux = temp = (side ? (position->pieces[BLACK_PAWN] & rank[(square / 8) + 2] & ~file[square % 8]) >> 8
				  : (position->pieces[WHITE_PAWN] & rank[(square / 8) - 2] & ~file[square % 8]) << 8)
	  & ~all_pieces(position);
	while (temp) {
	  target = pop_last_square(&temp);
	  add_move(position, side, target + (side ? 8 : -8), target, NONE, stack);
	}
	temp = (side ? (aux & SIXTH_RANK) >> 8 : (aux & THIRD_RANK) << 8)
	  & ~all_pieces(position);
	while (temp) {
	  target = pop_last_square(&temp);
	  add_move(position, side, target + (side ? 16 : -16), target, FLAG_PAWN_DOUBLE_MOVE, stack);
	}
  }
}

void do_castle(struct POSITION *const position,
			   struct STACK *const stack,
			   const unsigned int side,
			   const unsigned char king_piece,
			   const unsigned char king_begin_square,
			   const unsigned char king_end_square,
			   const unsigned char rook_piece,
			   const unsigned char rook_begin_square,
			   const unsigned char rook_end_square) {
  assert(side < 2);
  assert(king_piece == king_piece(side));
  assert(king_begin_square < 64);
  assert(king_end_square < 64);
  assert(rook_piece == rook_piece(side));
  assert(rook_begin_square < 64);
  assert(rook_end_square < 64);
  position->extra.en_passent_target_square = NO_EN_PASSENT_TARGET_SQUARE;
  position->extra.hash ^= zobrist_board[rook_begin_square][rook_piece]
	^ zobrist_board[rook_begin_square][NONE]
	^ zobrist_board[rook_end_square][NONE]
	^ zobrist_board[rook_end_square][rook_piece]
	^ zobrist_board[king_end_square][NONE]
	^ zobrist_board[king_end_square][king_piece];
  move_piece(position, king_begin_square, king_end_square, king_piece, side);
  move_piece(position, rook_begin_square, rook_end_square, rook_piece, side);
  stack->other_piece = NONE;
  position->extra.halfmove_clock = 0;
  if (is_check_from_piece(position, side, rook_end_square, rook_piece)) {
	set_flag(position->extra, FLAG_CHECK);
  } else {
	clear_flag(position->extra, FLAG_CHECK);
  }
}

void do_move(struct POSITION *const position,
			 const unsigned int side,
			 struct STACK *const stack) {
  assert(side < 2);
  const unsigned char begin_square = get_move_begin_square(stack->moves);
  assert(begin_square < 64);
  const unsigned char end_square = get_move_end_square(stack->moves);
  assert(end_square < 64);
  const unsigned char flag = get_move_flag(stack->moves);
  const unsigned char piece = get_piece_on_board(position, begin_square);
  const unsigned char end_piece = get_end_piece(side, flag);
  assert(stack->number_of_moves > 0);
  assert(is_color_on_board(position, begin_square, side));
  stack->extra = position->extra;
  position->extra.hash ^= zobrist_side[WHITE]
	^ zobrist_side[BLACK]
	^ zobrist_board[begin_square][piece]
	^ zobrist_board[begin_square][NONE];
  if (is_en_passent_target_square(position)) {
	position->extra.hash ^= zobrist_board[position->extra.en_passent_target_square][13];
  }
  position->extra.static_score = -position->extra.static_score;
  if (end_piece) {
	position->extra.static_score += evals[piece] - evals[end_piece];
  }
  switch (flag) {
  case FLAG_CASTLE_KING_WHITE:
	do_castle(position, stack, side, WHITE_KING, 4, 6, WHITE_ROOK, 7, 5);
	if (read_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE)) {
	  position->extra.hash ^= zobrist_queen_rocade[WHITE];
	  clear_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE);
	}
	position->extra.hash ^= zobrist_king_rocade[WHITE];
	clear_flag(position->extra, FLAG_CASTLE_KING_WHITE);
	return;
  case FLAG_CASTLE_QUEEN_WHITE:
	do_castle(position, stack, side, WHITE_KING, 4, 2, WHITE_ROOK, 0, 3);
	if (read_flag(position->extra, FLAG_CASTLE_KING_WHITE)) {
	  position->extra.hash ^= zobrist_king_rocade[WHITE];
	  clear_flag(position->extra, FLAG_CASTLE_KING_WHITE);
	}
	position->extra.hash ^= zobrist_queen_rocade[WHITE];
	clear_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE);
	return;
  case FLAG_CASTLE_KING_BLACK:
	do_castle(position, stack, side, BLACK_KING, 60, 62, BLACK_ROOK, 63, 61);
	if (read_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK)) {
	  position->extra.hash ^= zobrist_queen_rocade[BLACK];
	  clear_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK);
	}
	position->extra.hash ^= zobrist_king_rocade[BLACK];
	clear_flag(position->extra, FLAG_CASTLE_KING_BLACK);
	return;
  case FLAG_CASTLE_QUEEN_BLACK:
	do_castle(position, stack, side, BLACK_KING, 60, 58, BLACK_ROOK, 56, 59);
	if (read_flag(position->extra, FLAG_CASTLE_KING_BLACK)) {
	  position->extra.hash ^= zobrist_king_rocade[BLACK];
	  clear_flag(position->extra, FLAG_CASTLE_KING_BLACK);
	}
	position->extra.hash ^= zobrist_queen_rocade[BLACK];
	clear_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK);
	return;
  }
  if (flag == FLAG_EN_PASSENT) {
	stack->other_piece = pawn_piece(!side);
	stack->other_piece_square = square_behind_the_pawn(side, end_square);
	position->extra.static_score -= evals[stack->other_piece];
	position->extra.hash ^= zobrist_board[stack->other_piece_square][stack->other_piece]
	  ^ zobrist_board[stack->other_piece_square][NONE]
	  ^ zobrist_board[end_square][NONE]
	  ^ zobrist_board[end_square][piece];
	remove_piece_from_board(position, stack->other_piece_square, stack->other_piece, !side);
	position->extra.halfmove_clock = 0;
	position->extra.en_passent_target_square = NO_EN_PASSENT_TARGET_SQUARE;
	move_piece(position, begin_square, end_square, piece, side);
  } else {
	stack->other_piece = get_piece_on_board(position, end_square);
	if (is_capture(stack)) {
	  assert(is_color_on_board(position, end_square, !side));
	  stack->other_piece_square = end_square;
	  position->extra.static_score -= evals[stack->other_piece];
	  remove_piece_from_board(position, stack->other_piece_square, stack->other_piece, !side);
	  position->extra.halfmove_clock = 0;
	  position->extra.en_passent_target_square = NO_EN_PASSENT_TARGET_SQUARE;
	  position->extra.hash ^= zobrist_board[end_square][stack->other_piece]
		^ zobrist_board[end_square][NONE];
	  if (read_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE) && end_square == 0) {
		assert(side == BLACK);
		clear_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE);
		position->extra.hash ^= zobrist_queen_rocade[WHITE];
	  } else if (read_flag(position->extra, FLAG_CASTLE_KING_WHITE) && end_square == 7) {
		assert(side == BLACK);
		clear_flag(position->extra, FLAG_CASTLE_KING_WHITE);
		position->extra.hash ^= zobrist_king_rocade[WHITE];
	  } else if (read_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK) && end_square == 56) {
		assert(side == WHITE);
		clear_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK);
		position->extra.hash ^= zobrist_queen_rocade[BLACK];
	  } else if (read_flag(position->extra, FLAG_CASTLE_KING_BLACK) && end_square == 63) {
		assert(side == WHITE);
		clear_flag(position->extra, FLAG_CASTLE_KING_BLACK);
		position->extra.hash ^= zobrist_king_rocade[BLACK];
	  }
	} else if (piece == pawn_piece(side)) {
	  position->extra.halfmove_clock = 0;
	  if (flag == FLAG_PAWN_DOUBLE_MOVE) {
		position->extra.en_passent_target_square = square_behind_the_pawn(side, end_square);
		position->extra.hash ^= zobrist_board[position->extra.en_passent_target_square][13];
	  } else {
		position->extra.en_passent_target_square = NO_EN_PASSENT_TARGET_SQUARE;
	  }
	} else {
	  position->extra.halfmove_clock++;
	  position->extra.en_passent_target_square = NO_EN_PASSENT_TARGET_SQUARE;
	}
	if (read_flag(position->extra, FLAG_CASTLE_KING_WHITE)) {
	  if (piece == WHITE_KING || (begin_square == 7 && piece == WHITE_ROOK)) {
		assert(side == WHITE);
		clear_flag(position->extra, FLAG_CASTLE_KING_WHITE);
		position->extra.hash ^= zobrist_king_rocade[WHITE];
		position->extra.halfmove_clock = 0;
	  }
	}
	if (read_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE)) {
	  if (piece == WHITE_KING || (begin_square == 0 && piece == WHITE_ROOK)) {
		assert(side == WHITE);
		clear_flag(position->extra, FLAG_CASTLE_QUEEN_WHITE);
		position->extra.hash ^= zobrist_queen_rocade[WHITE];
		position->extra.halfmove_clock = 0;
	  }
	}
	if (read_flag(position->extra, FLAG_CASTLE_KING_BLACK)) {
	  if (piece == BLACK_KING || (begin_square == 63 && piece == BLACK_ROOK)) {
		assert(side == BLACK);
		clear_flag(position->extra, FLAG_CASTLE_KING_BLACK);
		position->extra.hash ^= zobrist_king_rocade[BLACK];
		position->extra.halfmove_clock = 0;
	  }
	}
	if (read_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK)) {
	  if (piece == BLACK_KING || (begin_square == 56 && piece == BLACK_ROOK)) {
		assert(side == BLACK);
		clear_flag(position->extra, FLAG_CASTLE_QUEEN_BLACK);
		position->extra.hash ^= zobrist_queen_rocade[BLACK];
		position->extra.halfmove_clock = 0;
	  }
	}
	move_piece(position,
			   begin_square,
			   end_square,
			   piece,
			   side);
	if (end_piece) {
	  change_piece(position, end_square, piece, end_piece, side);
	  position->extra.hash ^= zobrist_board[end_square][NONE]
		^ zobrist_board[end_square][end_piece];
	} else {
	  position->extra.hash ^= zobrist_board[end_square][NONE]
		^ zobrist_board[end_square][piece];
	}
  }
  if (king_attackers(position, !side, all_pieces(position))) {
	set_flag(position->extra, FLAG_CHECK);
  } else {
	clear_flag(position->extra, FLAG_CHECK);
  }
}

void uci_print_move(const struct MOVE *const move) {
  const unsigned char end_piece = get_end_piece(BLACK, get_move_flag(move));
  fprintf(uci_out,
		  "%.2s%.2s%c",
		  notation[get_move_begin_square(move)],
		  notation[get_move_end_square(move)],
		  piece_to_letter[end_piece]);
  if (end_piece) {
	fprintf(uci_out, " ");
  }
}

void check_time(const unsigned long current_time) {
  assert(!pondermove);
  if (move_time < current_time - start_time + iteration_duration) {
	if (abs(bestmove[bestmove_read].best_line->score - reference_score) > significant_change(reference_score)) {
	  reference_score = bestmove[bestmove_read].best_line->score;
	  move_time = min(move_time * 2, max_time);
	  if (move_time < current_time - start_time + iteration_duration) {
		can_run = false;
	  }
	} else {
	  can_run = false;
	}
  }
}

void publish_info(const struct STACK *const stack,
					  const unsigned char depth) {
  assert(depth > 0);
  if (stack->best_line_length && (depth > bestmove[bestmove_read].depth
								  || (stack->score > bestmove[bestmove_read].best_line->score
									  && depth == bestmove[bestmove_read].depth
									  && move_number(stack) > bestmove[bestmove_read].nodes))) {
	pthread_mutex_lock(&parallel_lock);
	if (depth > bestmove[bestmove_read].depth
		|| (stack->score != bestmove[bestmove_read].best_line->score
			&& depth == bestmove[bestmove_read].depth
			&& move_number(stack) > bestmove[bestmove_read].nodes)) {
	  bestmove[(bestmove_read + 1)].nodes = move_number(stack);
	  bestmove[(bestmove_read + 1)].best_line_length = stack->best_line_length;
	  memcpy(bestmove[(bestmove_read + 1)].best_line, stack->best_line, stack->best_line_length * MOVE_SIZE);
	  bestmove[(bestmove_read + 1)].best_line->score = stack->score;
	  bestmove[(bestmove_read + 1)].depth = depth;
	  bestmove_read++;
	  if (bestmove_read == MAX_BESTMOVES) {
		bestmove_read = 0;
	  }
	  pthread_cond_signal(&parallel_cond);
	}
	pthread_mutex_unlock(&parallel_lock);
  }
}

void *uci_publish_info(void *args) {
  unsigned long think_time, now;
  int i;
  while (1) {
	pthread_mutex_lock(&parallel_lock);
	pthread_cond_wait(&parallel_cond, &parallel_lock);
	pthread_mutex_unlock(&parallel_lock);
	if (can_run && bestmove_write < bestmove_read) {
	  now = get_time_in_milliseconds();
	  iteration_duration = now - iteration_start;
	  iteration_start = now;
	  while (bestmove_write < bestmove_read) {
		bestmove_write++;
		if (bestmove_write == MAX_BESTMOVES) {
		  bestmove_write = 0;
		}
		if (bestmove[bestmove_write].depth == 1) {
		  reference_score = bestmove[bestmove_write].best_line->score;
		}
		think_time = now - start_time;
		fprintf(uci_out,
				"info depth %d time %ld nodes %ld ",
				bestmove[bestmove_write].depth,
				think_time,
				nodes);
		if (think_time) {
		  fprintf(uci_out, "nps %ld ", (long) (nodes / (((double) think_time) / 1000)));
		}
		fprintf(uci_out, "score ");
		if (is_opponent_mate_score(bestmove[bestmove_write].best_line->score)) {
		  fprintf(uci_out, "mate %d", 1 + (evals[WHITE_KING] - bestmove[bestmove_write].best_line->score) / 2);
		} else if (is_mate_score(bestmove[bestmove_write].best_line->score)) {
		  fprintf(uci_out, "mate -%d", (evals[WHITE_KING] + bestmove[bestmove_write].best_line->score) / 2);
		} else {
		  fprintf(uci_out, "cp %d", bestmove[bestmove_write].best_line->score);
		}
		fprintf(uci_out, " pv ");
		for (i = 0; i < bestmove[bestmove_write].best_line_length; i++) {
		  uci_print_move(&bestmove[bestmove_write].best_line[i]);
		}
		fprintf(uci_out, "\n");
		/*
		  FILE *file = fopen("out.txt", "a");
		  fprintf(file, "%d %d: ", depth, bestmove[bestmove_write].best_line->score);
		  for (i = 0; i < bestmove[bestmove_write].best_line_length; i++) {
		  fprintf(file, "%.2s%.2s ",
		  notation[get_move_begin_square(&bestmove[bestmove_write].best_line[i])],
		  notation[get_move_end_square(&bestmove[bestmove_write].best_line[i])]);
		  }
		  fprintf(file, "\n");
		  fclose(file);
		*/
	  }
	}
	if (!pondermove && bestmove[bestmove_read].depth > 1) {
	  check_time(now);
	}
  }
}

//TODO: don't go below stack start
bool is_repeated(const struct POSITION *const position,
				 struct STACK *const stack) {
  unsigned int i;
  for (i = 4; i <= position->extra.halfmove_clock; i += 2) {
	if ((stack - i)->extra.hash == position->extra.hash) {
	  stack->best_line_length = 1;
	  stack->best_line[0] = (stack - i)->moves[0];
	  return true;
	}
  }
  return false;
}

void quiesce(struct POSITION *const position,
			 const unsigned int side,
			 struct STACK *const stack,
			 const bool check,
			 const bool pv) {
  assert(side < 2);
  struct HASH_TABLE_ENTRY entry;
  get_from_transposition_table(position->extra.hash, &entry);
  if (entry.hash == position->extra.hash) {
	if (entry.flag == FLAG_TRANSPOSITION_EXACT) {
	  stack->best_line_length = 1;
	  stack->best_line[0].data = entry.best_move;
	  stack->score = entry.score;
	  return;
	} else {
	  if (entry.flag == FLAG_TRANSPOSITION_LOWER_BOUND && entry.score > stack->lower_bound) {
		stack->lower_bound = entry.score;
	  } else if (entry.flag == FLAG_TRANSPOSITION_UPPER_BOUND && stack->upper_bound > entry.score) {
		stack->upper_bound = entry.score;
	  }
	  if (stack->lower_bound >= stack->upper_bound) {
		stack->best_line_length = 1;
		stack->best_line[0].data = entry.best_move;
		stack->score = entry.score;
		return;
	  }
	}
  }
  stack->best_line_length = 0;
  if (king_attackers(position, !side, all_pieces(position))) {
	stack->score = evals[WHITE_KING];
	return;
  } else {
	if (read_flag(position->extra, FLAG_CHECK)) {
	  stack->score = stack->lower_bound;
	  initialize_moves(stack);
	  get_moves_in_check(position, stack, side);
	  if (stack->number_of_moves == 0) {
		stack->score = -evals[WHITE_KING];
		return;
	  }
	} else {
	  nodes++;
	  stack->score = evaluate_position(position, side);
	  if (stack->score > stack->lower_bound) {
		stack->lower_bound = stack->score;
		if (stack->score >= stack->upper_bound) {
		  return;
		}
	  }
	  initialize_moves(stack);
	  get_attack_moves(position, stack, side);
	  get_pawn_transformation_moves(position, stack, side);
	  if (!check) {
		get_check_moves_without_capture(position, stack, side);
	  }
	}
	reset_moves(stack);
	sort_moves(stack);
  }
  while (can_run
		 && !no_more_moves(stack)
		 && (stack->moves->score >= 0 || read_flag(position->extra, FLAG_CHECK))) {
	if (!is_pinned(position,
				   side,
				   get_move_begin_square(stack->moves),
				   get_move_end_square(stack->moves),
				   get_move_flag(stack->moves) == FLAG_EN_PASSENT ? all_pieces(position) ^ square_behind_the_pawn(side, get_move_end_square(stack->moves)): all_pieces(position))) {
	  do_move(position, side, stack);
	  (stack + 1)->initial_lower_bound = (stack + 1)->lower_bound = -stack->upper_bound;
	  (stack + 1)->upper_bound = -stack->lower_bound;
	  quiesce(position, !side, stack + 1, check || read_flag(position->extra, FLAG_CHECK), pv);
	  unmove(position, side, stack, get_move_begin_square(stack->moves), get_move_end_square(stack->moves), get_move_flag(stack->moves));
	  if (-(stack + 1)->score > stack->score) {
		take_best_score(stack, stack + 1);
		if (stack->score > stack->lower_bound) {
		  if (pv && stack->score < stack->upper_bound) {
			stack->lower_bound = stack->score;
		  } else {
			break;
		  }
		}
	  }
	} else {
	  stack->moves++;
	}
  }
  put_in_transposition_table(position->extra.hash, stack, 0);
}

void add_killer_move(struct STACK *const stack,
					 const struct MOVE *const move) {
  if (stack->killer_move[0] != move->data && stack->killer_move[1] != move->data) {
	stack->killer_move[2] = stack->killer_move[1];
	stack->killer_move[1] = stack->killer_move[0];
	stack->killer_move[0] = move->data;
  }
}

bool is_safe_to_prune(const struct POSITION *const position,
					  const unsigned int side,
					  const struct STACK *const stack,
					  const unsigned char ply) {
  const unsigned char piece = get_piece_on_board(position, get_move_begin_square(stack->moves));
  const unsigned char square = get_move_end_square(stack->moves);
  return ply > 0
	&& !is_mate_score(stack->score)
	&& !read_flag(position->extra, FLAG_CHECK)
	&& !is_capture_move(position, stack->moves)
	&& !is_check_from_piece(position, side, square, piece)
	&& !is_transformation_move(stack->moves)
	&& ((piece != pawn_piece(side)) || (square & ~(SECOND_RANK | SEVENTH_RANK)));
}

void minmax(struct POSITION *const position,
			const unsigned int side,
			struct STACK *const stack,
			const short lower_bound,
			const short upper_bound,
			const unsigned char ply,
			const unsigned char depth,
			const bool pv) {
  assert(side < 2);
  assert(lower_bound < upper_bound);
  struct HASH_TABLE_ENTRY entry;
  if (king_attackers(position, !side, all_pieces(position))) {
	stack->best_line_length = 0;
	stack->score = evals[WHITE_KING];
	stack->lower_bound = stack->score - 1;
	stack->upper_bound = stack->score + 1;
	return;
  }
  if (ply >= depth) {
	stack->initial_lower_bound = stack->lower_bound = lower_bound;
	stack->upper_bound = upper_bound;
	quiesce(position, side, stack, read_flag(position->extra, FLAG_CHECK), pv);
	return;
  }
  get_from_transposition_table(position->extra.hash, &entry);
  if (pv && depth - ply > 4 && entry.hash != position->extra.hash) {
	minmax(position, side, stack, lower_bound, upper_bound, ply, depth - 2, true);
	get_from_transposition_table(position->extra.hash, &entry);
	assert(!can_run || !stack->best_line_length || entry.hash == position->extra.hash);
	//TODO: reuse moves if they were generated
	//reset_moves(stack);
  }
  initialize_moves(stack);
  stack->initial_lower_bound = stack->lower_bound = lower_bound;
  stack->upper_bound = upper_bound;
  stack->best_line_length = 0;
  stack->score = MIN_SCORE;
  if (entry.hash == position->extra.hash) {
	stack->hash_move = entry.best_move;
	if (!pv && entry.depth >= depth - ply) {
	  if (entry.flag == FLAG_TRANSPOSITION_EXACT) {
		stack->best_line_length = 1;
		stack->best_line[0].data = entry.best_move;
		stack->score = entry.score;
		return;
	  } else {
		if (entry.flag == FLAG_TRANSPOSITION_LOWER_BOUND && entry.score > stack->lower_bound) {
		  stack->lower_bound = entry.score;
		} else if (entry.flag == FLAG_TRANSPOSITION_UPPER_BOUND && stack->upper_bound > entry.score) {
		  stack->upper_bound = entry.score;
		}
		if (stack->lower_bound >= stack->upper_bound) {
		  stack->best_line_length = 1;
		  stack->best_line[0].data = entry.best_move;
		  stack->score = entry.score;
		  add_killer_move(stack, stack->best_line);
		  return;
		}
	  }
	}
  } else {
	stack->hash_move = 0;
  }
  if (stack->number_of_moves == 0) {
	generate_all_moves(position, stack, side);
  }
  if (stack->number_of_moves == 0) {
	if (!read_flag(position->extra, FLAG_CHECK)) {
	  stack->score = 0;
	} else {
	  stack->score = -evals[WHITE_KING];
	}
	return;
  } else {
	reset_moves(stack);
	sort_moves(stack);
  }
  while (can_run && !no_more_moves(stack)) {
	if (!is_pinned(position,
				   side,
				   get_move_begin_square(stack->moves),
				   get_move_end_square(stack->moves),
				   get_move_flag(stack->moves) == FLAG_EN_PASSENT ? all_pieces(position) ^ square_behind_the_pawn(side, get_move_end_square(stack->moves)): all_pieces(position))) {
	  if (is_safe_to_prune(position, side, stack, ply)) {
		if ((position->extra.static_score + stack->moves->score + 200 * (depth - ply) + 128 <= stack->lower_bound)
			|| (depth - ply < 4 && stack->moves->score < 0)
			|| (move_number(stack) > 3 + (depth - ply) * (depth - ply) / 2)) {
		  stack->moves++;
		  continue;
		}
	  }
	  do_move(position, side, stack);
	  if (position->extra.halfmove_clock >= 100 || is_repeated(position, stack + 1)) {
		(stack + 1)->score = 0;
	  } else {
		const unsigned char extension = (read_flag(position->extra, FLAG_CHECK) && stack->moves->score >= 0 ? 0 : -1);
		bool full_search = true;
		if (depth - ply > 4 && move_number(stack) > 3 && !is_capture(stack) && !is_transformation_move(stack->moves)) {
		  minmax(position, !side, stack + 1, -stack->lower_bound - 1, -stack->lower_bound, ply + 1, depth + extension - 2, false);
		  if (-(stack + 1)->score <= stack->lower_bound) {
			full_search = false;
		  }
		}
		if (full_search) {
		  if (!pv || !is_first_move(stack)) {
			minmax(position, !side, stack + 1, -stack->lower_bound - 1, -stack->lower_bound, ply + 1, depth + extension, false);
		  }
		  if (pv && (is_first_move(stack) || (-(stack + 1)->score > stack->lower_bound && (ply == 0 || -(stack + 1)->score < stack->upper_bound)))) {
			//TODO: reuse moves
			//reset_moves(stack + 1);
			minmax(position, !side, stack + 1, -stack->upper_bound, -stack->lower_bound, ply + 1, depth + extension, true);
		  }
		}
	  }
	  unmove(position, side, stack, get_move_begin_square(stack->moves), get_move_end_square(stack->moves), get_move_flag(stack->moves));
	  if (-(stack + 1)->score > stack->score) {
		take_best_score(stack, stack + 1);
		if (stack->score > stack->lower_bound) {
		  if (pv && stack->score < stack->upper_bound) {
			stack->lower_bound = stack->score;
			if (can_run && ply == 0) {
			  publish_info(stack, depth);
			}
		  } else {
			assert(stack->score >= stack->upper_bound);
			add_killer_move(stack, stack->moves - 1);
			break;
		  }
		}
	  }
	} else {
	  stack->moves++;
	}
  }
  if (no_more_moves(stack)
	  && is_mate_score(stack->score)
	  && stack->best_line_length == 0
	  && !read_flag(position->extra, FLAG_CHECK)) {
	stack->score = 0;
  }
  put_in_transposition_table(position->extra.hash, stack, depth - ply);
}

void uci_print_bestmove() {
  if (__sync_fetch_and_and(&first, 0)) {
	fprintf(uci_out, "bestmove ");
	uci_print_move(bestmove[bestmove_read].best_line);
	if (ponder && bestmove[bestmove_read].best_line_length > 1 && !pondermove) {
	  fprintf(uci_out, "ponder ");
	  uci_print_move(&bestmove[bestmove_read].best_line[1]);
	}
	fprintf(uci_out, "\n");
  }
}

void *loop(void *args) {
  struct LOOP_ARGUMENTS *const loop_args = (struct LOOP_ARGUMENTS *) args;
  unsigned int delta = 32;
  unsigned char depth = 1;
  short lower_bound, upper_bound;
  minmax(&loop_args->position, loop_args->side, loop_args->stack, MIN_SCORE, MAX_SCORE, 0, depth, true);
  while (can_run) {
	if (depth >= MAX_DEPTH || (loop_args->stack->number_of_moves < 2 && loop_args->stack->best_line_length > 1)) {
	  can_run = false;
	} else {
	  depth++;
	  lower_bound = max(loop_args->stack->score - delta, MIN_SCORE);
	  upper_bound = min(loop_args->stack->score + delta, MAX_SCORE);
	  //TODO: fix infinite loop
	  do {
		minmax(&loop_args->position, loop_args->side, loop_args->stack, lower_bound, upper_bound, 0, depth, true);
		if (loop_args->stack->score <= lower_bound) {
		  upper_bound = (lower_bound + upper_bound) / 2;
		  lower_bound = max(loop_args->stack->score - delta, MIN_SCORE);
		} else if (loop_args->stack->score >= upper_bound) {
		  lower_bound = (lower_bound + upper_bound) / 2;
		  upper_bound = min(loop_args->stack->score + delta, MAX_SCORE);
		} else {
		  break;
		}
		delta += delta / 2;
	  } while (can_run);
	  if (can_run) {
		publish_info(loop_args->stack, depth);
	  }
	}
  }
  assert(!can_run);
  assert(bestmove[bestmove_read].best_line_length);
  if (!pondermove) {
	uci_print_bestmove();
  }
}

unsigned int uci_read_move(const struct POSITION *const position,
						   const unsigned int side,
						   const char *token,
						   struct MOVE *const move) {
  assert(side < 2);
  unsigned int chars = 4;
  unsigned char piece, begin_square, end_square;
  unsigned char flag = 0;
  begin_square = notation_to_location(token);
  token += 2;
  end_square = notation_to_location(token);
  token += 2;
  if (*token != ' ' && *token != '\n') {
	switch (*token) {
	case 'q': flag = FLAG_TRANSFORMATION_QUEEN; break;
	case 'r': flag = FLAG_TRANSFORMATION_ROOK; break;
	case 'b': flag = FLAG_TRANSFORMATION_BISHOP; break;
	case 'k': flag = FLAG_TRANSFORMATION_KNIGHT;
	}
	token++;
	chars++;
  }
  if (*token == ' ') {
	chars++;
  }
  piece = get_piece_on_board(position, begin_square);
  if (piece == pawn_piece(side)) {
	if (abs(end_square - begin_square) == 16) {
	  flag = FLAG_PAWN_DOUBLE_MOVE;
	} else if (end_square == position->extra.en_passent_target_square) {
	  flag = FLAG_EN_PASSENT;
	}
  } else if (piece == WHITE_KING) {
	if (begin_square == 4) {
	  if (end_square == 6) {
		flag = FLAG_CASTLE_KING_WHITE;
	  } else if (end_square == 2) {
		flag = FLAG_CASTLE_QUEEN_WHITE;
	  }
	}
  } else if (piece == BLACK_KING) {
	if (begin_square == 60) {
	  if (end_square == 62) {
		flag = FLAG_CASTLE_KING_BLACK;
	  } else if (end_square == 58) {
		flag = FLAG_CASTLE_QUEEN_BLACK;
	  }
	}
  }
  move->data = pack_move(begin_square, end_square, flag);
  return chars;
}

void *uci(void *args) {
  struct POSITION *position = NULL;
  struct STACK stack[MAX_GAME], *user_move;
  char command[4096];
  char *token;
  int i, n, sel, movestogo, inc;
  unsigned int side;
  pthread_t loop_id[cpus], publish_id;
  struct LOOP_ARGUMENTS loops[cpus];
  const int handle = *((int *) args);
  fd_set fds;
  pthread_create(&publish_id, NULL, uci_publish_info, NULL);
  while (1) {
	FD_ZERO(&fds);
	FD_SET(handle, &fds);
	sel = select(handle + 1, &fds, NULL, NULL, NULL);
	if (sel > 0) {
	  n = read(handle, command, 4096);
	  if (n < 0) {
		break;
	  } else if (n == 0) {
		continue;
	  }
	  command[n] = 0;
	  //FILE *file = fopen("out.txt", "a"); fputs(command, file); fputs("****\n", file); fclose(file);
	  token = command;
	  do {
		if (strncmp(token, "ucinewgame", 10) == 0) {
		} else if (strncmp(token, "uci", 3) == 0) {
		  fprintf(uci_out, "id name Clarice 9\nid author Cristian Mocanu\n");
		  fprintf(uci_out, "option name Hash type spin default 32 min 1 max 1024\n");
		  fprintf(uci_out, "option name Ponder type check default true\n");
		  fprintf(uci_out, "uciok\n");
		} else if (strncmp(token, "setoption", 9) == 0) {
		  token += 10;
		  assert(strncmp(token, "name", 4) == 0);
		  token += 5;
		  if (strncmp(token, "Hash", 4) == 0) {
			token += 5;
			assert(strncmp(token, "value", 5) == 0);
			token += 6;
			transposition_size = (1 << (int) log2(atoi(token) * 1048576 / HASH_TABLE_ENTRY_PACKED_SIZE)) - 1;
		  }
		} else if (strncmp(token, "isready", 7) == 0) {
		  if (!transposition_table) {
			transposition_table = malloc((transposition_size + 1) * HASH_TABLE_ENTRY_PACKED_SIZE);
		  }
		  fprintf(uci_out, "readyok\n");
		} else if (strncmp(token, "position", 8) == 0) {
		  token += 9;
		  position = calloc(1, sizeof(struct POSITION));
		  if (strncmp(token, "startpos", 8) == 0) {
			parse_fen(position, &side, INITIAL_POSITION);
			token += 8;
			if (*token == ' ') {
			  token++;
			}
		  } else if (strncmp(token, "fen", 3) == 0 ) {
			token += 4;
			token += parse_fen(position, &side, token);
			if (*token == ' ') {
			  token++;
			}
		  }
		  user_move = stack;
		  if (strncmp(token, "moves", 5) == 0) {
			token += 6;
			do {
			  user_move->number_of_moves = 1;
			  reset_moves(user_move);
			  token += uci_read_move(position, side, token, user_move->moves);
			  do_move(position, side, user_move);
			  side = !side;
			  user_move++;
			} while (*token != '\n');
		  }
		} else if (strncmp(token, "go", 2) == 0) {
		  for (i = 0; i < cpus; i++) {
			pthread_join(loop_id[i], NULL);
		  }
		  pondermove = NULL;
		  token += 3;
		  if (strncmp(token, "infinite", 8) == 0) {
			token += 8;
			max_time = move_time = 2147483647;
		  } else {
			assert(strncmp(token, "wtime", 5) == 0);
			token += 6;
			if (side == WHITE) {
			  max_time = atoi(token);
			}
			token = strchr(token, ' ') + 1;
			assert(strncmp(token, "btime", 5) == 0);
			token += 6;
			if (side == BLACK) {
			  max_time = atoi(token);
			}
			inc = 0;
			movestogo = max(20, 70 - (user_move - stack) / 2);
			token = strchr(token, ' ');
			if (token) {
			  token++;
			  if (strncmp(token, "winc", 4) == 0) {
				token += 5;
				if (side == WHITE) {
				  inc = atoi(token);
				}
				token = strchr(token, ' ') + 1;
				assert(strncmp(token, "binc", 4) == 0);
				token += 5;
				if (side == BLACK) {
				  inc = atoi(token);
				}
				token = strchr(token, ' ');
				if (token) {
				  token++;
				}
			  }
			  if (token && strncmp(token, "movestogo", 9) == 0) {
				token += 10;
				movestogo = atoi(token) + 2;
				token = strchr(token, ' ');
				if (token) {
				  token++;
				}
			  }
			  if (token && strncmp(token, "ponder", 6) == 0) {
				token += 6;
				pondermove = (user_move - 1)->moves;
			  }
			}
			move_time = max_time / movestogo + inc / 2;
			max_time = min(max_time / 4, 2 * move_time);
		  }
		  assert(position);
		  can_run = true;
		  bestmove_read = bestmove_write = 0;
		  nodes = bestmove[bestmove_read].nodes = bestmove[bestmove_read].best_line_length = bestmove[bestmove_read].depth = 0;
		  first = 1;
		  start_time = iteration_start = get_time_in_milliseconds();
		  for (i = 0; i < cpus; i++) {
			memcpy(loops[i].position.board, position->board, 64 * sizeof(int));
			memcpy(loops[i].position.pieces, position->pieces, 13 * sizeof(U64));
			memcpy(loops[i].position.sides, position->sides, 2 * sizeof(U64));
			loops[i].position.extra = position->extra;
			memcpy(loops[i].base_stack, user_move - position->extra.halfmove_clock, (position->extra.halfmove_clock + 1) * sizeof(struct STACK));
			loops[i].stack = loops[i].base_stack + position->extra.halfmove_clock;
			loops[i].side = side;
			pthread_create(&loop_id[i], NULL, loop, &loops[i]);
		  }
		  if (!token) {
			break;
		  }
		} else if (strncmp(token, "ponderhit", 9) == 0) {
		  assert(pondermove);
		  if (can_run) {
			pondermove = NULL;
			first = 1;
			check_time(get_time_in_milliseconds());
		  } else {
			uci_print_bestmove();
		  }
		} else if (strncmp(token, "quit", 4) == 0) {
		  pthread_join(publish_id, NULL);
		  free(transposition_table);
		  return NULL;
		} else if (strncmp(token, "stop", 4) == 0) {
		  can_run = false;
		  if (pondermove) {
			uci_print_bestmove();
		  }
		} else if (strncmp(token, "debug", 5) == 0) {
		  token += 6;
		  debug = (strncmp(token, "on", 2) == 0);
		} else if (strncmp(token, "ponder", 6) == 0) {
		  token += 7;
		  ponder = (strncmp(token, "on", 2) == 0);
		}
		token = strchr(token, '\n') + 1;
	  } while (*token);
	}
  }
  can_run = false;
  for (i = 0; i < cpus; i++) {
	pthread_join(loop_id[i], NULL);
  }
  pthread_join(publish_id, NULL);
  free(transposition_table);
}


//TODO: fix ponder and ponderhit
void ics(const char *user, const char *pass) {
  int sockfd, n, sel, i, history_length, uci_handles_in[2], uci_handles_out[2];
  unsigned int side;
  long inc, wtime, btime;
  struct sockaddr_in serv_addr;
  struct hostent *server;
  char buffer[4096], history[512], *token;
  fd_set fds;
  FILE *uci_file_in;
  pthread_t uci_id;
  if (pipe(uci_handles_in)) {
	perror("Error opening pipes.");
	return;
  }
  if (pipe(uci_handles_out)) {
	perror("Error opening pipes.");
	return;
  }
  uci_file_in = fdopen(uci_handles_in[1], "w");
  uci_out = fdopen(uci_handles_out[1], "w");
  setlinebuf(uci_file_in);
  setlinebuf(uci_out);
  pthread_create(&uci_id, NULL, uci, (void *) &uci_handles_in[0]);
  fprintf(uci_file_in, "uci\nsetoption name Hash value 68\nisready\n");
  sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) {
	perror("Error opening socket.");
	return;
  }
  server = gethostbyname("freechess.org");
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  bcopy((char *)server->h_addr, (char *)&serv_addr.sin_addr.s_addr, server->h_length);
  serv_addr.sin_port = htons(5000);
  if (connect(sockfd,(struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0) {
	perror("Error connecting to socket.");
	return;
  }
  while (1) {
	FD_ZERO(&fds);
	FD_SET(sockfd, &fds);
	FD_SET(uci_handles_out[0], &fds);
	sel = select(max(sockfd, uci_handles_out[0]) + 1, &fds, NULL, NULL, NULL);
	if (FD_ISSET(sockfd, &fds)) {
	  n = recv(sockfd, buffer, 4096, 0);
	  if (n < 0) {
		break;
	  } else if (n == 0) {
		continue;
	  }
	  buffer[n] = 0;
	  printf("%s", buffer);
	  if (strstr(buffer, "login: ")) {
		write(sockfd, user, strlen(user));
		write(sockfd, "\n", 1);
	  } else if (strstr(buffer,  "password: ")) {
		history_length = 0;
		fprintf(uci_file_in, "ucinewgame\n");
		write(sockfd, pass, strlen(pass));
		write(sockfd, "\n", 1);
		//write(sockfd, "set style 12\n", 13);
		write(sockfd, "seek unrated\n", 13);
	  } else if (strstr(buffer, "Challenge: ")) {
		history_length = 0;
		fprintf(uci_file_in, "ucinewgame\n");
		write(sockfd, "accept\n", 7);
	  } else if (strstr(buffer, "{Game ")) {
		history_length = 0;
		fprintf(uci_file_in, "ucinewgame\n");
		write(sockfd, "seek unrated\n", 13);
	  } else if (token = strstr(buffer, "<12> ")) {
		token += 5;
		for (i = 0; i < 8; i++) {
		  token = strchr(token, ' ') + 1;
		}
		side = (*token == 'B');
		for (i = 0; i < 6; i++) {
		  token = strchr(token, ' ') + 1;
		}
		if (atoi(token) >= 100) {
		  send(sockfd, "draw\n", 5, 0);
		  fprintf(uci_file_in, "stop\n");
		} else {
		  token = strchr(token, ' ') + 1;
		  token = strchr(token, ' ') + 1;
		  token = strchr(token, ' ') + 1;
		  token = strchr(token, ' ') + 1;
		  if (atoi(token) == 1) {
			token = strchr(token, ' ') + 1;
			token = strchr(token, ' ') + 1;
			inc = atoi(token) * 1000;
			token = strchr(token, ' ') + 1;
			token = strchr(token, ' ') + 1;
			token = strchr(token, ' ') + 1;
			wtime = atoi(token) * 1000;
			token = strchr(token, ' ') + 1;
			btime = atoi(token) * 1000;
			token = strchr(token, ' ') + 1;
			token = strchr(token, ' ') + 1;
			fprintf(uci_file_in, "position startpos");
			if (strncmp(token, "none", 4) != 0) {
			  history[history_length++] = ' ';
			  if (strncmp(token, "o-o-o", 5) == 0) {
				if (side == BLACK) {
				  history[history_length++] = 'e';
				  history[history_length++] = '1';
				  history[history_length++] = 'c';
				  history[history_length++] = '1';
				} else {
				  history[history_length++] = 'e';
				  history[history_length++] = '8';
				  history[history_length++] = 'c';
				  history[history_length++] = '8';
				}
			  } else if (strncmp(token, "o-o", 3) == 0) {
				if (side == BLACK) {
				  history[history_length++] = 'e';
				  history[history_length++] = '1';
				  history[history_length++] = 'g';
				  history[history_length++] = '1';
				} else {
				  history[history_length++] = 'e';
				  history[history_length++] = '8';
				  history[history_length++] = 'g';
				  history[history_length++] = '8';
				}
			  } else {
				history[history_length++] = token[2];
				history[history_length++] = token[3];
				history[history_length++] = token[5];
				history[history_length++] = token[6];
				if (token[7] == '=') {
				  history[history_length++] = tolower(token[8]);
				}
			  }
			  history[history_length] = 0;
			  assert(history_length <= 512);
			  fprintf(uci_file_in, " moves%s", history);
			}
			fprintf(uci_file_in, "\ngo wtime %ld btime %ld winc %ld binc %ld\n", wtime, btime, inc, inc);
		  }
		}
	  }
	}
	if (FD_ISSET(uci_handles_out[0], &fds)) {
	  n = read(uci_handles_out[0], buffer, 4096);
	  if (n < 0) {
		break;
	  } else if (n == 0) {
		continue;
	  }
	  buffer[n] = 0;
	  if (token = strstr(buffer, "bestmove ")) {
		token += 9;
		history[history_length++] = ' ';
		history[history_length++] = token[0];
		history[history_length++] = token[1];
		history[history_length++] = token[2];
		history[history_length++] = token[3];
		history[history_length] = 0;
		token[4] = '\n';
		token[5] = 0;
		send(sockfd, token, 5, 0);
		/*
		if (token = strstr(token, "ponder ")) {
		  token += 7;
		  fprintf(uci_file_in, "position startpos moves%s\ngo ponder\n", history);
		}
		*/
	  }
	}
  }
  close(sockfd);
  fprintf(uci_file_in, "quit\n");
  pthread_join(uci_id, NULL);
  close(uci_handles_in[1]);
  close(uci_handles_in[0]);
  close(uci_handles_out[0]);
  close(uci_handles_out[1]);
}

//TODO: create threads once
//TODO: evaluate iteratively
//TODO: time management (recapture)
//TODO: when king is in check, is_pinned needs to check only if the start point is pinned
//TODO: don't generate all the moves at once and try to generate them in order
//TODO: reductions
//TODO: use syzygy endgame tablebases
//TODO: use opening book
//TODO: port to windows
//TODO: report uci selective depth
void main(const int argc, const char **argv)  {
  int uci_in = STDIN_FILENO;
  cpus = 4 * get_nprocs();
  init_hash();
  if (argc == 4 && strcmp(argv[1], "-ics") == 0) {
	ics(argv[2], argv[3]);
  } else {
	uci_out = stderr;
	uci(&uci_in);
  }
  pthread_mutex_destroy(&parallel_lock);
  pthread_cond_destroy(&parallel_cond);
}
