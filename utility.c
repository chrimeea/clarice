const short mobility[2][64] = {{2, 2, 2, 2, 2, 2, 2, 2,
								2, 2, 2, 2, 2, 2, 2, 2,
								2, 2, 2, 2, 2, 2, 2, 2,
								2, 2, 2, 4, 4, 2, 2, 2,
								2, 2, 2, 4, 4, 2, 2, 2,
								2, 2, 2, 3, 3, 2, 2, 2,
								3, 3, 3, 3, 3, 3, 3, 3,
								3, 3, 3, 3, 3, 3, 3, 3},
							   {3, 3, 3, 3, 3, 3, 3, 3,
								3, 3, 3, 3, 3, 3, 3, 3,
								2, 2, 2, 3, 3, 2, 2, 2,
								2, 2, 2, 4, 4, 2, 2, 2,
								2, 2, 2, 4, 4, 2, 2, 2,
								2, 2, 2, 2, 2, 2, 2, 2,
								2, 2, 2, 2, 2, 2, 2, 2,
								2, 2, 2, 2, 2, 2, 2, 2}};
  
void generate_bishop_mobility() {
  int i, j, s;
  U64 temp;
  char k;
  printf("{");
  for (i = 0; i < 64; i++) {
	printf("{");
	for (j = 0; j < 512; j++) {
	  s = 0;
	  temp = bishop_magic_index[i][j];
	  while (temp) {
		k = pop_last_square(&temp);
		s += mobility[0][k];
	  }
	  printf("%d", s);
	  if (j < 512 - 1) {
		printf(", ");
	  }
	}
	printf("},\n");
  }
  printf("};");
}

void generate_rook_mobility() {
  int i, j, s;
  U64 temp;
  char k;
  printf("{");
  for (i = 0; i < 64; i++) {
	printf("{");
	for (j = 0; j < 4096; j++) {
	  s = 0;
	  temp = rook_magic_index[i][j];
	  while (temp) {
		k = pop_last_square(&temp);
		s += mobility[0][k];
	  }
	  printf("%d", s);
	  if (j < 4096 - 1) {
		printf(", ");
	  }
	}
	printf("},\n");
  }
  printf("};");
}

void generate_knight_mobility() {
  int i, j, s;
  U64 temp;
  char k;
  printf("{");
  for (i = 0; i < 64; i++) {
	s = 0;
	temp = knight_attacks(i);
	while (temp) {
	  k = pop_last_square(&temp);
	  s += mobility[0][k];
	}
	printf("%d, ", s);
  }
  printf("};\n");
}

void generate_king_square_table() {
  char k;
  printf("{");
  for (k = 0; k < 64; k++) {
	printf("%d, ", 10 * (min(k / 8, 7 - k / 8) + min(k % 8, 7 - k % 8)));
  }
  printf("};\n");
}

void generate_pawn_square_table() {
  int i, j, k, d;
  U64 temp;
  printf("{");
  for (i = 0; i < 64; i++) {
	//d = 7 - i / 8;
	d = i / 8;
	temp = 0;
	//for (j = i / 8; j <= i / 8 + d; j++) {
	for (j = i / 8 - d; j <= i / 8; j++) {
	  for (k = max(j * 8, j * 8 + i % 8 - d); k <= min(j * 8 + 7, j * 8 + i % 8 + d); k++) {
		temp |= (1ULL << k);
	  }
	}
	printf("%lluULL, ", temp);
  }
  printf("}\n");
}

void generate_free_pawn_index() {
  int i, j;
  U64 temp;
  printf("{");
  for (i = 0; i < 64; i++) {
	temp = 0;
	//j = i + 8;
	j = i - 8;
	//while (j < 64) {
	while (j >= 0) {
	  temp |= (1ULL << j);
	  //j += 8;
	  j -= 8;
	}
	if (i % 8 > 0) {
	  //j = i + 7;
	  j = i - 9;
	  //while (j < 64) {
	  while (j >= 0) {
		temp |= (1ULL << j);
		//j += 8;
		j -= 8;
	  }
	}
	if (i % 8 < 7) {
	  //j = i + 9;
	  j = i - 7;
	  //while (j < 64) {
	  while (j >= 0) {
		temp |= (1ULL << j);
		//j += 8;
		j -= 8;
	  }
	}
	printf("%lluULL, ", temp);
  }
  printf("};\n");
}

void file_in_front_of_the_pawn() {
  int i, j;
  U64 temp;
  printf("{");
  for (i = 0; i < 64; i++) {
	temp = 0;
	//for (j = i + 8; j < 64; j += 8) {
	for (j = i - 8; j >= 0; j -= 8) {
	  temp |= (1ULL << j);
	}
	printf("%lluULL, ", temp);
  }
  printf("}\n");
}

void generate_diagonals() {
  int i, j;
  U64 temp;
  printf("{");
  for (i = 0; i < 7; i++) {
	j = i;
	temp = 0;
	while (j % 8 < 7) {
	  temp |= (1ULL << j);
	  j += 7;
	}
	printf("%lluULL, ", temp);
  }
  for (i = 7; i < 64; i += 8) {
	j = i;
	temp = 0;
	while (j < 64) {
	  temp |= (1ULL << j);
	  j += 7;
	}
	printf("%lluULL, ", temp);
  }
  for (i = 7; i > 0; i--) {
	j = i;
	temp = 0;
	while (j % 8 > 0) {
	  temp |= (1ULL << j);
	  j += 9;
	}
	printf("%lluULL, ", temp);
  }
  for (i = 0; i < 64; i += 8) {
	j = i;
	temp = 0;
	while (j < 64) {
	  temp |= (1ULL << j);
	  j += 9;
	}
	printf("%lluULL, ", temp);
  }
  printf("};\n");
}

void generate_black_squares() {
  int i, j, k = 0; //1
  U64 temp = 0;
  for (j = 0; j < 8; j++) {
	for (i = 0; i < 4; i++) {
	  temp |= (1ULL << k);
	  k += 2;
	}
	k += (k % 2 ? -1 : 1);
  }
  printf("%lluULL\n", temp);
}

void generate_king_extended_range() {
  int i, j;
  U64 temp;
  printf("{");
  for (i = 0; i < 64; i++) {
	temp = 0;
	for (j = 0; j < 64; j++) {
	  if (i != j && max(abs(i / 8 - j / 8), abs(i % 8 - j % 8)) < 3) {
		temp |= (1ULL << j);
	  }
	}
	printf("%lluULL, ", temp);
  }
  printf("}\n");
}
