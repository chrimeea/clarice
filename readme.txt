Clarice is a chess engine for educational purpose. ELO ~2200

To compile to a.out use the following command:

	gcc chess9.c -Ofast -lm -pthread -Wall

To run it with xboard use the following command:

	xboard -zp -zippyAcceptOnly '0' -fUCI -fcp ./a.out -fNoOwnBookUCI -polyglotBook ./varied.bin -usePolyglotBook T -smpCores 2

Or if you want to play on ICS add the following option to the above command:
	-ics -icshost freechess.org

To analyse a fen position launch a.out in a shell and type the following commands:

Example1:
	uci
	setoption name Hash value 68
	isready
	ucinewgame
	position fen r1k2b1r/pp3ppp/2p1p1n1/q3P3/3B4/2NQ2PP/P1P2PB1/1R4K1 b - - 7 21
	go infinite
	
Or Example2:
	uci
	setoption name Hash value 68
	isready
	debug on
	ucinewgame
	position startpos
	go infinite
	
