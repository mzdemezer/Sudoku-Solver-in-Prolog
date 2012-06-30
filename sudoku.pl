db_on:- guitracer, trace.
db_off:- notrace, nodebug, noguitracer.

%%%%%%%%%%%%%%%%%%%%   SUDOKU    %%%%%%%%%%%%%%%%%%%%%%%

%solve_sudoku(L_in, L_out).

index([H | _], 0, H):- !.
index([_ | T], N, H_in):-
	N1 is N - 1,
	index(T, N1, H_in).

check_row([]):- !.
check_row([0 | T]):-
	!, check_row(T).
check_row([H | T]):-
	not(member(H, T)),
	check_row(T).

check_rows([]):- !.
check_rows([H | T]):-
	check_row(H),
	check_rows(T).

check_col([], _, _):- !.
check_col([[0 | T1] | T], Heads, [T1 | Tails]):-
	!, check_col(T, Heads, Tails).
check_col([[H | T1] | T], Heads, [T1 | Tails]):-
	not(member(H, Heads)),
	check_col(T, [H | Heads], Tails).
check_col(Sudoku, SmallerSudoku):-
	check_col(Sudoku, [], SmallerSudoku).

check_cols([[]|_]):- !.
check_cols(Sudoku):-
	check_col(Sudoku, SmallerSudoku),
	check_cols(SmallerSudoku).

check_square([ [A1, A2, A3 | TA],
	       [B1, B2, B3 | TB],
	       [C1, C2, C3 | TC] | TRest],
	       [TA, TB, TC | TRest]):-
	check_row([A1, A2, A3, B1, B2, B3, C1, C2, C3]).
check_squares([[]|_]):- !.
check_squares([A, B, C, D, E, F, G, H, I]):-
	check_square([A, B, C], [Arest, Brest, Crest]),
	check_square([D, E, F], [Drest, Erest, Frest]),
	check_square([G, H, I], [Grest, Hrest, Irest]),
	check_squares([Arest, Brest, Crest,
		       Drest, Erest, Frest,
		       Grest, Hrest, Irest]).

check_sudoku(Sudoku):-
	check_rows(Sudoku),
	check_cols(Sudoku),
	check_squares(Sudoku), !.

%%	%

replace([[_ | T] | Rest], New, 0, 0, [[New | T] | Rest]). %_ or 0
replace([[H | T] | Rest], New, X, 0, [[H | NewT] | Rest]):-
	X1 is X - 1,
	replace([T | Rest], New, X1, 0, [NewT | Rest]).
replace([Row | Rest], New, X, Y, [Row | NewRest]):-
	Y1 is Y - 1,
	replace(Rest, New, X, Y1, NewRest).

delete(Lin, X, Lout):-
	append(Pre, [X | Post], Lin),
	append(Pre, Post, Lout).

deleteIfExists(Lin, X, Lout):-
	member(X, Lin),
	delete(Lin, X, Lout).
deleteIfExists(Lin, X, Lin):-
	not(member(X, Lin)).

%%%%%%%%%%%%%%%% metoda kratkowa %%%%%%%%%%%%%%%%%


getRevPossFromNine([], []).
getRevPossFromNine([0 | T], Poss):- getRevPossFromNine(T, Poss).
getRevPossFromNine([H | T], [H | Poss]):-
	H \= 0, %bez odciêæ to minimalizuje nawroty
	getRevPossFromNine(T, Poss).

reversePoss([], Acc, Acc).
reversePoss([H | T], Acc, Poss):-
	delete(Acc, H, NewAcc),
	reversePoss(T, NewAcc, Poss).
reversePoss(Rev, Poss):-
	reversePoss(Rev, [1, 2, 3, 4, 5, 6, 7, 8, 9], Poss).

condDeletePoss([], Acc, Acc).
condDeletePoss([H | T], Acc, Poss):-
	deleteIfExists(Acc, H, NewAcc),
	condDeletePoss(T, NewAcc, Poss).

getPossFromNine(Row, Poss):-
	getRevPossFromNine(Row, Rev),
	reversePoss(Rev, Poss).

fillRowWithPoss([], _, []).
fillRowWithPoss([0 | T], Poss, [Poss | Tposs]):-
	fillRowWithPoss(T, Poss, Tposs).
fillRowWithPoss([H | T], Poss, [[] | Tposs]):-
	H \= 0, %nie wiem, czy to konieczne, ale to te¿ pomaga
	fillRowWithPoss(T, Poss, Tposs).

deleteExistingPosses(_, _, [], []).
deleteExistingPosses([0 | Rest], Rev,
		     [Hposs | Tposs], [NewHposs | NewTposs]):-
	deleteExistingPosses(Rest, Rev, Tposs, NewTposs),
	condDeletePoss(Rev, Hposs, NewHposs).
deleteExistingPosses([H | Rest], Rev,
		     [Hposs | Tposs], [Hposs | NewTposs]):-
	H \= 0,
	deleteExistingPosses(Rest, Rev, Tposs, NewTposs).


getPossRow(Row, PossRow):-
	getPossFromNine(Row, Possibilities),
	fillRowWithPoss(Row, Possibilities, PossRow).
getPossRow(Row, OldPoss, NewPoss):-
	getRevPossFromNine(Row, Rev),
	deleteExistingPosses(Row, Rev, OldPoss, NewPoss).

getPossRows([], []).
getPossRows([H | T], [Hposs | Tposs]):-
	getPossRow(H, Hposs),
	getPossRows(T, Tposs).

getCol([], [], []).
getCol([[H | T] | Rest], [T | NewRest], [H | Col]):-
	getCol(Rest, NewRest, Col).

attachCol([], [], []).
attachCol([H | T], [Hposs | Tposs], [[H | Hposs] | NewTposs]):-
	attachCol(T, Tposs, NewTposs).

getPossCols([[] | _], RowPoss, RowPoss).
getPossCols(Sud, RowPoss, OutPoss):-
	getCol(Sud, SmallerSud, Col),
	getCol(RowPoss, SmallerRowPoss, ColPoss),
	getPossCols(SmallerSud, SmallerRowPoss, InPoss),
	getPossRow(Col, ColPoss, NewColPoss),
	attachCol(NewColPoss, InPoss, OutPoss).

getSquare([ [A1, A2, A3 | TA],
	    [B1, B2, B3 | TB],
	    [C1, C2, C3 | TC] | TRest],
	    [TA, TB, TC | TRest],
	  [A1, A2, A3, B1, B2, B3, C1, C2, C3]).

getPossSquares([[] | _], Poss, Poss).
getPossSquares([A, B, C, D, E, F, G, H, I],
	       [PA, PB, PC, PD, PE, PF, PG, PH, PI],
	       [NewAsquare, NewBsquare, NewCsquare,
		NewDsquare, NewEsquare, NewFsquare,
		NewGsquare, NewHsquare, NewIsquare]):-
	getSquare([A, B, C], [Arest, Brest, Crest], Sq1),
	getSquare([D, E, F], [Drest, Erest, Frest], Sq2),
	getSquare([G, H, I], [Grest, Hrest, Irest], Sq3),
	getSquare([PA, PB, PC], [PArest, PBrest, PCrest], PSq1),
	getSquare([PD, PE, PF], [PDrest, PErest, PFrest], PSq2),
	getSquare([PG, PH, PI], [PGrest, PHrest, PIrest], PSq3),
	getPossSquares([Arest, Brest, Crest,
		        Drest, Erest, Frest,
		        Grest, Hrest, Irest],
		       [PArest, PBrest, PCrest,
			PDrest, PErest, PFrest,
			PGrest, PHrest, PIrest],
		       [NewPArest, NewPBrest, NewPCrest,
			NewPDrest, NewPErest, NewPFrest,
			NewPGrest, NewPHrest, NewPIrest]),
	getPossRow(Sq1, PSq1, NewPSq1),
	getPossRow(Sq2, PSq2, NewPSq2),
	getPossRow(Sq3, PSq3, NewPSq3),
	getSquare([NewAsquare, NewBsquare, NewCsquare],
		  [NewPArest, NewPBrest, NewPCrest],
		  NewPSq1),
	getSquare([NewDsquare, NewEsquare, NewFsquare],
	          [NewPDrest, NewPErest, NewPFrest],
		  NewPSq2),
	getSquare([NewGsquare, NewHsquare, NewIsquare],
	          [NewPGrest, NewPHrest, NewPIrest],
		  NewPSq3).

getPoss(Sud, Poss):-
	getPossRows(Sud, RowPoss),
	getPossCols(Sud, RowPoss, ColPoss),
	getPossSquares(Sud, ColPoss, Poss).

getPossAcc(Poss, 0, 0, Poss).
getPossAcc([[_ | T] | Rest], X, 0, [NewT | Rest]):-
	X1 is X - 1,
	getPossAcc([T | Rest], X1, 0, [NewT| Rest]).
getPossAcc([_ | Rest], X, Y, NewRest):-
	Y \= 0,
	Y1 is Y - 1,
	getPossAcc(Rest, X, Y1, NewRest).

countNextXY(8, Y, 0, Y1):- Y1 is Y + 1.
countNextXY(X, Y, X1, Y):- X1 is X + 1.

process_kratkowa(_, _, _, _,
		 _, _, 0, [], _, _):- fail.
process_kratkowa(Sud, NewSud, _, NewPoss,
		 _, NewAcc, 0, [HPoss], X, Y):-
	replace(Sud, HPoss, X, Y, NewSud),
	getPoss(NewSud, NewPoss),
	countNextXY(X, Y, X1, Y1),
	getPossAcc(NewPoss, X1, Y1, NewAcc).
process_kratkowa(Sud, Sud, Poss, Poss,
		 Acc, Acc, _, _, _, _).

metoda_kratkowa(Sudoku, Sudoku, Poss, Poss, _, _, 0, 9).
metoda_kratkowa(Sudoku, NewSudoku, Poss, NewPoss,
		[[Hsud] | Tsud], [[Hposs] | Tposs], X, Y):-
	process_kratkowa(Sudoku, InSudoku, Poss, InPoss, Tposs, InPossAcc, Hsud, Hposs, X, Y),
	Y1 is Y + 1,
	metoda_kratkowa(InSudoku, NewSudoku, InPoss, NewPoss, Tsud, InPossAcc, 0, Y1).
metoda_kratkowa(Sudoku, NewSudoku, Poss, NewPoss,
		[[Hsud | HTsud] | Tsud], [[Hposs | HTposs] | Tposs], X, Y):-
	process_kratkowa(Sudoku, InSudoku, Poss, InPoss,
			 [HTposs |Tposs], InPossAcc, Hsud, Hposs, X, Y),
	X1 is X + 1,
	metoda_kratkowa(InSudoku, NewSudoku, InPoss, NewPoss,
			[HTsud | Tsud], InPossAcc, X1, Y).

metoda_kratkowa(Sud, Sud, Sud, _).
metoda_kratkowa(OldSud, NewSud, OutSud, Poss):-
	OldSud \= NewSud,
	metoda_kratkowa(NewSud, InSud, Poss, NewPoss,
			NewSud, Poss, 0, 0),
	metoda_kratkowa(NewSud, InSud, OutSud, NewPoss).
metoda_kratkowa(Sudoku, NewSudoku):-
	getPoss(Sudoku, Poss),
	metoda_kratkowa(Sudoku, InSudoku, Poss, NewPoss,
			Sudoku, Poss, 0, 0),
	metoda_kratkowa(Sudoku, InSudoku, NewSudoku, NewPoss),
	!.

%%%%%%%%%%%%%%%% metoda eliminacji %%%%%%%%%%%%%%%%%

getSquareIndex(X, Y, SqX, SqY):-
	SqX is X // 3,
	SqY is Y // 3.

getPointsProcess(N, N, X, Y, Points, [p(X, Y) | Points]).
getPointsProcess(H, N, _, _, Points, Points):- H \= N.

getPoints([], [], _, _, _).
getPoints([[H | T] | Rest], NewPoints, N, X, Y):-
	X1 is X + 1,
	getPoints([T | Rest], Points, N, X1, Y),
	getPointsProcess(H, N, X, Y, Points, NewPoints).
getPoints([[H] | Rest], NewPoints, N, X, Y):-
	Y1 is Y + 1,
	getPoints(Rest, Points, N, 0, Y1),
	getPointsProcess(H, N, X, Y, Points, NewPoints).
getPoints(Sud, N, Points):-
	getPoints(Sud, Points, N, 0, 0).

colliding(X, _, Points):-
	member(p(X, _), Points).
colliding(_, Y, Points):-
	member(p(_, Y), Points).
colliding(X, Y, Points):-
	getSquareIndex(X, Y, SqX, SqY),
	member(p(Xc, Yc), Points),
	getSquareIndex(Xc, Yc, SqX, SqY).
putPointIntoRow(Rows, X, Y, NewRows):-
	append(Pre, [row(Y, List) | Post], Rows),
	NewList = [X | List],
	append(Pre, [row(Y, NewList) | Post], NewRows).
putPointIntoCol(Cols, X, Y, NewCols):-
	putPointIntoRow(Cols, Y, X, NewCols).
putPointIntoSquare(Squares, X, Y, NewSquares):-
	getSquareIndex(X, Y, SqX, SqY),
	append(Pre, [sq(SqX, SqY, List) | Post], Squares),
	NewList = [p(X, Y) | List],
	append(Pre, [sq(SqX, SqY, NewList) | Post], NewSquares).

getElemInSchemesProcess(0, X, Y, Points, Schem, Schem):-
	colliding(X, Y, Points).
getElemInSchemesProcess(0, X, Y, _,
			[Rows, Cols, Squares],
			[NewRows, NewCols, NewSquares]):-
	%not(colliding(X, Y, Points),
	putPointIntoRow(Rows, X, Y, NewRows),
	putPointIntoCol(Cols, X, Y, NewCols),
	putPointIntoSquare(Squares, X, Y, NewSquares).
getElemInSchemesProcess(H, _, _, _, Schem, Schem):-
	H \= 0.

getElemInSchemes([], _, _, _,
	  [[row(0, []), row(1, []), row(2, []),
	   row(3, []), row(4, []), row(5, []),
	   row(6, []), row(7, []), row(8, [])],
	  [row(0, []), row(1, []), row(2, []),
	   row(3, []), row(4, []), row(5, []),
	   row(6, []), row(7, []), row(8, [])],
          [sq(0, 0, []), sq(1, 0, []), sq(2, 0, []),
	   sq(0, 1, []), sq(1, 1, []), sq(2, 1, []),
	   sq(0, 2, []), sq(1, 2, []), sq(2, 2, [])]]).
getElemInSchemes([[H] | Rest], Points, X, Y, Schemes):-
	Y1 is Y + 1,
	getElemInSchemes(Rest, Points, 0, Y1, InSchemes),
	getElemInSchemesProcess(H, X, Y, Points, InSchemes, Schemes).
getElemInSchemes([[H | T] | Rest], Points, X, Y, Schemes):-
	X1 is X + 1,
	getElemInSchemes([T | Rest], Points, X1, Y, InSchemes),
	getElemInSchemesProcess(H, X, Y, Points, InSchemes, Schemes).

getElemInSchemes(Sud, N, Schemes):-
	getPoints(Sud, N, Points),
	getElemInSchemes(Sud, Points, 0, 0, Schemes).

eliminujWWierszu(Sud, Sud, _, []).
eliminujWWierszu(Sud, NewSud, N, [row(_, []) | T]):-
	eliminujWWierszu(Sud, NewSud, N, T).
eliminujWWierszu(Sud, NewSud, N, [row(_, [_, _ | _]) | T]):-
	eliminujWWierszu(Sud, NewSud, N, T).
eliminujWWierszu(Sud, NewSud, N, [row(Y, [X]) | T]):-
	eliminujWWierszu(Sud, InSud, N, T),
	replace(InSud, N, X, Y, NewSud).

eliminujWKolumnie(Sud, Sud, _, []).
eliminujWKolumnie(Sud, NewSud, N, [row(_, []) | T]):-
	eliminujWKolumnie(Sud, NewSud, N, T).
eliminujWKolumnie(Sud, NewSud, N, [row(_, [_, _ | _]) | T]):-
	eliminujWKolumnie(Sud, NewSud, N, T).
eliminujWKolumnie(Sud, NewSud, N, [row(X, [Y]) | T]):-
	eliminujWKolumnie(Sud, InSud, N, T),
	replace(InSud, N, X, Y, NewSud).

eliminujWKwadracie(Sud, Sud, _, []).
eliminujWKwadracie(Sud, NewSud, N, [sq(_, _, []) | T]):-
	eliminujWKwadracie(Sud, NewSud, N, T).
eliminujWKwadracie(Sud, NewSud, N, [sq(_, _, [_, _| _]) | T]):-
	eliminujWKwadracie(Sud, NewSud, N, T).
eliminujWKwadracie(Sud, NewSud, N, [sq(_, _, [p(X, Y)]) | T]):-
	eliminujWKwadracie(Sud, InSud, N, T),
	replace(InSud, N, X, Y, NewSud).

eliminujProcess(Sud, NewSud, N):-
	getElemInSchemes(Sud, N, [Rows, Cols, Squares]),
	eliminujWWierszu(Sud, RowSud, N, Rows),
	eliminujWKolumnie(RowSud, ColSud, N, Cols),
	eliminujWKwadracie(ColSud, NewSud, N, Squares).

eliminuj(Sud, Sud, Sud, _).
eliminuj(OldSud, Sud, NewSud, N):-
	OldSud \= Sud,
	eliminujProcess(Sud, InSud, N),
	eliminuj(Sud, InSud, NewSud, N).
eliminuj(Sud, NewSud, N):-
	eliminuj([], Sud, NewSud, N).

eliminacja(Sud, Sud, 10).
eliminacja(Sud, NewSud, N):-
	eliminuj(Sud, InSud, N),
	N1 is N + 1,
	eliminacja(InSud, NewSud, N1).
eliminacja(Sud, NewSud):-
	eliminacja(Sud, NewSud, 1).

metoda_eliminacji(Sud, Sud, Sud).
metoda_eliminacji(OldSud, Sud, NewSud):-
	OldSud \= Sud,
	eliminacja(Sud, InSud),
	metoda_eliminacji(Sud, InSud, NewSud).
metoda_eliminacji(Sud, NewSud):-
	metoda_eliminacji([], Sud, NewSud),
	!.

%%%%%%%%%%%%%%%% œlepy strza³ %%%%%%%%%%%

findZero([[0 | _] | _], p(X, Y), p(X, Y)):- !.
findZero([[H] | Rest], p(_, Yin), p(X, Y)):- !,
	H \= 0,
	Y1 is Yin + 1,
	findZero(Rest, p(0, Y1), p(X, Y)).
findZero([[H | T] | Rest], p(Xin, Yin), p(X, Y)):-
	H \= 0,
	X1 is Xin + 1,
	findZero([T | Rest], p(X1, Yin), p(X, Y)).

getMatrixElem([[Elem | _] | _], 0, 0, Elem):- !.
getMatrixElem([[_ | T] | _], X, 0, Elem):- !,
	X1 is X - 1,
	getMatrixElem([T], X1, 0, Elem).
getMatrixElem([_ | Rest], X, Y, Elem):-
	Y1 is Y - 1,
	getMatrixElem(Rest, X, Y1, Elem).

getPossForPoint(Sud, X, Y, Poss):-
	getPoss(Sud, SudPoss),
	getMatrixElem(SudPoss, X, Y, Poss).

solve_sudoku(Sud, Sud, NewSud):-
	findZero(Sud, p(0, 0), p(X, Y)),
	getPossForPoint(Sud, X, Y, Poss), !,
	member(BlindShot, Poss),
	replace(Sud, BlindShot, X, Y, BlindSud),
	solve_sudoku(Sud, BlindSud, NewSud).
solve_sudoku(Sud, Sud, Sud).
solve_sudoku(OldSud, Sud, NewSud):-
	OldSud \= Sud,
	metoda_eliminacji(Sud, ElimSud),
	metoda_kratkowa(ElimSud, KratSud),
	solve_sudoku(Sud, KratSud, NewSud).

solve_sudoku(Sudoku, OutSudoku):-
       solve_sudoku([], Sudoku, OutSudoku),
       check_sudoku(OutSudoku).

write_sudoku([]):- !.
write_sudoku([[Last]]):-
	write(Last),
	write(']'), nl,
	write(']'), !.
write_sudoku([[H] | T]):- !,
	write(H),
	write(']'), nl, write(','), tab(1),
	write('['), !,
	write_sudoku(T).
write_sudoku([[H | T] | Rest]):- !,
	write(H),
	write(','), tab(1), !,
	write_sudoku([T | Rest]).
print_sudoku(Sud):-
	write('[ ['),
	write_sudoku(Sud).

%%%%%%%%   brute    %%%%%%

solve_one(Row, NewRow):-
	append(Pre, [0 | Post], Row),
	member(New, [1, 2, 3, 4, 5, 6, 7, 8, 9]),
	not(member(New, Row)),
	append(Pre, [New | Post], NewRow).

solve_row(Row, Row):-
	not(member(0, Row)).
solve_row(Row, DoneRow):-
	solve_one(Row, BetterRow),
	solve_row(BetterRow, DoneRow).

solve_row([Row | Rest], 0, [NewRow | Rest]):-
	solve_row(Row, NewRow).
solve_row([Row | Rest], N, [Row | NewRest]):-
	N1 is N - 1,
	solve_row(Rest, N1, NewRest).

solve_rows([], _, _):- !.
solve_rows(Sudoku, N, NewSudoku):-
	solve_row(Sudoku, N, NewSudoku),
	check_sudoku(NewSudoku).

brute_sudoku(_, 9, _):- !.
brute_sudoku(Sudoku, N, SolvedSudoku):-
	solve_rows(Sudoku, N, ABetterSudoku),
	N1 is N + 1,
	brute_sudoku(ABetterSudoku, N1, SolvedSudoku).
brute_sudoku(Sudoku, NewSudoku):-
	brute_sudoku(Sudoku, 0, NewSudoku).
