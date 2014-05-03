:- use_module(library(clpfd)).

max([X], X).
max([X|Xs], X):- max(Xs,Y), X >=Y.
max([X|Xs], N):- max(Xs,N), N > X.

indexOf([Y|_], Y, 0):- !.
indexOf([_|Ys], Y, I):-
  indexOf(Ys, Y, I1),
  !,
  I is I1+1.

constraint([], _, _).
constraint([Y1|Ys], Y2, X) :-
  Y1 #\= Y2,
  abs(Y2 - Y1) #\= X,
  X1 #= X + 1,
  constraint(Ys, Y2, X1).

solution([]).
solution([Q|B]) :-
  constraint(B, Q, 1),
  solution(B).

% count how many attacks this queen has
attacks(X, Y, X, Y, 0) :- !.
attacks(_, Y1, _, Y2, 1) :- Y1 #= Y2, !.
attacks(X1, Y1, X2, Y2, 1) :- abs(Y2 - Y1) #= abs(X2 - X1), !.
attacks(X1, Y1, X2, Y2, 1) :- abs(Y2 - Y1) #= abs(X1 - X2), !.
attacks(_, _, _, _, 0).

numberOfAttacks([], _, _, _, 0).
numberOfAttacks([Y1|Ys], X1, X2, Y2, Count) :-
  X #= X1 + 1,
  numberOfAttacks(Ys, X, X2, Y2, CountOthers),
  attacks(X1, Y1, X2, Y2, C),
  Count #= CountOthers + C.

putMax(_, [], _, []).
putMax(B, [Y1|Ys], X1, [Count|ArrayOthers]) :-
  XN #= X1 + 1,
  putMax(B, Ys, XN, ArrayOthers),
  numberOfAttacks(B, 1, X1, Y1, Count).

 % Find the queen in the board with the max number of conflicts.
findMaxConflict([], 0).
findMaxConflict([_], 0).
findMaxConflict(B, I) :-
  putMax(B, B, 1, Maxes),
  max(Maxes, M),
  indexOf(Maxes, M, I).

 % Move that queen to the row with the least possible conflicts.
 % newBoardWithMovedRow(B, I, newB) :- B.

minConflict(B) :-
 solution(B);           % DONE
 findMaxConflict(B, I), % DONE
 newBoardWithMovedRow(B, I, newB),
 minConflict(newB).
 
goodStart([], _, _).
goodStart([F], N, _) :- F is N.
goodStart([F,S | T], N, P) :-
  F is P, S is N,
  goodStart(T, N - 1, P + 1).

% Creating a good starting board with N queens.
generateInitialBoard(N, B) :-
  length(B, N),
  B ins 1..N,
  goodStart(B, N, 1).
 
solveQueens(N) :-
  generateInitialBoard(N, B), 
  print(B),
  minConflict(B).
