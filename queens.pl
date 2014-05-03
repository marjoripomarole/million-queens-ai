:- use_module(library(clpfd)).

constraint([], _, _).
constraint([Y1|Ys], Y2, X) :-
  print(X),
  Y1 #\= Y2,
  abs(Y2 - Y1) #\= X,
  X1 #= X + 1,
  constraint(Ys, Y2, X1).

solution([]).
solution([Q|B]) :-
  constraint(B, Q, 1),
  solution(B).

% count how many attacks this queen has
numberOfAttacks([], _, 0).
numberOfAttacks(B, Q, Count).

 % Find the queen in the board with the max number of conflicts.
findMaxConflict([], 0).
findMaxConflict([_], 1).
findMaxConflict(B, I) :- I is 1.

 % Move that queen to the row with the least possible conflicts.
newBoardWithMovedRow(B, I, newB) :- B.

minConflict(B) :-
 solution(B);
 findMaxConflict(B, I),
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
  

/***
del(X, [X|T], T).
del(X, [Y|T], [Y|T1]) :- del(X, T, T1).

remove(A, B, C) :- del(A,B,C).
remove(A, B, B) :- not(member(A,B)).

removeAll([H|T], LPos, L) :- remove(H, LPos, LPos_H), 
           removeAll(T, LPos_H, L), !.
removeAll([], L, L). 
***/
