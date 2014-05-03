:- use_module(library(clpfd)).

max([X], X).
max([X|Xs], X):- max(Xs,Y), X >=Y.
max([X|Xs], N):- max(Xs,N), N > X.

indexOf([Y|_], Y, 0):- !.
indexOf([_|Ys], Y, I):-
  indexOf(Ys, Y, I1),
  !,
  I is I1+1.

indexes([Y], Y, 0, [0]) :- !.
indexes([X], Y, 0, []) :- !.
indexes([Y|Ys], Y, I, [I|Rest]):-
  indexes(Ys, Y, I1, Rest),
  !,
  I is I1 + 1.
indexes([_|Ys], Y, I, L):-
  indexes(Ys, Y, I1, L),
  !,
  I is I1 + 1.

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
% findMaxConflict([_], 0).
findMaxConflict(B, I) :-
  putMax(B, B, 1, Maxes),
  max(Maxes, M),
  indexOf(Maxes, M, I).

numberOfAttacksWithY([], _, _, 0).
numberOfAttacksWithY([X1/Y1|Qs], X2, Y2, Count) :-
  numberOfAttacksWithY(Qs, X2, Y2, CountOthers),
  attacks(X1, Y1, X2, Y2, C),
  Count #= CountOthers + C.

getAttackCounts(_, _, 0, [], _).
getAttackCounts(B, X, Y, [C|CountsRest], I) :-
  Y #> 0,
  Y1 #= Y - 1,
  numberOfAttacksWithY(B, X, Y, C),
  getAttackCounts(B, X, Y1, CountsRest, I).

createNewBoard([_|T], 0, X, [X|T]).
createNewBoard([H|T], I, X, [H|R]):-
  I > 0,
  I1 is I-1,
  createNewBoard(T, I1, X, R).

reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

% Move that queen to the row with the least possible conflicts.
newBoardWithMinConflict(B, I, NextB) :-
  % removes element from list at index I
  nth0(I, B, X/Y, NewB),
  write('New B '), write(NewB), nl,
  length(B, L),
  % get attack counts on all Y of that index
  getAttackCounts(NewB, X, L, CountsR, I1),
  reverse(CountsR,Counts,[]),
  write('Counts '), write(Counts), nl,
  % find min index
  min_list(Counts, M),
  reverse(Counts, CR, []),
  write('CR '), write(CR), nl,
  indexes(CR, M, _, Indexes),
  write('Indexes '), write(Indexes), nl,
  delete(Indexes, I, Indexes1),
  min_list(Indexes1, NewIndex),
  NewIndex1 #= NewIndex + 1,
  write('NewIndex '), write(NewIndex1), nl,
  createNewBoard(B, I, X/NewIndex1, NextB).

boardWithXAndY([], [], _).
boardWithXAndY([Y1|Ys], [I/Y1|BXY], I) :-
  I1 #= I + 1,
  boardWithXAndY(Ys, BXY, I1).

boardWithoutXAndY([], []).
boardWithoutXAndY([_/Y|Qs], [Y|BXY]) :-
  boardWithoutXAndY(Qs, BXY).

minConflict(B) :- solution(B), write(B), !.
minConflict(B) :-
  findMaxConflict(B, I), % Index start from 0
  write('I Max '), write(I), nl,
  boardWithXAndY(B, BXY, 1),
  write('BXY '), write(BXY), nl,
  newBoardWithMinConflict(BXY, I, NewB),
  boardWithoutXAndY(NewB, NewB1),
  write('NewB1 '), write(NewB1), nl,
  sleep(1),
  minConflict(NewB1).
 
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
  write('Start'), write(B), nl,
  minConflict(B).
