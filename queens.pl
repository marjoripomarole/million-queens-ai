:- use_module(library(clpfd)).


pickRandomly(Count, From, To) :-
  random_permutation(From, Scrambled),
  append(To, _, Scrambled),
  length(To, Count).

indexOf([Y|_], Y, 0):- !.
indexOf([_|Ys], Y, I):-
  indexOf(Ys, Y, I1),
  !,
  I is I1+1.

constraint([], _, _).
constraint([Y1|Ys], Y2, X) :-
  Y1 #\= Y2,
  abs(Y2 - Y1) #\= X,
  X1 is X + 1,
  constraint(Ys, Y2, X1).

solution([]).
solution([Q|B]) :-
  constraint(B, Q, 1),
  solution(B).

% See if queen attacks one another or not. 0 if no, 1 if yes
attacks(X, Y, X, Y, 0) :- !.
attacks(_, Y1, _, Y2, 1) :- Y1 #= Y2, !.
attacks(X1, Y1, X2, Y2, 1) :- abs(Y2 - Y1) #= abs(X2 - X1), !.
attacks(X1, Y1, X2, Y2, 1) :- abs(Y2 - Y1) #= abs(X1 - X2), !.
attacks(_, _, _, _, 0).

numberOfAttacks([], _, _, _, 0).
numberOfAttacks([Y1|Ys], X1, X2, Y2, Count) :-
  X is X1 + 1,
  numberOfAttacks(Ys, X, X2, Y2, CountOthers),
  attacks(X1, Y1, X2, Y2, C),
  Count is CountOthers + C.

putMax(_, [], _, []) :- !.
putMax(B, [Y1|Ys], X1, [Count|ArrayOthers]) :-
  XN is X1 + 1,
  putMax(B, Ys, XN, ArrayOthers),
  numberOfAttacks(B, 1, X1, Y1, Count).

 % Find the queen in the board with the max number of conflicts.
findMaxConflict([], 0).
findMaxConflict([_], 0).
findMaxConflict(B, I) :-
  putMax(B, B, 1, Maxes),
  max_list(Maxes, M),
  findall(X, nth0(X, Maxes, M), Indexes),
  pickRandomly(1, Indexes, [I]).

numberOfAttacksWithY([], _, _, 0).
numberOfAttacksWithY([X1/Y1|Qs], X2, Y2, Count) :-
  numberOfAttacksWithY(Qs, X2, Y2, CountOthers),
  attacks(X1, Y1, X2, Y2, C),
  Count #= CountOthers + C.

getAttackCounts(_, _, 0, [], _).
getAttackCounts(B, X, Y, [C|CountsRest], YToSkip) :-
  Y #> 0,
  Y1 is Y - 1,
  ( 
    Y is YToSkip -> 
      C is 9999999, % Setting this number really high because it should never
                    % be picked
      getAttackCounts(B, X, Y1, CountsRest, YToSkip)

    ; 
      numberOfAttacksWithY(B, X, Y, C),
      getAttackCounts(B, X, Y1, CountsRest, YToSkip)
  ).

createNewBoard([_|T], 0, X, [X|T]).
createNewBoard([H|T], I, X, [H|R]):-
  I > 0,
  I1 is I-1,
  createNewBoard(T, I1, X, R).

% Moves queen at index I to the row with the least possible conflicts.
% Works with X\Y representation.
newBoardWithMinConflict(OriginalBoard, I, NextB) :-
  % removes element from list at index I
  nth0(I, OriginalBoard, X/Y, BoardWithoutI),
  length(OriginalBoard, Lenght),
  % get attack counts on each Y of that Queen.
  getAttackCounts(BoardWithoutI, X, Lenght, CountsR, Y),
  reverse(CountsR, Counts),
  % find min index
  min_list(Counts, Min),
  findall(Goal, nth0(Goal, Counts, Min), Indexes),
  % min_list(Indexes, NewIndex),
  pickRandomly(1, Indexes, [NewIndex]),
  NewIndex1 is NewIndex + 1,
  createNewBoard(OriginalBoard, I, X/NewIndex1, NextB).

% Returns representation of the board with X\Y position
boardWithXAndY([], [], _).
boardWithXAndY([Y1|Ys], [I/Y1|BXY], I) :-
  I1 is I + 1,
  boardWithXAndY(Ys, BXY, I1).

% Returns representation of the board without X\Y
boardWithoutXAndY([], []).
boardWithoutXAndY([_/Y|Qs], [Y|BXY]) :-
  boardWithoutXAndY(Qs, BXY).

% Top level algorithm
minConflict(B, B, Steps, Steps) :- solution(B), !.
minConflict(B, Result, Steps, Acc) :-
  findMaxConflict(B, I), % Index start from 0
  boardWithXAndY(B, BXY, 1),
  newBoardWithMinConflict(BXY, I, NewB),
  boardWithoutXAndY(NewB, NewB1),
  Acc1 is Acc + 1,
  Acc1 =< 100,
  minConflict(NewB1, Result, Steps, Acc1).
 
goodStart([], _, _).
goodStart([F], N, _) :- F is N.
goodStart([F,S | T], N, P) :-
  F is P, S is N,
  goodStart(T, N - 1, P + 1).

% Returns a NxN board with a queen in each column and row
generateInitialBoard(N, B) :-
  length(B, N),
  B ins 1..N,
  goodStart(B, N, 1).
 
% Returns solution for NxN board
solveQueens(N, Result, Steps) :-
  generateInitialBoard(N, B), 
  minConflict(B, Result, Steps, 0), !.

solveNQueensXTimes(_, 0, []).
solveNQueensXTimes(N, X, [Steps|Rest]) :-
  solveQueens(N, _, Steps),
  X1 is X - 1,
  solveNQueensXTimes(N, X1, Rest), !.

medianSolveNQueensXTimes(N, X, M) :-
  solveNQueensXTimes(N, X, L),
  median(L, M).

median(L, Z) :-
    length(L, Length),
    I is Length div 2,
    Rem is Length rem 2,
    msort(L, S),
    maplist(sumlist, [[I, Rem], [I, 1]], Mid),
    maplist(nth1, Mid, [S, S], X),
    sumlist(X, Y),
    Z is Y/2.
