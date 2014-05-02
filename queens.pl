solution(_, []).
solution(N, [X/Y|Others]) :-
    solution(N, Others),
    between(1, N, Y),
    noattack(X/Y, Others).

noattack(_,[]).
noattack(X/Y, [X1/Y1 | Others]) :-
    Y =\= Y1,       % Q e Q1 sono su righe diverse
    Y1-Y =\= X1-X,  % Q e Q1 sono su diagonali diverse
    Y1-Y =\= X-X1,
    noattack( X/Y, Others). % Q non attacca regine nella sottolista Others

% TEMPLATE DELLE SOLUZIONI: c'è una regina su ogni colonna:
template(N, L) :-
    findall(I/_, between(1,N,I), L).
