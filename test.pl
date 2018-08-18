
answer(X):-grandparent(ann, X).

grandparent(X, Z):-parent(X, Y),parent(Y, Z).
parent(X, Y):-mother(X, Y).
parent(X, Y):-father(X, Y).

mother(ann,betty).
mother(betty,doris).

