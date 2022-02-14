parent(julia, tomasz).
parent(julia, agata).
parent(agata, wladyslaw).
parent(tomasz, stanislawa).

cat(tom).
dog(snoop).
animal(X) :- cat(X).
animal(X) :- dog(X).

append([], X, X).
append([X|XS], YS, [X|R]) :-
  append(XS, YS ,R).
