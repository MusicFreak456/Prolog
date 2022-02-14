# Prolog

Very basic prolog implementation written in Ocaml. Based on some ideas from *Structure and Interpretation of Computer Programs*. Created as a final project for functional programming course at the University of Wrocław.

## Build and run

```
$ dune build
$ dune exec src/prolog.exe
```

## Interaction example

```
prolog > [./src/example.pl].
true.

prolog > ?- append([a,b,c], [d,e], R).

R: [a,b,c,d,e];

false.

prolog > ?- append([a,b,c], [d,e], R).

R: [a,b,c,d,e].

prolog > ?- append(X, Y, [a,b,c,d,e]).

X: [a,b,c,d,e]
Y: nil;

X: [a,b,c,d]
Y: [e];

X: [a,b,c]
Y: [d,e];

X: [a,b]
Y: [c,d,e];

X: [a]
Y: [b,c,d,e];

X: nil
Y: [a,b,c,d,e].

prolog > ?- parent(julia, Y), parent(Y, X).

X: wladyslaw
Y: agata;

X: stanislawa
Y: tomasz;

false.
```
