# Based on a note by Peter Ludemann

[Note](https://swi-prolog.discourse.group/t/friday-afternoon-post-a-quick-diagram-of-the-lp-landscape/1976/11?u=dtonhofer)

MU-Prolog and NU-Prolog [both from Melbourne University 1980s], took the position of “delay computing the negation until there’s
sufficient information for it to be sound.” 

Swipl’s [`dif/2`](https://www.swi-prolog.org/pldoc/doc_for?object=dif/2) 1 is an example of doing that.

This example might help.

- Query #1 gives a wrong result because `\=/2` was evaluated before `X` and `Y` were sufficiently ground.

```
1 ?- X \= Y, X = 1, Y = 2.
false.
```
- Query #2 is correct – `dif/2` is the same as `\=/2`, delayed until enough is known about the arguments to
give a correct answer.

```
2 ?- dif(X, Y), X = 1, Y = 2.
X = 1,
Y = 2.
```

```
3 ?- freeze(X, freeze(Y, X \= Y)), X = 1, Y = 2.
X = 1,
Y = 2.
```

Query #3 has too weak a delay condition:

```
4 ?- freeze(X, freeze(Y, X \= Y)), X = f(A), Y = f(B), A = 1, B = 2.
false.
```

```
5 ?- dif(X, Y), X = f(A), Y = f(B), A = 1, B = 2.
X = f(1),
Y = f(2),
A = 1,
B = 2.
```

