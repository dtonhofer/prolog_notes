# The concept of the Prolog "variable"

The noun "variable" is used confusingly in the Prolog universe. 

Let's attempt to clarify (hopefully).

## Variable names

"Variable names" are clause-local names found in source code and user-readable
representations. They may be "anonymous":

```
f(X) :- g(X,_).
```

They may be generated on demand by predicates like [`print/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=print/1):

```
?- print(f(X) :- g(X,_)).
f(_1668):-g(_1668,_1674)
```

## Cells and terms

A variable name appearing in a clause designates (or "denotes") a "cell" in the
"global term store" at runtime. The term store is _global_ because any clause
activation may access any cell therein as long as the activation has been handed a 
reference to said cell.  

Consider the clause:

```
f(X,Y) :- g(X,Z),h(Z,Y).
```

It uses variables names `X`, `Y` and `Z`. At runtime, the activation of the clause
(its stack frame) is initialized with references into the global term store for
the arguments passed in: one for `X` and one for `Y`. A new reference into the 
global term store for the fresh variable `Z` is added. The actual variable names
used in source code, `X`, `Y`, `Z`, are unknown to the activation. 

Cells form a network that are a concrete representation of terms.

A term is generally considered to have tree structure (a tree of terms, the 
definition being recursive). Inner nodes of the tree are _compound terms_ and leaf 
nodes are either _atomic terms_ or _empty terms_. An _empty term_ leaf is printed
as a variable name. 

In a term, the same _empty term_ may appear in several distinct leaf positions
("the variable is shared") and cycles may be constructed. In that case, the
term can no longer be considered a tree  but must be considered a directed
graph, possibly cyclic.

If there is sharing of subterms inside the term graph, there is in principle no 
direct way to find out from Prolog.

One can consider each node of the term graph to be represented by a (either
shared or unique) cell in the global term store. Inner nodes are cells with
(possibly 0) children. Leaf nodes (or rather, nodes the the edge of the graph)
are cells with no children. 

An empty term is represented by an empty cell (a "hole", not to be confused 
with an Haskell [type holes](https://wiki.haskell.org/GHC/Typed_holes)). The
idea is that this represents an as-yet-undefined datum. The cell may be filled 
through unification as computation proceeds. The empty term is "instantiated"
(or "refined"). This represents accumulation of information about the problem to 
solve. Prolog does allow one to "uninstantiate" an instantiated, previously empty
term except when backtracking.

![[Concept of a variable](concept_of_variable.svg)]

Fluidly, one talks about "the term `X`" when one really wants to talk about 
"the (possibly empty) term designated by the variable name `X` at runtime".
Indeed, whatever can be found inside the parentheses of a predicate call can 
may called a "term".

Also, "term" may be used to designate the full graph reachable from a given 
node but sometimes it is used to designate single node of a graph. Watch out 
for context!

Note this important unification behaviour of empty cells:

When unifying two empty cells, the cells are merged and one cell disappears:

```
?- print(X),nl,print(Y),nl,X=Y,print(X),nl,print(Y),nl.
_7780
_7784
_7780
_7780
X = Y.
```

When unifying an empty cell and a nonempty cell, the empty cell disappears:

```
?- print(X),nl,X=a,print(X).
_9280
a
X = a.
```

Another example:

![[Unification example](unification_example.svg)]

## Variables

Fluidly, the noun "variable" may be used for:

- an _empty cell_ in the term store. In particular 
[_Attributed Variables_](https://eu.swi-prolog.org/pldoc/man?section=attvar) 
should rightly be called _Attributed Empty Cells_; the predicate name
of var/1 makes sense under the interpretation of "variable" testing whether
what's within the parentheses designates an "empty cell"
- a _variable name_ as found in source code, which may designate an empty
or nonempty cell at runtime (as in "the variable `X` in `var(X)`")
- a _variable name_ printed out at runtime, like `X` or `_124`, which
always designates an empty cell (otherwise the content of the cell would be printed). 
Empty cells do not have a name by themselves, although the Prolog toplevel may
make efforts to use the variable name used in the query for the user's convenience.

If the variable name designates an empty cell, one talks about _an unbound variable_
or an _uninstantiated variable_. 

There is also the _fresh variable_ (always uninstantiated at first), which is a
newly introduced variable name.

If the variable name designates a cell holding structure (a nonempty term) one
talks about _a bound variable_ or an _instantiated variable_ or one says that
_the variable is bound to a term_ (note the direction; **not** 
_term is bound to a variable_).

Compare with the entry for 
[variable](http://www.cse.unsw.edu.au/~billw/prologdict.html#variable) and 
[`var`](http://www.cse.unsw.edu.au/~billw/prologdict.html#termtype) in 
Bill Wilson's Prolog dictionary.

## `var(X)` and `nonvar(X)`

When you ask `var(X)` you are actually asking whether the variable name `X` 
"currently" (at query time, a non-logical concept) designates an empty cell.

The predicate would be less confusing if named `unbound(X)`.

Note that if the text within the parentheses of the `var(.)` call is not a 
variable name, the answer is immediately `false`. The compiler should 
warn about a call that always fails when it sees source text like `var(foo)`.

Similarly, when you ask `nonvar(X)` you are actually asking whether the 
variable name `X` "currently" designates anything other than an empty cell.
This is the complement of `var(X)`: either `var(X)` or `nonvar(X)` is true for
any `X`.  

Note that if the text within the parentheses of the `nonvar(.)` call is not a
variable name, the answer is immediately `true` immediately. The compiler should
warn about a call that always succeeds when it sees source text like `nonvar(foo)`.

## _Free_ variables

The definition given by the SWI-Prolog reference manual for
[`var/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=var/1) uses the adjective
_free_, which is something else entirely and should not be used: a variable
is _free_ in a formula (of logic or a lambda expression) if it does not appear
in a quantifier (or a lambda prefix) closing over the formula.

