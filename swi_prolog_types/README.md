# About SWI-Prolog datatypes

## SWI Prolog datatype decision tree image

An image depicting the built-in "datatypes" one may encounter in SWI Prolog (which are 
similary to the types of other Prolog). As there is no way to define new datatypes from these
base datatypes ([algebraically](https://en.wikipedia.org/wiki/Algebraic_data_type) or
otherwise), these are the only one there are.

![screenshot](swi_prolog_type_tree/swi_prolog_type_tree_mini.png)

- [graphml file](swi_prolog_type_tree/swi_prolog_type_tree.graphml). **The editable diagram
  in [graphml](http://graphml.graphdrawing.org/) format.** 
  Edited with the free (but not open) Java-based [yEd](https://www.yworks.com/products/yed) editor
  (Note that you have to switch off antialiasing if you use it, otherwise it feels like driving
   an ocean liner).
- [PNG file](swi_prolog_type_tree/swi_prolog_type_tree.png). **The diagram in PNG format**, a bit
  awkward to use. PNG image data, 6704 x 4147, 8-bit/color RGB, non-interlaced.
- [SVG file](swi_prolog_type_tree/swi_prolog_type_tree.svg). **The diagram in SVG format**. Best! 
  Can be visualized in a browser and easily panned & zoomed. **Use this**.

### Compound terms of arity 0 vs atoms in predicate roles vs. in function roles

In predicate role, atoms and compound terms of arity 0 are the same:

```
a   :- write(a1),nl.
a() :- write(a2),nl.

?- a.
a1
true ;
a2
true.
```

```
b :- c,c().
c :- write(c),nl.

?- b.
c
c
true.
```

In arithmetic function role, likewise. They denote constant functions:

```
?- X is pi.
X = 3.141592653589793.

?- X is pi().
X = 3.141592653589793.
```

But these are not the same syntactic structures:

```
?- pi == pi().
false.

?- pi = pi().
false.
```

See the notes on [Compound terms with zero arguments](https://eu.swi-prolog.org/pldoc/man?section=ext-compound-zero)

## Code implementing the datatype decision tree

[This](code/tagging.pl) is Prolog code which follows the datatype tree above to "tag" the elements of a term.

Contains Unit Test code to be run with `rt(_).`.

Examples (all run with `once/1` to close the open choicepoint):

```logtalk
?- once(tag(X,S)).
S = var(X).

?- once(tag(100,S)).
S = int(100).

?- once(tag(100.1,S)).
S = float(100.1).

?- once(tag(1/3,S)).
S = compound(/, [gnd], [int(1), int(3)]).

?- once(tag(d{x:1,y:1,z:Z},S)).
S = dict(d, [nongnd], [atom(x)-int(1), atom(y)-int(1), atom(z)-var(Z)]).

?- once(tag([1,X,2],S)).
S = lbox([list, nongnd], int(1), lbox([list, nongnd], var(X), lbox([list, gnd], int(2), emptylist))).

?- once(tag(p(X,[1,2]),S)).
S = compound(p, [nongnd], [var(X), lbox([list, gnd], int(1), lbox([list, gnd], int(2), emptylist))]).
```

## Reading

- For a description of data types, see this SWI-Prolog wiki entry:
  [SWI-Prolog datatypes](https://eu.swi-prolog.org/datatypes.txt)

- For the type-testing predicates see this SWI-Prolog manual page:
  [Verify Type of a Term](https://eu.swi-prolog.org/pldoc/man?section=typetest)

## Notes

- There is a question on Stack Overflow about this: [What are the data types in Prolog?](https://stackoverflow.com/questions/12038009/what-are-the-data-types-in-prolog)
- [Logtalk](https://logtalk.org/) has actual datatypes and OO-style message handlers. This is achieved by setting up Prolog Modules around terms, which have the characteristics of objects (Prolog need a proper hierarchical Module system)
- Type tests seems to be non-logical but they are if one considers them to have a hidden additional argument: A term representing the "current computational state" of the system. That view probably doesn't help much.

## Disassembling the term

Résumé:

![Disassembling Terms](disassembling_terms/disassembling_terms.png)

## On Type testing

See this '92 paper collection: https://mitpress.mit.edu/books/types-logic-programming

[Covington et al.](https://arxiv.org/abs/0911.2899) says on page 30:

> Develop your own ad hoc run-time type and mode checking system. Many problems during development (especially if the program is large and/or there are several developers involved) are caused by passing incorrect arguments. Even if the documentation is there to explain, for each predicate, which arguments are expected on entry and on successful exit, they can be, and all too often they are, overlooked or ignored. Moreover, when a “wrong” argument is passed, erratic behavior can manifest itself far from where the mistake was made (and of course, following Murphy’s laws, at the most inconvenient time).
>
> In order to significantly mitigate such problems, do take the time to write your own predicates for checking the legality of arguments on entry to and on exit from your procedures. In the production version, the goals you added for these checks can be compiled away using goal_expansion/2.

The above is fun, but is not a scalable approach. What does exist? (How is it done in Logtalk? How in Lambda Prolog?)

### plspec

There is `plspec`, a "spec" approach inspired by the "spec" approach of Clojure (Clojure being a Scheme for the JVM that
has no type checking; although there is "Typed Clojure" it does not seem to be liked or used. Clojure specs provides the possibility to add annotations to perform runtime checks on precondition, postconditions and invariants). 

- Paper: [plspec – A Specification Languagefor Prolog Data](https://www3.hhu.de/stups/downloads/pdf/plspec.pdf)
- https://github.com/wysiib/plspec

### Typed Prolog

Among others...

- Paper: [Towards Typed Prolog](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.456.7365) _Tom Schrijvers, Vitor Santos Costa, Jan Wielemaker, and Bart Demoen_ (2008)
- https://eu.swi-prolog.org/pack/list?p=type_check (This probably is the corresponding pack, but it is dead) 

