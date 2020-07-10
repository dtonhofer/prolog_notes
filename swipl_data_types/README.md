# About SWI-Prolog data types

- This page is linked from SWI-Prolog comment section of ["Verify Type of a Term"](https://eu.swi-prolog.org/pldoc/man?section=typetest).
- The SWI-Prolog wiki has more on data types here: [SWI-Prolog data types](https://eu.swi-prolog.org/datatypes.txt).

The Wikipedia page for [data type](https://en.wikipedia.org/wiki/Data_type) says: 

> In computer science and computer programming, a data type or simply type is an attribute of data
> which tells the compiler or interpreter how the programmer intends to use the data.

Sounds about right as a definition.

(Is it _datatype_ or _data type_? Wikipedia says [_data type_](https://en.wikipedia.org/wiki/Data_type), so I will use that. In
German, it is of course [_Datentyp_](https://de.wikipedia.org/wiki/Datentyp))

Note that in basic Prolog, data types are all what could be called "primitive". There are generally no
[Abstract Data Types](https://en.wikipedia.org/wiki/Data_type#Abstract_data_types) or opaque data types.
(except maybe the Dictionary of SWI Prolog and the "blobs" hiding some system facility like sockets etc.)

Structure is given by arranging instances of the primitive data types into a graph of arbitrary depth, including depth zero, 
forming a **term**. That graph is generally acyclic, and generally a tree. Once created, a graph cannot be changed, but if it
has leaves that are "uninstantiated" (leaves which I call "fresh terms"), the graph can grow at those leaves over time 
as new terms are grafted onto those places. Once all the "fresh terms" remain, the term is called **ground**. Prolog **variables**
(aka. "logical variables") name places in a term graph. And "fresh terms" may or may not be named by variables. When
printed, these are represented by "default" variables names staring with an underscore:

```
?- format("~q",[f(Y,Z)]).
f(_2560,_2562)
true.
```

## SWI Prolog data type tree

An image depicting the built-in data types one may encounter in SWI Prolog can be found below.

- **The diagram in SVG format**: [SVG file](swipl_datatype_tree/swipl_data_type_tree.svg), can be visualized in a browser and easily panned & zoomed. 
- **The diagram in PNG format**: [PNG file](swipl_datatype_tree/swipl_data_type_tree.png), awkward to use.
- **The editable diagram**: [graphml file](swipl_datatype_tree/swipl_data_type_tree.graphml), in [graphml](http://graphml.graphdrawing.org/) format.
  Edited with the free (but not open) Java-based [yEd](https://www.yworks.com/products/yed) editor
  (Note that you have to switch off antialiasing if you use it, otherwise it feels like driving an ocean liner).
- [Pure text version](swipl_datatype_tree/swipl_data_type_tree.txt), not as complete, also shown below.
   - [Pure text version as ODT](swipl_datatype_tree/swipl_data_type_tree.odt)
   - [Pure text version as PDF](swipl_datatype_tree/swipl_data_type_tree.pdf)

SWI-Prolog built-in data types are similar to the types of other ISO-standard Prologs. Additional 
features may be found, in particular to support some kind of object orientation. See
[Comparison of Prolog implementations](https://en.wikipedia.org/wiki/Comparison_of_Prolog_implementations). 

In SWI-Prolog there is no way to define new data types from the base data types,
([algebraically](https://en.wikipedia.org/wiki/Algebraic_data_type) or otherwise), so these are the only one there are.

The diagram may change in the future. For example, the _dict_ data type may move out from under the _compound_ data type. 
We will see what happens.

```text
                                                                          any T 
                                                                            |
                                                  +-------------------------+------------------------+
                                                  |                                                  |
                                                var(T)                                            nonvar(T)
                                         ("is T a variable name                                      |
                                          that is still fresh                                        |
                                     variable at this point in time?")                               |
                                                                         +---------------------------+-----------------------------+
                                                                         |                                                         |      
                                                                     atomic(T)                                                 compound(T)                                
                                                                         |                                                         |
                                      +----------------------------------+------------------------+                 +--------------+-------------+
                                      |                                  |                        |                 |                            |
                                  blob(T,_)                           string(T)                number(T)       "compound term            "compound term
                                      |                                                           |             of arity 0"                of arity > 0"
                                      |                                                           |                                              |
              +-----------------------+-----------------------+                         +---------+---------+                   +----------------+-------------+
              |                       |                       |                         |                   |                   |                |             |            
       (other blob types)    blob(T,reserved_symbol)     blob(T,text)           rational(T,Nu,De)        float(T)              dict          "head of       ...others  
      encapsulated foreign            |                    atom(T)                      |                                       |            of a list"
           resources                  |                       |                         |                                (this seems to     '[|]'(H,Rs)
                                      |                       |                         |                              be an encapsulated      [H|Rs]
                             +--------+--------+              |              +----------+----------+                     data structure)         |
                             |                 |              |              |                     |                                       (a nonlocal
                           T==[]             T\==[]           |   rational(X),\+integer(X)      integer(T)                               structure; there 
                        empty list        dict functor        |      (proper rational)                                                    may or may not
                                                              |                                                                            be an actual
                                                              |                                                                        list beyond the head)
                                           +------------------+------------------+
                                           |                  |                  |
                                      lenghth=0           length=1            atom with
                                   "the empty atom"      "character"          length>1
```

Note the representation for sequences of characters:

- SWI-Prolog _string_, a dedicated data type (for example: `"Hello"`). Note that you can actually configure how the SWI-Prolog reader
  interpretes what is in between `"` double quotes. See the flag `double_quotes` (which can be any of `codes`, `chars`, `atom`, `string`) 
  in the manual page for [current_prolog_flag_2](https://www.swi-prolog.org/pldoc/doc_for?object=current_prolog_flag/2). For a lot
  of information about the SWI-Prolog _string_, see [The string type and its double quoted syntax](https://www.swi-prolog.org/pldoc/man?section=strings).
- The _atom_, the classical Prolog data type for sequences of characters: `'XYZ'` or `hello`. (Not to be confused with the _atom_ or 
  of [_atomic formula_](https://en.wikipedia.org/wiki/Atomic_formula) of First-Order Logic, which is a predicate applied to some terms: _p(x,y,f(z))_.
- A _list of characters_, where Prolog "characters" are understood to be atoms of length 1: `[h, e, l, l, o]`. See 
  [`atom_chars/2`](https://www.swi-prolog.org/pldoc/doc_for?object=atom_chars/2), which relates an atom or a string to a list of characters.
- A _list of code values (character codes)_. The code values are integers. In SWI-Prolog, they are the code values of the
  [UCS-2](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set) Unicode code points. For `hello`, the list of code values
  would thus be `[104, 101, 108, 108, 111]`. See [`atom_codes/2`](https://www.swi-prolog.org/pldoc/doc_for?object=atom_codes/2), 
  which relates an atom or a string to a list of code points.
  
 Note that a [DCG](https://www.swi-prolog.org/pldoc/doc_for?object=phrase/3) rule deals with _lists_ in general. When processing text,
 and depending on how it has been programmed, it may expect a list of characters, a list of character codes or even a list of
 strings of length 1 (the last situation would be unlikely though).

## Code implementing the type tree decision sequence when going from root to leaf 

[This](code/tagging.pl) is Prolog code which follows the data type tree above to "tag" the elements of a term.

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

## Notes

- There is a question on Stack Overflow about this:
  [What are the data types in Prolog?](https://stackoverflow.com/questions/12038009/what-are-the-data-types-in-prolog)
   - Where a reference is given to
     [Richard A. O'Keefe's draft Prolog Standard](http://www.complang.tuwien.ac.at/ulrich/iso-prolog/okeefe.txt) (1984), a 
      document which contains a very simple type tree.
- [Logtalk](https://logtalk.org/) has actual data types and OO-style message handlers. This is achieved by setting
  up Prolog Modules around terms, which have the characteristics of objects (Prolog need a proper hierarchical Module system)
- Type tests seems to be **non-logical** because they may succeed or fail on the same call depending on the current 
  state of the computation. The "logicalness" can be recovered if one considers that they have a hidden additional 
  argument: A term representing the "current computational state" of the system. Well, that view probably doesn't help much, 
  except in a philosophical sense.

## Compound terms of arity 0 vs. atoms in "predicate roles" and in "function roles"

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

## Predicates for Analyzing/Constructing a Term

[See this page](term_analysis_construction/)

## On Type testing

See this '92 paper collection: https://mitpress.mit.edu/books/types-logic-programming

[Covington et al.](https://arxiv.org/abs/0911.2899) says on page 30:

> Develop your own ad hoc run-time type and mode checking system. Many problems during development
> (especially if the program is large and/or there are several developers involved) are caused by
> passing incorrect arguments. Even if the documentation is there to explain, for each predicate,
> which arguments are expected on entry and on successful exit, they can be, and all too often they
> are, overlooked or ignored. Moreover, when a “wrong” argument is passed, erratic behavior can manifest
> itself far from where the mistake was made (and of course, following Murphy’s laws, at the most inconvenient time).
>
> In order to significantly mitigate such problems, do take the time to write your own predicates
> for checking the legality of arguments on entry to and on exit from your procedures. In the production
> version, the goals you added for these checks can be compiled away using goal_expansion/2.

The above is fun, but is not a scalable approach. What does exist? (How is it done in Logtalk? How in Lambda Prolog?)

### plspec

There is `plspec`, a "spec" approach inspired by the "spec" approach of Clojure (Clojure being a Scheme for the JVM that
has no type checking; although there is "Typed Clojure" it does not seem to be liked or used. Clojure specs 
provides the possibility to add annotations to perform runtime checks on precondition, postconditions and invariants). 

- Paper: [plspec – A Specification Languagefor Prolog Data](https://www3.hhu.de/stups/downloads/pdf/plspec.pdf)
- https://github.com/wysiib/plspec

### "Typed Prolog*

Among others...

- Paper: [Towards Typed Prolog](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.456.7365) 
  _Tom Schrijvers, Vitor Santos Costa, Jan Wielemaker, and Bart Demoen_ (2008)
- https://eu.swi-prolog.org/pack/list?p=type_check (This probably is the corresponding pack, but it is dead??) 

### Simple checks at runtime

Comparing various type-testing approaches with a bit of [Unit Test Code](../code/unit_tests_for_must_be.pl)

  - Default approach which fails silently if the answer is "don't know" (the on implemented currently in most Prologs)
  - "Sufficiently instantiated" approach which throws if the answer is "don't know"
  - [`must_be/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=must_be/2) approach which throws unless the answer
    is "yes, the type matches"
  - `can_be/2` approach which throws only if the answer is "it's never going to be that type"
  
