# Using "double negation"

If you want to run some goal in an "isolated context":

```
\+ \+ Goal
```

The above really makes clear that you are only interested in whether `Goal` will succeed or fail 
and that any bindings shall be discarded and have no influence on further computation (except 
for any side-effects generated when proving `Goal`, which are forever inscribed in the 
outer universe and cannot be rolled back).

Take the program:

```
f(1,2).
```

Then:

```
?- A=2, ( \+ \+ f(X,A) ), format("X is now ~q\n", [X]).
X is now _7808
A = 2.
```

Especially useful if you want to isolate your debugging printouts lest they change something due to small error:

```
ddd_isolate(X) :-
   debug(topic,"X is ~q\n",[X]),
   (X=[] ->  % ERROR: = instead of ==
    debug(topic,"X is the empty list\n",[]) ; true).
      
test(X) :- 
   debug(topic),                      % switch on debug printing for topic "topic"
   debug(topic,"X before: ~q\n",[X]), 
   \+ \+ ddd_isolate(X),
   debug(topic,"X after: ~q\n",[X]). 
```

Yes, it works:

```
?- test(12).

% X before: 12
% X is 12
% X after: 12
true.

?- test([]).

% X before: []
% X is []
% X is the empty list
% X after: []
true.

?- test(X).

% X before: _5354
% X is _5354
% X is the empty list
% X after: _5354          % changes have been erased 
true.
```

See also: [Salvaging a term out of a dropped search branch](/swipl_notes/about_salvaging_a_term_out_of_a_dropped_search_branch/README.md)

### Double negation hides information<a name="double_negation_hides_information"></a>

```
p(X) :- \+ \+ q(X).
q(1).
```

`p/1` and `q/1` are true for exactly the same values, but if you have only `p/1` (outside of a module for example), you cannot _ask_ for those values
to be delivered to you:

```
?- p(2).   % 2 is not a solution
false.

?- p(X).   % yes there is a solution but Prolog won't tell you which
true.

?- p(1).   % you guessed correctly!
true.
```

### Double negation in _forall/2_<a name="double_negation_in_foralltwo"></a>

Double negation is used for the predicate [`forall/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=forall/2):

```
forall(Cond, Action) :- \+ (Cond, \+ Action).
```

The SWI-Prolog manual states:

> The predicate `forall/2` is implemented as `\+ ( Cond, \+ Action)`, i.e., _There is no instantiation of `Cond` for which `Action` is false_.
> The use of double negation implies that `forall/2` does not change any variable bindings. 
> It proves a relation. The `forall/2` control structure can be used for its side-effects.

### A note on "double negation" from the Mercury manual<a name="double_negation_in_mercury"></a>

In the Mercury language ("Prolog with types"), the compiler apparently gets rid of double negation:

> [2.18 Elimination of double negation](https://mercurylang.org/information/doc-release/mercury_ref/Elimination-of-double-negation.html#Elimination-of-double-negation)
> 
> The treatment of inequality, universal quantification, implication, and logical equivalence
> as abbreviations can cause the introduction of double negations which could make otherwise
> well-formed code mode-incorrect. To avoid this problem, the language specifies that
> after syntax analysis and implicit quantification, and before mode analysis is performed, the
> implementation must delete any double negations and must replace any negations of con-
> junctions of negations with disjunctions. (Both of these transformations preserve the logical
> meaning and type-correctness of the code, and they preserve or improve mode-correctness:
> they never transform code fragments that would be well-moded into ones that would be ill-moded.)
