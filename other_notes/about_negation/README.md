# Negation-as-failure

These are comments relative to the [page for the `\+` operator](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%2B)/1) in the SWI-Prolog manual.

## Interpretation

Read `\+ p(X)` as 

- _there is no evidence for `p(X)`_ or 
- _there is no proof for `p(X)`_.

This is not the same as the "classical negation" (alias "strong negation") normally seen in logic. 
Instead it is a kind of default reasoning whereby everything that cannot be proven by the program
is assumed to have a special truth value "false by default" or "false unless proven differenty".
One then conflates this truth value with the classical truth value `false` (maybe an error?).
In literatue, the `\+` operator is sometimes written as `naf` (for negation-as-failure) or `~`.

Using this "negation as failure" or "default negation" is often sufficient. Modeling 
some problems, however, demands classical negation.

An example of when classical negation is useful,
from the _smodels_ manual ([Lparse 1.0 User's Manual](http://www.tcs.hut.fi/Software/smodels/lparse.ps.gz)), p.33:

Suppose that we want to check whether it is safe to cross railroad tracks This could be expressed by the rule:

```
safe :- \+ train.
```

The problem here is that we consider the crossing to be safe if we can't prove that a train is coming.
A more safe approach would be to declare the crossing safe only if we _can_ prove that the train is,
indeed, _not_ coming:

```
safe :- ¬train.
```

In this case, approaches other than Prolog may be needed: Full first-order theorem provers
like [Vampire](http://www.vprover.org/) or 
Answer-Set-Programming systems ("stable model semantics") like
[smodels](http://www.tcs.hut.fi/Software/smodels/) or [Potassco](https://potassco.org/doc/).

## Literature

Wikipedia: ["Negation as Failure"](https://en.wikipedia.org/wiki/Negation_as_failure). Check out the references there, too.

   - [Negation as failure](http://www.doc.ic.ac.uk/~klc/neg.html) (First Published in: Logic and Data Bases, editors Gallaire and Minker, 1978)
   - Sheperdson Surveys: 
      - [Negation as Failure: A Comparison of Clark's Completed Data Dase and Reiter's Closed World Assumption](https://www.sciencedirect.com/science/article/pii/0743106684900232) J.C. Sheperdson, in: The Journal of Logic Programming, vol 1, 1984, pages 51–81.
      - [Negation as Failure II](https://www.sciencedirect.com/science/article/pii/0743106685900184) J.C. Sheperdson, in: The Journal of Logic Programming, vol 3, 1985, pages 185-202.
   - [Logic programming and negation: A survey](https://www.sciencedirect.com/science/article/pii/0743106694900248), Krzysztof R. Apt, Roland N. Bol, in: The Journal of Logic Programming, Volumes 19–20, Supplement 1, May–July 1994, Pages 9-71

## Non-monotonicity

Negation-as-failure is a "nonomontonic" computation of truth values in that, if the logic program
is expanded by adding positive facts (we can't add negative facts facts to a Prolog program)
some statements formerly `true` may flip to `false`:

```
:- dynamic(q/1).
q(1).
p(X) :- \+ q(X).
```

Then

```
?- p(1).
false.

?- p(2).
true.
```

But if we add a new (and positive) fact `q(2)`, non-monotonicity arises. Not all the statements formerly `true` stay `true`:

```
?- assertz(q(2)).
true.

?- p(2).
false.
```

In fact, in Prolog the problem of having a consistent program (a program that doesn't give both classical
truth values to a statement) is whisked away by onyl allowing programs that give the value `true` to statements.
All the remaining statements are assumed to be `default false`. However, as seen above, the `\+` allows
one to indirectly give the value `true` to statements by passing through the pool of `default false` statements.

Can we create a inconsistent program? One in which `p(2)` is both `true` and `false`?

```
q(1).
q(2).
p(2).
p(X) :- \+ q(X).
```

Actually not:

```
?- p(2).      
true ;        % p(2) is true, maybe there are other solutions
false.        % No.
```

No, we can't! The success `p(2)` "paints over" the failure of `p(2)` via `q(2)`.  
In Prolog, only success counts, and Prolog actively looks only for success, never for failure.
It's quite "asymmetric".

One could imagine a logic programming language with assigns truth values `true` or `false`, or better 
one of the four values of Belnap's four-valued logic 
namely `true`, `false`, `both`, `none` (arranged on a lattice) to statements, but it wouldn't be Prolog. 

For more on Belnap's 4-valued logic see;

   - [This overview](http://www.filosofia.unimi.it/dagostino/wp-content/uploads/2017/05/Belnap.pdf) (PDF) by Marcello D’Agostino
   - [Original paper by Belnap](https://link.springer.com/chapter/10.1007/978-94-010-1161-7_2) at Strpinger (paywalled)
   - [Paraconsistent Logic](https://plato.stanford.edu/entries/logic-paraconsistent/) at the Stanford Encyclopedia of Philosophy.
   - [Four-valued logic](https://en.wikipedia.org/wiki/Four-valued_logic) at Wikipedia

This can be an effect that is desired or not. See also:

- [Non-monotonic Logic](https://plato.stanford.edu/entries/logic-nonmonotonic/)
- [Logical Approaches to Defeasible Reasoning](https://plato.stanford.edu/entries/reasoning-defeasible/#LogiAppr)

## "Floundering"

In [Logic programming and negation: A survey](https://www.sciencedirect.com/science/article/pii/0743106694900248),
(Krzysztof R.Apt, Roland N.Bol, 1994), we read:

> One of the complications concerning SLDNF resolution (i.e. the Prolog proof search: SLD resolution with Negation as Failure)
> is so-called _floundering_ - a generation of a node which consists exclusively of nonground negative literals,
> because then selection of any literal ends the derivation in an abnormal way. In the definition here provided, 
> floundering is treated differently
> - it arises as soon as a nonground negative literal is selected. Clearly, this small change has no effect on 
> the theory of SLDNF resolution, since the original notion of floundering can be easily defined."

This seems to be about the following problem:

Consider the program

```
q(1).
p(X) :- \+ q(X).
```

Now ask _Is it true that `p(d)`?_ 

```
?- p(d).
true.
```

The answer is _Yes, because `q(d)` fails ("there is no evidence that `q(d)` is true") and thus `\+ q(d)` succeeds_.

However, if you use a query with an unbound variable

```
?- p(X).
false.
```

The question is _Is there any `X` such that `p(X)`, i.e. such that `\+ q(X)`, i.e. such that there is no proof for `q(X)`_ ? 
Note that this is a very weak question - it is highly likely that there is such an `X` (even in the actual domain of
`q/1`) unless `q/1` is true everywhere. The correct answer would be 
_Yes, any `X` of the domain of `q/1` different from 1 fits_. 
This is not expressible in Prolog but _would_ be expressed by an enumeration if the domain for `p/1` were finite.
Prolog would generate all elements of the domain except 1. However, the goal `\+ q(X)` with unbound `X` has a
different meaning than the intended one. It asks _is there no `X` such that `q(X)`?_.
This is `false` because there is `q(1)`. (Note that that 1 is never returned as answer, because the query fails at precisely that point,
and that binding of `X` to 1 will be erased due to backtracking.)

**An inconsistency arises!**

Maybe the Prolog processor should throw an exception when it finds a body subject to floundering.

The problem stems from the fact that Prolog basically relies on explicit enumerations over domains 
followed by tests (but for some reason, explicit domains have never been given explicit treatment in
Prolog syntax and unification) 

Explicitly specifying the domain makes the problem go away because X is successively bound to an actual 
value, so the goal affected by the `\+` is ground:

```
?- member(X,[a,b,c]),p(X).
X = a ;
X = b ;
X = c.

?- member(X,[1,2,3]),p(X).
X = 2 ;
X = 3.
``` 

One could force the variable to be ground and in-domain:

```
q(1).
p(X) :- must_be(integer,X) -> \+ q(X).
                   
?- p(1).
false.

?- p(2).
true.

?- p(a).
ERROR: Type error: `integer' expected, found `a' (an atom)

?- p(X).
ERROR: Arguments are not sufficiently instantiated
```

Or one could "freeze" the negative goal using freeze/2, so that it is only run once the variable `X` is bound. 
In this example, although the call to freeze/2 succeeds, the query ultimately fails:

```
q(1).
p(X) :- freeze(X, \+ q(X)),format("past freeze").
```

Then

```
?- p(X).
past freeze
freeze(X, \+q(X)).  <--- residual goal that has not been resolved yet

?- p(X),X=2.
past freeze
X = 2.

?- p(X),X=1.
past freeze
false.
```

If Prolog were more "modeling in logic" than "programming in logic" one could consider a symbolic 
answer describing the complement of q(X) relative to the domain of p/1, something like a list comprehension:

```
?- p(X).
setof(X,q(X),Xs), complement(Xs,domain(p/1),Result).
```

## Using "double negation"

If you want to run some goal in an "isolated context":

```
\+ \+ Goal
```

The above really makes clear that you are only interested in whether Goal will succeed or fail 
and that any bindings shall be trashed and have no influence on further computation (except 
for any side-effects generated when proving Goal, which are forever inscribed in the universe and cannot be rolled back).

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

Especially useful if you want to isolate your debugging printouts lest they change something due to small detail:

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
