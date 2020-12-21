## "Floundering"<a name="floundering"></a>

"Flounding" suggest that the Prolog Processor trashes around or performs something repeatedly without progress:

From [The Free Dictionary](https://www.thefreedictionary.com/floundering), _to flounder_:

   1. To move clumsily or with little progress, as through water or mud.
   2. To act or function in a confused or directionless manner; struggle:
      _"Some ... floundered professionally, never quite deciding what they wanted to do"_ (Steve Olson).
     
but it's not a well-chosen vocabulary. In fact, "breakdown of `\+`" would be clearer.

From _"The Art of Prolog"_ 1st ed. p. 166:

> The implementation of negation as failure using the cut-fail combination does not work correctly for nonground
> goals (...). In most standard implementations of Prolog, it is the responsibility of the programmer to ensure
> that negated goals are grounded before they are solved. This can be done either by a static analysis of the
> program, or by a runtime heck, using the predicate _ground_ (...)

In [Logic programming and negation: A survey](https://www.sciencedirect.com/science/article/pii/0743106694900248),
(Krzysztof R.Apt, Roland N.Bol, 1994), we read:

> One of the complications concerning SLDNF resolution (i.e. the Prolog proof search: SLD resolution with Negation as Failure)
> is so-called _floundering_ - a generation of a node which consists exclusively of nonground negative literals,
> because then selection of any literal ends the derivation in an abnormal way. In the definition here provided, 
> floundering is treated differently
> - it arises as soon as a nonground negative literal is selected. Clearly, this small change has no effect on 
> the theory of SLDNF resolution, since the original notion of floundering can be easily defined."

In [Efficiently Iplementing SLG Resolution](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.49.5979) (Terrance Swift, David S. Warren, 1994), the following definition is provided in a footnote:

> A program _flounders_ if there is an atom \[an "atom" in the logical sense, i.e. a positive literal, i.e. a non-negated atomic Prolog goal\] 
> whose truth cannot be proven without making a call to a non-gound negative literal.

This seems to be about the following problem:

Consider the program:

```
q(1).
p(X) :- \+ q(X).
```

Now ask: _Is it true that `p(d)`?_ 

```
?- p(d).
true.
```

The answer is: _Yes, because `q(d)` fails ("there is no evidence that `q(d)` is true") and thus `\+ q(d)` succeeds_.

However, if you use a query with an unbound variable:

```
?- p(X).
false.
```

The question is 

_Is there any `X` such that `p(X)`_ i.e. such that `\+ q(X)`, i.e. such that there is no proof for `q(X)`? 

Note that this is a very weak question - it is highly likely that there is such an `X` (even in the actual domain of
`q/1`) unless `q/1` is true everywhere. The correct answer would be

_Yes, any `X` of the domain of `q/1` different from 1 fits_. 

This is not expressible in Prolog but _would_ be expressed by an enumeration if the domain for `p/1` were finite.
Prolog would generate all elements of the domain except 1. 

However, the goal `\+ q(X)` with unbound `X` has a different meaning than the intended one. It asks 

_is there no `X` such that `q(X)`?_

This is `false` because there is `q(1)`. (Note that that 1 is never returned as answer, because the query fails at precisely that point,
and that binding of `X` to 1 will be erased due to backtracking.)

**An inconsistency arises!**

This seems to happen whenever the goal wrapped by `\+` contains unbound "free variables" that also occur outside the wrapped goal. Do not do this!

Maybe the Prolog processor should throw an exception when it finds a body subject to floundering, but in general this would only be detectable at runtime.

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

Or maybe like this:

```
q(1).                 

p(X) :- 
   ground(X),
   !,
   \+ q(X). 

p(X) :- 
   \+ ground(X),
   !,
   (not_domain(q/1,Enumerator)
    -> 
    (call(Enumerator,C),C=X)    % generate a candidate from the not-domain of q/1 which must unify with X
    ; 
    throw("not-domain for q/1 undefined")).

not_domain(q/1,
   [C]>>(
      between(1,100,C),
      \+ q(C))).
```

Then:

```
?- p(2).
true.

?- p(1).
false.

?- p(X).
X = 2 ;
X = 3 ;
X = 4 ;
X = 5 
....
```

Makes sense, and may be even usable in certain settings. 

We really identify certain domains over which a `q` either

- throws because the argument tuple makes no sense for `q`
- succeeds the proof / returns true because of positive knowledge about `q`:  truth-value-wise, the argument tuple is in the pre-image of `{true}` for `q`.
- fails the proof & returns nothing because there is nothing ("default negation"); turned into success with no data by the `\+` operator
- fails the proof & returns nothing because there is negative knowlegde about `q`:  truth-value-wise,
  the argument tuple is in the pre-image of `{false}` for `q`; turned into success with no data by the `\+` operator; this is the case where a
  a query for an `X` such that `\+ q(X)` makes sense.
    
Sadly the two last cases are indistinguishable in Prolog. Failure of establishing is a proof is just ... failure.  

![domains of negation](pics/domains_of_negation.png)
