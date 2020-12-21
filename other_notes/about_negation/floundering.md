# Floundering

This is weirdly chosen vocabulary. "Floundering" might suggest that the Prolog Processor trashes around or
performs something repeatedly without progress:

From [The Free Dictionary](https://www.thefreedictionary.com/floundering), _to flounder_:

   1. To move clumsily or with little progress, as through water or mud.
   2. To act or function in a confused or directionless manner; struggle:
      _"Some ... floundered professionally, never quite deciding what they wanted to do"_ (Steve Olson).
     
In fact, "floundering" is all about the breakdown of the `\+` operator.   

From _The Art of Prolog_ 1st ed. p. 166:

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

This all seems to be about the following problem:

Consider the program:

```none
q(1).
p(X) :- \+ q(X).
```

Evidently if we ask 

> Is it true that `p(1)`?

```none
?- 
p(1).

false.
```

And if we ask (even though `d` is not mentioned anywhere):  

> Is it true that `p(d)`?

```none
?- 
p(d).

true.
```

The answer is: 

> Yes, because there is no evidence anywhere that `q(d)` is true_ (trying to prove `q(d)` failed)

However, if you use a query with an unbound variable:

```
?- 
p(X).

false.
```

The question is: 

> Is there any `X` such that `p(X)`_ i.e. such that `\+ q(X)`, i.e. such that there is no proof for `q(X)`? 

Note that this is a very weak question - it is highly likely that there is such an `X` (even in the actual domain of
`q/1`) unless `q/1` is true everywhere. 

The correct answer would be: 

> Yes, any `X` of the domain of `q/1` different from `1` is an answer 

This is not expressible in Prolog but _would_ be expressed by an enumeration if the domain for `p/1` were finite.
Prolog would generate all elements of the domain except `1`. 

However, the goal `\+ q(X)` with unbound `X` has a **different meaning than the intended one**. It asks: 

> Is there no `X` such that `q(X)`?

This is `false` because there is `q(1)`. 

Note that `1` is never returned as answer, because the query is made to fail if the proof succeeds with `1`.
And failure means no bindings will be retained.

**An inconsistency arises!**

This seems to happen whenever the goal wrapped by `\+` contains unbound "free variables" 
that also occur outside the wrapped goal. So make sure that's not happening!

As `\+` can be implemented using `->/2`, `->/2` is also subject to floundering.

Trivially:

```none
q(1).
p(X) :- q(X) -> fail ; true.
```

```
?- p(1).
false.

?- p(d).
true.

?- p(X).
false.
```

### A less abstract example

Still looking for a good one. A try:

```none
allergic_to(bart,penicillin).
allergic_to(lisa,penicillin).
allergic_to(homer,vancomycin).
can_take(Who,What) :- \+ allergic_to(Who,What).
```

Then:

```
?- can_take(bart,What).        % "Computer says no": bart can' take anything (wrong)
false.

?- can_take(bart,vancomycin).  % But not really (correct, although to be honest, we just don't have any data about this)
true.

?- can_take(bart,penicillin).  % Better don't give him that (correct)
false.

?- can_take(Who,penicillin).   % Nobody can take penicllin (wrong, as far as we know)
false.

?- can_take(homer,What).       % homer can take nothing either?
false.

?- can_take(homer,penicillin). % At this point the pharmacy gives up...
true.

?- can_take(Who,levofloxacin). % A "true" but no value for "Who". That's the sign of a bad `\+` in the proof tree. 
true.
```

## Mitigations

Maybe the Prolog processor should throw an exception when it finds a body subject to floundering, but in general this would 
only be doable at runtime.

The problem stems from the fact that Prolog basically relies on explicit enumerations over domains 
followed by tests (but for some reason, explicit domains have never been given explicit treatment in Prolog) 

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

Or one could "freeze" the negative goal using [`freeze/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=freeze/2),
so that it is only run once the variable `X` is bound. 
In this example, although the call to `freeze/2` succeeds, the query ultimately fails:

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

## Mitigatons

In [The execution algorithm of Mercury, an efficient purely declarative logic programming language](https://www.sciencedirect.com/science/article/pii/S0743106696000684) (Zoltan Somogyi, Fergus Henderson, and Thomas Conway, The Journal of Logic Programming, Volume 29, Issues 1–3, October–December 1996, Pages 17-64), we read:

> _3.6. If-Then-Else and Negation_
>
> The if-then-else and negation constructs in most variants of Prolog and nonlogical 
> and unsound: they can cause the system to compute answers which are inconsistent with the
> program viewed as a logical theory. Some existing logic programming
> systems such as [NU-Prolog](https://www.researchgate.net/publication/220282520_The_NU-Prolog_Deductive_Database_System) \[link added\]
> and [Gödel](https://en.wikipedia.org/wiki/G%C3%B6del_(programming_language)) \[link added\] provide logical and sound replacements for
> these Prolog constructs. Unfortunately, these systems enforce safety via run-time
> groundness checks. This effect can increase the run-time of a program by an arbitrarily large factor;
> if the goals checked for groundness include large terms, the checks can be prohibitively expensive.
>
> The real requirements for the safety of a negated goal is that the negated goal
> not export any bindings to the rest of the computation. The Mercury mode system
> can ensure this at compile time, removing the need for any run-time checks. The
> mode system also allows increased flexibility by allowing the negated goal to contain
> unbound variables that are instantiated by the goal, as long as these variables are
> not visible outside the negation. For example, if one wants to test whether two
> lists are disjoint, one may use the goal `not (member (E, Xs), member (E, Ys))`,
> where the variable `E` occurs only inside the negation.
>
> The rules for if-then-elses are somewhat different. Since `(Cond -> Then; Else)`
> is logically equivalent to `(Cond, Then; not Cond, Else)`, the condition may export
> its bindings to the then part of the if-then-else, but not to the else part or to the
> rest of the computation.

In [What is failure? An approach to constructive negation](https://link.springer.com/article/10.1007/BF01185404) (paywalled),
(Wlodzimierz Drabent, Acta Informatica 32, 27–59, January 1995)

> _Abstract._ A standard approach to negation in logic programming is negation as failure.
> Its major drawback is that it cannot produce answer substitutions to negated
> queries. Approaches to overcoming this limitation are termed _constructive negation_.
> This work proposes an approach based on construction of failed trees for some instances of a negated 
> query. For this purpose a generalization of the standard notion of a failed tree is needed.
> We show that a straightforward generalization leads to unsoundness and present a correct one.
> The method is applicable to arbitrary normal programs. If finitely failed trees are concerned then
> its semantics is given by Clark completion in 3-valued logic (and our approach is a proper extension
> of SLDNF-resolution). If infinite failed trees are allowed then we obtain a method for the
> well-founded semantics. In both cases soundness and completeness are proved.
