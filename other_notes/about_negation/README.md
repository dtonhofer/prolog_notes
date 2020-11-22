# Negation-as-Failure

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

## Literature links

See the end of this page.

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
truth values to a statement) is whisked away by only allowing programs that give the value `true` to statements.
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

This can be an effect that is desired or not. See also:

- [Non-monotonic Logic](https://plato.stanford.edu/entries/logic-nonmonotonic/)
- [Logical Approaches to Defeasible Reasoning](https://plato.stanford.edu/entries/reasoning-defeasible/#LogiAppr)

## What's the meaning of this?

I haven't found a discussion in literature concerning the "shift in meaning" that occurs due to `\+` but it must have been discussed somewhere.

Prolog only admits to to truth values "Prolog True" (signalled by a query that succeeds) and "Prolog False"
(signalled by a query that fails). Arguably there is also a potentially infinite number of out-of-band truth values
signalled by exceptions, but let's not consider these. 
There is an additonal peculiarity in that `fail`/ `failure` /`false` can also signal that the computation failed, for example
because a value was out-of-domain. Let's not consider this either (restricting ourselves to "modeling" rather
than "computation").

Let a "positive proof" be one which encounters no `\+` during proof search. 

Take the query `p`. Then:

The proof for `q` is based on positive proofs only:

- If `p` succeeds, the meaning is _"there is evidence for `p`, and `p` is strongly true"_, i.e. there is indeed at least one proof of `p`.
- If `p` fails the meaning is _"there is no evidence for `p`, and `p` is weakly false"_, i.e. all the attempts at proving `p` failed and we assume that `p` takes on truth value `false` as default (closed world assumption).

Suppose the proof for `p` involves a subgoal `q` wrapped by `\+`, and the proof of `q` is a positive proof. Then:

- If `q` succeeds, the meaning is _"there is evidence for `q`, and `q` is strongly true"_.
   - And thus `p` fails, but a reasonable meaning of that failure would be _"`p` is strongly false"_ (because `q` is strongly true).
   - This is different from the meaning of _"`p` is weakly false"_ that is assumed (and implemented: you can override a failed `p` with another proof that succeeds).
- If `q` fails, the meaning is _"there is no evidence for `q`, and `q` is weakly false"_.
   - And thus `p` succeeds, but a reasonable meaning of that success would be _"`p` is weakly true"_ (because `q` is strongly false).
   - Again, this is different from the meaning of _"`p` is strongly true"_ that is assumed (and implemented: you cannot override a successful `p` at all).
   
The "weakness" attribute (default assumption for a truth value) in Prolog only exists for the truth value `false`. However, it 
seems an application of `\+` should switch it from the subgoal's truth value to the complementary goal's truth value.

One could imagine a logic programming language with more truth values than "true" and "default false" to
make the above clearer, or even consistent. It wouldn't be Prolog though. 

[some extended logical values](some_extended_logical_values.svg)

### Logic programming with extended logics

Belnap has a four-valued logic that abandons the pretense of classical logic at global consistency and determinate
truth values `true` or `false` for every statement (so the idea of having "proofs by contradiction" is no longer applicable,
but both the philosophy and the actual maintenance of large databases become viable). It uses the truth values
 `true`, `false`, `both`, `none` (arranged on a lattice).

On Belnap's 4-valued logic:

   - [Four-valued logic](https://en.wikipedia.org/wiki/Four-valued_logic) at Wikipedia
   - [An overview](http://www.filosofia.unimi.it/dagostino/wp-content/uploads/2017/05/Belnap.pdf) (PDF) by Marcello D’Agostino   
   - Belnap's original paper (paywalled): [A Useful Four-Valued Logic](https://link.springer.com/chapter/10.1007/978-94-010-1161-7_2) (Nuel D. Belnap Jr.), 1977 
     _appears in: Dunn J.M., Epstein G. (eds) Modern Uses of Multiple-Valued Logic. Episteme, vol 2. Springer, Dordrecht._ 
   - A variation of Belnap's logic: [A Constructive Four-Valued Logic](http://www.cs.cas.cz/tacl2017/abstracts/TACL_2017_paper_66.pdf) 
     (Yuanlei Lin and Minghui Ma)
   - [Some Useful 16-Valued Logics: How a Computer Network Should Think](https://link.springer.com/article/10.1007/s10992-005-0556-5)
     (Yaroslav Shramko, Heinrich Wansing), in: _Journal of Philosophical Logic volume 34, pages 121–153 (2005)_. 
     ([PDF at Research Gate](https://www.researchgate.net/publication/226314931_Some_Useful_16-Valued_Logics_How_a_Computer_Network_Should_Think)).
   
For logics able to deal with proofs that yield both `true` and `false` see:

   - [Paraconsistent Logic](https://plato.stanford.edu/entries/logic-paraconsistent/) at the Stanford Encyclopedia of Philosophy.
   - [Quantitative Deduction and its Fixpoint Theory](https://www.sciencedirect.com/science/article/pii/0743106686900038) (M.H van Emden)
     _appears in: The Journal of Logic Programming, Volume 3, Issue 1, April 1986, Pages 37-53_
   - [Paraconsistent logic programming](https://www.sciencedirect.com/science/article/pii/0304397589901266) (Howard A.Blair, V.S.Subrahmanian)
     _appears in: Theoretical Computer Science Volume 68, Issue 2, 30 October 1989, Pages 135-154_
   - Chapter 6.1 - Paraconsistent Logic Programming in 
     [Introduction to Annotated Logics: Foundations for Paracomplete and Paraconsistent Reasoning](https://www.springer.com/gp/book/9783319179117),
     (Jair M Abe, Seiki Akama,  Kazumi Nakamatsu), Springer Intelligent Systems Reference Library 88, 2015.
   
## "Floundering"

"Flounding" suggest that the Prolog Processor trashes around or performs something repeatedly without progress:

From [The Free Dictionary](https://www.thefreedictionary.com/floundering), _to flounder_:

   1. To move clumsily or with little progress, as through water or mud.
   2. To act or function in a confused or directionless manner; struggle:
      _"Some ... floundered professionally, never quite deciding what they wanted to do"_ (Steve Olson).
     
but it's not a well-chosen vocabulary. "Faceplanting" is much closer to the intended meaning.

In [Logic programming and negation: A survey](https://www.sciencedirect.com/science/article/pii/0743106694900248),
(Krzysztof R.Apt, Roland N.Bol, 1994), we read:

> One of the complications concerning SLDNF resolution (i.e. the Prolog proof search: SLD resolution with Negation as Failure)
> is so-called _floundering_ - a generation of a node which consists exclusively of nonground negative literals,
> because then selection of any literal ends the derivation in an abnormal way. In the definition here provided, 
> floundering is treated differently
> - it arises as soon as a nonground negative literal is selected. Clearly, this small change has no effect on 
> the theory of SLDNF resolution, since the original notion of floundering can be easily defined."

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

This seems to happen whenever the goal wrapped by `\+` contains unbound "free variables" that also occur outside the wrapped goal.

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

## Reading

- [Negation as such](https://plato.stanford.edu/entries/negation/) at the Stanford Encyclopedia of Philosophy
- ["Negation as Failure"](https://en.wikipedia.org/wiki/Negation_as_failure) at Wikipedia

### "Negation as Failure" (1978)

   - Keith Clark
   - 1978
   - http://www.doc.ic.ac.uk/~klc/neg.html

### “Negation and Control in Prolog” (1986)

   - Lee Naish
   - 1986
   - [Springer Lecture Notes in Computer Science (LNCS) No. 238, Springer-Verlag Berlin Heidelberg 1986, 129 pages](https://link.springer.com/book/10.1007/BFb0021681)

### "Negation as Failure I & II" (Surveys by Sheperdson) (1984, 1985)

**Negation as Failure: A Comparison of Clark's Completed Data Dase and Reiter's Closed World Assumption**

   - J.C. Sheperdson   
   - https://www.sciencedirect.com/science/article/pii/0743106684900232
   - Appears in: _The Journal of Logic Programming, vol 1, 1984, pages 51–81._
   
**Negation as Failure II**

   - J.C. Sheperdson
   - https://www.sciencedirect.com/science/article/pii/0743106685900184
   - Appears in: _The Journal of Logic Programming, vol 3, 1985, pages 185-202._

### "A Kripke-like Model for Negation as Failure" (1989)

   - James Harland, Department of Computer Science, University of Edinburgh
   - https://www.researchgate.net/publication/2818956_A_Kripke-like_Model_for_Negation_as_Failure
   - Appears in: _Proceedings of the North American Conference on Logic Programming, 626-642, 1989._
   
> We extend the Kripke-like model theory given in 
> [D.A. Miller, A Logical Analysis of Modules in Logic Programming, Journal of Logic Programming 6:79-108, 1989].
> for a fragment of first-order hereditary Harrop formulae to include negated atoms in goals. This
> gives us a formal framework in which to study the role of Negation as Failure rule. The class of predicates
> for which Negation As Failure is applicable is discussed, as well as the predicates for which some other form of negation
> will need to be used. We show how the former class may be incorporated into the model theory, giving a 
> generalisation of the usual T^𝜔 construction. No restriction on the class of programs is needed for this approach; the
> construction may be used for programs which are not locally stratified [T. Przymusinski, On the Declarative Semantics of Deductive
> Databases and Logic Programs, Foundations of Deductive Databases and Logic Programming, (ed. J. Minker) 193-216, Morgan Kaufmann, 1988].
> This is accomplished by the use of a _success level_ and a _failure level_ of a goal, either or both of which may be infinite.
> The resulting T operator is not monotonic, which necessitates a slight departure from the standard
> procedure, but the important properties of the construction still hold.

### "Logic Programming with Strong Negation" (1989)

   - David Pearce, Gerd Wagner, FU Berlin
   - Appears in: _[Springer Lecture Notes in Computer Science (LNCS) No. 475](https://link.springer.com/book/10.1007/BFb0038689): Extensions of Logic Programming (ELP 1989), International Workshop Tübingen, FRG, December 8–10, 1989 Proceedings_

### "Classical negation in logic programs and disjunctive databases" (1991)

   - Michael Gelfond, Vladimir Lifschitz
   - Appears in: _"New Generation Computing volume 9, pages 365–385 (1991)"_
   - https://link.springer.com/article/10.1007%2FBF03037169 
   - http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.56.7150 
   
### "Logic programming and negation: A survey" (1994)

   - Krzysztof R. Apt, Roland N. Bol
   - https://www.sciencedirect.com/science/article/pii/0743106694900248
   - Appears in: _The Journal of Logic Programming, Volumes 19–20, Supplement 1, May–July 1994, Pages 9-71_

### "A logical semantics for depth-first Prolog with ground negation" (1997)

   - James Andrews
   - https://www.sciencedirect.com/science/article/pii/S0304397596001387
   - Appears in: Theoretical Computer Science, Volume 184, Issues 1–2, 30 September 1997, Pages 105-143
   - _"This material was published earlier as a technical report and is the expanded version, including proofs, of a paper presented at the 1993 International Logic Programming Symposium (ILPS)."_
   
### "How to Handle Negation in Prolog: Several Logical Approaches" (2008)

   - https://canbaskent.net/logic/early/prolog.pdf
   - Can Başkent
   - 2008-02-26
   
> Present paper surveys several formal and axiomatic methods to deal with the problem of negation in PROLOG. Furthermore, a brief philosophical
> account is also mentioned.

> The reason why the negation is problematic in Prolog is the fact that “it is not possible to express negative information with pure
> Horn clauses” as Prolog’s resolution works by utilizing Horn clauses. In other words, it has been remarked that “a logic program is
> a set of definite Horn clauses”.

The following approaches are proposed:

- Implement a four-valued logic with ordered truth values: F ≤ U ≤ T ≤ N, with U meaning "undefined" and F "floundering on negation", as 
  described by James Andrews in _"A logical semantics for depth-first Prolog with ground negation"_
- Extend programs with classical negation so that they include negative information explicitly. The author says _As a result, there will be two types of queries which do not succeed. One type does not succeed since it fails and the other does not succeed since its negation succeeds._ This means abandoning the Prolog model based on resolution and passing to the Answer-Set-Programming model based on Stable Models of Gelfond and Lifschitz.
- Extending the Models for Negation: As proposed by James Harland in _A Kripke-like Model for Negation as Failure_. 
