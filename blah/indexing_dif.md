Notes made while reading "[Indexing `dif/2`](https://arxiv.org/abs/1607.01590v1)" (2016-07: Ulrich Neumerkel, Stefan Kral).

This paper raises a lot of further paths for exploration. Also about the semantics of Prolog.

`member/2` is really the drosophila of Logic Programming.

Predicate discussed:

- [`member/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=member/2)
- [`memberchk/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=memberchk/2)
- [`->`](https://eu.swi-prolog.org/pldoc/doc_for?object=(-%3E)/2)
- [`*->`](https://eu.swi-prolog.org/pldoc/doc_for?object=(*-%3E)/2)
- [`dif/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=dif/2)

The SWI-Prolog manual is a bit bare!

Exercises:

- What is the general behaviour of `->` and `*->`. Write some unit test.

TODO:

- More on floundering for [\+](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%2B)/1)?
  From "Declarative Diagnosis of Floundering in Prolog": 
  > Many logic programming languages have delay primitives which allow coroutining. This introduces a class  
  > of bug symptoms — computations can flounder when they are intended to succeed or finitely fail. For concurrent
  > logic programs this is normally called deadlock.


- [Declarative Diagnosis of Floundering in Prolog](https://arxiv.org/abs/0711.0048) (Lee Naish, 2012, in: _Proceedings of the Thirty-Fifth Australasian Computer Science Conference (ACSC 2012), Melbourne, Australia_)
- Lee naish has bunch of Good Stuff at the arxiv: [Query](https://arxiv.org/search/?query=lee+naish&searchtype=all&source=header). 

## A reflection on Prolog Truth Values

See also: https://plato.stanford.edu/entries/truth-values/

See also: https://plato.stanford.edu/entries/negation/#LogNeg

Better read: https://arxiv.org/pdf/1305.0141.pdf  Truth versus Informationin Logic Programming

And maybe Prolog could do with Modal Logic ("It is necessary that" and "It is possible that" 

Prolog pretends to work in "classical two-value logic", but there is more going on. Is it important? Maybe not.

Some predicates like `var(X)` (better called `fresh(X)`) are called non-logical, but they really aren't: It's just that there is a hidden argument to every call, namely C(t), the current computational state. `var(X,C(t),C(t+1))` can thus have a different truth value from `var(X,C(t+1),C(t+2))`

The only non-logical predicates are those that connect to oracles, like STDIN. `read(X,C(t))` can give a completely different `X` on two identical  `C(t)` for two program runs.

A predicate may return the following "truth values" (in a large sense)

## TRUE

Prolog (constructively) established the truth of a predicate for certain parameters. There seem to be a few truth values that are commingled:

- Strong truth: Appears mainly in predicates that are bottommost or have a direct mathematical interpretation: `member(1,[1,2,3])` is strongly true. So its `atom(x)`. 

Strong truth can be maintained in a proof tree, but may be weakened by mixing-in NAFs or predicate which are "true by default".

## FALSE

- Strong falsity: Does not exist as there is no way to state that a predicate point is "objectively false". 
- Weak falsity: Implemented by NAF. If there is no proff/evidence dor a predicate point, default to "false". This is what `\+`: "is the predicate weakly false"? Two views based on Closed World Assumption
   - Completed database; For a clause "A <- B", which is axiomatically true, A is true whenever B is true, but A could be true for other reasons (not listed in the program) even if B is false. If we assume that "there are no other reasons", then this becomes "A <-> B": The completed database. (In any reasonable AI system, being able to handle "other reasons" is of prime importance, so Prolog is not so good for modeling AI problems in its bare form). 
   - Negation as failure: Similarly, if a C does not follow from the program (C cannot be proven), one can assume that by default C is false: `Prog |-/- C -> naf(C)`

## Unknown

This is a truth value that occurs whenever things get realistic (i.e. you have to compute with bounded resources). 
Even in mathematics "classical two-value logic" needs to break out of its straightjacket when the truth value self-referential or meaningless sentences is being considered. Intuitionistic logic considers "unknown" sentences as
a feature (however, "unknown" is not a separate truth value in that case; it just means "no truth value has been established yet in the current system". Could one consider NAF as something from a false-by-default intuitionistic logic w/o strong negation?)

"Unknown" may be specialized into:

- "unknown now, but more info can resolve this"
- "unknown whatever you do" (need to extend axioms or even the logic calculus)
- "unknown due to lack of (finit) resources"
- "unknonw due to lack of (potentially infinite) resources" (instances of "undecidable/semideciable" problems are here)

What does a predicate return concretely in case of "Unknown"?

- Forced classicism:
   - Default to FALSE: "Silently fail". Often dubious and even unsound. E.g. if `X` is freshvar, then `atom(X)` gives FALSE.
   - Default to TRUE: "Silently true". Even more dubious! (Any examples?)
- Do not return anything and wait until more is known (the predicate is put into "work on this later" queue - it's done with coroutining). E.g. `dif(1,X)` gives no answer.
- Throw an exception, generally an "instantiation error". This comes closest to a built-in "third truth value" in Prolog. E.g. if `X` is freshvar, then `must_be(atom,X)` throws (any better example?)
   - Special case: Computation takes too long or goes too deep down the search tree. Even "Stack Overflow" conforms to this, in essence.
- Return a 3rd truth value, "unknown". This is not available in base Prolog (one needs a metainterpreter). Problem: The 3rd truth value quickly infects processing, rendering too much "unknown" I guess. Need to read more. https://plato.stanford.edu/entries/lukasiewicz/#PosThiVal
- The "unknown" of intuitionistic logic. This would not be very practical except for saying that computation can be halted because there is not enough infor to proceed. On the hand, NAF seems like a hardcoded defaulty reasoning in intuitionistic logic: "Can you establish the truth of X?" "NO" "Then default it to FALSE"!
