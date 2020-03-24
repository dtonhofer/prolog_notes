# Prolog's comparison predicates

WORK IN PROGRESS.

EXAMPLES & TEST CASES TO BE ADDED

I'm going with the [SWI Prolog manual](https://eu.swi-prolog.org/pldoc/doc_for?object=manual) at this point and also
copying text from there.

Good stuff:

- Stackoverflow: [Using \==/2 or dif/2](https://stackoverflow.com/questions/13757261/using-2-or-dif-2/13770020)

The vocabulary is not clear...

- Predicate
- Constraint
- Operator
- Functions. "Functions are terms that can appear in the argument of (the predicates) is/2, =:=/2, >/2, etc." But notation os the same as for predicates. `functor/arity`
- (two terms are) identical
- (two terms are) equivalent
- (two terms are) equal
- (two terms are) (can) unify


## The `=` predicate: "do the terms unify?"

This is classed under:

[Comparison and unification of terms](https://eu.swi-prolog.org/pldoc/man?section=compare)/[Predicate `=/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D)/2) 

> `?Term1 = ?Term2`: Unify `Term1` with `Term2`. True if the unification succeeds. 
> or behaviour on cyclic terms see the Prolog flag 
> [occurs_check](https://eu.swi-prolog.org/pldoc/man?section=flags#flag:occurs_check). 
> It acts as if defined by the following fact: `=(Term, Term).`

- For unification see: [Unification](https://en.wikipedia.org/wiki/Unification_(computer_science))
- For occurs check see: [Occurs check](https://en.wikipedia.org/wiki/Occurs_check)

**Negation using NAF**

[\=](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%3D)/2) 

> Equivalent to `\+(Term1 = Term2)`.
> This predicate is logically sound if its arguments are sufficiently instantiated. In other cases,
> such as `?- X \= Y.`, the predicate fails although there are solutions. This is due to the incomplete
> nature of `\+/1`.
> 
> To make your programs work correctly also in situations where the arguments are not yet sufficiently
> instantiated, use dif/2 instead.

## The `is` predicate: Force evaluation, then unify

This is really not a predicate, it is a special instruction to trigger evaluation/reduction of the right-hand side.

This is classed under:

[Arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arith)/[General purpose arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arithpreds)/[Predicate `is`](https://eu.swi-prolog.org/pldoc/doc_for?object=(is)/2)
  
> `-Number is +Expr`: True when _Number_ is the value to which _Expr_ evaluates. 
> Typically, `is/2` should be used with unbound left operand. If equality is to be
> tested, =:=/2 should be used. 

- A "force evaluation" instruction should be used for more than just numeric terms! String operations, obtaining data 
  from non-logical sources (`CurrentTime is time()`, `Data is read(Source)`) or even side-effects in obvious manner
  (`BytesWritten is writeBytes(Sink,String)`) should all be used that way instead of pretending rather disingeneously that
  such operations are "prediates". `is` is the gateway to function calls!
- Consider using `#=` to state constraints between variables instead.

## The `==` predicate: "after unification, do terms compare 'the same'?"

This is classed under:

[Comparison and unification of terms](https://eu.swi-prolog.org/pldoc/man?section=compare)/[Standard order of Terms](https://eu.swi-prolog.org/pldoc/man?section=standardorder)/[Predicate `==/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%3D)/2)

> `@Term1 == @Term2`: True if `Term1` is equivalent to `Term2`. A variable is only identical to a sharing variable.

**Negation using NAF**

[\==](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%3D%3D)/2)

> Equivalent to `\+Term1 == Term2`.

## The `=:=` predicate: "arithmetically equal"

This is classed under:

[Arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arith)/[General purpose arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arithpreds)/[Predicate =:=](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%3A%3D)/2)

**Negation:**

[=\=](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%5C%3D)/2)

>`+Expr1 =\= +Expr2`: True if expression `Expr1` evaluates to a number non-equal to `Expr2`.
      
## The `#=` predicate: Constraint to be arithmetically equal

This is classed under:

[library(clpfd): CLP(FD): Constraint Logic Programming over Finite Domains](https://eu.swi-prolog.org/pldoc/man?section=clpfd)/[Arithmetic Constraints](https://eu.swi-prolog.org/pldoc/man?section=clpfd-arith-constraints)/[Predicate #=](https://eu.swi-prolog.org/pldoc/doc_for?object=%23%3D%20/%202)

`?X #= ?Y`: The arithmetic expression _X_ equals _Y_. This is the most important arithmetic constraint
([section A.9.2](https://eu.swi-prolog.org/pldoc/man?section=clpfd-arith-constraints)), subsuming and replacing
both `(is)/2` and `(=:=)/2` over integers. See _declarative integer arithmetic_ ([section A.9.3](https://eu.swi-prolog.org/pldoc/man?section=clpfd-integer-arith)).

## The `dif` predicate: Constrain to be different

This is classed under:

- [constraint logic programming](https://www.swi-prolog.org/pldoc/man?section=clp)
- ...[coroutining](https://www.swi-prolog.org/pldoc/man?section=coroutining)
- ......[Predicate dif/2](https://eu.swi-prolog.org/pldoc/doc_for?object=dif/2)

> `dif(@A, @B)`: The `dif/2` predicate is a constraint that is true if and only if _A_ and _B_ are different terms.
> If _A_ and _B_ can **never unify**, `dif/2` succeeds deterministically. If _A_ and _B_ are **identical**, it fails
> immediately. Finally, if _A_ and _B_ **can unify**, goals are delayed that prevent _A_ and _B_ to become equal. 
> It is this last property that makes `dif/2` a more general and more declarative alternative for `\=/2` and 
> related predicates.
> 
>  This predicate behaves as if defined by `dif(X, Y) :- when(?=(X,Y), X \== Y)`. See also `?=/2`. The
> implementation can deal with cyclic terms.
> 
> The `dif/2` predicate is realised using attributed variables associated with the module `dif`. It is an autoloaded
> predicate that is defined in the library `library(dif)`.

