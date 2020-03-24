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

## The `=` predicate: "do the terms unify?"

This is classed under:

- [Comparison and unification of terms](https://eu.swi-prolog.org/pldoc/man?section=compare)
   - [Predicate `=/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D)/2) 

> `?Term1 = ?Term2`: Unify `Term1` with `Term2`. True if the unification succeeds. 
> or behaviour on cyclic terms see the Prolog flag 
> [occurs_check](https://eu.swi-prolog.org/pldoc/man?section=flags#flag:occurs_check). 
> It acts as if defined by the following fact: `=(Term, Term).`

- For unification see: [Unification](https://en.wikipedia.org/wiki/Unification_(computer_science))
- For occurs check see: [Occurs check](https://en.wikipedia.org/wiki/Occurs_check)

**Negation using NAF**

- [\=](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%3D)/2) 

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

- [Arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arith)
   - [General purpose arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arithpreds)
      - [Predicate is](https://eu.swi-prolog.org/pldoc/doc_for?object=(is)/2)
            
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

- [Comparison and unification of terms](https://eu.swi-prolog.org/pldoc/man?section=compare)
   - [Standard order of Terms](https://eu.swi-prolog.org/pldoc/man?section=standardorder)
      - [Predicate `==/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%3D)/2)

> `@Term1 == @Term2`: True if `Term1` is equivalent to `Term2`. A variable is only identical to a sharing variable.

**Negation using NAF**

- [\==](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%3D%3D)/2)

> Equivalent to `\+Term1 == Term2`.

## The `=:=` predicate: "arithmetically equal"

This is classed under:

- [Arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arith)
   - [General purpose arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arithpreds)
      - [Predicate =:=](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%3A%3D)/2)

Negation: [=\=](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%5C%3D)/2)


      
## The `#=` predicate: Constraint to be arithmetically equal

This is classed under:

- [library(clpfd): CLP(FD): Constraint Logic Programming over Finite Domains](https://eu.swi-prolog.org/pldoc/man?section=clpfd)
   - [Arithmetic Constraints](https://eu.swi-prolog.org/pldoc/man?section=clpfd-arith-constraints)
      - [Predicate #=](https://eu.swi-prolog.org/pldoc/doc_for?object=%23%3D%20/%202)

## The `dif` predicate: Constrain to be different

Link: [dif/2](https://www.swi-prolog.org/pldoc/doc_for?object=dif/2)

This is classed under:

- [constraint logic programming](https://www.swi-prolog.org/pldoc/man?section=clp)
   - [coroutining](https://www.swi-prolog.org/pldoc/man?section=coroutining)
      - [Predicate dif/2](https://eu.swi-prolog.org/pldoc/doc_for?object=dif/2)
      

