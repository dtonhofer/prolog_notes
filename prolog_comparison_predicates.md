# Prolog's comparison predicates

WORK IN PROGRESS.

I'm going with the SWI Prolog documentation at this point.

Good stuff:

- Stackoverflow: [Using \==/2 or dif/2](https://stackoverflow.com/questions/13757261/using-2-or-dif-2/13770020)

The vocabulary is not clear...

- Predicate
- Constraint
- Operator

## The `=` predicate: Unify

This is classed under:

- [Comparison and unification of terms](https://eu.swi-prolog.org/pldoc/man?section=compare)
   - [Predicate =/2](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D)/2) 

Negation using NAF: [\=](httpss://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%3D)/2) "does not unify"

## The `==` predicate: Test term equality

This is classed under:

- [Comparison and unification of terms](https://eu.swi-prolog.org/pldoc/man?section=compare)
   - [Standard order of Terms](https://eu.swi-prolog.org/pldoc/man?section=standardorder)
      - [Predicate ==/2](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%3D)/2)

Negation using NAF: [\==](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%3D%3D)/2) - "is not the same under standard order"

## The `=:=` predicate: Test arithmetic equality

This is classed under:

- [Arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arith)
   - [General purpose arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arithpreds)
      - [Predicate =:=](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%3A%3D)/2)

Negation: [=\=](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%5C%3D)/2)

## The `is` predicate: Force evaluation, then unify

This is really not a predicate, it is a special instruction to trigger evaluation/reduction of the right-hand side.
It feels like the GOTO of logic programming.

This is classed under:

- [Arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arith)
   - [General purpose arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arithpreds)
      - [Predicate is](https://eu.swi-prolog.org/pldoc/doc_for?object=(is)/2)
      
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
      

