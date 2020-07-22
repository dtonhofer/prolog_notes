# Predicates for Analyzing/Constructing a Term

## Vocabulary

- *Analyzing/Constructing* a term or 
- *Disassembling/Assembling* a term or maybe
- *Decomposing/Composition* a term
- In Clojure there is also ["Destructuring"](https://clojure.org/guides/destructuring), which is pattern-matching a term and disassembling it into parts
  like one-way unification. In Prolog this would by copy_term, followed by unification.
  - This is also how calls work in [Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language))
  - The [Picat](http://picat-lang.org/) language both has both unification and pattern matching in the head of clauses.

## Predicates

These are predicates found in the section [Analysing and Constructing Terms](https://eu.swi-prolog.org/pldoc/man?section=manipterm) of the SWI-Prolog manual.

- [`compound_name_arity/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=compound_name_arity/3) (_non_ ISO standard)
- [`compound_name_arguments/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=compound_name_arguments/3) (_non_ ISO standard)
   - ... and the related [`arg/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=arg/3) (ISO standard)
- [`functor/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=functor/3) (ISO standard)
- [`=../`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D..)/2) (ISO standard)

And this is one is for distinguishing _compound_ terms from _atomic_ terms. Both are complements of each other: Anything that
is not a fresh variable is either _compound_ or _atomic_ (at least in the current implementation). Atoms are a sublcass
of the atomic terms and are tested with [`atom/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=atom/1).

- [`compound/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=compound/1) (ISO standard)
- [`atomic/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=atomic/1) (ISO standard)

## Overview 
![Analyzing and Constructing Terms](term_analysis_construction.png)

**Other formats**

- [Analyzing and Constructing Terms: LibreOffice Calc](term_analysis_construction.ods) 
- [Analyzing and Constructing Terms: PDF](term_analysis_construction.pdf)

