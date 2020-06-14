# Predicates for Analyzing/Constructing a Term

## Vocabulary

- *Analyzing/Constructing* a Term or 
- *Disassembling/Assembling* a Term.
- In Clojure there is also ["Destructuring"](https://clojure.org/guides/destructuring), which is pattern-matching a term and disassembling it into parts
  like one-way unification. In Prolog this would by copy_term, followed by unification.
  - This is also how calls work in [Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language))
  - The [Picat](http://picat-lang.org/) language both has both unification and pattern matching in the head of clauses.

## Predicates

These are predicates found in the section [Analysing and Constructing Terms](https://eu.swi-prolog.org/pldoc/man?section=manipterm) of the SWI-Prolog manual.

- [`functor/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=functor/3)
- [`../`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D..)/2) 
- [`compound_name_arguments/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=compound_name_arguments/3)
- [`compound_name_arity/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=compound_name_arity/3)

## Overview 
![Analyzing and Constructing Terms](term_analysis_construction.png)

**Other formats**

- [Analyzing and Constructing Terms: LibreOffice Calc](term_analysis_construction.ods) 
- [Analyzing and Constructing Terms: PDF](term_analysis_construction.pdf)

