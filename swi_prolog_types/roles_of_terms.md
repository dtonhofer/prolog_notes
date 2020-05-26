# Roles of Terms

Terms can appear with various Roles

- **Role of a Skeleton**. These are terms whose arguments are all different variables. Created by `functor/3`. 
- **Role of a Predicate**
  - Predicates of various arities, arity 0 is allowed! (as in `foo :- bar.`)    
- **Role of a Goal**, as argument to meta-predicates which call the Goal
  - **Simple Goal**: A predicate name and the parameters which which it shall be called. As in `p(X,Y,12,"Hello")`. 
  - **Complex Goal**: May be a conjunction, disjunction, implication, may even include cuts.
  - **Closures**: Predicates with arguments partially filled-in (filling them in from the left only): This is not
    quite the same as a "closure" of functional programming. For example, if you have 
    predicate `foo/5`, a corresponding closure might be `foo(1,2)`: a term indicating the name of the predicate
    and its two leftmost arguments, which have to be completed to 5 arguments before a clal can be issued.  
  - **Lambda Expressions**: Lambda Expressions are used to "wrap around" other predicates to make
    meta-calling convenient. See: [`library(yall)`](https://www.swi-prolog.org/pldoc/man?section=yall)
    - "Lambda Prolog" Lambda expressions are something else...  
- **Role of an (Arithmetic) Function** 
  - These appear on the right side of the `is/2^ predicate.
  - Constants are atoms.
  - Functions are compound terms of arity >= 1. 
  - Might be of interest to have extension to allow functions other-than-arithmetic and in other places than
    on the right side of `is/2`.
- **Role of arbitrary data** in the form of a tree or list structure. Cyclic terms allow directed, single-rooted graphs.

