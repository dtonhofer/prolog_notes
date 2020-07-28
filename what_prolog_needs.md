# What Prolog needs as additional features

... Just some crazy ideas I'm writing down as I code.

A lot of things could be done by having a code preprocessor & editor support. There would be additional syntax, but no changes to the implementation (WAM or otherwise).

- Predicate arguments appearing in the head should be accessible by special variables like `$ARG1`, `$ARG2` etc. This is an application of the DRY principle.
  With this, Then one can write clauses which can both have structure in the head (for matching) and which can access that structure in the body w/o
  the need to reconstruct it by hand: `foo([X,Y|Z],A,B) :- g($ARG1).` instead of `foo([X,Y|Z],A,B) :- g([X,Y|Z]).`    
- [HERE documents](https://en.wikipedia.org/wiki/Here_document) to embed text blocks to output elegantly in code. (but you would also need string 
  interpolation to make this really useful)
- Local naming contexts so that helper predicates are only visible to and can only be called from their "master predicates" 
  (and so that helper predicates can be identified as helper predicates). This can be achived by some naming convention for
  predicate names with some additional enforcement, e.g. hierarchical names for the helper predicates: `master$helper(..) :- ....`
  Or maybe modules need to be made hierarchical.
