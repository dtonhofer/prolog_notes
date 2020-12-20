# Some ides on what would be useful in Prolog

For now just a five-minute note.

## Sorts for terms

Sorting terms would of course demand a serious extension of unification.

## Enum types

This is an extension of the "special symbol" `[]` of SWI-Prolog.

One would like to generate "atom-like" names that are mutually distinguishable and unfakeable, and not atoms, each belonging to a "sort".

This would be of some interest when manipulating formulas with variables, as variable are _not nice to handle_ (because treating them as "objects" is not 
the tright thing to do). One would replace the variables by element from a created-at-runtime enum, then manipulate that - it just contains terms.

This has effects on term I/O and probably modules.

## A unification with two new constraints

- One would like to state in the head that "this variable in the head shall only unify with another variable"

This would allow us to get rid of those awkward `var(X)` guards in the bodies. 

Contrariwise:

- One would like to state in the head that "this variable in the head shall not unify with another variable, but shall only unify with a non-variable partial term"

This would allows us to get rid of any `nonvar(X)` guards (which are actually rare).

This cannot be done using attributed variables - one cannot set attributes on head variables.

On the other hand, from the caller's side, attributed variables can be used to make sure a variable in an argument term can only be unified with 
another variable or a nonvariable. But that's not very useful. 

## Local namespaces 

Or maybe hierarchical modules?

The idea is that if I write a predicate `foo` that will only be ever called as a helper predicate from a predicate
`bar`, then I want to make it only visible in the immediate vicinity of `foo`, which is to say "attach it to `foo`.
Local namespaces or maybe a special syntax for predicate names (`bar.foo` maybe?) could help.

Another idea is that one might define a predicate directly inside some other predicate, overriding any predicate with the same
name defined "outside".

like define a predicate directly inside another and it's only visible there (and even overrides any outside)

```
foo(X,Y) :-
   bar(X,Z),
   {
      something(X,Y,Z) :- Z is X*Y.
   }
   something(X,Z,Y).
```

## More syntax

Maybe a bit of syntactic sugar in the head to support reeadability:

- Accumulator In/Out variables `...,[AccIn \/ AccOut],...`
- This would also cover the movable end of an "open list" difference list `....[FinIn \/ FinOut]....`

## Editor support for comments

Comments uglify the code quickly. What do. Speech bubbles that open on hover?

