# Some ides on what would be useful in Prolog

For now just a five-minute note.

## Sorts for terms

"Sorted terms" would of course demand a serious extension of unification. And maybe more. 
Still, this is described in Socher-Ambrosius. 
(Rolf Socher-Ambrosius, Patricia Johann: _Deduction System_, Springer 1997; Chapter 9: _Resolution in Sorted Logic_)

It's been nearly 25 years 😬

## A special symbol for NULL/NIL

Relational Database Systems have the special value NULL to express "missing values". It would be nice to have
a dedicated, unfakeable, non-var, unique value for this case in Prolog, too (similar to the special symbol `[]` signifying the
empty list, as used in SWI-Prolog, which is absolutely the right thing to do).

I will use `[]` for now...

## Enum types

This is an extension of the "special symbol" `[]` of SWI-Prolog.

One would like to generate "atom-like" names that are mutually distinguishable and unfakeable, and not atoms, each belonging to a "sort".

This would be of some interest when manipulating formulas with variables, as variable are _not nice to handle_ (because treating them as "objects" is not 
the tright thing to do). One would replace the variables by element from a created-at-runtime enum, then manipulate that - it just contains terms.

This has effects on term I/O and probably modules.

## Name-based parameter passing

Prolog may show a lot of "action" due to in-lined unification directly in calls or headers and 
information comin-in, going-out or being matched. This yields code that is very concise
at writing time, but really hard to read later.

For example, in this code:

https://github.com/dtonhofer/prolog_notes/blob/master/code/heavycarbon/utils/rotate_list.pl

The base case of processing reads like this:

```
copy_list([],0, Tip1 , []  ,  _   , Tip1).
```

This can only be unraveled with appropriate commenting:

```
% If we hit the end of the (input) "List", then we are done!
% The tip of the first open difference list (the collected prefix),
% given by "Tip1", is unified with the fin of the second open
% difference list, "Fin2" (here, already unified), thus appending
% the collected prefix to the list rooted in "Tip2". Simultaneously,
% the fin of the collected prefix "Fin1" is unified with [], closing
% the prefix, and closing the final result list, now rooted in Tip2.

             %  Collected    Becomes new
             %    Prefix       Prefix
             %  Tip1  Fin1   Tip2   Fin2
copy_list([],0, Tip1 , []  ,  _   , Tip1).
```

And it's still hard ot understand.

If we had name-based arguments instead of position-based, one could annotate
the arguments with the names indicating _the meaning the arguments have to the 
called predicate_.

```
copy_list(list           : [L|Ls],
          prefix_counter : PrefixCounter,
          difflist1_tip  : Tip1,
          difflist1_fin  : [L|NewFin1],
          difflist2_tip  : Tip2,
          difflist2_fin  : Fin2)
```

Then the mystery call above would read as:

```
copy_list(list           : [],
          prefix_counter : 0,
          difflist1_tip  : Tip1,
          difflist1_fin  : [],
          difflist2_tip  : _,
          difflist2_fin  : Tip1)
```

This would also alleviate the problem of not being sure on what position 
a certain argument should be found and making refactoring involving
argument position reshuffling needlessly difficult.

In sense, we can now have that if we collapse all the arguments into one,
taking an SWI-Prolog dict with appropriately named keys, and passing
only that. But that's not really a good solution, especially as unification
of the individual values suffers.

Rule-based systems supplemented position-based arguments with
name-based arguments quickly. Languages like Clojure support them
via maps.

Prolog is lagging!

## Global constants that are resolved at compile time.

It's annoying to write a special predicate to retrieve a constant in code:

```
constant(foo,44455).

bar :- constant(foo,FOO),p(FOO).
```

When one could simply do something like this for example:

```
:- define foo:44455.

bar :- p($foo).
```

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

Comments uglify the code quickly. What do. Speech bubbles that open on hover? Folding?

## Module headers are annoying and hard to maintain!

The practice of having a separate declaration for exported functionality was already extremely annoying in Modula-2 and
has not become more appetizing since, rather the contrary. Do classes have separate lists of public methods? No
they dont. Then why do Modules have those? 

There should be local annotations on the predicates to say that "this predicate is exported from the module". 
A simple "public" might suffice.

If one wants to have the list of exports, there should be a tool to extract a relevant definition.





