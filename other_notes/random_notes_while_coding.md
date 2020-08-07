## Random notes taken while coding

None of these may be based in reality or be good ideas.

## Guard Handling

Should fresh variables in the guard be able to "bleed out" into the rest of the clause?
It may be efficient but style-wise this does not seem like a good idea.

```
openlist_last_nonempty(Olist,Last) :-
   nonvar(Olist),
   Olist=[Stuff|More], % it would be cool to specify that More cannot be var here, in the unification
   var(More),
   !,
   Last=Stuff. % could be done "in the guard" but that doesn't sound right
```   

In fact, the guard above is best moved out into a proper guard predicate:

```
is_final_listbox(Olist) :-
   nonvar(Olist),
   Olist=[_|More],
   var(More).
   
openlist_last_nonempty(Olist,Last) :-
   is_final_listbox(Olist), % guard
   !,
   Olist=[Last|_]. 
```
   
## Asymmetric Unification (Pattern Matching)

There might be a need for an asymmetric unify operation: Something like `A <<= B`.
Nothing of B is "further instantiated", instead only A is further instantiated .. .would that make sense?
What happens to variables? It would be a lot more like pattern matching.

## Specify that a variable cannot/must unify to a hole _inside of unification_

Instead of:

```
is_final_listbox(Olist) :-
   nonvar(Olist),
   Olist=[Stuff|More], 
   var(More).
```

Why not have somethign like:

```
is_final_listbox(Olist) :-
   nonvar(Olist),
   Olist=[Stuff|More#]. % the # means unificatoin only works if More is unified with a freshvar
```

## Where is my declarativity

Prolog is touted as declarative, but it actually works only because it throws
the declarativeness of FOL overboard and explicitly performs an exhaustive search for
"witnesses" in defined order instead of formula manipulation and deduction.

## Problems with Module visibility

Calling predicates defined inside a plunit using metacalls demans that on
indicate the "plunit module" (one needs to guess a bit because that is a 
constructed name.

Metacalls made from Module X should really be able to see predicates of Module X auotmatically.

## Styleguide items

- Do not be afraid to add task-specific mini-predicates! But Prolog should support attaching these to the predicates where they are used.
- Helper predicates: add a _2 etc.




   

