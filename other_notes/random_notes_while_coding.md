# Random notes taken while coding

None of these may be based in reality or be good ideas.

## Local naming contexts

Wouldn't it be cool if one had this:

```
cumulative_2([K-V|Pairs,TotalStars,[K-CumulV|Cumul]) :-
   % Keep the Chars inside!
   Chars^{
     atom_chars(V,Chars),
     phrase(stars(StarsCount),Chars),
   }
   TotalStars #= StarsCount + TotalStarsLower,
   cumulative_2(Pairs,TotalStarsLower,Cumul).
```

```
cumulative_2([K-V|Pairs,TotalStars,[K-CumulV|Cumul]) :-
   % Make StarsCount and V "connect to their context"
   StarsCount&V&{
     atom_chars(V,Chars),
     phrase(stars(StarsCount),Chars),
   }
   TotalStars #= StarsCount + TotalStarsLower,
   cumulative_2(Pairs,TotalStarsLower,Cumul).
```

It's another way of writing a separate predicate of course..

## Local naming contexts (would that be hoierachical modules?)

One should be able to create a naming context for helper predicates:
They should only be visible from the "master predicate". Constricting
scope is always win. In fact, Prolog should generally make it easier
to add helper predicates. However, this demands specific support from
the editor.

## Are cyclic terms taken seriously

If they are, there should be a built-in to "break a cycle" into a linear
cycle-less graph so that a graphy with cycles can be safely examined.

## Prolog is like taking a hike along the crest of a mountain

You have to follow the small path of truth (computational success and domain/type adequacy) 
while left and right vats abysses of falseness open up (computational failure and domain/type inadequacy)
Still, do not check every single step in detail!

## Don't try to throw for this and that unless you need precise exception messages

It is generally easier to write a predicate which fails radically and in carefree fashion
when it gets "stuff it cannot digest". You can then wrap that predicate into a predicte
which throws if the wrapped predicaste fails. You will not get detailed exceptions, but
at least the code is readable and low-maintenance.

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

## Truth value reification

When is it appropriate to not succeed/fail a predicate but to deterministcially succed and return the truth value as an atom `true`, `false`
(and possibly `unknown`, although that cannot be called unlike the others, for de-reification). In this case "predicate success" means
"computation success", not query success.

Maybe wehn you don't need to backtrack or don't want to have the control construct `->`? 

## Meta-predicates

Using meta-predicates is quite helpful as it de-litters the code (especially getting rid of the ugly and unreadable `->`).
Prolog is on the same level of possibilites as LISP/Clojure for that (in fact, whole goals can be moved around as structures,
but there is problem as there is no problem Lambda abstraction, but we have yall, but does it always work?). Also good for
expressing intentions.

The "programming communnity" does not seem to use metapredicates often though.

## Compiler warnings

- The compiler should probably warn if predicate names differ in capitalization only
- Compiler does not warn here: `MiDict.get_dict(Id)` even if `MiDict` is fresh. It should.

## Add must_be, but in assertions

Of course you can always compile-out the must-be's .. but why not have them in assertions?

## Assertions should have a hierarchical topic structure, like debugs

So that you can switch them off by sector.

Or, as this is logic, one may even use a more general goal to decide whether an assertion is on-topic or not.

## Accumulator generation naming

Name them AccIn, AccOut in the argument list

Name them Acc0, Acc1, Acc2 etc in a clause. This is easily recognizable by eye scan.

## The Prolog stlye guid by Covington should be in an annotable Wiki

Currently the Prolog style guide is a dead document ... buried in a PDF like a scientific paper. What a tragedy!

## Modules should be small 

Currently modules are overly fat. I don't know why. Weird tradition.

## Hierarchical Module system

There needs to be a study for Module Best Practice in e.g. the Java world (inlcuding OSGi), CIAO Prolog etc.

Should Modules be like objects or classes?

## Add helper predicates to make intention clear

Even if they are just a name indicating intention and calling another predicate with a less clear name.

Again, editor support for such operaions would be a great help.








