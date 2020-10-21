# Predicate Structure

(Just freestyle notes)

## What should a Prolog predicate put into the unbound variables it is given?

Basically, set a "single value on its search dimension" (e.g. an integer) so that:

- The predicate to the left can search in the next dimension with a fixed value for the previous one
- If that search eventually fails, the predicate can perform a "redo" and move the single value somewhere else (under constraint of the values set by predicates "to the left")
- This may happen for several variables "at a time", e.g. for a pair (X,Y) in a n N² subspace, which is traversed in some order      

...so one would not return a closed solution, e.g. a statement about the possible values that (X,Y) would take, at least not in the vanilla approach. Returning a closed solution would be more powerful though.

The point selecte however, must be minimally constrained. For example length(L,6) sets L to list of 6 unbound variables. 
This is a "template" for solutions (the template itself is 6-dimensional).

If Prolog could exploit "information attached to an unbound variable" then length(L,6) could NOT return a template
but annotate L with "must be a list of length 6", in effect doing nothing but folding itself into a constraint on L.
Then one could add constraints like "must be a list of integer" etc. This can be done using attributed variables.

In fact, there should be much more usage of attributed variables.

## The problem of "false"

"false" has no information about "why the failure occured". 

- In logic, this is just a statement that the "subspace fixed by predicates on my left does not allow for a solution"
- If the predicate is deterministic (e.g. I/O) it is a statement that "the computation failed somehow" (and then what?)
- Some predicates "silently fail" (actualy, just fail) when they dropped into a bad domain (e.g. get passed a string that is not a number instead of a string that is a n umber).

It seems that instead of "failing" there should be a lot more "throwing".

There is also the problem of code that exploited the fact that a predicate fails on certain input.
If later, behaviour changes because the predicate is made consistent with other parts of the system
and is made to propertly process previously-failing input, this will cause the program to succeed
where it formerly failed ... such an error is likely to go unseen.

Maybe "false" should carry additional exploitable information (but which one, and who would even use that?)

## Prolog needs an "around" predicate markup

There needs to be a simple way to call a check predicate prior to calling another predicate.
To do type checks, debugging etc. Once that predicate passes, the appropriate clause is selected (and maybe there should even be an "around clause")
Some on "success" and "failure". It's actually: attaching predicates to the tracer! Can that be done?

## When do you throw, when you return false? 

Deciding is a hairy business:

![falsing and error throwing](about_predicate_structure/falsing_and_error_throwing.svg)

- [graphml](about_predicate_structure/falsing_and_error_throwing.graphml)
   - [PNG](about_predicate_structure/falsing_and_error_throwing.png)
   - [SVG](about_predicate_structure/falsing_and_error_throwing.svg)

