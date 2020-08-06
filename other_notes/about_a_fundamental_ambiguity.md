# A fundamental ambiguity in Prolog

In Prolog, _variables_ are clause-local _names_ denoting stuff on the term store.

Generally the "stuff" on the term store are trees or directed graphs, with or without cycles.

A special type of term store stuff are _holes_.

If the trees or directed graphs contain themselves _holes_ (which is only possible on leaf positions), we call that stuff "nonground", otherwise "ground".

Stuff on the term store is called a "term". By an utter abuse of language, the names denoting these terms, namely variables, are ALSO called terms. This
is silly (I have somewhere a book about Category Theory in which is widespread but confusing notation concerning domains is derided as "so silly
that anyone who uses  it should be laughed out of the classroom" or something similar. This is the same case!). This is also why `var(X)` makes
no sense at face value: _of course_ `X` is a variable. Why are you asking? Wlel, because what you actual want to ask `freshvar(X)`.

Other abuses of language are however justified: "unifying variables" for example, which is shorthand for "unifying the stuff denoted by two variables".

We will also use freshvar and hole equivalently, even if we are working in a context where there are no variables because we are discussing store stuff.

Therefore:

- Variables: Names of stuff on the term store
- Holes: Stuff on the term store that can be filled with actual structure: a tree or a graph
   - Holes are never visible in program code. They can only be named by using a variable. However, other stuff on the term store can be written
     literally: `[1,2,3]`.
- Terms: The confusing name encopassing both Variables and Stuff on the term store.
- TermStuff: Actual terms: Stuff on the term store, and not variables! This includes holes.
- FreshVar: A fresh variable, aka. uninstantiated or unbound variable: a variable naming a hole

Note that Variables can designate various places of the same TermStuff graph of tree.

Note that a larger structure with a hole filled in with a substructure cannot in any way be distinguished from the "merged structure":

```
X1=f(a,b)   , Y1=g(X1,X1)  
Y2=g(X2,X2) , X2=f(a,b) 
Z3=g(f(a,b),f(a,b))
```

Holes are quite real objects. However, they can only be manipulated through their names (as is the case for anything on the Term Store).

In particular, holes can be:

- Tested for equality X == Y
- Unified, which merges two different holes into one (making a tree a graph for example) or sets up an constraint that says "these must be equal". It depends: X = Y
- Annotated with constraints: Once the holes is implicated in unification, the constraint, which is predicate, is scheduled for execution. If it fails, the unification fails.

They are lacking a bit in the meta-annotation department though: "User readable variable names", "Types" and things I probably can't think of.

Noet that Prolog is a "write-only" language: As computation progresses, "holes" can be filled in with structure (possibly containing more holes, which
are then filled in etc) until, maximally, everything is ground. On backtracking these bindings are trashed again, in reverse order. This is actually
the "core element" of Prolog, a bit difficult to get for newcomers.

Note that "holes" in Prolog are used as "template places". As computation progresses, they can be set to various values (witnesses of a proof) and
on backtracking, the binding is trashed and new values are assigned. In logic of mathematics, "variables" are mean to represent "any valid value
from a given domain fulfilling some additional constraints" which is a bit different: There we deal in formulas, here we deal in concrete witnesses.
Dealing in formulas is the domain of theorem provers.

## So where is the ambiguity?

Well there is no way to distinguish:

- Holes which are *meant to be filled in* (using unification) during computation with witnesses
- Holes which are *subject to manipulation* (also using unification) during computation and which should NOT be filled in

This leads to ambiguity in predicate semantics.

Consider the predicate

```
openlist_last(Olist,Last)
```

which extracts the last valid element of an open list (a list terminating in a hole). If both arguments are freshvar:

- This might mean trying to extract (fill in Last) from an empty open list (Olist), - failure.
- This might mean trying to generate longer and longer Olist which have the variable Last on last positions - a standard nondeterministic predicate.

Suppose we can write 

- !X for "X is a variable denoting a hole subject to manipulation" and
- ?X for "X is a variable denoting a hole subject to filling in"

Then the call

```
openlist_last(!Olist,?Last) - means trying to extract (fill in Last) from an empty open list
openlist_last(?Olist,!Last) - means generating Olists with Last as last element
openlist_last(!Olist,!Last) - could mean unifying whether the hole Last and the actually last entry of Olist (if it is a hole), are the same
openlist_last(?Olist,?Last) - is too weak for meaningful answer
```

Ok, I will be looking out for more examples.

## Related

Suppose we are working with formulas which contain variables that we may want to count etc. but don't want to "fill in". 

We dont want to use atoms instead because that's nasty syntax: variables hould be variables:

```
forall([X,Y]): X*X>100 and Y*Y*Y <1000 
```

this is a perfectly good sentence that we might want to work with at runtime. One might want to rename the variables (in Prolog
variables do not have proper names, although the toplevel of SWI prolog tries to help out, so that cannot be done out of the box),
or rearrange the formula without having to think all that much about variables clashing..








