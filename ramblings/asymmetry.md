A thought on asymetry in Prolog-like Logic Programming

In the paper [Logic Programming HANSEI: The append relation](http://okmij.org/ftp/kakuritu/logic-programming.html#append), Oleg Kiselyov (designer of [Kanren](https://en.wikipedia.org/wiki/MiniKanren)?) makes this observation (I have renamed his `append/3` to `nappend/3` and corrected to the out to what SWIPL says)

> In Prolog, the append relation is stated as:
>
> ```logtalk
> nappend([],L,L).
> nappend([H|T],L,[H|R]) :- nappend(T,L,R).
> ```
> 
> declaring that an empty list is a prefix of any list, a list is a suffix of itself, and prepending to the prefix of a list prepends > to the list. Certainly `nappend/3` can concatenate two lists:
>
> ```logtalk
> ?- nappend([t,t,t],[f,f],X).
> X = [t, t, t, f, f].
> ```
>
> By passing a free [better: fresh] logic variable as one of the two first arguments, we can concatenate not fully-determined lists:
>
> ```logtalk
> ?- nappend([t,t,t],X,R).
> R = [t, t, t|X].
> ```
>
> We see the single result, which stands for every list with the prefix `[t,t,t]`. Such a compact representation of an infinite set
> is an asset. Alas, it is not always available, and is often a mirage. For example, if we pass a free variable as the first argument
> of `nappend/3`, Prolog responds differently:
>
> ```logtalk
> ?- nappend(X,[f,f],R).
> X = [], R = [f, f] ;
> X = [_18268], R = [_18268, f, f] ;
> X = [_18268, _19306], R = [_18268, _19306, f, f] ;
> ```
> 
> with an infinite set of answers, to be enumerated indefinitely. It may not be fully apparent that our Prolog code has not
> faithfully represented the original problem: to relate boolean lists. The last two answers describe lists of more than mere
> booleans. We have to impose the restriction, by defining the type predicates:
>
> ```logtalk
> bool(t).
> bool(f).
> boollist([]).
> boollist([H|T]) :- bool(H), boollist(T).
> ```
>
> and re-writing our queries:
> 
> ```logtalk
> ?- nappend([t,t,t],X,R), boollist(X), boollist(R).
> X = [], R = [t, t, t] ;
> X = [t], R = [t, t, t, t] ;
> X = [t, t], R = [t, t, t, t, t] ;
> X = [t, t, t], R = [t, t, t, t, t, t] ...
> ```
>
> One of `boollist(X)` or `boollist(R)` would have been enough: if `X` is a boolean list, so is `R`. Alas, Prolog has no
> type inference and is unable to infer or take advantage of that fact. To be safe, we add both predicates. 
> The compact representation for the lists with the `[t, t, t]` prefix has disappeared. More seriously, the 
> default depth-first search strategy of Prolog gives us only half of the answers; we won't ever see the lists 
> with an `f` after the first three `t`.

This is a demand to shift the dial more from "search-assisted programming" towards proper "theorem proving".

- Impose additional compile-time _guarantees_ on the form of syntactic structures at certain points of the computation (which is what "types" are when they are annotations on terms and predicate argument positions that are "information outflow" from predicates). 
   - Note that to make "types" acceptable to man & machine, they are expressed hierarchically: They are never given as a general 
     first-order formula, but always in a form of "this type is composed these types in a certain easily verifiable way" with the
     leaf types constraints being simple set membership statements.
- Impose additional run-time _demands_ on the form of syntactic structures at certain points of the computation (which is what "types" are when they are annotations on predicate arguments positions that are "information inflow" to predicates). 

The goal `nappend([t,t,t],X,R), boollist(X), boollist(R).` is evidently a "spread package" of "generate, then check" and is not the same as proper typing at all. We would be able to say `boollist(X),boollist(R),nappend([t,t,t],X,R)` if that were the case, but writing a goal this way leads to infinite recursion. 

Prolog is about arranging the goals to have "generators" and "checkers" in the right order, with the generators tuned to generate as little choices as possible, at best maximally 1 to keep computation from exploding. (The AND-OR-tree of predicate calls should look like OR nodes generating a few possibilites connected by long chains where OR nodes just generate 1 possibility.)

There is also an ugly asymmetry in that we can state a template for syntactic structures that are lists and have a "free end of arbitrary length", `[1,2,3|X]`, but we can't state a template for syntactic structures that are lists and a have a "free end of arbitrary length", `[X ??? 1,2,3]` or even `[X ??? 1,2,3|X]`. In fact, one has to deploy `reverse/2` in that case. This comes from the fact that... 

- The underlying structure of lists (chained consboxes) is inherently asymetrical
   - we can never generate "from the end of the list towards the front"
   - we can never test "from the end of the list towards the front"
- The expression of a list is "as eager as can be". For example for `length(L,6)`, `L` is not constrained to be "an 
  arbitrary list of length L" (which would just be annotation on the fresh variable L, to be checked later) but
we get right down to business: `L` is set to `[_716, _722, _728, _734, _740, _746].` 

More pointers:

- How does Mercury solve this (it's Haskell-ish?)
- Lambda Prolog has type expression for terms and predicates using arrows (from OCaml, being programmed in OCaml).


