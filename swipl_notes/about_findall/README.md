# Notes about findall/3

_This is companion information to the SWI-Prolog manual page of the [`findall/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=findall/3) predicate._

## `findall/3` is problematic!

This predicate has been behaving non-declaratively for at least 35 years (i.e. at least since Lee Naish wrote
"Negation and Control in Prolog" in 1985 - reference at the end of this page). It still made it into the
ISO Standard of 1995. Shouldn't it be deprecated?

Note that it _does_ work with unbound variables that have constraints, at least in SWI Prolog. See "Will constraints be retained?" below.

## Intro

`findall/3` is one of the traditional "all-solution predicates" (which are "higher-order predicates" aka "metapredicates"),
There are:

- [`findall/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=findall/3) (via Marseille Prolog)   
- [`setof/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=setof/3) (via Edinburgh Prolog)
- [`bagof/3`](https://eu.swi-prolog.org/pldoc/doc_for?object=bagof/3) (via Edinburgh Prolog)
- [`findall/4`](https://eu.swi-prolog.org/pldoc/doc_for?object=findall/4) (which is `findall/3` using a difference list, so you can perform "stream processing" on an _open list_ from which one continuously reads at the front and appends at the end)

## Where to find explainers

- In **The Art of Prolog: Advanced Programming Techniques**, (Leon Sterling and Ehud Shapiro, 1st edition 1984, MIT Press),
  the predicates `bag_of/3`, `set_of/3` and `find_all_dl/3` (corresponding to `findall/4`) are discussed in
  Chapter 17: "Second-Order Programming", pages 266 ff.
- In **Programming in Prolog**, (William Clocksin and Christopher Mellish, 2003, Springer), `findall/3` is explained 
  in chapter 7.8.3. (p. 167 ff), and an implementation in Prolog that uses `asserta/1` and `retract/1` to 
  store solutions prior to unifying a list of the same with the `Instances` argument is provided.

`findall/3` is described in the ISO/IEC 13211-1 (1995 version) Prolog standard on page 82 as follows:

> _findall (Template, Goal, Instances)_ is true iff _Instances_ unifies with the list of values to
> which a variable _X_ not occurring in _Template_ or _Goal_ \[i.e. a fresh variable\] would be 
> instantiated by successive re-executions of _call(Goal), X=Template_ after systematic
> replacement of all variables in _X_ by new variables.

That's pretty obscure, although it _is_ followed by a pseudocode description. It's all "operational semantics" though.

## Notes on history

See at the end of this page.

## Some examples for findall/3

In the following text, parameter names shall be given by `findall(+Template, :Goal, -Bag)` 

The _mode indicators_ are as follows (text from [Notation of Predicate Descriptions](https://eu.swi-prolog.org/pldoc/man?section=preddesc)):

- `+`	"At call time, the argument must be instantiated to a term satisfying some (informal) type specification. The argument need not
      necessarily be ground." In this case `Template` need just be some (probably nonground) term, probably simply an unbound variable, 
      and probably sharing variables with `Goal`.
- `:`	"Argument is a meta-argument, for example a term that can be called as goal." 
- `-`	"Argument is an output argument. It may or may not be bound at call-time. If the argument is bound at call time,
      the goal behaves as if the argument were unbound, and then unified with that term after the goal succeeds. This is what is called being _steadfast_".

### Standard behaviour

`member/2` can be used as a generic example of a backtrackable predicate.

```
?- findall(X,member(X,[1,2,3]),Bag).
Bag = [1, 2, 3].
```

The bag is not a set, and it preserves the order of the generated solutions:

```
?- findall(X,member(X,[3,2,1,3,2,1]),Bag).
Bag = [3, 2, 1, 3, 2, 1].
```

The generated solution need not have to do anything with the goal:

```
?- findall(true,member(X,[1,2,3]),Bag).
Bag = [true, true, true].
```

The generated solution need can be some complex term based on the goal:

```
?- findall(found(X,Y),(member(X,[1,2]),member(Y,[3,4])),Bag).
Bag = [found(1, 3), found(1, 4), found(2, 3), found(2, 4)].
```

If the goal becomes hard to read, better de-inline it into a program:

```
subgoal(found(X,Y)) :- member(X,[1,2]),member(Y,[3,4]).
```

Then:

```
?- findall(F,subgoal(F),Bag).
Bag = [found(1, 3), found(1, 4), found(2, 3), found(2, 4)].
```

which gives you the great advantage that you can call the subgoal separately:

```
?- subgoal(F).
F = found(1, 3) ;
F = found(1, 4) ;
F = found(2, 3) ;
F = found(2, 4).
```

### Bad style warning

Writing

```
?- findall(X,member(X,[1,2,3]),X).
X = [1, 2, 3].
```

is allowed but bad style. The same variable name `X` appears in two distinct roles, in two distinct contexts. 

What we are asking is:

> the sequence of X such_that member(X,[1,2,3]) is true

and the result is just put into a the bag which happens to be called `X`.
The first two `X` appear in a "goal context". The `X` designating the bag has nothing to do with it! 

This should be caught by a Prolog [code linter](https://sourcelevel.io/blog/what-is-a-linter-and-why-your-team-should-use-it).

Jan Wielemaker writes:

> It is just that `findall/3` first uses the first argument and the goal, backtracking over all solutions.
> In the end, the first argument is unchanged (i.e., still a variable if it was one to begin with) and 
> thus we can reuse it for the second stage (collecting the answers). It is not wrong. It should
> be considered bad style. Not much different that reusing a variable in imperative coding for two
> totally different purposes instead of introducing a new variable and assume that the compiler
> reuses the same location if the scopes do not overlap (and even if not, the damage is really small).
> So yes, a linter issue.

### What if there is no solution for the subgoal?

`bagof/3` and also `setof/3` fail if there are no solutions for the subgoal:

```
?- bagof(X,member(X,[]),Bag).
false.

?- bagof(X,(between(1,10,X),X<0),Bag).
false.
```

Unlike the above `findall/3` _succeeds with an empty `Bag`_ if there are no solutions:

```
?- findall(X,member(X,[]),Bag).
Bag = [].

?- findall(X,(between(1,10,X),X<0),Bag).
Bag = [].
```

At first sight, this looks user friendly - it's an approach that reminds one of an imperative language. 
On second sight, there is a problem with the logical interpretation. 
Leaving out the ancillary condition of whether an instantiated `Bag` unifies with the `Bag` of 
results actually collected (the subgoal's proof witnesses), `findall/3` succeeds in all cases.
If there is no proof of the subgoal, `bagof/3` properly fails whereas `findall/3` is
[vacuously true](https://en.wikipedia.org/wiki/Vacuous_truth): all elements of `[]` are a solution of the subgoal.
But we _do_ want failure: a predicate is _supposed_ to fail if there is no proofs of any subgoal, that's the idea. 

In fact, one can implement negation-as-failure (mis-)using `findall/3`'s "succeed even on failure" approach
simply through checking whether the resulting `Bag` is `[]` (see the end of this page).

One may recover logically-acceptable behaviour by following up with a comparison against `[]` (we don't even
need to use [if-then-else](https://eu.swi-prolog.org/pldoc/doc_for?object=(-%3E)/2)

For example (how do I generalize this to arbitrary (`Template`,`Subgoal`) pairs?):

```
foo(1) :- write("Called 1\n").
foo(2) :- write("Called 2\n").
findall_which_fails(Bag,M) :- findall(X,(foo(X),X>M),Bag), Bag \== [].
```

Then:

```
?- findall_which_fails(Bag,0).
Called 1
Called 2
Bag = [1, 2].

?- findall_which_fails(Bag,1).
Called 1
Called 2
Bag = [2].

?- findall_which_fails(Bag,3).
Called 1
Called 2
false.
```

### findall/3 always generates all solutions of subgoal, irrespective of the size of Bag

Generally one passes a `Bag` that is an unbound variable. `findall/3` will then
unify `Bag` with the list of solutions collected once the collection is done (this behaviour is according to ISO standard specification,
and also is indicated by the mode indicator `-` of the third parameter).

```
?- findall(X,(between(0,4,X),format("Found ~q\n",[X])),Bag).
Found 0
Found 1
Found 2
Found 3
Found 4
Bag = [0, 1, 2, 3, 4].
```

`findall/3`, similar to `bagof/3` and `setof/3` **is not** limited by any initially set 
length of `Bag`.  If the final unification fails, the `findall/3` call fails.

Here we give it a `Bag` of 5 fresh variables to fill with solutions from a `Goal` that "redoes" forever.
`findall/3` does not care and goes on ... forever! Even thought it _does_ have the information that it _could_ stop at the 6th solution.

```
?- findall(X,between(0,inf,X),[A0,A1,A2,A3,A4]).
ERROR: Stack limit (1.0Gb) exceeded
```

Similarly, if `Bag` is too small or too large, `findall/3` will collect all subgoal solutions, and then fail at unifying those with `Bag`:

```
?- findall(X,(between(0,4,X),format("Found ~q\n",[X])),[A0,A1,A2]).
Found 0
Found 1
Found 2
Found 3
Found 4
false.

?- findall(X,(between(0,4,X),format("Found ~q\n",[X])),[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9]).
Found 0
Found 1
Found 2
Found 3
Found 4
false.
```

Similarly, if the `Bag` may fit but be non-unifiable in the end:

```
?- findall(X,(between(0,4,X),format("Found ~q\n",[X])),[A0,A1,A2,A3,A4]).
Found 0
Found 1
Found 2
Found 3
Found 4
A0 = 0,
A1 = 1,
A2 = 2,
A3 = 3,
A4 = 4.

?- findall(X,(between(0,4,X),format("Found ~q\n",[X])),[A0,0,0,0,A4]).
Found 0
Found 1
Found 2
Found 3
Found 4
false.
```

The above also applies to `bagof/3` and `setof/3`

There is space in the market for a `findall_cautious/3` which checks before/after each call 
to `Goal` whether it will be able to unify the new element with the current position in `Bag`
and thus is able to fail early (due to `Bag` running out of space or having a value at some position
that does not unify). In fact, there could even be a `findall_overflow/4` which generates a
[Continuation](https://eu.swi-prolog.org/pldoc/man?section=delcont) that can be called for
more solutions if the `Bag` turns out to be too small after all. To your editors!

(Is demanding steadfastness from a predicate, as the ISO standard demands of `findall/3`,
 even reasonable? I doubt it. Predicates should "fail fast"
 with the full information that can be obtained from the arguments they have been given. 
 If you absolutely want steadfastness, you can always pass an 
 unbound variable (which gives nothing away) at the parameter position marked `-` and unify 
 that with a not-full-unbound term afterwards.)

### Edge case; bad bag

Edge case: `findall/3` accepts a non-list `Bag` instead of throwing a type error.
Might be useful to fix that. In the first case below, the unification fails trivially, in the second, it loops forever:

```
?- findall(X,between(0,4,X),1).
false.

?- findall(X,between(0,inf,X),1).
ERROR: Stack limit (1.0Gb) exceeded
```

### findall/3 has no "caret syntax" to existentially quantify variables

`findall/3` does not have a special syntax to indicate which variables in `Goal`
should be "shielded off" (existentially quantified) from the namespace of variables existing outside of `Goal`. 

With the database

```
f(a,1).
f(b,1).
f(c,2).
f(d,2).
```

**`bagof/3` without caret**

With `bagof/3`: 

```
?- bagof(X,f(X,Y),Bag).
Y = 1,
Bag = [a, b] ;
Y = 2,
Bag = [c, d].
```

- The first call to the subgoal `f(X,Y)` fixes `Y` to 1. 
- Then `bagof/3` runs to completion with the constraing `Y=1`, resulting in the bag `[a, b]`.
- Thus the whole toplevel goal succeeds.
- If the user asks for more solutions with `:`...
- Prolog backtracks over `bagof/3`.
- The the second call to the subgoal `f(X,Y)` fixes `Y` to 2 (that's pretty magical, actually; Prolog retains what solutions have been generated?)
- Then `bagof/3` runs to completion with the constraing `Y=2`, resulting in the bag `[c, d]`.

What we are asking for is:

> the sequence of `(Y,Bag)` such that : `Y` exists and `Bag` is the sequence of `X` such that : `f(X,Y)` is true

This works even if the database has a different order, no solutions are lost:

```
f(a,1).
f(d,2).
f(c,2).
f(b,1).

?- bagof(X,f(X,Y),Bag).
Y = 1,
Bag = [a, b] ;
Y = 2,
Bag = [d, c].
```

(Not sure how this is done though).

**`bagof/3` with caret**

**Conversely**, you can tell bagof/3 to _internally_ backtrack over `Y`,
by "existentially quantifying Y" with `Y^`, thus separating it from any connection to a namespace outside of the inner goal:

With the same database:

```
?- bagof(X,Y^f(X,Y),Bag).
Bag = [a, b, c, d].
```

What we are asking for is:

> the sequence `Bag` of `X` such that : there is some `Y` such that : `f(X,Y)` is true

This is the same as de-inlining the inner goal like this:

```
subgoal(X) :- f(X,_).

?- bagof(X,subgoal(X),Bag).
Bag = [a, b, c, d].
```

**`bagof/3` with ineffective caret**

However, if `Y` is bound prior to call to `bagof/3`, the existential quantification of `Y` by a `Y^` might as well not exist.
This is frankly unexpected and IMHO, should not be handled like this:

```
?- Y=2,bagof(X,Y^f(X,Y),Bag).
Y = 2,
Bag = [c, d].
```

> the sequence of `(Y,Bag)` such that : `Y` == 2 and `Bag` is the sequence of `X` such that : `f(X,Y)` is true

The value to which `Y` is bound is visble in the subgoal.

**`findall/3` always behaves like the "existentially quantified" `bagof/3`**

```
?- findall(X,f(X,Y),Bag).
Bag = [a, b, c, d].
```

> the sequence `Bag` of `X` such that : there is some `Y` such that : `f(X,Y)` is true

The same phenomenon as for `bagof/3` occurs if a variable is bound before the call (this looks correct, unlike for  `bagof/3` where the `^` 
muddies the water) 

```
?- Y=2,findall(X,f(X,Y),Bag).
Y = 2,
Bag = [c, d].
```

> the sequence of `(Y,Bag)` such that : `Y` == 2 and `Bag` is the sequence of `X` such that : `f(X,Y)` is true

## Beware the copying of unbound variables

Note that if the `findall/3` subgoal emits variables, these are not the same variables as those that can be found in `Bag`.
Those in `Bag` are fresh, they denote different empty cells in memory.

```
?- findall(X,member(X,[A,B,C]),Bag). 
Bag = [_26582, _26576, _26570].         % The solution contains fresh variables, not A,B,C
```

More involved:

```
subgoal_p(X,L) :-
   member(X,L),
   format("X is now ~q\n",[X]).
   
?- L=[A,B,C],format("L is now: ~q\n",[L]),findall(X,subgoal_p(X,L),Bag).
L is now: [_8208,_8214,_8220]    % fresh variables in L
X is now _8208                   % the fresh variable at L[0] is correctly seen by subgoal_p/2
X is now _8214                   % the fresh variable at L[0] is correctly seen by subgoal_p/2
X is now _8220                   % the fresh variable at L[0] is correctly seen by subgoal_p/2 
L = [A, B, C],                 
Bag = [_9222, _9216, _9210].     % none of the variables of L is in Bag
```

More practical, changing `L` after `findall/3` doesn't change `Bag`:

```
?- L=[A,B,C],findall(X,subgoal_p(X,L),Bag),L=[1,2,3].
X is now _14416
X is now _14422
X is now _14428
L = [1, 2, 3],
A = 1,
B = 2,
C = 3,
Bag = [_15438, _15432, _15426].
```

You cannot transparently "look for variables" that way.

### Will constraints be retained?

What about using `findall/3` on a term that has attributed variables or constraints?

Will this result in a "neutered" collection?

Consider:

```
:- use_module(library(clpfd)).

constrain(X,Y,Z) :- 
   (0 #=< X, X #< Y, Y #< Z, Z #=< 3).
```

Then, without passing the unbound variables in the constraint through a `findall/3` copy&mangle operation:

```
?- constrain(X,Y,Z),label([X,Y,Z]).
X = 0,
Y = 1,
Z = 2 ;
X = 0,
Y = 1,
Z = 3 ;
X = 0,
Y = 2,
Z = 3 ;
X = 1,
Y = 2,
Z = 3.
```

or, collecting all solutions, actually using  `findall/3` to do that:

```
?- constrain(X,Y,Z),findall([X,Y,Z], label([X,Y,Z]), All).

All = [[0, 1, 2], [0, 1, 3], [0, 2, 3], [1, 2, 3]],  <--- all collected solutions
X in 0..1,  <--- residual constraints on unlabled X,Y,Z
X#=<Y+ -1,
Y in 1..2,
Y#=<Z+ -1,
Z in 2..3.
```

Note that "residual constraints" are printed (I don't know whether that's the official term, but it
sounds reasonable to call them that, in analogy to the "residual goals" of `dif/2` and family: 
constraints that have not been resolved yet, indicating that you should make doubly sure that
the program's success is due to actual success or rank optimism). The residual constraints
are relative to the original `X`, `Y`, `Z`, which remain "unprocessed".

Pleasingly, the above **also** works if the variables are first copied via `findall/3` (at least in SWI-Prolog. Apparently not in SICStus?)

First, a SWI-Prolog specific item: we want to see the result term printed in full.

```
?- set_prolog_flag(answer_write_options,[max_depth(100)]).
```

Then:

```
?- constrain(X,Y,Z), 
   findall(K,member(K,[X,Y,Z]),L),  % copy variables into L, creating fresh variables
   label(L).                        % label the fresh variables
   
L = [0, 1, 2],   <--- a solution for L
X in 0..1,       <--- residual constraints on unlabled X,Y,Z
X#=<Y+ -1,
Y in 1..2,
Y#=<Z+ -1,
Z in 2..3 ;      <--- more?
L = [0, 1, 3],   <--- another solution for L
X in 0..1,       <--- residual constraints on unlabled X,Y,Z
X#=<Y+ -1,
Y in 1..2,
Y#=<Z+ -1,
Z in 2..3 ;      <--- more? 
```

etc.

Gotta collect them all using, yes, `findall/3`.
Note that for some reason the [`numbervars(3)`](https://eu.swi-prolog.org/pldoc/doc_for?object=numbervars/3)
notation is not resolved (i.e. there is a `write_term/2` somehwere which is not called with option 
`numbervars(true)`).

```
?- constrain(X,Y,Z), 
   findall(K,member(K,[X,Y,Z]),L),  % copy variables into L, creating fresh variables
   findall(L, label(L), All).       % label whatever is in L now repeatedly, collecting solutions
   
L = [_59552,_59558,_59564],         <--- L has been left a list of unbound variables
All = [[0,1,2],[0,1,3],             <--- The solutions have been collected in All
       [0,2,2],[0,2,3],
       [1,1,2],[1,1,3],
       [1,2,2],[1,2,3]],
$VAR(X)in 0..1,                     <--- residual constraints on unlabled X,Y,Z (but with a printing bug)
$VAR(X)#=< $VAR(Y)+ -1,
$VAR(Y)in 1..2,
$VAR(Y)#=< $VAR(Z)+ -1,
$VAR(Z)in 2..3,
_59564 in 2..3,                     <--- residual constraints on variables left unused due to copying
_59930#=<_59564+ -1,
_59930 in 1..2,
_59978#=<_59930+ -1,
_59978 in 0..1,
_59558 in 1..2,                    
_59558#=<_60056+ -1,
_60074#=<_59558+ -1,
_60056 in 2..3,
_60074 in 0..1,
_59552 in 0..1,                    
_59552#=<_60176+ -1,
_60176 in 1..2,
_60176#=<_60224+ -1,
_60224 in 2..3.
```

## Beware the reuse of variable names

This has been discussed above, but to reiterate:

```
bagit(X,Bag)        :- bagof(X,member(X,[1,2,3]),Bag).    % There is a messy variable name clash here

findit(X,Bag)       :- findall(X,member(X,[1,2,3]),Bag).  % There is a messy variable name clash here

bagit_caret(X,Bag)  :- bagof(X,X^member(X,[1,2,3]),Bag).  % There is STILL a messy variable name clash here

findit_solid(_,Bag) :- findit_name_isolate(Bag).          % Let's fix this!

findit_name_isolate(Bag) :- 
   findall(X,member(X,[1,2,3]),Bag).
   
bagit_solid(_,Bag)  :- bagit_name_isolate(Bag).           % Let's fix this!

bagit_name_isolate(Bag) :- 
   bagof(X,member(X,[1,2,3]),Bag).
```

Then:

`bagit/2` and `findit/2` have the same problem: they only work correctly if `X` is unbound.

```
?- bagit(X,Bag).
Bag = [1, 2, 3].

?- bagit(a,Bag).
false.

?- bagit(1,Bag).
Bag = [1].
```

```
?- findit(X,Bag).
Bag = [1, 2, 3].

?- findit(a,Bag).
Bag = [].

?- findit(1,Bag).
Bag = [1].
```

Using the caret doesn't help:

```
?- bagit_caret(X,Bag).
Bag = [1, 2, 3].

?- bagit_caret(a,Bag).
false.

?- bagit_caret(1,Bag).
Bag = [1].
```

Only isolation works:

```
?- bagit_solid(X,Bag).
Bag = [1, 2, 3].

?- bagit_solid(a,Bag).
Bag = [1, 2, 3].

?- bagit_solid(1,Bag).
Bag = [1, 2, 3].
```

```
?- findit_solid(X,Bag).
Bag = [1, 2, 3].

?- findit_solid(a,Bag).
Bag = [1, 2, 3].

?- findit_solid(1,Bag).
Bag = [1, 2, 3].
```

## Uncommon usage

Via Peter Ludemann:

### Construct `\+`

Using the fact that the Bag of `findall/3` is unified with the empty list if its Goal fails, we can 
implement a `findall/3`-based version of the (non-logical) negation-as-failure:

```
another_not(Goal) :- findall(., Goal, []).
```

Then:

```
?- another_not(false).
true.

?- another_not(true).
false.
```

### Construct `var/1`

Remember that `var/1` asks whether the argument is an unbound variable at call time (it's not logic, it's a question about the state of the computation, and it should have been called `unbound/1`):

```
another_var(X) :- findall(_, (X=a; X=b), [_,_]).
```

```
?- another_var(X).
true.

?- another_var(foo).
false.
```

This works because:

- only a unbound variable can successively unify with `a` and `b`, giving exactly solutions. Consider a debugging extension:
- the `X` in the Goal of `findall/3` is the `X` from the outside naming context, it is not "shadowed"
by using `X` as the template (confusing? yes!).

Consider the debugging version:

```
another_var(X,[U,V]) :- findall(_, ((X=a,write("unif(a)\n")); (X=b,write("unif(b)\n"))), [U,V]).
```

Then:

```
?- another_var(foo,[U,V]).
false.

?- another_var(a,[U,V]).
unif(a)
false.

?- another_var(b,[U,V]).
unif(b)
false.

?- another_var(X,[U,V]).
unif(a)
unif(b)
U = a,
V = b.
```

This phenomenon is discussed in "Negation and Control in Prolog" by Lee Naish, 1985 on page 13:

> 1.7.3.1. Declarative Semantics
> 
> If the convention of findall is used to determine what variables are local, then it is generally
> not possible to determine what relation is computed from the program text. The semantics of the
> call depends on which variables are local - and this depends on the variable bindings when the call is
> executed. For example, the var predicate can be defined as follows:
> 
> ```
> var(X) :- findall(_,(X=1;X=2),[_,_]).
> ```
> 
> If `X` is a \[unbound\] variable when var(X) is called, it will be treated as a local variable. Two solutions to
> the goal _∃X (X=1 ⋁ X=2)_ are found and `var(X)` succeeds. If `X` is not a variable, however, it acts as a
> global variable and the goal has one solution at most, so the call fails. Declarative semantics can be
> given only if it is known exactly which variables will be bound at the time of the call. Variables that
> appear only in the call to solutions cannot be bound, but the bindings of other variables depend on
> the order of calls. Calls to all can be made safe by explicitly declaring all these variables to be
> global.
> 
> With `setof`, etc., local variables can be determined from the program text so the semantics are
> clear. However, if local variables appear outside the call they may become bound and change the
> `setof` call. Ensuring that variables declared to be local are indeed local should be done when a
> program is read. Unfortunately, current implementations do not do this.

Rather disconcertingly, this is still the case 35 years later.

## Some notes on history

- 1978: The "User's Guide to DECSystem-10 Prolog" (Luis Moniz Pereira, Fernando Pereira, David Warren - October 1978)
  ([PDF](https://userweb.fct.unl.pt/~lmp/publications/online-papers/USER%20GUIDE%20TO%20DECSYSTEM-10%20PROLOG.pdf))
  lists neither of the all-solution predicates yet.
- 1982: David Warren introduces `setof/3` in "Higher-order extensions to PROLOG: are they needed?" This paper can be found 
  in _Hayes, J. E., Michie, D., and Pao, Y.-H. (Eds.), Machine Intelligence 10, 1982. Ellis Horwood._
  ([Online at AAAI AITopics](https://aitopics.org/doc/classics:C65CF540/)). See also the _Papers_ section of
  the page [Prolog and Logic Programming Historical Sources Archive](www.softwarepreservation.org/projects/prolog/) at 
  the Computer History Museum.
- 1984: The `findall/3` predicate can be found in [Prolog II](http://prolog-heritage.org/fr/ph20.html).
- 1984: The CProlog User's Manual V1.2 (edited by Fernando Pereira - September 1984)
  ([PDF](http://www.softwarepreservation.org/projects/prolog/edinburgh/doc/CPrologUMV1.2.pdf)) 
  lists `bagof/3` and `setof/3` but not `findall/3`.
  
In _Higher-order extensions to PROLOG: are they needed?_ David Warren writes:

> I believe it is possible to replace these ad hoc solutions with a single more
> principled extension to PROLOG, which preserves the "declarative" aspect of
> the language. This extension has already been incorporated in the latest version
> of DEC-10 PROLOG. The implementation is essentially an encapsulation of the
> standard hack in a more general and robust form.
>
> The extension takes the form of a new built-in predicate:
>
> `setof(X,P,S)`
>
> to be read as:
>
> "The set of instances of `X` such that `P` is provable is `S`".
>
> The term `P` represents a goal or goals, specified exactly as in the right-hand side
> of a clause. The term `X` is a variable occurring in `P`, or more generally any term
> containing such variables. The set `S` is represented as a list whose elements are
> sorted into a standard order, without any duplicates.

In "Negation and Control in Prolog", on page 12, Lee Naish lists implementations of "collection predicates" that existed in 1985:

> **1.7.2.3. Implementations**
> 
> Many all-solutions predicates have been implemented. In this section we describe a selection of
> them, concentrating of the differences with the solutions predicate we have described so far.
> 
> **Findall**
> 
> This is one of the simplest implementations, defined in PROLOG in \[Clocksin 84: "Programming in Prolog, 2nd edition"\]. The main
> difference between _findall_ and _solutions_ is the way local variables are distinguished. All variables
> which are unbound at the time of the call are treated as local. This ensures that calls to _findall_ are
> deterministic.
> 
> **Collect**
> 
> The behaviour of _collect_ \[Kahn 84: "A Primitive for the Control of Logic Program"\] is the same as _findall_ except that it has features which
> permit more flexible control of the execution. The solutions can be computed in a lazy, as needed,
> fashion or eagerly, using a separate process.
> 
> **All**
> 
> This implementation is given in \[Pereira 81: "All Solutions"\] and also uses the same default as _findall_ for local
> variables, but it can be overridden. Using a goal of the form _Goal same Global_vars_, forces variables
> in the term _Global_vars_ to be global. Empty lists of solutions are never returned; _all_ just falls in this
> case. One version of this predicate also removes duplicates from the list of solutions. If two
> solutions can be unified, only the result (sometimes less general) is put in the list of solutions.
> 
> **Bagof**
> 
> This is provided in DEC-10 PROLOG \[Bowen 82: "DECSystem-10 Prolog User's Manual"\] and has the convention for declaring local
> variables we have adopted for _solutions_. Like _all_, _bagof_ falls if there are no solutions to the goal.
> 
> **Setof**: 
> 
> _Setof_ \[Warren 82: "Higher-Order Extensions to Prolog: Are They Needed?"\] is the same as _bagof_ except
> that the list of solutions is sorted and duplicates are removed. Unlike _all_, the most general solutions are retained.
> 
> **Quantified-bag-of**
> 
> This is available as part of the LM-PROLOG's "DEC-10 compatibility package" \[Carlsson 83: "LM-Prolog User Manual"\],
> and is intended to be equivalent to _bagof_. However, according to our reading of the
> manual, the treatment of local variables is similar to _all_, rather than _bagof_.
> 
> **Quantified-set-of**
> 
> This is the same as _quantified-bag-of_, except that duplicates are removed.
> 
> **Set Expressions**
> 
> In IC-PROLOG's set expressions \[Clark 80: "IC-Prolog - Language Features"\], variables that only occur within the set expression
> are considered local, and the others are global. If a proof of the goal is found in which a global
> variable is bound, the execution terminates with a control error. Set expression (and some variants)
> have also been incorporated in some parallel logic programming languages, such as Parlog
> \[Clark 83a: "PARLOG: A Paralel Logic Programming Language"\]. These languages are beyond the scope of this thesis.
> 
> **Set_of_all**
> 
> Rather than return a list, this proposed predicatet returns a set of solutions. Sets must be
> implemented as a primitive data type of PROLOG.

## References

- **Negation and Control in Prolog**
   - Lee Naish
   - Springer Lecture Notes in Computer Science, N° 238
   - Springer-Verlag 1985 
   - [Book Page](https://www.springer.com/gp/book/9783540168157)
   
