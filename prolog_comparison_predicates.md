# Prolog's comparison predicates

WORK IN PROGRESS. EXAMPLES & TEST CASES TO BE ADDED

I'm going with the [SWI Prolog manual](https://eu.swi-prolog.org/pldoc/doc_for?object=manual) at this point and also
copying text from there.

**Good stuff:**

- Stackoverflow: [Using \==/2 or dif/2](https://stackoverflow.com/questions/13757261/using-2-or-dif-2/13770020)

## Vocabulary

The vocabulary is a bit confusing...

- Predicate
- Constraint
- Operator
- Functions. "Functions are terms that can appear in the argument of (the predicates) is/2, =:=/2, >/2, etc." But notation os the same as for predicates. `functor/arity`. See [list of functions](https://eu.swi-prolog.org/pldoc/man?section=functions)
- (two terms are) identical
- (two terms are) equivalent
- (two terms are) equal
- (two terms are) (can) unify

## The `=` predicate

**Description**

> `?Term1 = ?Term2`
> Unify `Term1` with `Term2`. True if the unification succeeds. 
>
> For behaviour on cyclic terms see the Prolog flag 
> [occurs_check](https://eu.swi-prolog.org/pldoc/man?section=flags#flag:occurs_check). 
> It acts as if defined by the following fact: `=(Term, Term).`

**Found under**

- [Comparison and unification of terms](https://eu.swi-prolog.org/pldoc/man?section=compare)
- ...[Predicate `=/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D)/2) 

**Can be read as**

"To continue past this point, first unify the terms on the left and right"

**Intention**

Given two terms `A` and `B`, fully or partially constrained, try to find a _most general unifier_ (a variable substitution) which constrains any variables appearing in `A` and `B` so that the  `A` and `B` are the same term after the substitution has been applied. If this succeeds, continue with the proof search, assuming the suitably constrained variables. If no variable substitution can be found, the terms `A` and `B` are dissimilar: a solution cannot be found by proceeding,  `A = B` cannot be made true. Unification fails, and backtracking to an anterior point of the computation will happen.

Unification is applied at every predicate call by matching the parameters on the caller's side with the arguments appearing in the head of the called predicate. This can be used to "broadcast" values constructed in the called predicate to all predicates on the stack that use a common variable (TBD: example): information is returned from callee to caller. Compare with simple pattern matching against the head, which communicates information only from caller to callee.

Prolog may be more about _unification_ than proof search because unification is _the_ operation that constructs complex data structures on the heap.

- More on [Unification](https://en.wikipedia.org/wiki/Unification_(computer_science))
- More on [Occurs check](https://en.wikipedia.org/wiki/Occurs_check)

**Examples**

Two syntax trees describing variables `Left` and `Right` are unified, leading to then being considered as equal past the point of unification, with other variables being constrained in the same process:

```logtalk
Left  = f(h(X),X,g(K)),        format("Left  = ~w\n",[Left]),  % Left unified with complex term
Right = f(h(a),Z,g(l(X,Z,H))), format("Right = ~w\n",[Right]), % Right unified with complex term
format("Unifying...\n"),
Left = Right,
format("Both terms are exactly the same now!\n"),
format("Left  = ~w\n",[Left]),
format("Right = ~w\n",[Right]).
```

```logtalk
Left  = f(h(_2934),_2934,g(_2938))
Right = f(h(a),_2982,g(l(_2934,_2982,_2984)))
Unifying...
Both terms are exactly the same now!
Left  = f(h(a),a,g(l(a,a,_2984)))
Right = f(h(a),a,g(l(a,a,_2984)))

Left = Right, Right = f(h(a), a, g(l(a, a, H))),
X = Z, Z = a,
K = l(a, a, H).
```

You cannot unify values of different type!

Integer vs. Float:

```logtalk
?- 1 = 1.0.
false.
```

or

```logtalk
?- I=1, integer(I), F=1.0, float(F), F=I.
false.
```

String vs. Atom:

```logtalk
?- "string vs. atom" = 'string vs. atom'.
false.
```

## Negation of `=` using NAF: `\=`

**Description**

> Equivalent to `\+(Term1 = Term2)`.
> This predicate is logically sound if its arguments are sufficiently instantiated. In other cases,
> such as `?- X \= Y.`, the predicate fails although there are solutions. This is due to the incomplete
> nature of `\+/1`.
> 
> To make your programs work correctly also in situations where the arguments are not yet sufficiently
> instantiated, use `dif/2` instead.

**Found under**

- [Comparison and unification of terms](https://eu.swi-prolog.org/pldoc/man?section=compare)
- ...[Predicate `\=`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%3D)/2)

**Can be read as**

"is there no evidence that terms X and Y can be unified (at this point in time)?

(alternatively, "if there is any way that terms X and Y can be unified, fail!"). 

**Examples**

```Logtalk
?- 1 \= 1.0.
true.
```

```Logtalk
?- X \= Y.
false.
```

We don't know whether X and Y cannot be unified. They could be the same! So the answer to _"is there no evidence that terms X and Y can be unified (at this point in time)"_ is indeed false.

### NAF of NAF unification

There is a subtle difference between 

- "unify the terms on the left and right and proceed if that succeeds.
- "test whether the terms on the left and right unify, and procced if yes" and

One may want to drop information collected during the successful unification in the second case!

Here is the trick how to Test/Unify something and proceed on success without retaining information about the  unification:

Retain MGU information:

```Logtalk
?- Left=f(G,1), Right=f(1,H),
   format("%> Left = ~w, Right = ~w, G = ~w, H = ~w\n",[Left,Right,G,H]),
   Left = Right, 
   format("%> Left = ~w, Right = ~w, G = ~w, H = ~w\n",[Left,Right,G,H]).

%> Left = f(_13862,1), Right = f(1,_13876), G = _13862, H = _13876
%> Left = f(1,1), Right = f(1,1), G = 1, H = 1

Left = Right, Right = f(1, 1),
G = H, H = 1.
```

Drop MGU information:

```Logtalk
?- Left=f(G,1), Right=f(1,H),
   format("%> Left = ~w, Right = ~w, G = ~w, H = ~w\n",[Left,Right,G,H]),
   \+( Left \= Right ), 
   format("%> Left = ~w, Right = ~w, G = ~w, H = ~w\n",[Left,Right,G,H]).

%> Left = f(_15796,1), Right = f(1,_15810), G = _15796, H = _15810
%> Left = f(_15796,1), Right = f(1,_15810), G = _15796, H = _15810

Left = f(G, 1),
Right = f(1, H).
```

## The `is` predicate: Force evaluation, then unify

This is really not a predicate, it is a special instruction to trigger evaluation (reduction?) of a function 
on the right-hand side and unify the (always numeric?) result with the left-hand side.

**Description** 

> `-Number is +Expr`: True when _Number_ is the value to which _Expr_ evaluates. 
> Typically, `is/2` should be used with unbound left operand. If equality is to be
> tested, `=:=/2` should be used. 

**Found under**

- [Arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arith)
- ...[General purpose arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arithpreds)
- ......[Predicate `is`](https://eu.swi-prolog.org/pldoc/doc_for?object=(is)/2)
  
**Can be read as**

"Evaluate the RHS and unify with the LHS"

**Note**

- A "force evaluation" instruction should be used for more than just numeric terms! String operations, obtaining data 
  from non-logical sources (`CurrentTime is time()`, `Data is read(Source)`) or even side-effects in obvious manner
  (`BytesWritten is writeBytes(Sink,String)`) should all be used that way instead of pretending rather disingeneously that
  such operations are "prediates". `is` is the gateway drug to function calls!
- Use `#=` to state constraints between numeric variables instead, what is generally what you want to do.

**Example**

Evaluate RHS and unify with LHS, i.e. 12

```Logtalk
?- 12 is 2*6.  
true.
```

Same as above but now we have different types on the LHS and RHS, so unification fails!

```Logtalk
12.0 is 2*6.
false.
```

Generally one evals the RHS and unifies with an unbound variable on the LHS. This looks like assignment:

```Logtalk
?- Result is 2*6.
Result = 12.
```

This should really work but doesn't. Why? Historical reasons! Maybe this will change someday.

```Logtalk
?- Result is "Hello," + 1 + " World".
ERROR: Type error: ...
```

## The `==` predicate: "after unification, do terms compare 'the same'?"

This is classed under:

- [Comparison and unification of terms](https://eu.swi-prolog.org/pldoc/man?section=compare)
- ...[Standard order of Terms](https://eu.swi-prolog.org/pldoc/man?section=standardorder)
- ......[Predicate `==/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%3D)/2)

> `@Term1 == @Term2`: True if `Term1` is equivalent to `Term2`. A variable is only identical to a sharing variable.

**Negation using NAF: [\==](https://eu.swi-prolog.org/pldoc/doc_for?object=(%5C%3D%3D)/2)**

> Equivalent to `\+Term1 == Term2`.

## The `=:=` predicate: "arithmetically equal"

This is classed under:

- [Arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arith)
- ...[General purpose arithmetic](https://eu.swi-prolog.org/pldoc/man?section=arithpreds)
- ......[Predicate =:=](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%3A%3D)/2)

> `+Expr1 =:= +Expr2`: True if expression `Expr1` evaluates to a number equal to `Expr2`.
    
**Negation: [`=\=`](https://eu.swi-prolog.org/pldoc/doc_for?object=(%3D%5C%3D)/2)**

>`+Expr1 =\= +Expr2`: True if expression `Expr1` evaluates to a number non-equal to `Expr2`.
      
## The `#=` predicate: "constraint to be arithmetically equal"

This is classed under:

- [library(clpfd): CLP(FD): Constraint Logic Programming over Finite Domains](https://eu.swi-prolog.org/pldoc/man?section=clpfd)
- ...[arithmetic constraints](https://eu.swi-prolog.org/pldoc/man?section=clpfd-arith-constraints)
- ......[predicate #=](https://eu.swi-prolog.org/pldoc/doc_for?object=%23%3D%20/%202)

> `?X #= ?Y`: The arithmetic expression `X` equals `Y`. This is the most important arithmetic constraint
> ([section A.9.2](https://eu.swi-prolog.org/pldoc/man?section=clpfd-arith-constraints)), subsuming
> and replacing both `(is)/2` and `(=:=)/2` over integers. See _declarative integer arithmetic_
> ([section A.9.3](https://eu.swi-prolog.org/pldoc/man?section=clpfd-integer-arith)).

**Negation: [`#\=/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=%23%5C%3D%20/%202)**

> The arithmetic expressions _X_ and _Y_ evaluate to distinct integers. When reasoning over integers, 
> replace `(=\=)/2` by `#\=/2` to obtain more general relations. See declarative integer arithmetic
([section A.9.3](https://eu.swi-prolog.org/pldoc/man?section=clpfd-integer-arith)).

## The `dif` predicate: "constrain to be different"

This is classed under:

- [constraint logic programming](https://www.swi-prolog.org/pldoc/man?section=clp)
- ...[coroutining](https://www.swi-prolog.org/pldoc/man?section=coroutining)
- ......[predicate dif/2](https://eu.swi-prolog.org/pldoc/doc_for?object=dif/2)

> `dif(@A, @B)`: The `dif/2` predicate is a constraint that is true if and only if _A_ and _B_ are different terms.
> If _A_ and _B_ can **never unify**, `dif/2` succeeds deterministically. If _A_ and _B_ are **identical**, it fails
> immediately. Finally, if _A_ and _B_ **can unify**, goals are delayed that prevent _A_ and _B_ to become equal. 
> It is this last property that makes `dif/2` a more general and more declarative alternative for `\=/2` and 
> related predicates.
> 
>  This predicate behaves as if defined by `dif(X, Y) :- when(?=(X,Y), X \== Y)`. See also `?=/2`. The
> implementation can deal with cyclic terms.
> 
> The `dif/2` predicate is realised using attributed variables associated with the module `dif`. It is an autoloaded
> predicate that is defined in the library `library(dif)`.

