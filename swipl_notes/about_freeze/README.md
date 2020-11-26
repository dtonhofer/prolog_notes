# About `freeze/2`

Some notes on [`freeze/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=freeze/2) to complement the SWI-Prolog manual.

`freeze/2` is classed under "coroutining", this being common terminology. 
However, a ["coroutine"](https://en.wikipedia.org/wiki/Coroutine) is much more general. 
In fact, `freeze/2`  can be implemented with coroutines.
On the page [Delimited Continuations](https://eu.swi-prolog.org/pldoc/man?section=delcont)
the SWI-Prolog manual says _"coroutines \[are\] two or more routines whose execution is interleaved, while they exchange data."_ 
which sounds correct.

The intention is to freeze evaluation of a goal until a variable that is used by that goal has been bound. The
reason would be that the goal cannot compute if that variable is unbound: We will skip the goal (pretending
it succeeded) and continue "further to the right", where `X` may hopefully be bound. At which point that goal can run:

```
compute(X,Y) :- Y is X*2.
problematic(Y) :- freeze(X,compute(X,Y)),X=12.
```

The goal `compute(X,Y)` is only run (immediately) after `X` has been unified with 12.

The other view about what we are doing is that we are defining goals that are called when an unbound variable
participates in an unification (binding it serves as a wakeup signal). Depending on whether that goal then fails or succeeds, 
the unification corresponding fails or succeeds. In effect, we are sticking additional unification checks onto unbound
variables. This is the view of ["attributed variables"](https://eu.swi-prolog.org/pldoc/man?section=attvar),
which are actually used to implement `freeze/2` and friends.

Te thought arises that this could also be used for general debugging, to monitor instantiation of a variable. A bit borderline.

## Historical note

From 

_Parallelism and Implementation Technology for Logic Programming Languages_ (~2001?) by Vitor Santos Costa, p.7

> One of the most serious criticisms of Prolog is that the selection function used by Prolog is too restrictive. 
> From the beginning, authors such as Kowalski [a] remarked the effect on the size of the search space of 
> solving different goals in different sequences. A more flexible execution than the one used by Prolog 
> can be obtained through coroutining. Coroutines cooperatively produce and consume data, allowing for 
> data-driven execution. Several designs for coroutining became very influential in the logic programming
> community. IC-Prolog, designed by Clark and others [b], was one of the first logic programming languages 
> to support delaying of goals until data is available. Colmerauer’s Prolog-II [c] supported "geler" \[french for "freeze"\],
> a built-in that would delay a goal until a variable would be instantiated. Naish’s MU-Prolog and 
> NU-Prolog [d] supports "wait", a form of declaring that a goal should only execute if certain arguments were
> instantiated. Features similar to "geler" and "wait" are now common in modern logic programming systems.

  - [a]: R. A. Kowalski. Logic for Problem Solving. Elsevier North-Holland Inc., 1979.
  - [b]: K. L. Clark, F. G. McCabe, and S. Gregory. IC-PROLOG – language features. In K. L. Clark and S. A. Tärnlund, editors, Logic Programming, pages 253–266. Academic Press, London, 1982.
  - [c]: A. Colmerauer. Prolog II: Reference Manual and Theoretical Model. Groupe d’Intelligence Artificielle, Faculté des Sciences de Luminy, Marseilles, October 1982. (It's on page 67 of the Prolog II manual here: http://prolog-heritage.org/fr/m2.html )
  - [d] L. Naish. Negation and Control in Prolog. Lecture notes in Computer Science 238. Springer Verlag, 1985.

## Also check

Chapter 35 (p. 175) of the Flora-2 Manual [PDF](http://flora.sourceforge.net/docs/floraManual.pdf): _"Rearranging the Order of Subgoals at Run Time"_. Superior.

## Reading

Stack Overflow: [How to write/edit own coroutines in Prolog?](https://stackoverflow.com/questions/23450103/how-to-write-edit-own-coroutines-in-prolog/)

## What happens when `freeze/2` is called:

A goal is "attached" to an unbound variable `W`.

`freeze/2` performs this "attachment", but otherwise, behaves normally, i.e. it succeeds.

If `W` is not an unbound variable `freeze/2` time, `freeze/2` executes the goal immediately, i.e. behaves as `call/1` and succeeds or fails accordingly.

Call the goal immediately if the first argument is not an unbound variable (i.e. `nonvar(X)`):

```
?- freeze(true,format("thawed!\n")).
thawed!
true.

?- freeze(g(A),format("thawed!\n")).
thawed!
true.
```

Delay the goal only if the first argument is an unbound variable (i.e. `var(X)`):

 Here, the goal results in a (at least computational, but otherwise unreal) success with
 two "residual goals" / "delayed goals" printed out:
 
```
?- freeze(X,format("Hello,")),freeze(X,format("World")).
freeze(X, format("Hello,")),
freeze(X, format("World")).
```

Immediately after a unification involving `W`, the attached goal is "thawed" and is let to run. 

```
?- freeze(X,format("World")),format("Hello,"),X=1.
Hello,World
X = 1.
```

If the thawed goal has multiple solutions, Prolog backtracks over them:

```
?- freeze(X,(format("Hello,");format("World"))),X=1,format(" EOL ").
Hello, EOL 
X = 1 ;
World EOL 
X = 1.
```

With a cut:

```
?- freeze(X,(format("Hello,"),!;format("World"))),X=1,format(" EOL ").
Hello, EOL 
X = 1.
```

If there are multiple goals, Prolog runs them in order of attachment:

```
?- freeze(X,format("Hello,")),freeze(X,format("World")),X=1.
Hello,World
X = 1.
```

and they all must succeed for the unification to succeed:

```
?- freeze(X,format("Hello,")),freeze(X,false),freeze(X,format("World")),X=1.
Hello,
false.
```

If the unification keeps the variable unbound (but merges it with another), the goal is not executed yet:

```
?- freeze(X,format("Hello,")),freeze(X,format("World")),X=Y.
X = Y,
freeze(Y, format("Hello,")),
freeze(Y, format("World")).
```

```
?- freeze(X,format("Hello,")),freeze(Y,format("World")),X=Y.
X = Y,
freeze(Y, format("Hello,")),
freeze(Y, format("World")).
```

But:

```
?- freeze(X,format("Hello,")),freeze(Y,format("World")),X=Y,X=1.
Hello,World
X = Y, Y = 1.
```

The value of `W` can be used to select various actions of the attached goals:

```
?- freeze(X,format("Hello ~q are you really > 0?\n",[X])),freeze(X,(X>0)),X=1.
Hello 1 are you really > 0?
X = 1.

?- freeze(X,format("Hello ~q are you really > 0?\n",[X])),freeze(X,(X>0)),X= -1.
Hello -1 are you really > 0?
false.
```

The thawed goal is run by the thread that performs the unification (I don't see any other possibility).

If the system does not find the goal at "thaw time" (maybe because it has been removed from the database),
an exception is thrown in the thread that performs the unification.

If the thawed goal throws an exception then the exception is thrown  in the thread that performs the unification.

```
?- freeze(X,format("Hello ~q are you really > 0?\n",[X])),freeze(X,must_be(nonneg,X)),X= 1.
Hello 1 are you really > 0?
X = 1.

?- freeze(X,format("Hello ~q are you really > 0?\n",[X])),freeze(X,must_be(nonneg,X)),X= -1.
Hello -1 are you really > 0?
ERROR: Type error: `nonneg' expected, found `-1' (an integer)
ERROR: In:
ERROR:   [20] throw(error(type_error(nonneg,-1),_8552))
ERROR:   [16] unfreeze(user:must_be(nonneg,-1)) at swipl/lib/swipl/boot/attvar.pl:103
ERROR:   [13] call_all_attr_uhooks(att(freeze,'$and'(...,...),[]),-1) at swipl/lib/swipl/boot/attvar.pl:63
ERROR:   [12] '$wakeup'(wakeup(att(freeze,...,[]),-1,[])) at swipl/lib/swipl/boot/attvar.pl:58
ERROR:   [11] -1= -1 <foreign>
ERROR:    [9] toplevel_call(user:user: ...) at swipl/lib/swipl/boot/toplevel.pl:1113
```

If there are frozen goals when the program returns to toplevel (finally succeeds), 
they are printed out (which is why stray `dif/2` goals may appear on the toplevel). These goals are called **residual goals**.

Here, we find 3 residual goals:

```
?- freeze(X,format("Hello ")),freeze(X,false),freeze(X,format("World!")).
freeze(X, format("Hello ")),
freeze(X, false),
freeze(X, format("World!")).
```

`frozen(K,Conj)` outputs not the goals but a conjunction (i.e. a chain of `,(_,_)`) of the goals (wouldn't it be better to return a list?)

Below, `K` is printed and then the three residual goals are printed. Note that the goal predicates will be looked for in module `user` (which
would be useful if this were user-defined predicates).

```
?- freeze(X,format("Hello ")),freeze(X,false),freeze(X,format("World!")),frozen(X,K).
K =  (freeze(X, user:false), freeze(X, user:format("Hello ")), freeze(X, user:format("World!"))),

freeze(X, format("Hello ")),
freeze(X, false),
freeze(X, format("World!")).
```

Here, residual goals are left over in one of two solutions:

```
?- freeze(X,format("Hello")),(X=1;X=Y).
Hello
X = 1 ;
X = Y,
freeze(Y, format("Hello")).

?- freeze(X,format("Hello")),(X=Y;X=1).
X = Y,
freeze(Y, format("Hello")) ;
Hello
X = 1.
```

## But what happens if there are residual goals?

Suppose:

```
q(X) :- 
   freeze(X,X>0),
   format("After freeze/1").
```

Then:

`q(1)` succeeds

```
?- q(1).
After freeze/1
true.
```

`q(-1)` fails

```
q(-1).
false.
```

`q(X)` succeeds (computationally speaking) but the **truth value** should actually be "neither true nor false".
This can be seen because the "residual goal", which is the goal waiting for `X` to be bound, is printed out:

```
?- q(X).
After freeze/1
freeze(X, X>0).
```

Prolog fails in expressiveness here; this is not really "success".

One can check whether there is a residual "freeze" goal on `X` with `frozen/2`:

```
q(X) :- 
   freeze(X,X>0),
   format("After freeze/1\n").
   
freezy_q(X,Late) :- 
   q(X),                         
   (
      frozen(X,K)
      ->
      (
         (K == true) 
         -> 
         format("A clear success. No residual goal\n") 
         ;
         (format("Result ambivalent. Residual goal: ~q\n",[K]),
          assertion(var(X)),
          X=Late) % this may cause the residual goal to fail, failing the unification
      ) 
      ;
      format("Not frozen\n")
   ).
```

Then:

```
?- freezy_q(X,Late).
After freeze/1
Result ambivalent. Frozen goal: freeze(_12418,user:(_12418>0))
X = Late,
freeze(Late, Late>0).
```

```
?- freezy_q(1,Late).
After freeze/1
A clear success. No residual goal
true.
```

```
?- freezy_q(-1,Late).
false.
```

But we can succeed or fail late:

```
?- freezy_q(X,1).
After freeze/1
Result ambivalent. Residual goal: freeze(_12548,user:(_12548>0))
X = 1.
```

```
?- freezy_q(X,-1).
After freeze/1
Result ambivalent. Residual goal: freeze(_13674,user:(_13674>0))
false.
```

## A little example code: Using `freeze/2` to wait for signals

We have a bucket brigade implemented by 

- Goal `bucket_brigade(a ,V0,V1,Ws)` frozen on `V0`, when thawed transfers data from `V0 -> V1`
- Goal `bucket_brigade(b ,V1,V2,Ws)` frozen on `V1`, when thawed transfers data from `V1 -> V2`
- Goal `bucket_brigade(c ,V2,V3,Ws)` frozen on `V2`, when thawed transfers data from `V2 -> V3`
- Goal `bucket_brigade(d ,V3,V4,Ws)` frozen on `V3`, when thawed transfers data from `V3 -> VX`

```
:- debug(bb).

bb :-   
   Ws=[V0,V1,V2,V3],      
   freeze(V0,bucket_brigade(a,V0,V1,Ws)),
   freeze(V1,bucket_brigade(b,V1,V2,Ws)),
   freeze(V2,bucket_brigade(c,V2,V3,Ws)),
   freeze(V3,bucket_brigade(d,V3,VX,Ws)),   
   what_is_frozen(bb,Ws),   
   debug(bb,"wait for it...",[]),
   sleep(1),
   debug(bb,"thawing bucket_brigade(a)",[]),
   V0=[],
   debug(bb,"bucket arrived: ~q",[VX]).

what_is_frozen(_,[]) :- !.
what_is_frozen(Name,[W|Ws]) :-
   frozen(W,G),
   (G==true
    -> debug(bb,"~q : Nothing is frozen on: ~q",[Name,W])
    ;  debug(bb,"~q : A Goal is frozen on: ~q",[Name,W])),
   what_is_frozen(Name,Ws).

bucket_brigade(Name,Vin,Vout,Ws) :-
   debug(bb,"bucket_brigade(~q): Woke up with Vin = ~q!",[Name,Vin]),
   what_is_frozen(Name,Ws),
   Vout=[Name|Vin]. % thaws the next in line
```

The above outputs something along these lines:

```
?- bb.
% bb : A Goal is frozen on: _6138
% bb : A Goal is frozen on: _6168
% bb : A Goal is frozen on: _6198
% bb : A Goal is frozen on: _6228
% wait for it...
% thawing bucket_brigade(a)
% bucket_brigade(a): Woke up with Vin = []!
% a : Nothing is frozen on: []
% a : A Goal is frozen on: _6168
% a : A Goal is frozen on: _6198
% a : A Goal is frozen on: _6228
% bucket_brigade(b): Woke up with Vin = [a]!
% b : Nothing is frozen on: []
% b : Nothing is frozen on: [a]
% b : A Goal is frozen on: _6198
% b : A Goal is frozen on: _6228
% bucket_brigade(c): Woke up with Vin = [b,a]!
% c : Nothing is frozen on: []
% c : Nothing is frozen on: [a]
% c : Nothing is frozen on: [b,a]
% c : A Goal is frozen on: _6228
% bucket_brigade(d): Woke up with Vin = [c,b,a]!
% d : Nothing is frozen on: []
% d : Nothing is frozen on: [a]
% d : Nothing is frozen on: [b,a]
% d : Nothing is frozen on: [c,b,a]
% bucket arrived: [d,c,b,a]
```

If you change the `bucket_brigade4` predicate to do perform unification of `Vout` directly in the head, like this:

```
bucket_brigade(Name,Vin,[Name|Vin],Ws) :-
   debug(bb,"bucket_brigade(~q): Woke up with Vin = ~q!",[Name,Vin]),
   what_is_frozen(Name,Ws).
```

Then the following happens. All the frozen goals are thawed via the head unification and
the printout occurs in reverse (last goal thawed prints first):

``` 
?- bb.
% bb : A Goal is frozen on: _4580
% bb : A Goal is frozen on: _4610
% bb : A Goal is frozen on: _4640
% bb : A Goal is frozen on: _4670
% wait for it...
% thawing bucket_brigade(a)
% bucket_brigade(d): Woke up with Vin = [c,b,a]!
% d : Nothing is frozen on: []
% d : Nothing is frozen on: [a]
% d : Nothing is frozen on: [b,a]
% d : Nothing is frozen on: [c,b,a]
% bucket_brigade(c): Woke up with Vin = [b,a]!
% c : Nothing is frozen on: []
% c : Nothing is frozen on: [a]
% c : Nothing is frozen on: [b,a]
% c : Nothing is frozen on: [c,b,a]
% bucket_brigade(b): Woke up with Vin = [a]!
% b : Nothing is frozen on: []
% b : Nothing is frozen on: [a]
% b : Nothing is frozen on: [b,a]
% b : Nothing is frozen on: [c,b,a]
% bucket_brigade(a): Woke up with Vin = []!
% a : Nothing is frozen on: []
% a : Nothing is frozen on: [a]
% a : Nothing is frozen on: [b,a]
% a : Nothing is frozen on: [c,b,a]
% bucket arrived: [d,c,b,a]
true.
```    

