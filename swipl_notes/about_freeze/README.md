# About `freeze/2`

Some notes on [`freeze/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=freeze/2)

This is classed under "coroutining" but is that the right terminology? 

What we are doing is defining goals that are called when an unbound variable participates in an unification.
Depending on whether that goal then fails or succeeds, the corresponding unification fails or succeeds.

We are sticking additional unification checks onto unbound variables (which is what attributed variables are about).

A ["coroutine"](https://en.wikipedia.org/wiki/Coroutine) is much more general. Coroutines can be used to _implement_ the above.

## What happens when `freeze/2` is called:

A goal is "attached" to an unbound variable `W`.

`freeze/2` performs this "attachment", but otherwise, behaves normally, i.e. it succeeds.
If `W` is already bound at `freeze/2` time, `freeze/2` executes the goal immediately, i.e. behaves as `call/1` and succeeds or fails accordingly.

Immediately after a unification involving `W`, the attached goal is "thawed" and is let to run. 
If the thawed goal has multiple solutions, Prolog backtracks over them:

```
?- freeze(X,(format("Hello ");format("World!"))),X=1,format(" EOL ").
Hello  EOL 
X = 1 ;
World! EOL 
X = 1.
```

With a cut:

```
?- freeze(X,(format("Hello "),!;format("World!"))),X=1,format(" EOL ").
Hello  EOL 
X = 1.
```

If there are multiple goals, Prolog runs them in order of attachment:

```
?- freeze(X,format("Hello ")),freeze(X,format("World!")),X=1.
Hello World!
X = 1.
```

and they all must succeed for the unification to succeed:

```
?- freeze(X,format("Hello ")),freeze(X,false),freeze(X,format("World!")),X=1.
Hello 
false.
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

## Nice reading

https://stackoverflow.com/questions/23450103/how-to-write-edit-own-coroutines-in-prolog/

## A little example code

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
    ;  debug(bb,"~q : Goal ~q is frozen on: ~q",[Name,G,W])),
   what_is_frozen(Name,Ws).

bucket_brigade(Name,Vin,Vout,Ws) :-
   debug(bb,"bucket_brigade(~q): Woke up with Vin = ~q!",[Name,Vin]),
   what_is_frozen(Name,Ws),
   Vout=[Name|Vin]. % thaws the next in line
==

The above outputs something along these lines:

```
?- bb.

% bb : Goal freeze(_11204,user:bucket_brigade(a,_11204,_11234,[_11204,_11234,_11264,_11294])),freeze(_11234,user:bucket_brigade(b,_11234,_11264,[_11204,_11234,_11264,_11294])),freeze(_11264,user:bucket_brigade(c,_11264,_11294,[_11204,_11234,_11264,_11294])),freeze(_11294,user:bucket_brigade(d,_11294,_11282,[_11204,_11234,_11264,_11294])) is frozen on: _11204
% bb : Goal freeze(_11204,user:bucket_brigade(a,_11204,_11234,[_11204,_11234,_11264,_11294])),freeze(_11234,user:bucket_brigade(b,_11234,_11264,[_11204,_11234,_11264,_11294])),freeze(_11264,user:bucket_brigade(c,_11264,_11294,[_11204,_11234,_11264,_11294])),freeze(_11294,user:bucket_brigade(d,_11294,_11282,[_11204,_11234,_11264,_11294])) is frozen on: _11234
% bb : Goal freeze(_11204,user:bucket_brigade(a,_11204,_11234,[_11204,_11234,_11264,_11294])),freeze(_11234,user:bucket_brigade(b,_11234,_11264,[_11204,_11234,_11264,_11294])),freeze(_11264,user:bucket_brigade(c,_11264,_11294,[_11204,_11234,_11264,_11294])),freeze(_11294,user:bucket_brigade(d,_11294,_11282,[_11204,_11234,_11264,_11294])) is frozen on: _11264
% bb : Goal freeze(_11204,user:bucket_brigade(a,_11204,_11234,[_11204,_11234,_11264,_11294])),freeze(_11234,user:bucket_brigade(b,_11234,_11264,[_11204,_11234,_11264,_11294])),freeze(_11264,user:bucket_brigade(c,_11264,_11294,[_11204,_11234,_11264,_11294])),freeze(_11294,user:bucket_brigade(d,_11294,_11282,[_11204,_11234,_11264,_11294])) is frozen on: _11294
% wait for it...
% thawing bucket_brigade(a)
% bucket_brigade(a): Woke up with Vin = []!
% a : Nothing is frozen on: []
% a : Goal freeze(_11234,user:bucket_brigade(b,_11234,_11264,[[],_11234,_11264,_11294])),freeze(_11264,user:bucket_brigade(c,_11264,_11294,[[],_11234,_11264,_11294])),freeze(_11294,user:bucket_brigade(d,_11294,_11282,[[],_11234,_11264,_11294])) is frozen on: _11234
% a : Goal freeze(_11234,user:bucket_brigade(b,_11234,_11264,[[],_11234,_11264,_11294])),freeze(_11264,user:bucket_brigade(c,_11264,_11294,[[],_11234,_11264,_11294])),freeze(_11294,user:bucket_brigade(d,_11294,_11282,[[],_11234,_11264,_11294])) is frozen on: _11264
% a : Goal freeze(_11234,user:bucket_brigade(b,_11234,_11264,[[],_11234,_11264,_11294])),freeze(_11264,user:bucket_brigade(c,_11264,_11294,[[],_11234,_11264,_11294])),freeze(_11294,user:bucket_brigade(d,_11294,_11282,[[],_11234,_11264,_11294])) is frozen on: _11294
% bucket_brigade(b): Woke up with Vin = [a]!
% b : Nothing is frozen on: []
% b : Nothing is frozen on: [a]
% b : Goal freeze(_11264,user:bucket_brigade(c,_11264,_11294,[[],[a],_11264,_11294])),freeze(_11294,user:bucket_brigade(d,_11294,_11282,[[],[a],_11264,_11294])) is frozen on: _11264
% b : Goal freeze(_11264,user:bucket_brigade(c,_11264,_11294,[[],[a],_11264,_11294])),freeze(_11294,user:bucket_brigade(d,_11294,_11282,[[],[a],_11264,_11294])) is frozen on: _11294
% bucket_brigade(c): Woke up with Vin = [b,a]!
% c : Nothing is frozen on: []
% c : Nothing is frozen on: [a]
% c : Nothing is frozen on: [b,a]
% c : Goal freeze(_11294,user:bucket_brigade(d,_11294,_11282,[[],[a],[b,a],_11294])) is frozen on: _11294
% bucket_brigade(d): Woke up with Vin = [c,b,a]!
% d : Nothing is frozen on: []
% d : Nothing is frozen on: [a]
% d : Nothing is frozen on: [b,a]
% d : Nothing is frozen on: [c,b,a]
% bucket arrived: [d,c,b,a]
true.

==

## Historical note

From 

_Parallelism and Implementation Technology for Logic Programming Languages_ (~2001?)

by Vitor Santos Costa, p.7

One of the most serious criticisms of Prolog is that the selection function used by Prolog is too restrictive. From the beginning, authors such as Kowalski [a] remarked the effect on the size of the search space of solving different goals in different sequences. A more flexible execution than the one used by Prolog can be obtained through coroutining. Coroutines cooperatively produce and consume data, allowing for data-driven execution. Several designs for coroutining became very influential in the logic programming community. IC-Prolog, designed by Clark and others [b], was one of the first logic programming languages to support delaying of goals until data is available. Colmerauer’s Prolog-II [c] supported "geler", a built-in that would delay a goal until a variable would be instantiated. Naish’s MU-Prolog and NU-Prolog [d] supports "wait", a form of declaring that a goal should only
execute if certain arguments were instantiated. Features similar to "geler" and "wait" are now common in modern logic programming systems.

  - [a]: R. A. Kowalski. Logic for Problem Solving. Elsevier North-Holland Inc., 1979.
  - [b]: K. L. Clark, F. G. McCabe, and S. Gregory. IC-PROLOG – language features. In K. L. Clark and S. A. Tärnlund, editors, Logic Programming, pages 253–266. Academic Press, London, 1982.
  - [c]: A. Colmerauer. Prolog II: Reference Manual and Theoretical Model. Groupe d’Intelligence Artificielle, Faculté des Sciences de Luminy, Marseilles, October 1982. (It's on page 67 of the Prolog II manual here: http://prolog-heritage.org/fr/m2.html )
  - [d] L. Naish. Negation and Control in Prolog. Lecture notes in Computer Science 238. Springer Verlag, 1985.
