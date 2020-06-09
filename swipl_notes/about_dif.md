# About `dif/2`

A description of predicate `dif/2` can be found [here](https://eu.swi-prolog.org/pldoc/doc_for?object=dif/2), but it is a bit short!

Consider the call `dif(A,B)`.

A program that issues this call is in one of three states when this happens (it can start in any of the three states):

- **State A: "Unrefined"** - In this state, `A` and `B` would unify (i.e. `A=B` would succeed). But `A` and `B` are still 
  sufficiently unconstrained/unrefined that subsequent program operations can change that situation: `A=B`, if issued
  later, might succeed or fail. (The way to test `A=B` without retaining any changes to `A` nor `B` is the call `\+ \+ A=B`
  or `\+ A\=B`)
- **State B: "Sufficiently refined for dif=TRUE certainty"** - In this state, `A` and `B` won't unify, and no subsequent
  operation can change this. Once the program has switched to State B, it will stay there.
- **State C: "Sufficiently refined for dif=FALSE certainty"** - In this state, `A` and `B` will unify, and no subsequent
  operation can change this. In fact, `A` and `B` are identical. (They may be nonground, and in that case, any variables
  in the terms appears on the some positions and are shared.) Once the program has switched to State C, it will stay there.
  
The behavious of `dif/2` in all three states is:

- **State A**: `dif(A,B)` succeeds "optimistically". If we are still in State A on return to the toplevel, the "open" `dif/2` expression is printed out.
- **State B**: `dif(A,B)` succeeds deterministically.
- **State C**: `dif(A,B)` fails immediately.

The interesting case occurs if a `dif(A,B)` has been issued in State A and the program subsequently switches to State C.

In that case, an unwinding of the program progress back to the point where the `dif(A,B)` occured is performed (any side-effects stay,
of course, side-effected). The idea is to pretend nothing ever happened and to continue as if `dif(A,B)` had failed.

Let's try some examples:

First a predicate `check/2` which prints out information about the situation:

```logtalk
check(X,Y) :- 
   ((\+ \+ (X=Y))  -> writeln("they do unify!") ; true),      % \+ \+ ensures consequence-less unification attempt ("unify in pocket universe")
   ((\+ (X\=Y))    -> writeln("they don't unify!") ; true),   % X\=Y is already consequence-less, just reverse the truth value with \+
   ((X==Y)         -> writeln("they are identical") ; true),  % Term equivalence/identity (not unification, A and B are not refined by ==)
   ((X\==Y)        -> writeln("they are different") ; true).  % Term difference/non-identiy
```



```
?- A=f(x,X),B=f(Y,y),writeln("State A: Unrefined"),check(A,B),dif(A,B),writeln("end of clause").
unsure
end of clause
A = f(x, X),
B = f(Y, y),
dif(f(Y, X), f(x, y)).
```





