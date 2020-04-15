# Behaviour of the caret `^` in `setof/3` and `bagof/3` goal expressions.

- This is based on a lengthy note I made at Stack Overflow for [What is the Prolog operator `^` (“caret”)?](https://stackoverflow.com/questions/19931801/what-is-the-prolog-operator)
- See also ["existential qualifier in prolog, using setof / bagof"](https://stackoverflow.com/questions/1986751/existential-qualifier-in-prolog-using-setof-bagof).

**Upfront note**:

Personally, I consider the semantics of `^` to be a failure, because it _looks_ as if this
were an "existential quantification" and is even described as such sometimes (for 
example: [GNU Prolog](http://www.gprolog.org/manual/gprolog.html#sec114), 
[SWI-Prolog library(yall)](http://eu.swi-prolog.org/pldoc/man?section=yall)) 
but it actually is **NOT**.

It's better to **avoid this misfeature**, and write a separate predicate to 
be called by `setof/3` and `bagof/3` instead. And ISO Prolog-2, if it ever happens,
should really clean this up.

**We start with an overview ASCII Image:**

```text
                                 Clause-wide variable
                                         |
                                         |
                +------------------------+------------------------+
                |                                                 |
                |          Clause-wide variables                  |
                |          that are collected via the             |
                |          template at arg-position 1 by          |
                |          setof/3 (NOT local to setof/3)         |
                |          thus can be constrained elsewhere      |
                |          in the clause (possibly accidentally)  |
                |                           |                     |
                |                           |                     |                
                |                +-+--------+----------+-+        |
                |                | |                   | |        |
                |                | |                   | |        |            
get_closed_set(Set,K) :- setof( [X,Y] , P^R^search(P,R,X,Y,K) , Set).
                   |                    | |        | |     |
                   |                    <-------------------> Goal expression 
                   |                    | |        | |     |
                   |                    | |        | |     |                   
                   +---------------------------------------+-----+                    
                                        | |        | |           |
                                        | |        | |           |
                                        +-+----+---+-+          Clause-wide variable.
                                               |                Backtracking over this
                                               |                is done by the caller
                                               |                of get_closed_set/2.
                                               |
                                       Variables marked as "free for
                                       backtracking if fresh".
                                       This is NEARLY the same as "being
                                       local to the goal expression" or
                                       "being existentially quantified."
                                       Backtracking over these is done by setof/3.
                                       If these appear elsewhere in the clause,
                                       they be constrained (possibly accidentally)!
```

# Test Cases for Expected Behaviour

```logtalk
search(1,n,a,g).
search(2,m,a,g).

search(2,m,a,j).
search(1,m,a,j).
search(3,w,a,j).
search(3,v,a,j).

search(2,v,b,g).
search(3,m,b,g).
search(5,m,b,g).

search(2,w,b,h).


% ===
% CARET-USING EXPRESSIONS ("CLOSED EXPRESSIONS")
% ===

% If P and R do not appear anywhere else than in the goal expression.
% "P^R^" (not quite) closes off variables P,R: they are not (not quite) 
% invisible outside of the goal expression "P^R^search(P,R,X,Y)"

get_closed_set(Set) :- setof( [X,Y] , P^R^search(P,R,X,Y) , Set).
get_closed_bag(Bag) :- bagof( [X,Y] , P^R^search(P,R,X,Y) , Bag).

% The above is the same as this (which I recommend for clarity and
% to avoid annoying bug searches):

indirect_search(X,Y) :- search(_P,_R,X,Y).

get_closed_set_indirect(Set) :- setof( [X,Y] , indirect_search(X,Y) , Set).
get_closed_bag_indirect(Bag) :- bagof( [X,Y] , indirect_search(X,Y) , Bag).

% ===
% CARET-LESS EXPRESSIONS ("OPEN EXPRESSIONS")
% ===

get_open_set(Set,P,R) :- setof( [X,Y] , search(P,R,X,Y) , Set).
get_open_bag(Bag,P,R) :- bagof( [X,Y] , search(P,R,X,Y) , Bag).

% ===
% TESTING
% ===

:- begin_tests(caret).

test(clo_set)     :- get_closed_set(Set),
                     format("Closed Set:\n  ~q\n",[Set]),
                     Set = [[a,g],[a,j],[b,g],[b,h]].

test(clo_bag)     :- get_closed_bag(Bag),
                     format("Closed Bag:\n  ~q\n",[Bag]),
                     Bag = [[a,g],[a,g],[a,j],[a,j],[a,j],[a,j],[b,g],[b,g],[b,g],[b,h]].

test(clo_set_ind) :- get_closed_set_indirect(Set),
                     format("Closed Set, indirect:\n  ~q\n",[Set]),
                     Set = [[a,g],[a,j],[b,g],[b,h]].

test(clo_bag_ind) :- get_closed_bag_indirect(Bag),
                     format("Closed Bag, indirect:\n  ~q\n",[Bag]),
                     Bag = [[a,g],[a,g],[a,j],[a,j],[a,j],[a,j],[b,g],[b,g],[b,g],[b,h]].

test(opn_set)     :- bagof(solution(Set,P,R), get_open_set(Set,P,R), OuterBag),
                     format("Bag for get_open_set/3:\n  ~q\n",[OuterBag]).

test(opn_bag)     :- bagof(solution(Bag,P,R), get_open_bag(Bag,P,R), OuterBag),
                     format("Bag for get_open_bag/3:\n  ~q\n",[OuterBag]).

:- end_tests(caret).

rt :- run_tests(caret).
```

When we run `rt`, nothing unexpected occurs, we are like Fonzi with existential quantifiers:

```
Closed Set:            [[a,g],[a,j],[b,g],[b,h]]

Closed Bag:            [[a,g],[a,g],[a,j],[a,j],[a,j],[a,j], 
                        [b,g],[b,g],[b,g],[b,h]]

Closed Set, indirect:  [[a,g],[a,j],[b,g],[b,h]]

Closed Bag, indirect:  [[a,g],[a,g],[a,j],[a,j],[a,j],[a,j],
                        [b,g],[b,g],[b,g],[b,h]]

Bag for get_open_set/3:  [solution([[a,j]],1,m),solution([[a,g]],1,n),
                          solution([[a,g],[a,j]],2,m),solution([[b,g]],2,v),
                          solution([[b,h]],2,w),solution([[b,g]],3,m),
                          solution([[a,j]],3,v),solution([[a,j]],3,w),
                          solution([[b,g]],5,m)]

Bag for get_open_bag/3:  [solution([[a,j]],1,m),solution([[a,g]],1,n),
                          solution([[a,g],[a,j]],2,m),solution([[b,g]],2,v),
                          solution([[b,h]],2,w),solution([[b,g]],3,m),
                          solution([[a,j]],3,v),solution([[a,j]],3,w),
                          solution([[b,g]],5,m)]
```

# Trying out behaviour for less obvious expressions 

You may have to run this to see more list output (case of SWI-Prolog):

```
set_prolog_flag(answer_write_options,[max_depth(100)]).
set_prolog_flag(debugger_write_options,[max_depth(100)]).
```

## Singletons in goal expression

If you enter the following, Prolog correctly warns about "singleton variables P,R". Good.

```logtalk
get_open_set(Set) :- setof([X,Y],search(P,R,X,Y),Set).
```

## Carets outside of setof/3 or bagof/3

This is accepted and could be given a meaning, but Prolog will be looking for procedure `^/2` on call and say that _"^/2 can only appear as the 2nd argument of setof/3 and bagof/3"_. Okay.

```logtalk
get_outerly_closed_set(Set) :- P^R^setof([X,Y],search(P,R,X,Y),Set).
```

A possible meaning for the above might be the utterly mundane:

```logtalk
get_outerly_closed_set(Set) :- close_it_off(Set).
close_it_off(Set) :- setof([X,Y],search(_P,X,_R,Y),Set).
```

## Closed-off variable used elsewhere in clause: Problematic!

Now we are getting into "failure of semantics" territory: Prolog does not consider the outside `P` as a variable different from the `P` in `P^`. This is why `P^` does NOT mean _"∃P such that"_: 

```logtalk
get_closed_set_weird_1(Set,P) :- 
   setof( [X,Y] , P^R^search(P,R,X,Y) , Set),
   format("P=~q\n",[P]).
```

```
?- get_closed_set_weird_1(Set,P).
P=_14996
Set = [[a, g], [a, j], [b, g], [b, h]].

?- get_closed_set_weird_1(Set,1).
P=1
Set = [[a, g], [a, j]].
```

## Variation of closed-off variable used elsewhere in clause: Problematic!

No warnings occur if you write such a thing:

```logtalk
get_closed_set_weird_2(Set) :-
   setof( [X,Y,P], P^R^search(P,R,X,Y), Set).
```

```logtalk
?- get_closed_set_weird_2(Set).
Set = [[a, g, 1], [a, g, 2], [a, j, 1], [a, j, 2], [a, j, 3], ...
```

In fact, `P^` ends up being ignored. The above is the same as:

```logtalk
get_closed_set_weird_2e(Set) :-
   setof( [X,Y,P], R^search(P,R,X,Y), Set).
```

## Free variable over which to range used elsewhere in clause: Problematic!

This is entirely expected behaviour, but a casual reading of `setof([X,Y], ...` would lead one to think that
`[X,Y]` are free variables over which `setof/3` ranges. This is not the case: `[X,Y]` is just a 
template and `X` and `Y` are actually clause-wide variables, which can be constrained elsewhere:

```  
get_closed_set_weird_2(Set,X) :- 
   setof( [X,Y], P^R^search(P,R,X,Y) , Set),
   format("X=~q\n",[X]).
```

```
?- get_closed_set_weird_2(Set,X).
X=_20346
Set = [[a, g], [a, j], [b, g], [b, h]].

?- get_closed_set_weird_2(Set,b).
X=b
Set = [[b, g], [b, h]].
```

The above would have been more clear as

```
get_closed_set_weird_2c(Set,V) :- 
   setof( [V,Y], close_it_off(V,Y), Set),
   format("V=~q\n",[V]).
close_it_off(X,Y) :- search(_P,_R,X,Y).
```

```
?- get_closed_set_weird_2c(Set,V).
V=_21682
Set = [[a, g], [a, j], [b, g], [b, h]].
```

but note that this is absolutely not the same as this, where we backtrack over `V` outside of `setof/3`:

```
get_closed_set_weird_2x(Set,V) :- 
   setof( [X,Y], close_it_off(V,X,Y), Set),
   format("V=~q\n",[V]).
close_it_off(V,X,Y) :- V=X,search(_P,_R,X,Y).
```

```
?- get_closed_set_weird_2x(Set,V).
V=a
Set = [[a, g], [a, j]], V = a ;
V=b
Set = [[b, g], [b, h]], V = b.
```

# There should be acceptable notation

One would like to have a clean way of indicating which variables of the goal expression are visible outside of the goal expression, which ones are not, and which ones to range over. 

How about this:

- If there is a `λX.` at the head of the goal expression, the `X` is visible outside the goal expression. Any `X` elsewhere in the clause is the same `X`.
- If there is a `∃X.` at the head of the goal expression, the `X` is invisible outside the goal expression. Any `X` elsewhere in the clause is a different `X` (you are then invited to proceed to rename by the editor).
- Any `X` that appears in the goal expression without a preceding `λX.` or a `∃X.` is a **compiler error**. 
- You can put anything you want into the template, lambda-ed, or existentialized, or clause-global.
- The called goal ranges over any variables that it sees as fresh: either fresh ones appearing as `λX.` and any variables appearing as `∃X.` 

_(Don't complain about the lowercase `x` above; it just looks that way. `λX. ∃X. Xx`)_
