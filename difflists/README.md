# The Difference List

- ["Difference List" wiki entry](https://swi-prolog.discourse.group/t/difference-list/959) at Prolog Discourse site.
- ["Difference Lists" by Frank Pfenning](https://www.cs.cmu.edu/~fp/courses/lp/lectures/11-diff.pdf) (PDF)
- [Applying "Difference Lists" to DCGs by Markus Triska](https://www.metalevel.at/prolog/dcg). "Difference Lists" are called "List Differences" here: _In the literature, you will also encounter the term "difference list". However, this terminology is misleading: We are not talking about—as the name may suggest—a special kind of list. The additional arguments are completely ordinary lists. It is their differences that matter especially in such cases._
- [Presentation on Difference Lists](https://www.cl.cam.ac.uk/teaching/0809/Prolog/Prolog08ML5R2.pdf) by David Eyers at University of Cambridge, for the [Prolog Course](https://www.cl.cam.ac.uk/teaching/0809/Prolog/).

"Difflist as a queue" is actually very illustrative of the "list difference" idea:

- Items are appended at the front of the tail, the variable of the tail "wanders rightwards"
- Items are dropped off the front of the head, the variable of the head "wanders rightwards"
- What is actually in the difflist is alwasy the list difference ... what is between tail var and head var.      

## Naming and Graphing

First take a look at this page, which defines the symbols and the vocabulary used below: [Naming and Symbols](../naming_and_symbols/README.md)

## Special cases

- The empty difflist, same as the initial difflist. Both head and tail variables name the same fresh variable.

## Constructing a list by appending to it via a Difference List

In Prolog, it is cheap to prepend an item to a list (also called "pushing an item onto a list" if it is regarded
as a stack.  

However, in order to append efficiently, you need the difference list pattern. It is a "pattern" because
there is real data structure corresponding to a "difference list". Just a set of conventions.

Consider this program, which exercises the difference list pattern, in short form and extensive form complete with
debugging output:

### (Very) Short Form

```Logtalk
dl_do(Data,Result) :- 
   dl_append_all(Data,Result-Result).

dl_append_all([X|Xs],DL) :- 
   dl_append(X,DL,DLlonger),
   dl_append_all(Xs,DLlonger).
   
% close the difflist

dl_append_all([],_-[]). 

% append a single item to the difflist

dl_append(X,Tip-[X|NewFin],Tip-NewFin). 

% ===
% Tests
% ===

:- begin_tests(difflist).

test(one, true(R=[1,2,3])) :- dl_do([1,2,3],R).
test(two, true(R=[]))      :- dl_do([],R).
   
:- end_tests(difflist).

rt :- run_tests(difflist).
```

### Extensive Form, with Debug Output

#### Debugging output predicates

```logtalk
% ---
% Printing a difflist via debug/3.
% We use ∅ to express a "fresh term".
% ---

express_dl(Depth,Msg,Tip-Fin) :- 
   ,integer(Depth)
   ,express2_dl(Tip,Fin,Str)
   ,debug(dl,"~d: ~s: ~s",[Depth,Msg,Str])
   .

% ---
% Express the various cases of Tip-Fin combinations
% ---

express2_dl(Tip,Fin,Str) :-
   ,Tip =@= Fin  % "structurally equal"
   ,var(Fin)     % and this must also be true
   ,!           
   ,Str="Empty difflist: ∅"
   .
   
express2_dl(Tip,Fin,Str) :-
   ,Tip \=@= Fin % "not structurally equal"
   ,var(Fin)     % and this must also be true
   ,!
   ,express_dl_items(Tip,Fin,MoreStr)
   ,with_output_to(string(Str),format("Nonempty difflist: [~s]",[MoreStr]))
   .
   
express2_dl(Tip,Fin,Str) :-
   ,is_list(Tip)
   ,is_list(Fin)
   ,append(_Prefix,Fin,Tip)
   ,!
   ,with_output_to(string(Str),format("Closed difflist: ~q-~q",[Tip,Fin]))
   .

express2_dl(Tip,Fin,Str) :-   
   ,with_output_to(string(Str)
   ,format("Unknown stuff: ~q-~q",[Tip,Fin]))
   .
   
% ---
% Express the content of a difflist
% ---

express_dl_items([X|Xs],Fin,Str) :-
   ,Xs \=@= Fin
   ,!
   ,express_dl_items(Xs,Fin,MoreStr)
   ,with_output_to(string(Str),format("~q,~s",[X,MoreStr]))
   .
   
express_dl_items([X|Xs],Fin,Str) :-
   ,Xs =@= Fin
   ,!
   ,with_output_to(string(Str),format("~q|∅",[X]))
   .
```

#### Worker predicates

```logtalk
% ---
% Main entry point. Note that the parameter "Dp" (Depth), used 
% only in debugging is on last place in dl_append_all/3. If it is
% on first place, the compiler makes the program semideterministic.
% ---

dl_do(Data,Result) :- 
   ,DL = I-I                  % Construct an initial difflist based on a single "fresh term".
                              % The I on the left of "-" is the "open Tip", the other I the "open Fin".
   ,dl_append_all(Data,DL,0)  % When this returns, "I" has been constrained as follows: all "Data" 
                              % has been appended and the difflist has been "closed". I is a real List.
                              % On the other hand, DL is no longer a difflist at all! The quality
   ,Result = I                % of "difflisty-ness" is quite fleeting! 
   .

% ---
% Recurse over input list, appending to difflist
% ---

dl_append_all([X|Xs],DL,Dp) :- 
   ,express_dl(Dp,"DL before dl_append", DL)
   ,dl_append(X,DL,DLlonger)
   ,express_dl(Dp,"DL after dl_append", DL)
   ,succ(Dp,Dpp)
   ,dl_append_all(Xs,DLlonger,Dpp)
   ,express_dl(Dp,"DL after dl_append_all", DL)
   .
   
dl_append_all([],DL,Dp) :-
   ,express_dl(Dp,"DL before closing", DL)
   ,dl_close(DL)
   ,express_dl(Dp,"DL after closing", DL)
   .

% ---
% "Close" the difflist and create a real list. This is simply done by
% Disassembling DL into "open Tip" and "open Fin", then constraining 
% the "open Fin" to be []. Can be simplified to: "dl_close(Tip-[])."
% ---

dl_close(DL) :- 
   ,DL = _Tip-Fin
   ,Fin=[]
   .         

% ---
% Append a single item.
% ---

dl_append(X,DL,DLlonger) :-
   ,DL       = Tip-Fin           % Disassemble DL into "open Tip" and "open Fin"
   ,Fin      = [X|NewFin]        % Constrain the "open Fin" to be the item with a new "open Fin"
   ,DLlonger = Tip-NewFin        % Longer difflist has same "open Tip" and the new "open Fin"
   .
```

#### Unit tests

```logtalk
:- begin_tests(difflist).

test(one, true(R=[1,2,3])) :- dl_do([1,2,3],R).
test(two, true(R=[]))      :- dl_do([],R).
   
:- end_tests(difflist).

rt :- debug(dl),run_tests(difflist).
```

## Graphing the data structure

**THIS IS STILL CRAP**

### Construct initial difference list in `do/2`

```
DL = F-F
```

![Initial construction](pics/01A.png)

Results in

![Initial construction](pics/01B.png)

### Append first item inside `append_to_difflist/3`

```
DLin = H-T
```

![Initial construction](pics/02A.png)

Results in

![Initial construction](pics/02A2.png)

```
T=[Item|NewT]
```

![Initial construction](pics/02B.png)

Results in

![Initial construction](pics/02B2.png)

After unification, the unconstrained-tail-list rooted at `H` has become longer by `Item`
(in effect, the list has been constrained some more -- you have uncovered new info
about the list -- ... but it still has an unconstrained tail).

```
DLout=H-NewT
```

Construct new difflist according to our `H-T` convention.
`DLout` combines the unconstrained-tail-list rooted at `H` and the new unconstrained tail `NewT`.

![Initial construction](pics/02B3.png)



-------------

### Append second item inside `append_to_difflist/3`

Just about to unify `T` and `[Item|NewT]`.

![Just about to unify](03A.png)

After unification, the unconstrained-tail-list rooted at `H` has become longer by `Item` (in effect, the list has been constrained
some more ... but it still has an unconstrained tail).

![After unification](03B.png)

Construct new difflist according to our `H-T` convention.
`DLout` combines the unconstrained-tail-list rooted at `H` and the new unconstrained tail `NewT`.

![Construct new difflist](03C.png)

### Redux: Append third item inside `append_to_difflist/3`

Just about to unify `T` and `[Item|NewT]`.

![Just about to unify](04A.png)

After unification, the unconstrained-tail-list rooted at `H` has become longer by `Item` (in effect, the list has been constrained
some more ... but it still has an unconstrained tail).

![After unification](04B.png)

Construct new difflist according to our `H-T` convention.
`DLout` combines the unconstrained-tail-list rooted at `H` and the new unconstrained tail `NewT`.

![Construct new difflist](04C.png)

### Close the difflist inside `close_difflist/2`

To close the list and create a "real list", unify the "ion" with `[]`.

![Just about to close the list](05A.png)

This leaves us with a "real list", correctly terminated and all.

![Real list](05B.png)

Finally get rid of the first dummy element by deconstructing the list reachable by `H`.

![Drop first element](05C.png)

This leaves us just with the correctly constructed list in `Result`, which is what we want.

![All done](05D.png)







