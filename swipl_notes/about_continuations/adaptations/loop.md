# An infinite loop, from which on breaks out using `shift/1`

## In Scheme

In Scheme, as given in
[Call with current continuation patterns](https://www.researchgate.net/publication/228576802_Call_with_current_continuation_patterns), 
a bit modified:

```scheme
; "infinitizer" runs "action-function" infinitely often inside a (tail-recursive) loop

(define infinitizer
   (lambda (action-function)
      ; "loop" is a closure taking no parameters which calls "action-procedure" and 
      ; then calls "loop". After defining it, just call "loop".
      (letrec
         ((loop
            (lambda ()
               (begin
                  ;;; ... code to execute before each action would go here ...
                  (action-function)
                  ;;; ... code to execute after each action would go here ...
                  (loop)))))
         (loop))))
```

Rhe above seems convoluted; one may write it simpler (employing a "named let") as:

```scheme
(define (infinitizer-2 action-function)
   (let loop()
      ;;; ... code to execute before each action would go here ...
      (action-function)
      ;;; ... code to execute after each action would go here ...
      (loop)))
```

The function to be called at the toplevel, for example: `(loop-until 4)`.

It is hardcoded to use the `infinitizer-2` higher-order function. 

- It grabs the continuation active before the loop starts, then
- Uses `infinitizer-2` in an inversion-of-control mode, so that
- `infinitizer-2` calls a (here pre-coded) action-function potentially infinitely often.
- However, the continuation helps the action-function to escape from the loop when a stop criterium has been met.

```scheme
(define (loop-until max)
      (let
         ((receiver
            (lambda (exit-function)  ; exit-function will be the "current continuation" at call
               (let ((cur 0))            ; cur will be mutated using "set!"
                  (infinitizer-2
                     (lambda ()                 ; this is the action-function
                        (if (= cur max)              ; breakoff criterium
                           (exit-function cur)         ; call received continuation with exit value
                           (begin
                              (display "The current count is: ")
                              (display cur)
                              (newline)
                              ; communicate with the next action-function instance by POKE-ing
                              ; "cur". "cur" is not a global variable but local to the
                              ; "receiver" function 
                              (set! cur (+ cur 1)
                              ))))))))) ; end of receiver definition
         (call/cc receiver))) ; call with the above receiver
```

Try it on repl.it: https://repl.it/@dtonhofer/ContinuationBasedLoopBreakout

```
(loop-until 4)
The current count is: 0
The current count is: 1
The current count is: 2
The current count is: 3
=> 4
```

## In Prolog:

### With a global variable

First create the "service predicate" which creates an an infinite loop of calls of `Goal`.
We don't add a `!` anywhere because `Goal` may have several solutions (though here it
does not) - it is thus possible that the stack will be filled with leftover activations
because the choicepoints cannot be pruned.

```none
infinitizer(Goal) :-
   call(Goal),
   infinitizer(Goal).
```

Then write the predicate which makes use of `infinitizer/1` to create a loop, which
is is (evidently) infinite but from which one can get out with a call to `shift/1`.

The communication protocol to be followed by an "in-loop action" is:

- _Call `shift(escape(_))` to escape the loop_

There is another part of the communication protocol which allows the 
"in-loop action" to determine whether it should stop:

- There is a "global variable" called `cur` which is the current counter value.
  
The global variable is read with
[`nb_getval/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=nb_getval/2) and set
with [`nb_setval/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=nb_setval/2).

```none
loop_until(Max) :-
   nb_setval(cur,0),
   reset(
      infinitizer(in_loop_action(Max)),   % The goal underneath the "reset" point
      escape(FinalCur),                   % The "Ball" term matching the "escape command"
      _),                                 % The continuation to get back into the loop
   format("The final count is: ~d\n",[FinalCur]).
```

The `in_loop_action` predicate is called inside the `infinitizer` loop. It can check
its stop criterium and can escape the loop.

The actual `in-loop-action` has been moved "out" (code-wise) because Prolog forces us
to write all predicates at the toplevel. There are no nested predicates nor are
there (real) closures. This is generally not a hindrance, and is not in this case. 

```none
in_loop_action(Max) :-
   nb_getval(cur,Cur),
   ((Max == Cur) 
    -> shift(escape(Cur))
    ;  true),   
   format("The count is ~d\n",[Cur]),
   NewCur is Cur+1,
   nb_setval(cur,NewCur).
```

The above works perfectly well:

```none
?- loop_until(3).
The count is 0
The count is 1
The count is 2
The final count is: 3
true.
```

Let's recapitulate all in a single code block

```none
% What the user calls

loop_until(Max) :-
   nb_setval(cur,0),
   reset(
      infinitizer(in_loop_action(Max)),   % The goal underneath the "reset" point
      escape(FinalCur),                   % The "Ball" term matching the "escape command"
      _),                                 % The continuation to get back into the loop
   format("The final count is: ~d\n",[FinalCur]).

% The infinite loop maker

infinitizer(Goal) :-
   call(Goal),
   infinitizer(Goal).
   
% The action function called from the infinite loop
% N.B.: Not a loop, just a straight conjunction.

in_loop_action(Max) :-
   nb_getval(cur,Cur),
   ((Max == Cur) 
    -> shift(escape(Cur))
    ;  true),   
   format("The count is ~d\n",[Cur]),
   NewCur is Cur+1,
   nb_setval(cur,NewCur).
```

## Without a global variable

Can we keep the `Cur` variable in a local context, which would be the context of `loop_until/1`? 

Yes: we can manage it through an "effect handler" pattern: `loop_until/1`
can handle "get and "set" commands for `Cur` received through `shift/1`, but then it needs to
loop itself (i.e. perform tail-recursive calls) to repeatedly call `reset/3` so as to get
back to into the `infinitizer(in_loop_action(Max))` context. In that way we get the correct 
pairwise reset-shift sequence.

After some experimentation: we get the code below. It becomes a bit larger due to the
various clauses which handle shifter commands:

```
% What you call

loop_until(Max) :-
   loop_until_2(
      0,                                  % Start counting to Max at 0
      infinitizer(in_loop_action(Max))).  % and pass an initial goal

% Helper predicate

loop_until_2(Cur,Goal) :-                 % "Cur", the current counter value (it could actually be some random state description),
                                          % is held in the loop_until_2 activation context (instead of in a global variable).
   reset(
      Goal,                               % This is the infinitizer on first loop passage, a continuation on subsequent ones.
      Cmd,                                % A generic (catch-all) catcher term, it will contain a "command".
      Cont),                              % The continuation to get back into the in_loop_action/1.
   assertion(nonvar(Cont)),               % To be clear: "Cont" is never unbound.
   assertion((Cont==0;                    % Our protocol also demands that if there was a "shift(Cmd)" (Cont\==0),
           nonvar(Cmd))),                 % ....then "Cmd" is (at least partially) bound: something like "foo(X)".
                                          % This allows us to match "Cmd" in the head of branch/5.
   branch(Cmd,Cont,Cur,NewCur,What),      % Determine a NewCur from the "Cmd" and "Cur" and decide whether to loop
   ((What == loop)
      -> loop_until_2(NewCur,Cont)        % Loop around, creating a new context with a new "Cur" and a new "Cont" to use in reset/3.
       ; true).

% Continuation is 0: The goal under the reset point succeeded.
% Note that this is not the in_loop_action/1 (which succeeds regularly inside
% the infinitizer loop) but the original goal infinitizer(in_loop_action(Max).

branch(_,0,_,_,stop) :-
   !,
   format("The goal underneath reset succeeded all by itself. This is unexpected!").

% Continuation is nonzero: We received a command.

% "Escape from loop" ordered.
% This basically means dump the continuation, no more reset/3 calls:

branch(escape(FinalCur),Cont,_,FinalCur,stop) :-
   assertion(Cont \== 0),
   format("The final count is ~d\n",[FinalCur]).

% "get Cur" ordered. "Cur" is communicated to the shift point by unifying the
% variable "X" in "get(X)" with "Cur". This is made explicit below.

branch(get(X),Cont,Cur,Cur,loop) :-
   assertion(Cont \== 0),
   assertion(var(X)),
   X = Cur.

% "set Cur" ordered. The "Cur" received via becomes the "NewCur".
% This is made explicit below.

branch(set(X),Cont,_,NewCur,loop) :-
   assertion(Cont \== 0),
   assertion(nonvar(X)),
   X = NewCur.

% The infinite loop maker

infinitizer(Goal) :-
   call(Goal),
   infinitizer(Goal).

% The action function called from the infinite loop
% N.B.: It's not loop, it just calls shift/1 several times.

in_loop_action(Max) :-
   shift(get(Cur)),
   ((Max == Cur)               % The stop criterium
    -> shift(escape(Cur))      % Jumping "up" to the reset point
    ;  true),
   format("The count is ~d\n",[Cur]),
   NewCur is Cur+1,
   shift(set(NewCur)),
   format("Back at where the count was ~d\n",[Cur]).
```

Does it work?

```
?- loop_until(3).
The count is 0
Back at where the count was 0
The count is 1
Back at where the count was 1
The count is 2
Back at where the count was 2
The final count is 3
true.
```








   

