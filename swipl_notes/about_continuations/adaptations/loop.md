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
                  ; ... code to execute before each action would go here ...
                  (action-function)
                  ; ... code to execute after each action would go here ...
                  (loop)))))
         (loop))))

; the above seems convoluted; one may write it simpler (employing a "named let") as:

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
(define loop-until
   (lambda (n)
      (let
         ((receiver
            (lambda (exit-function)  ; exit-function will be the "current continuation" at call
               (let ((count 0))            ; count will be mutated using "set!"
                  (infinitizer-2
                     (lambda ()                 ; this is the action-function
                        (if (= count n)              ; breakoff criterium
                           (exit-function count)         ; call received continuation with exit value
                           (begin
                              (display "The count is: ")
                              (display count)
                              (newline)
                              ; communicate with the next action-function
                              ; instance by POKE-ing count
                              (set! count (+ count 1)
                              ))))))))) ; end of receiver definition
         (call/cc receiver)))) ; call with the above receiver
```

Try it on repl.it: https://repl.it/@dtonhofer/ContinuationBasedLoopBreakout

```
(loop-until 4)
The count is: 0
The count is: 1
The count is: 2
The count is: 3
=> 4
```

## In Prolog:

### With a global variable

First create the "service predicate" which creates an an infinite loop of calls of `Goal`.
We don't add a `!` anywhere because `Goal` may have several solutions (though here it
does not) - it is thus possible that the stack will be filled with leftover activations
because the choicepoints cannot be pruned.

```erlang
infinitizer(Goal) :-
   call(Goal),
   infinitizer(Goal).
```

Then write the predicate which makes use of `infinitizer/1` to create a loop, which
is is infinite but from which one can get out with a call to `shift/1`.

The communication protocol to be followed by an "in-loop action" is:

- _Call `shift(escape_from_loop(Arg))` to escape the loop_

There is a _hidden_ part of the communication protocol which allows the 
"in-loop action" to determine whether it should stop:

- The "global variables" called `count` which is the current invocation count.
  
The global variable is read with
[`nb_getval/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=nb_getval/2) and set
with [`nb_setval/2`](https://eu.swi-prolog.org/pldoc/doc_for?object=nb_setval/2).

```erlang
loop_until(N) :-
   nb_setval(count,0),
   reset(
      infinitizer(in_loop_action(N)),   
      escape_from_loop(FinalN),
      _),
   format("The final count is (unsurprisingly) equal to ~d: ~d\n",[N,FinalN]).
```

The "in-loop action" predicate is called inside the `infinitizer` loop, can check
its stop criterium and can escape the loop.

The actual `in-loop-action` has been moved out (code-wide) because Prolog forces us
to write all predicates at the toplevel. There are no nested predicates nor are
there closures. This is generally not a hindrance, and is not in this case. 

```erlang
in_loop_action(N) :-
   nb_getval(count,Count),
   ((N == Count) 
    -> shift(escape_from_loop(Count))
    ;  true),   
   format("The count is ~d\n",[Count]),
   NewCount is Count+1,
   nb_setval(count,NewCount).
```

The above works perfectly well:

```
?- loop_until(3).
The count is 0
The count is 1
The count is 2
The final count is (unsurprisingly) equal to 3: 3
true.
```

## Without a global variable

Can we keep the `Count` variable in a local context? 

An idea: We can manage it through an "effect handler" pattern: the uppermost
activation could handle "read" and "write" commands received through `shift/1`. 
Let's try that! (Success is not guaranteed.)





   

