# An infinite loop, from which on breaks out using `shift/1`

## In Scheme

In Scheme, as given in
[Call with current continuation patterns](https://www.researchgate.net/publication/228576802_Call_with_current_continuation_patterns), 
a bit modified:

The "infinitizer" service function which slaps an infinite loop around its (function) argument:

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

; the above seems convoluted; one may write it simpler as:

(define infinitizer-2
   (lambda (action-function)
      (begin
         ;;; ... code to execute before each action would go here ...
         (action-function)
         ;;; ... code to execute after each action would go here ...
         (infinitizer-2 action-function))))
```

The function to be called at the toplevel, for example: `(loop-until 4)`.

It is hardcoded to use the `infinitizer-2` service function. 

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

## In Prolog:

The "service predicate" which creates an an infinite loop of calls to `Goal`.
We don't add a "!" anywhere because `Goal` may have several solutions (though here it does not).
This may thus fill the stack with leftover activations.

```erlang
infinitizer(Goal) :-
   call(Goal),
   infinitizer(Goal).
```

The predicate making use of `infinitizer/1` to create a potentially infinite loop. But,
given the following communication protocol to be followed by `in_loop_action`:

- Call `shift(escape_from_loop(Arg))` to escape the loop

we should be fine!

There is a _hidden_ part of the communication protocol: 

- There is a variable in the "global variables" namespace called `count` which is your current invocation count.

```erlang
loop_until(N) :-
   nb_setval(count,0),
   reset(
      infinitizer(in_loop_action(N)),   
      escape_from_loop(FinalN),
      _),
   format("The final count is (unsurprisingly) equal to ~d: ~d\n",[N,FinalN]).
```

The "action predicate" called inside the `infinitizer` loop. The actual `in-loop-action`
has been broken out because Prolog forces us to write all predicates at the toplevel. 
No nested predicates (and thus no "real closures" because there can be no particular
context of a predicate). This is generally not a hindrance, and is not in this case.
We get the current `Count` not from the predicate context as in Scheme, but from the 
"global variables" namespace. 

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


   

