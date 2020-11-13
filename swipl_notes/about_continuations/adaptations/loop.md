### Loop

In Scheme, as given in
[Call with current continuation patterns](https://www.researchgate.net/publication/228576802_Call_with_current_continuation_patterns), 
a bit modified:

The "infinitizer" which slaps an infinite loop around its (function) argument:

```scheme
; "infinitizer" runs "action-function" infinitely often
; inside a (tail-recursive) loop

(define infinitizer
   (lambda (action-function)
      ; "loop" is a closure taking no parameters which calls
      ; "action-procedure" and then calls "loop".
      ; After defining it, just call "loop".
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

The function to be called at the toplevel as `(loop-until 4)` for example 
(here hardcoded to use `infinitizer-2`). It grabs the continuation active
ar start, then uses `infinitizer-2` in an inversion-of-control mode, so
that `infinitizer-2` calls a defined action-function potentially infinitely
often. However, the continuation helps the action-function to escape from
the loop.

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

In Prolog:

