# Various Prolog implementations to compute numbers from the Fibonacci Series 

More on this series: [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number)

The idea for the lazy list comes the Prolog part for "Fibonacci sequence" at [Rosettacode.org](http://rosettacode.org/wiki/Fibonacci_sequence#Prolog) (that part needs a good cleanup on 2020-10-20. Plus at least 2 algorithms I don't understand).

Here is compressed explainer in SVG (you can zoom in!), showing how the algorithms can be organized.

![Approaches](Fibonacci.svg)

Here is the complete source code (nothing has been copied from other sources, and just grab what you need).

![fibonacci_various.pl](fibonacci_various.pl)

The solution that caches `(x,fib(x))` using [`assertz/1`](https://eu.swi-prolog.org/pldoc/doc_for?object=assertz/1) (as found at [Rosettacode.org](http://rosettacode.org/wiki/Fibonacci_sequence#Prolog) or [This Stack Overflow question](https://stackoverflow.com/questions/16358747/why-this-dynamic-version-of-fibonacci-program-is-incredibly-faster-then-this-oth)) is not in the set of approaches because I feel doing things that way should be avoided.

The above has been run on SWI Prolog 8.3.9 (some SWI-Prolog specificities are in there).

When running the tests, one obtains:

```
?- run_tests.
% PL-Unit: fib ........
% ######## Running naive_tabled                                                                                                                                                                                                              
% ...to compute fib(2000)
% 39,008 inferences, 0.022 CPU in 0.022 seconds (98% CPU, 1802498 Lips)
% ...to compute fib(3000)
% 39,007 inferences, 0.027 CPU in 0.028 seconds (98% CPU, 1437484 Lips)
% ...to compute fib(4000)
% 39,007 inferences, 0.025 CPU in 0.025 seconds (98% CPU, 1574523 Lips)
% ######## Running topdown_cache_using_list_ascending
% ...to compute fib(2000)
% 1,343,340 inferences, 0.161 CPU in 0.163 seconds (99% CPU, 8362007 Lips)
% ...to compute fib(3000)
% 3,015,006 inferences, 0.349 CPU in 0.354 seconds (99% CPU, 8628714 Lips)
% ...to compute fib(4000)
% 5,353,340 inferences, 0.608 CPU in 0.611 seconds (99% CPU, 8811788 Lips)
% ######## Running topdown_cache_using_list_descending
% ...to compute fib(2000)
% 8,002 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 3373405 Lips)
% ...to compute fib(3000)
% 12,002 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 3960528 Lips)
% ...to compute fib(4000)
% 16,002 inferences, 0.005 CPU in 0.005 seconds (100% CPU, 2921220 Lips)
% ######## Running topdown_cache_with_clpfd_constraints
% ...to compute fib(2000)
% 8,002 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 2842368 Lips)
% ...to compute fib(3000)
% 12,002 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 4008707 Lips)
% ...to compute fib(4000)
% 16,002 inferences, 0.005 CPU in 0.005 seconds (100% CPU, 2935547 Lips)
% ######## Running topdown_cache_using_dict
% ...to compute fib(2000)
% 37,988 inferences, 0.060 CPU in 0.060 seconds (99% CPU, 637886 Lips)
% ...to compute fib(3000)
% 56,988 inferences, 0.168 CPU in 0.168 seconds (100% CPU, 339982 Lips)
% ...to compute fib(4000)
% 75,988 inferences, 0.256 CPU in 0.257 seconds (100% CPU, 296831 Lips)
% ######## Running bottomup_direct
% ...to compute fib(2000)
% 6,003 inferences, 0.005 CPU in 0.005 seconds (100% CPU, 1320647 Lips)
% ...to compute fib(3000)
% 9,003 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 4735211 Lips)
% ...to compute fib(4000)
% 12,003 inferences, 0.003 CPU in 0.003 seconds (99% CPU, 4648047 Lips)
% ######## Running bottomup_cache_using_lazy_list
% ...to compute fib(2000)
% 10,347 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 4861037 Lips)
% ...to compute fib(3000)
% 15,518 inferences, 0.005 CPU in 0.005 seconds (100% CPU, 3104742 Lips)
% ...to compute fib(4000)
% 20,676 inferences, 0.005 CPU in 0.005 seconds (100% CPU, 4541925 Lips)
% ######## Running bottomup_cache_using_dict
% ...to compute fib(2000)
% 15,999 inferences, 0.105 CPU in 0.105 seconds (100% CPU, 153011 Lips)
% ...to compute fib(3000)
% 23,999 inferences, 0.148 CPU in 0.149 seconds (100% CPU, 161694 Lips)
% ...to compute fib(4000)
% 31,999 inferences, 0.193 CPU in 0.194 seconds (100% CPU, 165662 Lips)
. done
% All 9 tests passed
```
