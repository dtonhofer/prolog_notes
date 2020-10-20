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
% 6 inferences, 0.000 CPU in 0.000 seconds (96% CPU, 319251 Lips)
% ...to compute fib(3000)
% 6 inferences, 0.000 CPU in 0.000 seconds (96% CPU, 554888 Lips)
% ...to compute fib(4000)
% 6 inferences, 0.000 CPU in 0.000 seconds (97% CPU, 552283 Lips)
% ######## Running topdown_cache_using_list_ascending
% ...to compute fib(2000)                                                                                                                                                                                                                    
% 1,343,340 inferences, 0.150 CPU in 0.151 seconds (99% CPU, 8944548 Lips)
% ...to compute fib(3000)
% 3,015,006 inferences, 0.374 CPU in 0.380 seconds (98% CPU, 8062620 Lips)
% ...to compute fib(4000)
% 5,353,340 inferences, 0.640 CPU in 0.646 seconds (99% CPU, 8367913 Lips)
% ######## Running topdown_cache_using_list_descending
% ...to compute fib(2000)
% 8,002 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 4014156 Lips)
% ...to compute fib(3000)
% 12,002 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 3811958 Lips)
% ...to compute fib(4000)
% 16,002 inferences, 0.006 CPU in 0.006 seconds (99% CPU, 2500250 Lips)
% ######## Running topdown_cache_with_clpfd_constraints
% ...to compute fib(2000)
% 8,002 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 3812275 Lips)
% ...to compute fib(3000)
% 12,002 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 3531911 Lips)
% ...to compute fib(4000)
% 16,002 inferences, 0.008 CPU in 0.008 seconds (99% CPU, 1975958 Lips)
% ######## Running topdown_cache_using_dict
% ...to compute fib(2000)
% 37,988 inferences, 0.112 CPU in 0.113 seconds (99% CPU, 337902 Lips)
% ...to compute fib(3000)
% 56,988 inferences, 0.168 CPU in 0.172 seconds (98% CPU, 339685 Lips)
% ...to compute fib(4000)
% 75,988 inferences, 0.222 CPU in 0.224 seconds (99% CPU, 341993 Lips)
% ######## Running bottomup_direct
% ...to compute fib(2000)
% 6,003 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 5108758 Lips)
% ...to compute fib(3000)
% 9,003 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 5027441 Lips)
% ...to compute fib(4000)
% 12,003 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 4867057 Lips)
% ######## Running bottomup_cache_using_lazy_list
% ...to compute fib(2000)
% 10,347 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 4887369 Lips)
% ...to compute fib(3000)
% 15,518 inferences, 0.004 CPU in 0.004 seconds (100% CPU, 3681784 Lips)
% ...to compute fib(4000)
% 20,676 inferences, 0.005 CPU in 0.005 seconds (100% CPU, 4582406 Lips)
% ######## Running bottomup_cache_using_dict
% ...to compute fib(2000)
% 15,999 inferences, 0.106 CPU in 0.106 seconds (99% CPU, 151465 Lips)
% ...to compute fib(3000)
% 23,999 inferences, 0.151 CPU in 0.152 seconds (99% CPU, 158845 Lips)
% ...to compute fib(4000)
% 31,999 inferences, 0.195 CPU in 0.196 seconds (99% CPU, 164152 Lips)
. done
% All 9 tests passed
true.
```
