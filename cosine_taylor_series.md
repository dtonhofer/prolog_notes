# Compute the Taylor Series for `cos(x)`

This is based on the StackOverflow question:

[Recursive program to compute Taylor approx of cosine not functioning in Prolog](https://stackoverflow.com/questions/60213894/recursive-program-to-compute-taylor-approx-of-cosine-not-functioning-in-prolog)

![Taylor Series](pics/cosine_taylor_series/taylor_series.png)

## Generator to generate test points on the float line

````
generator(X,From,To,1) :-
   From =< To,
   From_f is From*1.0,
   To_f   is To*1.0,
   X      is (From_f + To_f) / 2.0.
 
generator(X,From,To,Count) :-
   integer(Count),
   Count > 1,
   From =< To,
   From_f  is From*1.0,
   To_f    is To*1.0,
   Delta_f is (To_f - From_f)/(Count * 1.0),
   CountM1 is Count-1,
   between(0,CountM1,I),
   X is From_f + Delta_f*I.
 
cos_compare(Z,Kmax,Verbose) :-
   taylor_cos(Res,Z,Kmax,Verbose),
   Cos is cos(Z),
   Delta is abs(Res-Cos),
   format("For z = ~e, k_max = ~d, difference to real cos = ~e\n", [Z, Kmax, Delta]).
````

## Computing the Taylor Series as it would be done usually in Prolog

````
% ===
% Entry point!
% Evaluate the Taylor series for cos(z) at "z" (not too far from 0, probably
% less than 1). The terms (sum elements) for index values 0..K are computed
5 and added. (K >= 0)
% ===
 
taylor_cos(Res,Z,Kmax,Verbose) :-
   Zf is Z*1.0, % make a float
   float(Zf),
   integer(Kmax),Kmax >= 0,
   Zsq is Zf*Zf,
   at_element_k(Res,0,Kmax,Zsq,_,_,Verbose).
 
% The value computed is always the first one
 
even(K) :- integer(K), (K mod 2) =:= 0. % eval left & compare numerically
odd(K)  :- integer(K), (K mod 2) =:= 1. % eval left & compare numerically
 
% Compute (-1)^k, k an integer >= 0.
% Computed value is on first place in predicate argument list.
 
minus_one_tothe_k( 1,K) :- even(K),!. % ! to make this deterministic
minus_one_tothe_k(-1,K) :- odd(K).    % actually no need to test odd(K)
 
% Compute (2*k)!, k an integer >= 0, if (2*(k-1))! is known.
% Computed value is on first place in predicate argument list.
% The base case is conceptually jarring as the "prior value" can be anything.
% This is not unlike a function becoming evaluatable because of lazy evaluation.
 
two_times_k_factorial(1  ,0,_)        :- !.
two_times_k_factorial(Res,K,ResPrior) :- K>0, Res is ResPrior*K*(4*K-2).
 
% Compute (z^(2*k)), k an integer >= 0, if (z^(2*(k-1))) is known.
% z² is passed too so that we do not need to recompute it again and again.
% Computed value is on first place in predicate argument list.
 
z_tothe_2k(1,   0, _   ,_)        :- !.
z_tothe_2k(Res, K, Zsq ,ResPrior) :- K>0, Res is ResPrior * Zsq.
 
% Compute the Taylor series by summing the elements(k) with k in [0..Kmax)
% (so Kmax >= 1).
% When calling this initially, the values for TTKFprior and ZTT2Kprior
% are of no importance.
% The procedures calls itself recursively to compute element(i), element(i+1)
% etc. based on prior intermediate values. The base case is attained when
% K > Kmax. The sum accumulates in SumFromKmaxBackwards when the recursion
% comes back up the stack.
 
at_element_k(0.0,K,Kmax,_,_,_,Verbose) :-
   K > Kmax,!,
   ((Verbose = verbose) ->
   format("past the end as K=~d > Kmax=~d, returning back up the stack\n",[K,Kmax]) ; true).
 
at_element_k(SumFromKmaxBackwards,K,Kmax,Zsq,TTKFprior,ZTT2Kprior,Verbose) :-
   minus_one_tothe_k(M1TTK,K),                 % M1TTK = (-1)^K
   two_times_k_factorial(TTKF,K,TTKFprior),    % TTKF  = f(K,TTKFprior)
   z_tothe_2k(ZTT2K,K,Zsq,ZTT2Kprior),         % ZTT2K = f(K,z²,ZTT2Kprior)
   ElementK is M1TTK * ZTT2K / TTKF,           % element_k = M1TTK * (ZTT2K / TTKF)
   ((Verbose = verbose) -> format("element(~d) = ~e\n",[K,ElementK]) ; true),
   KP1 is K+1,
   at_element_k(SumFromKmaxBackwardsPrior,KP1,Kmax,Zsq,TTKF,ZTT2K,Verbose),
   SumFromKmaxBackwards is SumFromKmaxBackwardsPrior + ElementK,
   ((Verbose = verbose) -> format("taylor-series-sum(~d ... ~d) = ~e (added ~e to prior value ~e)\n",
                                  [K,Kmax,SumFromKmaxBackwards, ElementK, SumFromKmaxBackwardsPrior]) ; true).
````

## Run it

````
run(Verbose) :- forall(generator(Z,-4.0,+4.0,100), cos_compare(Z,10,Verbose)).
````

Then:

````
?- run(quiet). 
For z = -4.000000e+00, k_max = 10, difference to real cos = 1.520867e-08
For z = -3.920000e+00, k_max = 10, difference to real cos = 9.762336e-09
For z = -3.840000e+00, k_max = 10, difference to real cos = 6.209067e-09
For z = -3.760000e+00, k_max = 10, difference to real cos = 3.911487e-09
For z = -3.680000e+00, k_max = 10, difference to real cos = 2.439615e-09
......
For z = 3.680000e+00, k_max = 10, difference to real cos = 2.439615e-09
For z = 3.760000e+00, k_max = 10, difference to real cos = 3.911487e-09
For z = 3.840000e+00, k_max = 10, difference to real cos = 6.209067e-09
For z = 3.920000e+00, k_max = 10, difference to real cos = 9.762336e-09
true.
````

Not looking so bad.

## Computing the Taylor Series using SWI-Prolog "dicts"

This is a nonstandard approach and the Prolog compiler cannot use any optimization
when selecting the predicate to call (it must call them in order), but it's 
interesting

````
taylor_cos(Res,Z,Kmax,Verbose) :-
   Zf is Z*1.0, % make a float
   float(Zf),
   integer(Kmax),Kmax >= 0,
   Zsq is Zf*Zf,
   at_element_k(taylor{  sum     : Res  % the result
                        ,k       : 0
                        ,kmax    : Kmax
                        ,zsq     : Zsq
                        ,ttkf_prior  : _
                        ,ztt2k_prior : _
                        ,verbose : Verbose }).
 
 
% ---
% Base case, when k > kmax
% ---
 
% We map the passed "Dict" to a sub-Dict to grab values.
% As this is "unification", not only "pattern matching" the value for
% sum "0.0" is shared into "Dict".
 
at_element_k(Dict) :-
   taylor{  sum     : 0.0
           ,k       : K
           ,kmax    : Kmax
           ,verbose : Verbose } :< Dict,
 
   K > Kmax,  % guard
   !,         % commit
   ((Verbose = verbose) ->
      format("past the end as K=~d > Kmax=~d, returning back up the stack\n",[K,Kmax])
      ; true).
 
% ---
% Default case, when k <= kmax
% ---
 
% We map the passed "Dict" to a sub-Dict to grab values.
% We use ":<" instead of "=" so that, if the passed Dict has more values
% than expected (which can happen during program extension and fiddling),
% "partial unification" can still proceed, "=" would fail. However, no
% values can be missing!
% This gives us also the funny possibility of completely ignoring Kmax in
% the "input Dict", it doesn't appear anywhere and is still passed down
% through the recursive call. Well, it *does* appear because we print it
% out.
 
at_element_k(Dict) :-
   taylor{  sum         : SumFromKmaxBackwards  % the output value, to be captured by the caller
           ,k           : K                     % index of the current term/element in the Taylor sum
           ,kmax        : Kmax                  % max index for which a term/element will be computed
           ,zsq         : Zsq                   % z², a constant
           ,ttkf_prior  : TTKFprior             % prior "two times k factorial" i.e. (2*(k-1))!
           ,ztt2k_prior : ZTT2Kprior            % prior "z to the 2*k" i.e. z^(2*(k-1))
           ,verbose     : Verbose } :< Dict,    % emit messages about progress if Verbose = verbose
 
   minus_one_tothe_k(M1TTK,K),                       % compute (-1)^K
   two_times_k_factorial(TTKF,K,TTKFprior),          % compute (2*k)! based on prior value
   z_tothe_2k(ZTT2K,K,Zsq,ZTT2Kprior),               % compute z^(2*k) based on prior value
   ElementK is M1TTK * ZTT2K / TTKF,                 % compute value for Taylor sum term/element at k
 
   % (isn't there a better way to print conditionally?)
 
   ((Verbose = verbose) ->
      format("element(~d) = ~e\n",[K,ElementK])
      ; true),
 
   % create a NextDict from Dict for recursive call
 
   KP1 is K+1,
   put_dict( _{ sum        : SumFromKmaxBackwardsPrior
               ,k          : KP1
               ,ttkf_prior : TTKF
               ,ztt2k_prior: ZTT2K }, Dict, NextDict),
 
   % recursive call
   % (foundational thought: the procedure is really a **channel-doing-computations between the series of dicts**)
 
   at_element_k(NextDict),
 
   % on return, complete summing the Taylor series backwards from highest index to the current index k
 
   SumFromKmaxBackwards is SumFromKmaxBackwardsPrior + ElementK,
 
   % (more conditional printing)
 
   ((Verbose = verbose) ->
      format("taylor-series-sum(~d ... ~d) = ~e (added ~e to prior value ~e)\n",
            [K,Kmax,SumFromKmaxBackwards,ElementK,SumFromKmaxBackwardsPrior])
      ; true).
````



