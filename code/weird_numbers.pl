% ===
% UNDER CONSTRUCTION
% ===
%
% http://rosettacode.org/wiki/Weird_numbers
%
% In number theory, a weird number is a natural number that is abundant but
% not semiperfect.
%
% In other words, the sum of the proper divisors of the number (divisors including
% 1 but not itself) is greater than the number itself (the number is "abundant"), 
% but no subset of those divisors sums to the number itself (the number is not
% "semiperfect").
%
% For example:
%
%  12 is not a weird number.
%       It is abundant; its proper divisors 1, 2, 3, 4, 6 sum to 16 > 12,
%       but it is semiperfect, 6 + 4 + 2 == 12.
%  70 is a weird number.
%       It is abundant; its proper divisors 1, 2, 5, 7, 10, 14, 35 sum to 74 > 70,
%       and there is no subset of proper divisors that sum to 70.
%
% Task
%
% Find and display, here on this page, the first 25 weird numbers. 
%
% ===
% Runs in SWI Prolog 8.1
% Specials: Uses library(yall) lambda expression for legibility inside a maplist/3.
% ==

% ---
% Declare a dynamic predicate to store an ascendingly sorted list of prime numbers
% in the Prolog database, like so:
%
% primes([2,3,5,7])
%
% This entry is replaced with a fact about a longer list once we find a new 
% prime number during our peregrinations.
%
% An alternative would be to thread the list through the calls, but that's just
% unnecessarily bothersome.
% ---

:- dynamic(primes/1).

reset_primes :-
   retractall(primes(_)),  % Reset list of primes (actually "P-P²" expressions)
   assertz(primes([2-4])). % List cannot be full empty to avoid tedious edge case code

% two implementations for storing a new prime;
% it turns out the one with keysort is faster

store_new_prime_maybe(P,insert) :-
   primes(Primes),
   PSq is P*P,
   insert(P-PSq,Primes,PrimesNew),      % task-specific insertion; won't insert duplicates; is it faster??
   retractall(primes(_)),
   assertz(primes(PrimesNew)).

store_new_prime_maybe(P,keysort) :-
   primes(Primes),
   (memberchk(P-_,Primes)               % check whether an entry for P exists
   -> true
   ;  store_new_prime_with_keysort(P)). % if not, store

store_new_prime_with_keysort(P) :-
   primes(Primes),
   PSq is P*P,
   keysort([P-PSq|Primes],PrimesNew), % sort by P of "P-PSq", keysort doesn't drop duplicates!
   retractall(primes(_)),
   assertz(primes(PrimesNew)).

% ---
% Calls to format/1 or format/2 are in the code to help in debugging.
% Rename them to qformat/1 or qfromat/2 to suppresses output.
% ---
 
qformat(_). 
qformat(_,_). 

% ---
% ENTRY POINT: collect Count weird numbers into list WeirdNumbers.
% The REPL (Prolog toplevel) will print out the contents of WeirdNumbers.
% ---

go(WeirdNumbers,Count) :- 
   length(WeirdNumbers,Count),          % creates a list of Count fresh variables
   reset_primes,
   collect_from(2,WeirdNumbers).        % 1 cannot be a weird number, so collect from 2

% ---
% Collect by testing increasingly large candidates until "WeirdNumbers" has been 
% filled to satisfaction.
% ---

verify_abundance(N,ProperDivisors,SumPDs) :-
   sum_list(ProperDivisors,SumPDs),
   SumPDs > N.

verify_semiperfect(N,ProperDivisors) :-
   length(ProperDivisors,L),
   bit_pattern(L,Pat),       % generate bit patterns
   sort(ProperDivisors,Best),% reverse(T,BiggestFirst),

   verify_sum_reaches(0,N,Best,Pat,FinalSum,[],ValuesOut),
   format("Pattern for ~q is ~q and Sum reached ~q via ~q\n", [N, Pat, FinalSum, ValuesOut]).
 
 
verify_sum_reaches(Sum,N,[__|PDs],[0|Bs],FS,Vin,Vout) :- verify_sum_reaches(Sum,N,PDs,Bs,FS,Vin,Vout).
verify_sum_reaches(Sum,N,[PD|___],[1|__],NewSum,_,_) :- NewSum is Sum+PD, NewSum>N, !, fail.
verify_sum_reaches(Sum,N,[PD|PDs],[1|Bs],FS,Vin,Vout)     :- NewSum is Sum+PD, NewSum<N, !, verify_sum_reaches(NewSum,N,PDs,Bs,FS,[PD|Vin],Vout).
verify_sum_reaches(Sum,N,[PD|___],[1|__],NewSum,Vin,[PD|Vin]) :- NewSum is Sum+PD, NewSum=N.
verify_sum_reaches(Sum,_,[],[],Sum,_,_) :- false.

% Case of: N is a weird number

collect_from(N,[N|Ws]) :-
   collect_prime_factors(N,PrimeFactors,keysort),
   proper_divisors(N,PrimeFactors,ProperDivisors),
   verify_abundance(N,ProperDivisors,SumPDs),
   \+verify_semiperfect(N,ProperDivisors),
   !,
   format("A new Weird Number: ~q, PDs = ~q, Sum = ~q\n",[N,ProperDivisors,SumPDs]),
   succ(N,Np),
   collect_from(Np,Ws).

% Case of: N is not a weird number

collect_from(N,[W|Ws]) :-
   !,
   succ(N,Np),
   collect_from(Np,[W|Ws]).

% Case of: "WeirdNumbers" filled

collect_from(N,[]) :- succ(Nm,N), format("Done at N=~w\n",[Nm]).

% ---
% Collect all proper divisors
% ---

proper_divisors(N,PrimeFactors,ProperDivisors) :-
   bagof(Product,baggit(PrimeFactors,Product),Products),
   qformat("N=~w Products=~w\n", [N,Products]),
   sort(Products,ProperDivisorsWithN),                  % eliminiates duplicates
   append(ProperDivisors,[N],ProperDivisorsWithN),      % eliminates N itself
   qformat("N=~w ProperDivisors=~w\n", [N,ProperDivisors]).
 
baggit(PrimeFactors,Product) :-
   length(PrimeFactors,L),
   bit_pattern(L,Pat),
   selection_product(PrimeFactors,Pat,Product,FactorsUsed),
   qformat("Bitpattern=~q FactorsUsed=~q Product=~q\n",[Pat,FactorsUsed,Product]).
 
selection_product([_|Fs],[0|Bits],Result,FactorsUsed) :-
   selection_product(Fs,Bits,Result,FactorsUsed).

selection_product([F|Fs],[1|Bits],Result,[F|FactorsUsed]) :-
   selection_product(Fs,Bits,SubResult,FactorsUsed),
   Result is F*SubResult.

selection_product([],[],1,[]).

% ---
% Test: find prime factors of integers up to Max (inclusive)
% This fills the primes(PrimesList). fact.
% How is either "insert" or "keysort" and indicates which algorithm to use.
% ---

go_find_prime_factors_up_to(Max,How) :-
   2=<Max,
   reset_primes,
   find_prime_factors_from_to(2,Max,How).

find_prime_factors_from_to(I,Max,How) :-
   I=<Max,
   !,
   collect_prime_factors(I,_Factors,How), % don't care about Factors
   succ(I,Ip),
   find_prime_factors_from_to(Ip,Max,How).

find_prime_factors_from_to(I,Max,_) :- 
   Max<I.
  
% ---
% Collect prime factors of N by using known Primes stored in the database.
% If N turns out to be Prime, the list of Primes in the database is updated.
% ---

collect_prime_factors(N,Factors,How) :-
   2=<N,   
   primes(Primes),
   collect_prime_factors_iter(N,Primes,Factors,How),
   check_prime_factors(N,Factors),
   printout_primes_fact_changes(Primes).
 
check_prime_factors(N,Factors) :-
   (mult_list(Factors,N) 
      ->
      true
      ;
      throw("Incorrect factors")).

printout_primes_fact_changes(OldPrimes)  :-
   primes(NewPrimes),
   length(OldPrimes,L1),
   length(NewPrimes,L2),
   ((L1\=L2)
       -> 
       (nth1(L2,NewPrimes,P),
       qformat("New prime discovered: ~q. There are now ~q primes in store\n",[P,L2]))
       ;
       true).

% ---
% Helper for prime_factors/2
% ---

% Case of: N has no prime factor <= sqrt(N) ... thus, found a new prime: N.
% Store it in Prolog database, if it isn't already there.
 
collect_prime_factors_iter(N,[_Prime-PrimeSq|_Primes],[N],How) :-
   N<PrimeSq,
   !,
   store_new_prime_maybe(N,How).

% Case of: found a new prime factor, recursively continue with new 
% quotient, restart scanning list of primes from the beginning.

collect_prime_factors_iter(N,[Prime-PrimeSq|_Primes],[Prime|Factors],How) :-
   PrimeSq=<N,
   mod(N,Prime) =:= 0,
   !,
   Q is div(N,Prime),
   primes(Primes),
   collect_prime_factors_iter(Q,Primes,Factors,How).

% Case of: not a prime factor, continue with larger factors.

collect_prime_factors_iter(N,[Prime-PrimeSq|Primes],Factors,How) :-
   PrimeSq=<N,
   mod(N,Prime) > 0,
   !,
   collect_prime_factors_iter(N,Primes,Factors,How).
 
% ---
% Backtrackable predicate generating all bit patterns of L bits
% as a list of 0,1.
% The ">>" is not ISO Prolog but the Lambda Notation from library(yall)
% Used here for readability.
% ---

bit_pattern(L,P) :-
   length(P,L),
   maplist([X]>>(member(X,[0,1])),P).
 
% ---
% Sum over values in list.
% ---

% Render this subject to tail-call optimization
% TODO: Test running times with/without
% Rrun = Running result
% Rfin = Final result, constrained in the base case

sum_list(L,Out) :- 
   sum_list(L,0,Out).

sum_list([],R,R).

sum_list([X|Xs],Rrun,Rfin) :-
   Rnex is Rrun + X,
   sum_list(Xs,Rnex,Rfin).

% ---
% Product over values in list.
% ---

% Render this subject to tail-call optimization
% TODO: Test running times with/without
% Rrun = Running result
% Rfin = Final result, constrained in the base case

mult_list(L,Out) :- 
   mult_list(L,1,Out).

mult_list([],R,R).

mult_list([X|Xs],Rrun,Rfin) :-
   Rnex is Rrun * X,
   mult_list(Xs,Rnex,Rfin).

% ---
% Task-special insertion sort to insert a prime into a sorted list
% ---

insert(P-PSq,ListIn,T) :- insert_help(P-PSq,ListIn,[1-1|T]-T).
   
insert_help(P-PSq,[Px-PSqx|R],H-T)  :- P>Px,!, qformat("go_right"),      T=[Px-PSqx|TT],insert_help(P-PSq,R,H-TT).
insert_help(P-PSq,[P-PSq|R],_H-T)   :-      !, qformat("exists"),        T=[P-PSq|R].
insert_help(P-PSq,[Px-PSqx|R],_H-T) :- P<Px,!, qformat("insert_before"), T=[P-PSq,Px-PSqx|R].
insert_help(P-PSq,[],_H-T)          :-      !, qformat("insert at end"), T=[P-PSq].


test_insert :-
   insert(11-121,[2-4,3-9,5-25,7-49],[2-4,3-9,5-25,7-49,11-121]),
   insert(2-4,[3-9,5-25,7-49,11-121],[2-4,3-9,5-25,7-49,11-121]),
   insert(3-9,[2-4,3-9,5-25,7-49,11-121],[2-4,3-9,5-25,7-49,11-121]),
   insert(3-9,[2-4,5-25,7-49,11-121],[2-4,3-9,5-25,7-49,11-121]),
   insert(7-49,[2-4,3-9,5-25,11-121],[2-4,3-9,5-25,7-49,11-121]),
   insert(2-4,[],[2-4]).

 









