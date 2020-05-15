% From 
% https://stackoverflow.com/questions/61804572/arithmetics-in-prolog-represent-a-number-using-powers-of-2
% which also lists a solution using CLP(FD)

:- use_module(library(debug)).

% ---
% powersum(+N,+Target,?Solution)
% ---
% Entry point. Relate a list "Solution" of "N" integers to the integer
% "Target", which is the sum of 2^Solution[i].
% This works only in the "functional" direction
% "Compute Solution as powersum(N,Target)"
% or the "verification" direction
% "is Solution a solution of powersum(N,Target)"?
%
% An extension of some interest would be to NOT have a fixed "N".
% Let powersum/2 find appropriate N.
%
% The search is subject to exponential slowdown as the list length
% increases, so one gets bogged down quickly.
% ---

powersum(N,Target,Solution) :- 
   ((integer(N),N>0,integer(Target),Target>=1) -> true ; throw("Bad args!")),   
   length(RS,N),                             % create a list RN of N fresh variables
   MaxPower is floor(log(Target)/log(2)),    % that's the largest power we will find in the solution
   propose(RS,MaxPower,Target,0),            % generate & test a solution into RS
   reverse(RS,Solution),                     % if we are here, we found something! Reverse RS so that it is increasing
   my_write(Solution,String,Value),          % prettyprinting
   format("~s = ~d\n",[String,Value]).

% ---
% propose(ListForSolution,MaxPowerHere,Target,SumSoFar)
% ---
% This is an integrate "generate-and-test". It is integrated
% to "fail fast" during proposal - we don't want to propose a
% complete solution, then compute the value for that solution 
% and find out that we overshot the target. If we overshoot, we
% want to find ozut immediately!
%
% So: Propose a new value for the leftmost position L of the 
% solution list. We are allowed to propose any integer for L 
% from the sequence [MaxPowerHere,...,0]. "Target" is the target
% value we must not overshoot (indeed, we which must meet
% exactly at the end of recursion). "SumSoFar" is the sum of
% powers "to our left" in the solution list, to which we already
% committed.

propose([L|Ls],MaxPowerHere,Target,SumSoFar) :- 
   assertion(SumSoFar=<Target),
   (SumSoFar=Target -> false ; true),          % a slight optimization, no solution if we already reached Target!
   propose_value(L,MaxPowerHere),              % Generate: L is now (backtrackably) some value from [MaxPowerHere,...,0]
   NewSum is (SumSoFar + 2**L),                
   NewSum =< Target,                           % Test; if this fails, we backtrack to propose_value/2 and will be back with a next L
   NewMaxPowerHere = L,                        % Test passed; the next power in the sequence should be no larger than the current, i.e. L
   propose(Ls,NewMaxPowerHere,Target,NewSum).  % Recurse over rest-of-list.

propose([],_,Target,Target).                   % Terminal test: Only succeed if all values set and the Sum is the Target!

% ---
% propose_value(?X,+Max).
% ---
% Give me a new value X between [Max,0].
% Backtracks over monotonically decreasing integers.
% See the test code for examples.
%
% One could also construct a list of integers [Max,...,0], then
% use "member/2" for backtracking. This would "concretize" the predicate's
% behaviour with an explicit list structure.
%
% "between/3" sadly only generates increasing sequences otherwise one
% could use that. Maybe there is a "between/4" taking a step value somewhere?
% ---

propose_value(X,Max) :- 
   assertion((integer(Max),Max>=0)),
   Max=X.
propose_value(X,Max) :- 
   assertion((integer(Max),Max>=0)),
   Max>0, succ(NewMax,Max), 
   propose_value(X,NewMax).
 
% ---
% I like some nice output, so generate a string representing the solution.
% Also, recompute the value to make doubly sure!
% ---

my_write([L|Ls],String,Value) :-
   my_write(Ls,StringOnTheRight,ValueOnTheRight),
   Value is ValueOnTheRight + 2**L,
   with_output_to(string(String),format("2^~d + ~s",[L,StringOnTheRight])).
   
my_write([L],String,Value) :-
   with_output_to(string(String),format("2^~d",[L])),
   Value is 2**L.



:- begin_tests(powersum).

% powersum(N,Target,Solution) 

test(pv1)       :- bagof(X,propose_value(X,3),Bag), Bag = [3,2,1,0].
test(pv2)       :- bagof(X,propose_value(X,2),Bag), Bag = [2,1,0].
test(pv2)       :- bagof(X,propose_value(X,1),Bag), Bag = [1,0].
test(pv3)       :- bagof(X,propose_value(X,0),Bag), Bag = [0].

test(one)       :- bagof(S,powersum(1,1,S),Bag), Bag = [[0]].
test(two)       :- bagof(S,powersum(3,10,S),Bag), Bag = [[0,0,3],[1,2,2]].
test(three)     :- bagof(S,powersum(3,145,S),Bag), Bag = [[0,4,7]].
test(four,fail) :- powersum(3,8457894,_).
test(five)      :- bagof(S,powersum(9,8457894,S), Bag), Bag = [[1, 2, 5, 7, 9, 10, 11, 16, 23]]. %% VERY SLOW

:- end_tests(powersum).

rt :- run_tests(powersum).




