:- use_module(library('heavycarbon/utils/randomly_select.pl')).

:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).

% This is "somewhat of a test", but is mainly a code to exercise the 
% randomly_select module.
%
% There is no failure criterium!

:- begin_tests(randomly_select).

test("run often") :- 
   % debug(criterium),
   generate_stats(0,1000,stats{},Out),
   debug(criterium,"At termination: ~q",[Out]).

% ---
% Generate statistics (actually histogram data because we are counting)
% in a tail-recursive loop. Test whether the relative frequencies are
% as expected, and terminate the loop if this is the case.
% ---

generate_stats(Count,CountLimit,StatsIn,Out) :-
   Count < CountLimit,!,
   randomly_select(
     _{
       alfa     : ''             % selection probability = 0
      ,bravo    : xxxxx          % selection probability = 5/24
      ,charlie  : xxx            % selection probability = 3/24
      ,echo     : xxxxxxxxxx     % selection probability = 10/24
      ,foxtrott : xxxxxx},       % selection probability = 6/24
     Selected),
   inc_value(Selected,StatsIn,Stats0),
   succ(Count,CountNow),
   if_then_else(

      % IF...
      % need to name the plunit module to make it visible to the metacall ... this is a wart

      plunit_randomly_select:frequencies_are_good(Stats0,CountNow,Frequencies),

      % THEN...
      % If "frequencies are good", we can stop the recursive descent
      % Non-classically for Logic Programming, set "Out" to a "Dict".
      % This is actually far cleaner than doing a series of unifications.
      % This way of doing has a "functional feel" that cannot be denied.

      (Out = out{stats:Stats0,stop_reason:frequencies_good,count:CountNow,frequencies:Frequencies}),

      % ELSE...
      % If "frequencies are not good", we should continue. But
      % we might well break off directly after the recursive call because
      % the limit has been reached.

      plunit_randomly_select:generate_stats(CountNow,CountLimit,Stats0,Out)).

% - - - - -
% The recursion breakoff case. We recompute the frequencies here to tell them to the user
% - - - - -

generate_stats(CountLimit,CountLimit,Stats,Out) :-
   compute_frequencies(Stats,CountLimit,Frequencies),
   Out = out{stats:Stats,stop_reason:limit_reached,count:CountLimit,frequencies:Frequencies}.
 
% ---
% Increment the counter value for a given outcome in "DictIn",
% giving "DictOut". If there is no entry for the selection yet,
% it is added to the dictionary with value 1.
% ---

inc_value(K,DictIn,DictOut) :-
   assertion(var(DictOut)),
   assertion(atom(K)),
   if_then_else(
      get_dict(K,DictIn,V),
      (succ(V,Vp),put_dict(K,DictIn,Vp,DictOut)),
      put_dict(K,DictIn,1,DictOut)).

% ---
% Check whether frequencies are as expected.
% ---

frequencies_are_good(Stats,Count,Frequencies) :-
   assertion(Count>0),
   assertion(is_dict(Stats)),
   compute_frequencies(Stats,Count,Frequencies),
   debug(criterium,"Stats now: ~q ; Frequencies now: ~q",[Stats,Frequencies]),
   match_expected_frequencies(Frequencies,Out), % Reified result Out is either 'true' or 'false'
   Out == true. % succeeds if there is an approx match (obscurely, once could also write call(Out))
 
% ---
% Compute frequencies by dividing the counters by Count
% ---

compute_frequencies(Stats,Count,Frequencies) :-
   Countf is Count*1.0, % float
   dict_pairs(Stats,Tag,Pairs),
   maplist(
      ({Countf}/[K-Counter,K-Frequency]>>(Frequency is Counter/Countf)),
      Pairs,PairsOut),
   dict_pairs(Frequencies,Tag,PairsOut).

% ---
% Approximate matching. Succeeds on match only.
% ---

match_expected_frequencies(Frequencies,Out) :-
   Shalls=[bravo-0.21,charlie-0.12,echo-0.42,foxtrott-0.25],
   foldl(foldy_goal(Frequencies),Shalls,true,Out),
   debug(criterium,"After foldl: ~q",[Out]).
   
foldy_goal(_,_,false,false). % once the result becomes false, it stays false

foldy_goal(Frequencies,K-Shall,true,ToRight) :-
   reify_outcome((get_dict(K,Frequencies,F),(abs(Shall-F)<0.01)),true,false,ToRight).

:- end_tests(randomly_select).


