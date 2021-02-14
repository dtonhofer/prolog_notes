:- module(tuple_via_member_escalator,
          [
          tuple_via_member_escalator/4  % tuple_via_member_escalator(State:Dict,Min:Integer,Max:Integer:Dim:Integer)
          ]).

% =============================================================================
% Enumerate all tuples of "D" (dimensionality) integers ranged from "Min" to
% "Max" both inclusive (i.e. the numbers of D places of radix Max-Min+1)
%
% A tuple is represented by an SWI-Prolog dict where the keys are integers
% going from 0 to D-1 and the values are from [Min,Max]. 
%
% Enumerate tuples using an emumeration in D dimensions, performed
% by creating a recursive escalator D deep, chaining D member/2 calls.
%
% Example
% =======
%
% On backtracking, this generates all possibilities. 
%
% Generate all 3125 entries of the set with value range 1..5, 
% dimensionalty 5:
%
% ?- bagof(T,tuple_via_member_escalator(T,1,5,5),AllStatesUnsorted),
%    sort(AllStatesUnsorted,AllStates),
%    length(AllStates,L).
% L = 3125
%
% Speed testing by demanding it generate something impossible:
%
% Go wide:
% ========
%
% All dimensionality-20 binary possibilities:
%
% ?- time(tuple_via_member_escalator(foo,0,1,20)).
%    7,340,052 inferences, 2.652 CPU in 2.661 seconds (100% CPU, 2767944 Lips)
%
% All dimensionality-12 ternary possiblities:
%
% ?- time(tuple_via_member_escalator(foo,0,3,12)).
%    55,924,081 inferences, 21.265 CPU in 21.368 seconds (100% CPU, 2629840 Lips)
%
% Go deep:
% ========
%
% All dimensionality-3 500-ary possibilities:
% 
% ?- time(tuple_via_member_escalator(foo,0,999,3)).
%    % 2,003,009,029 inferences, 478.537 CPU in 480.515 seconds (100% CPU, 4185691 Lips)
%
% ===

% ===
% tuple_via_member_escalator(Tuple:Dict,Min:Integer,Max:Integer:Dim:Integer)
% ===

tuple_via_member_escalator(Tuple,Min,Max,Dim) :-
   assertion((integer(Dim),Dim>0)),
   assertion((integer(Min),integer(Max),Min=<Max)),   
   bagof(X,between(Min,Max,X),Values),                    % Values is the list of values from which to pick members
   Count is Dim-1,
   member_escalator(Count,Values,List),                   % backtrackably create a list of numbers
   dict_create(Tuple,s,List).                             % transform the List into a dict called Tuple with tag 's'
   
% ---
% Recursive descent to obtain values in a way that one can backtrack over.
% For a fixed dimensionality, this can be replaced by a conjunction of
% "dimensionality" number of calls to member/2.
% Another way to do this would be a goal with these "dimensionality"
% number of calls one can then execute.
% ---

member_escalator(0,Values,[0:X]) :-   
   !,
   member(X,Values). % position 0 is base case of the escalator and thus changes fastest
   
member_escalator(Count,Values,[Count:X|More]) :-
   assertion(Count>0),
   succ(CountMinus,Count),
   member(X,Values), % backtrackably select a member   
   member_escalator(CountMinus,Values,More).
   
