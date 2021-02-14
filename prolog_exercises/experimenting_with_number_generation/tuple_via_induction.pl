:- module(tuple_via_induction,
          [
          tuple_via_induction/4  % tuple_via_induction(Tuple:Dict,Min:Integer,Max:Integer:Dim:Integer)
          ]).

% =============================================================================
% Enumerate all tuples of "D" (dimensionality) integers ranged from "Min" to
% "Max" both inclusive (i.e. the numbers of D places of radix Max-Min+1)
%
% A tuple is represented by an SWI-Prolog dict where the keys are integers
% going from 0 to D-1 and the values are from [Min,Max].
%
% Enumerate tuples using a direct implementation of the inductive definition:
% A tuple is either the initial tuple or a tuple "update to the next tuple".
%
% tuple = ( initial tuple | tuple + 1 )
%       = ( initial tuple | initial_tuple + 1 | tuple + 1 + 1 )
%       = ( initial tuple | initial_tuple + 1 | initial_tuple + 1 + 1 | initial_tuple + 1 + 1 + 1)
%       = ...
%
% The performance stays only acceptable using tabling.
%
% Example
% =======
%
% On backtracking, this generates all possibilities. 
%
% ?- tuple_via_induction(Tuple,1,4,5).
% Tuple = s{0:3,1:3,2:3,3:3,4:3} ;
% Tuple = s{0:3,1:3,2:3,3:3,4:2} ;
% Tuple = s{0:3,1:3,2:3,3:3,4:1} ;
% Tuple = s{0:3,1:3,2:3,3:3,4:4} ;
% Tuple = s{0:3,1:3,2:3,3:2,4:3} ;
% Tuple = s{0:3,1:3,2:3,3:2,4:2} ;
% Tuple = s{0:3,1:3,2:3,3:2,4:1} ;
% Tuple = s{0:3,1:3,2:3,3:2,4:4} ;
% Tuple = s{0:3,1:3,2:3,3:1,4:3} ;
% Tuple = s{0:3,1:3,2:3,3:1,4:2} .....
% 
% Generate all 3125 entries of the set with value range 1..5, 
% dimensionalty 5:
%
% ?- bagof(T,tuple_via_induction(T,1,5,5),AllStatesUnsorted),
%    sort(AllStatesUnsorted,AllStates),
%    length(AllStates,L).
% L = 3125
%
% =============================================================================

% ===
% tuple_via_induction(Tuple:Dict,Min:Integer,Max:Integer:Dim:Integer)
% ===

% To not become exponentially slower at high positions: tabling!

:- table tuple_via_induction/4.

tuple_via_induction(Tuple,Min,_Max,Dim) :-
   tuple_initial(Tuple,Min,Dim).

tuple_via_induction(Tuple,Min,Max,Dim) :-
   tuple_via_induction(PrevTuple,Min,Max,Dim),
   % If tuple_next/5 fails, we have had "carry beyond the end" and we must cut
   % to avoid backtracking into tuple_via_induction/4 and attempting to find
   % more possibilities which will always fail ad infinitum.
   % ("at this point, if it is false, then it will stay false, even when
   % backtracking into tuple_via_induction/4")
   (tuple_next(Min,Max,Dim,PrevTuple,Tuple) -> true ; (!,false)).

% ===
% Get an initial state where everything is a "Min"
% ===

tuple_initial(Tuple,Min,Dim) :-
   DimMax is Dim-1,
   bagof(D:Min,between(0,DimMax,D),List),
   dict_create(Tuple,s,List).

% ===
% tuple_next(+Min:Integer,+Max:Integer,+Dim:Integer,+TupleIn:Dict,?TupleOut:Dict) :-
%
% Advance the state by one step; fails if that is not possible, i.e. if the
% highest state possible for Dim positions has been reached.
% ===

tuple_next(Min,Max,Dim,TupleIn,TupleOut) :-
   pos_advances(0,Min,Max,Dim,TupleIn,TupleOut).

% ===
% pos_advance(+Pos:Integer,+Min:Integer,+Max:Integer,+Dim:Integer,+TupleIn:Dict,?TupleOut:Dict)
%
% Advance position Pos (0..D-1) of TupleIn "by 1", possibly carrying into higher
% positions Px > Pos, yielding TupleOut.
% ===

pos_advances(Pos,Min,Max,Dim,TupleIn,TupleOut) :-
   get_dict(Pos,TupleIn,Max),                     % carry situation: "Max" at "Pos": need to advance the next-slowest position
   !,
   PosNext is Pos+1,                              % next position is "slower"
   PosNext<Dim,                                   % fail if we fell out at the topmost position
   put_dict(Pos,TupleIn,Min,T1),                  % Value at Pos is reset to Min
   pos_advances(PosNext,Min,Max,Dim,T1,TupleOut). % Tail recursion to "ripple down" to the slower position

pos_advances(Pos,_Min,Max,_Dim,TupleIn,TupleOut) :-
   get_dict(Pos,TupleIn,Value),                   % fails if Pos does not exist, which means we have "fallen out at the top"
   assertion(Value < Max),                        % not carry situation: need to advance the next-slowest position
   ValueNext is Value+1,
   put_dict(Pos,TupleIn,ValueNext,TupleOut).      % Value at Pos is set; no recursive call/moving to the next-highest position

