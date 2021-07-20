% Trying "Escape to Jing Provice" in Prolog
% =========================================

% ---
% Nice terrain testing: Is (Row, Col) of the type given by the predicate name?
% ---

terrain_plains(Row,Col)   :- terrain(Row,Col,p).
terrain_mountain(Row,Col) :- terrain(Row,Col,m).
terrain_forest(Row,Col)   :- terrain(Row,Col,f).
terrain_city(Row,Col)     :- terrain(Row,Col,c).
terrain_river(Row,Col)    :- terrain(Row,Col,r).

% ---
% Golden retrievers: They get values for (Row, Col) out of the respective matrixes.
% ---

% Unifies X with the 1-character terrain type at (Row,Col).
% Fails if (Row,Col) is out of bounds.

terrain(Row,Col,X) :-
   terrain(Matrix),
   nth1(Row,Matrix,RowList),
   nth1(Col,RowList,X).

% Succeeds if (Row,Col) belongs to Jing Province, otherwise fails.
% Fails if (Row,Col) is out of bounds.

jin(Row,Col) :-
   jin(Matrix),
   nth1(Row,Matrix,RowList),
   nth1(Col,RowList,true). % assuming that the atom 'true' means "Jing Province"

% Unifies Count with the number of soldiers at (Row,Col).
% Fails if (Row,Col) is out of bounds.

soldier(Row,Col,Count) :-
   soldier(Matrix),
   nth1(Row,Matrix,RowList),
   nth1(Col,RowList,Count).

% Unifies Delay with the delay associated with the terrain type at (Row,Col).
% Fails if (Row,Col) is out of bounds.

delay(Row,Col,Delay) :-
   terrain(Row,Col,X),
   delay(X,Delay).

% ================ SEARCH CODE ==================

main :-
   print_terrain,
   time(find_best_path(Path,CityCount,Delay,MinSoldierCount)),
   format("Soldiers : ~d~n",[MinSoldierCount]),
   format("Cities   : ~d~n",[CityCount]),
   format("Delay    : ~d~n",[Delay]),
   format("Path     : ~q~n",[Path]),
   print_terrain_with_path(Path).

% ---
% Kick off search for the path with the minimum number of soldiers
% (actually a/the global optimum over all paths with maximum length
% "maxstep" fulfilling all constraints, so this is exhaustive search)
% We use the "aggregate" meta-call from library(aggregate)
% https://eu.swi-prolog.org/pldoc/man?section=aggregate
% ---

find_best_path(Path,CityCount,Delay,MinSoldierCount) :-
   % This was foreseen to use an "iterative deepening" approach, but
   % we will just set MaxDepth to the maximum allowed.
   const(maxstep,MaxStep),
   assertion(MaxStep >= 1),
   MaxDepth is MaxStep - 1,
   aggregate(
      min(SoldierCount,[PathW,CityCountW,DelayW]),                  % Template: minimize SoldierCount and collect "witness values", too
      find_any_path(MaxDepth,PathW,CityCountW,DelayW,SoldierCount), % Backtrackable predicate
      Result),                                                      % Collect the result
   Result = min(MinSoldierCount,[Path,CityCount,Delay]).            % Unification on a separate line to avoid confusion

% ---
% Blind search with iterative deepening. MaxDepth gives the maximum recursive
% descent depth. Enumerates all paths on backtracking.
% ---

find_any_path(MaxDepth,PathOut,CityCountOut,DelayOut,SoldierCountOut) :-
   assertion(MaxDepth >= 0),
   const(start_row,Row),
   const(start_col,Col),
   verify_initial_position(Row,Col,InitialStatus), % fails if (Row,Col) already breaks a constraint
   find_path_from_here(
      MaxDepth,
      [[Row,Col,start]],
      InitialStatus,
      PathOutReversed,
      StatusOut),
   reverse(PathOutReversed,PathOut),
   _{ city_count: CityCountOut, delay_so_far: DelayOut, soldier_count: SoldierCountOut } :< StatusOut.

% Succeeds if the initial position breaks no constraints.
% Fills InitialStatus with the initial values for city count, soldier count, and delay-so-delay_so_far

verify_initial_position(Row,Col,InitialStatus) :-
   assertion(terrain(Row,Col,_)),    % (Row, Col) is in-bounds
   verify(Row,Col,[],_{city_count: 0, delay_so_far: 0, soldier_count: 0 },InitialStatus).

% find_path_from_here(+MaxDepth,+Path:List,+Status:Dict,-PathOut:List,-StatusOut:Dict)
%
% We are at the position stored in the head of the Path variable, and the path fulfills
% all constraints. What now?
% - If we are in "Jing Province", succeed.
% - If we are not yet in "Jing Province", try to move in any of the four cardinal directions
%   from the current position.

find_path_from_here(_MaxDepth,[[Row,Col,Direction]|Path],Status,[[Row,Col,Direction]|Path],Status) :-
   jin(Row,Col),
   !.

find_path_from_here(MaxDepth,PathSoFar,Status,PathOut,StatusOut) :-
   PathSoFar = [[Row,Col,_]|OldPath],
   NewMaxDepth is MaxDepth - 1,
   NewMaxDepth >= 0,                                  % iterative deepening fail, equivelnt to exceeeding max step
   explore(Row,Col,NewRow,NewCol,Direction),          % backtrack over this into all four cardinal directions
   debug(find_path_from_here,"MaxDepth = ~d ... Maybe (~d,~d) -> (~d,~d) (going ~a?) ~q~n",[MaxDepth,Row,Col,NewRow,NewCol,Direction,PathSoFar]),
   \+ member([NewRow,NewCol,_],OldPath),              % we don't want to go in circles
   verify(NewRow,NewCol,PathSoFar,Status,NewStatus),  % must pass all tests, creates NewStatus
   find_path_from_here(
      NewMaxDepth,
      [[NewRow,NewCol,Direction]|PathSoFar],
      NewStatus,
      PathOut,
      StatusOut).

% ---
% Non-deterministically go into the four cardinal directions
% ---

explore(Row,Col,NewRow,Col,north) :- NewRow is Row-1.
explore(Row,Col,NewRow,Col,south) :- NewRow is Row+1.
explore(Row,Col,Row,NewCol,east)  :- NewCol is Col+1.
explore(Row,Col,Row,NewCol,west)  :- NewCol is Col-1.

% ---
% Check all conditions of exploration for the new position (Row,Col).
% Construct the map NewStatus with updated counts.
% ---

% verify(+Row,+Col,+Path,+Status:Dict,-NewStatus:Dict)

verify(Row,Col,Path,Status,_{ city_count: NewCityCount, delay_so_far: NewDelay, soldier_count: NewSoldierCount }) :-
   \+ terrain_mountain(Row,Col),
   new_delay(Row,Col,Status,NewDelay),
   new_citycount(Row,Col,Status,NewCityCount),
   new_soldiercount(Row,Col,Status,NewSoldierCount),
   new_steps(Path,_NewSteps).

new_delay(Row,Col,Status,NewDelay) :-
   delay(Row,Col,LocalDelay),
   const(timelimit,TimeLimit),
   NewDelay is Status.delay_so_far + LocalDelay,
   NewDelay =< TimeLimit.

new_citycount(Row,Col,Status,NewCityCount) :-
   terrain_city(Row,Col),
   !,
   NewCityCount is Status.city_count + 1,
   NewCityCount =< 1.
new_citycount(_Row,_Col,Status,Status.city_count).

new_soldiercount(Row,Col,Status,NewSoldierCount) :-
   soldier(Row,Col,Count),
   NewSoldierCount is Status.soldier_count + Count.

new_steps(Path,NewSteps) :-
   const(maxstep,MaxStep),
   length(Path,Length),
   NewSteps is Length + 1,
   NewSteps =< MaxStep.

% ---
% Printing the path on top of the terrain
% ---

print_terrain_with_path(Path) :-
   terrain_to_dict(MatrixDict),
   overlay_path(Path,MatrixDict,MatrixDictOut),
   print_terrain(MatrixDictOut).

overlay_path([],MatrixDict,MatrixDict).
overlay_path([[Row,Col,Direction]|RestPath],MatrixDictIn,MatrixDictOut) :-
   get_dict(Row,MatrixDictIn,RowDict),
   direction_symbol(Direction,Symbol),
   put_dict(Col,RowDict,Symbol,NewRowDict),
   put_dict(Row,MatrixDictIn,NewRowDict,MatrixDictMid),
   overlay_path(RestPath,MatrixDictMid,MatrixDictOut).

direction_symbol(north,'↑').
direction_symbol(south,'↓').
direction_symbol(east ,'→').
direction_symbol(west ,'←').
direction_symbol(start,'⊗').
% ---
% Printing the terrain
% ---

print_terrain :-
   terrain_to_dict(MatrixDict),
   print_terrain(MatrixDict).
print_terrain(MatrixDict) :-
   print_terrain(1,1,MatrixDict).
print_terrain(Row,Col,MatrixDict) :-
   get_dict(Row,MatrixDict,RowDict)
   ->
   (get_dict(Col,RowDict,X)
     ->
     (print_single_symbol(X),Col2 is Col+1,print_terrain(Row,Col2,MatrixDict))
     ;
     (format("\n",[]),Row2 is Row+1,print_terrain(Row2,1,MatrixDict))
   )
   ;
   true.

print_single_symbol(X) :-
   translate_terrain(X,T)
   -> format("~a ",[T])
   ;  format("~a ",[X]).

% ---
% Getting a Dict of the terrain
% ---

terrain_to_dict(MatrixDict) :-
   terrain(Matrix),
   rows_to_dict(1,Matrix,_{},MatrixDict).

% rows_to_dict(Row,Matrix,DictIn,DictOut)

rows_to_dict(_Row,[],Dict,Dict) :- !.
rows_to_dict(Row,[X|More],DictIn,DictOut) :-
   cols_to_dict(1,X,_{},SubDict),
   put_dict(Row,DictIn,SubDict,DictOut2),
   Row2 is Row+1,
   rows_to_dict(Row2,More,DictOut2,DictOut).

% cols_to_dict(Col,MatrixRow,DictIn,DictOut)

cols_to_dict(_Col,[],Dict,Dict) :- !.
cols_to_dict(Col,[X|More],DictIn,DictOut) :-
   put_dict(Col,DictIn,X,DictOut2),
   Col2 is Col+1,
   cols_to_dict(Col2,More,DictOut2,DictOut).

translate_terrain(p,'_').
translate_terrain(m,'∆').
translate_terrain(f,'⫛').
translate_terrain(c,'⊞').
translate_terrain(r,'∿').
