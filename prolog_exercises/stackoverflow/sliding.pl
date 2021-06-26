% https://stackoverflow.com/questions/67642302/prolog-for-eight-puzzle

% ---
% The current "state" in the search space is represented as a list of list. 
% The positions in this matrix are given by column and row coordinates,
% each ranging from 0 to 2:
% 
%
% +--------------> Col (0,1,2)
% |
% |   [[A0,B0,C0],
% |    [D0,E0,F0],
% |    [G0,H0,I0]]
% V
% Row (0,1,2)
%
% If a matrix position shall represent the "empty cell", we write empty list
% at that position (because it looks nice), otherwise we write one of the
% integers 0..7
% ---

target( [[ [] ,0 ,1],   
         [  2 ,3 ,4],
         [  5 ,6 ,7]]).

from( [[6 ,1  ,3],
       [4 ,[] ,5],
       [7 ,2  ,0]]).

% A *backtrackable* predicate which proposes a new position (RowNew,ColNew)
% for the hole at position (Row,Col). The hole is moved in direction 
% MoveDirection

% new_hole_position(Row,Col,RowNew,ColNew,MoveDirection)

new_hole_position(Row,Col,RowNew,Col,down)  :- Row < 2, RowNew is Row+1. 
new_hole_position(Row,Col,RowNew,Col,up)    :- Row > 0, RowNew is Row-1. 
new_hole_position(Row,Col,Row,ColNew,right) :- Col < 2, ColNew is Col+1.
new_hole_position(Row,Col,Row,ColNew,left)  :- Col > 0, ColNew is Col-1.

% Pick the value at (Row,Col) from MatrixIn into ValOld and
% Put ValNew at (Row,Col), giving MatrixOut. This is used to
% generate a new state from an existing state and a "hole move".

pick_and_put_matrix(Row,Col,MatrixIn,ValOld,ValNew,MatrixOut) :-
   pick_and_put(Row,MatrixIn,RowlistOld,RowlistNew,MatrixOut),
   pick_and_put(Col,RowlistOld,ValOld,ValNew,RowlistNew).

pick_and_put(Index,ListIn,ValOld,ValNew,ListOut) :-
   length(Prefix,Index), 
   append([Prefix,[ValOld],Suffix],ListIn),
   append([Prefix,[ValNew],Suffix],ListOut),
   !.

% Moving the hole from (Row,Col) to (RowNew,ColNew)

move_hole(Row,Col,RowNew,ColNew,MatrixIn,MatrixOut) :-
   pick_and_put_matrix(Row,Col,MatrixIn,[],Val,MatrixMid),
   pick_and_put_matrix(RowNew,ColNew,MatrixMid,Val,[],MatrixOut).

% Find out where the hole is in MatrixIn as we don't
% keep track of that information.

cur_hole_position(Row,Col,MatrixIn) :-
   nth0(Row,MatrixIn,RowList),
   cur_hole_position_in_row(Col,RowList),!.
   
cur_hole_position_in_row(Col,RowList) :- 
   nth0(Col,RowList,[]).

% For showing off, the number of states visited is counted in
% a thread-local variable that is non-backtrackably incremented.

nb_inc_counter :-
  nb_getval(counter,X),
  XX is X+1,
  nb_setval(counter,XX).
  
% The search proper. Perform a single move from one state (matrix) 
% to the next state (matrix)
%
% move(+CurrentState,+GoalState,
%      -SolutionAsGrowingOpenListToWhichOneAppends
%      +StatesOnPathSoAsToNotVisitAStateTwiceToWhichOnePrepends,
%      +DepthCountdownForIterativeDeepening)

move(Matrix,Matrix,[],_,_) :- !.
move(MatrixIn,MatrixTarget,[MatrixMid|Moves],MatrixesOnPath,Depth) :-
   Depth > 1, 
   nb_inc_counter,
   cur_hole_position(Row,Col,MatrixIn),
   new_hole_position(Row,Col,RowNew,ColNew,_MoveDirection),
   move_hole(Row,Col,RowNew,ColNew,MatrixIn,MatrixMid),
   \+ member(MatrixMid,MatrixesOnPath),
   SmallerDepth is Depth-1,
   move(MatrixMid,MatrixTarget,Moves,[MatrixMid|MatrixesOnPath],SmallerDepth).

% Printout curclicues

print_and_reset_counter :-
   nb_getval(counter,C),
   (C>0 
    -> format("Examined ~d positions~n",[C]) 
    ;  true),
   nb_setval(counter,0).

format_moves([Matrix],_) :-
   format_matrix(Matrix).
format_moves([Matrix,Matrix2|Moves],Index) :-
   format_matrix(Matrix),
   format("Move ~d~n",[Index]),
   Index2 is Index+1,
   format_moves([Matrix2|Moves],Index2).

format_matrix([[A,B,C],[D,E,F],[G,H,I]]) :-
   enlarge(A,AE),
   enlarge(B,BE),
   enlarge(C,CE),
   enlarge(D,DE),
   enlarge(E,EE),
   enlarge(F,FE),
   enlarge(G,GE),
   enlarge(H,HE),
   enlarge(I,IE),
   format("+--------+~n",[]),
   format("|~s,~s,~s|~n",[AE,BE,CE]),
   format("|~s,~s,~s|~n",[DE,EE,FE]),
   format("|~s,~s,~s|~n",[GE,HE,IE]),
   format("+--------+~n",[]).
   
enlarge(X,XE) :-
   format(string(S)," ~q",[X]),
   sub_string(S,_,2,0,XE).

% "Main" predicate.

run(Moves) :- 
   from(MatrixFrom),
   target(MatrixTarget),  
   nb_setval(counter,0),
   between(1,30,MaxDepth), % backtrackable; iterative deepening
   print_and_reset_counter,
   format("Maximum depth is ~d~n",[MaxDepth]),
   move(MatrixFrom,MatrixTarget,Moves,[MatrixFrom],MaxDepth),
   announce_success([MatrixFrom|Moves]).

announce_success(Moves) :-   
   length(Moves,Length),
   AdjustedLength is Length-1,
   nb_getval(counter,C),
   format("Found a solution of ~d moves by examination of ~d positions.~n",[AdjustedLength,C]),
   format_moves(Moves,1).
