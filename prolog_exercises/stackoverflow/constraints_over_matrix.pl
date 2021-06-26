% https://stackoverflow.com/questions/67061653/prolog-combined-predicate-failed

/*

?- trial_run(S),print_solution(S).
---
14 = 2 + 3 + 9
15 = 1 + 6 + 8
28 = 4 * 1 * 7
---
S = [[row(14,add,[2,3,9]),row(15,add,[1,6,8]),row(28,mult,[4,1,7])]].

*/

:- use_module(library(clpfd)).

% ---
% Make sure you see all the information in a list printed at the 
% toplevel, i.e. tell the toplevel to not elide large lists (print
% them in full, don't use '...')
% ---

my_print :-
   Depth=100,
   Options=[quoted(true), portray(true), max_depth(Depth), attributes(portray)],
   set_prolog_flag(answer_write_options,Options),
   set_prolog_flag(debugger_write_options,Options).
   
:- my_print.   

% ---
% Set up constraint: all the elements in List, multiplied
% together, must yield Result. If the List is empty, the
% Result must be 1.
% ---

multiply(Factors,Product):-
    multiply_2(Factors,1,Product).

multiply_2([Factor|Factors],PartialProduct,Product):-
    PartialProduct2 #= PartialProduct * Factor,
    multiply_2(Factors,PartialProduct2,Product).

multiply_2([],Product,Product).

solve_multiply([Product|Factors]):-
    Factors ins 1..9,
    multiply(Factors,Product),
    all_distinct(Factors).
    
solve_sum([Sum|Terms]):-
    Terms ins 1..9,
    all_distinct(Terms),
    sum(Terms, #=, Sum).
    
print_solution([]).

print_solution([Labeling|Labelings]) :-
   format("---~n"),
   print_rows_of_solution(Labeling),
   format("---~n"),
   print_solution(Labelings).

print_rows_of_solution([]).

print_rows_of_solution([row(Result,Op,RowValues)|Rows]) :-
   print_row(Result,RowValues,Op),
   print_rows_of_solution(Rows).

print_row(Result,[RowEntry|RowEntries],Op) :-
   format("~q = ~q",[Result,RowEntry]),
   print_row_2(RowEntries,Op).
   
print_row_2([RowEntry|RowEntries],Op) :-
   ((Op == mult) -> OpChar = '*'
   ; (Op == add)  -> OpChar = '+'
   ; OpChar = '?'),
   format(" ~q ~q",[OpChar,RowEntry]),
   print_row_2(RowEntries,Op).
   
print_row_2([],_) :-
   format("~n").
   
solve_row_sum_or_multiply([],[]).

solve_row_sum_or_multiply([Row|MoreRows],[mult|Ops]) :-
   Row = [X|_Xs],
   X >= 25,   
   solve_multiply(Row), % we now have imposed a product constraint on the current Row
   solve_row_sum_or_multiply(MoreRows,Ops).

solve_row_sum_or_multiply([Row|MoreRows],[add|Ops]) :-
   Row = [X|_Xs],
   X < 25,
   solve_sum(Row), % we now have imposed a sum constraint on the current Row
   solve_row_sum_or_multiply(MoreRows,Ops).
 
solve_row_sum_or_multiply([Row|MoreRows],[mult|Ops]) :-
   Row = [X|_Xs],
   X < 25,
   solve_multiply(Row), % alternatively, we now have imposed a product constraint on the current Row   
   solve_row_sum_or_multiply(MoreRows,Ops).

trial_run(Solutions) :-
   all_distinct([X11,X12,X13,X21,X22,X23,X31,X33]),
   X32=1,
   X11 #=< X12, X12 #=< X13,
   X21 #=< X22, X22 #=< X23,
   X31 #=< X33,
   % multiple solutions solving x labeling may exist; collect them all
   bagof(
      [
         row(14,Op1,[X11,X12,X13]),
         row(15,Op2,[X21,X22,X23]),
         row(28,Op3,[X31,X32,X33])
      ],
      (
         solve_row_sum_or_multiply( [[14,X11,X12,X13],[15,X21,X22,X23],[28,X31,X32,X33]], [Op1,Op2,Op3] ),
         label([X11,X12,X13,X21,X22,X23,X31,X32,X33])
      ),   
      Solutions).
    
% ---
% Testing
% ---

:- begin_tests(multiply).

test(1) :- 
   multiply([X,Y,Z],6),
   [X,Y,Z] ins 1..9,
   X #=< Y, Y #=< Z, 
   bagof([X,Y,Z],label([X,Y,Z]),Bag),
   sort(Bag,BagS),
   assertion(BagS == [[1, 1, 6], [1, 2, 3]]).

test(2) :- 
   multiply([],Result),
   assertion(Result == 1).

test(3) :- 
   multiply([X,Y],3),
   [X,Y] ins 1..9,
   X #=< Y, 
   bagof([X,Y],label([X,Y]),Bag),
   sort(Bag,BagS),
   assertion(BagS == [[1,3]]).

test(4) :-
   solve_multiply([6,X,Y,Z]),
   bagof([X,Y,Z],label([X,Y,Z]),Bag),
   sort(Bag,BagS),
   assertion(BagS == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]).
   
test(5) :-
   solve_multiply([362880,F1,F2,F3,F4,F5,F6,F7,F8,F9]),
   F1 #=< F2,
   F2 #=< F3,
   F3 #=< F4,
   F4 #=< F5,
   F5 #=< F6,
   F6 #=< F7,
   F7 #=< F8,
   F8 #=< F9,
   bagof([F1,F2,F3,F4,F5,F6,F7,F8,F9],label([F1,F2,F3,F4,F5,F6,F7,F8,F9]),Bag),
   sort(Bag,BagS),
   assertion(BagS == [[1,2,3,4,5,6,7,8,9]]).

test(6,fail) :-
   solve_multiply([-1,_X,_Y,_Z]).
   
:- end_tests(multiply).
    
% ---
% Testing
% ---

:- begin_tests(sum).

test(0) :- 
   solve_sum([0]).
   
test(1) :- 
   solve_sum([6,X,Y,Z]),
   X #=< Y, Y #=< Z, 
   bagof([X,Y,Z],label([X,Y,Z]),Bag),
   sort(Bag,BagS),
   assertion(BagS == [[1, 2, 3]]).

test(2) :- 
   solve_sum([9,X,Y,Z]),
   X #=< Y, Y #=< Z, 
   bagof([X,Y,Z],label([X,Y,Z]),Bag),
   sort(Bag,BagS),
   assertion(BagS == [[1,2,6],[1,3,5],[2,3,4]]).

test(3,fail) :- 
   solve_sum([1,_X,_Y,_Z]).
   
:- end_tests(sum).   









