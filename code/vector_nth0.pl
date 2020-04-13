% ===
% Sun 12 Apr 21:39:05 UTC 2020
% ---
% This is free and unencumbered software released into the public domain.
% 
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
% 
% For more information, please refer to <http://unlicense.org/>
% ===

% ===
% vector_nth0(+Indexes,+List,?Elements)
% vector_nth0(+Indexes,?List,+Elements)
% vector_nth0(?Indexes,+List,+Elements)
%
% A vector version of nth0/3
%
% Example, working forwards:
%
% [],[a,b,c,d,e,f],?Elements      --> []
% [0,1,2],[a,b,c,d,e,f],?Elements --> [a,b,c]
% [2,0,1],[a,b,c,d,e,f],'Elements --> [c,a,b]
% [3,3,3],[a,b,c,d,e,f],?Elements --> [d,d,d]
%
% Example, finding a matching List:
%
% [0,1,2],[_,_,_,_,_,_],[a,b,c] --> [0,1,2],[a,b,c,_,_,_],[a,b,c]
%
% Example, finding matching indexes:
%
% _9584,[a,b,c,b,e,c,g],[b,c,a] --> [1,2,0],[a,b,c,b,e,c,g],[b,c,a]
%                                   [1,5,0],[a,b,c,b,e,c,g],[b,c,a]
%                                   [3,2,0],[a,b,c,b,e,c,g],[b,c,a]
%                                   [3,5,0],[a,b,c,b,e,c,g],[b,c,a]
% ===

vector_nth0(Indexes,List,Elements) :-
   % Make sure at least one of "Indexes" and "Elements" is a "nonvar"
   % and that lengths are equal. This actually constructs lists of 
   % fresh variables if "Indexes" or "Elements" is "var"
   (nonvar(Elements) 
       -> (length(Elements,L),length(Indexes,L)) 
    ;
    nonvar(Indexes)
       -> (length(Indexes,L),length(Elements,L))
   ),
   maplist(
      ({List}/[Index,Element]>>nth0(Index,List,Element)),
      Indexes,
      Elements).

% ===
% Unit tests
% ===

:- begin_tests(vector_nth0).

% Extracting elements using known "Indexes"

test(extract_1)    :- vector_nth0([]     ,[a,b,c,d,e,f],R), R=[].
test(extract_2)    :- vector_nth0([0,1,2],[a,b,c,d,e,f],R), R=[a,b,c].
test(extract_3)    :- vector_nth0([2,0,1],[a,b,c,d,e,f],R), R=[c,a,b].
test(extract_3)    :- vector_nth0([3,3,3],[a,b,c,d,e,f],R), R=[d,d,d].

% Pulling values into "List" with fresh variables

test(pull)         :- vector_nth0([0,1,2],[A,B,C,_,_,_],[a,b,c]), A=a,B=b,C=c.

% Searching for matching element if "Indexes" contains fresh variables

test(search_1)     :- findall(Ixs,vector_nth0(Ixs,[a,b,c,d,e,f,g],[d,c,b]),SetIxs), SetIxs=[[3,2,1]].
test(search_2)     :- findall(Ixs,vector_nth0(Ixs,[a,b,c,b,e,c,g],[d,c,b]),SetIxs), SetIxs=[].
test(search_3)     :- findall(Ixs,vector_nth0(Ixs,[a,b,c,b,e,c,g],[b,c,a]),SetIxs), SetIxs=[[1,2,0], [1,5,0], [3,2,0], [3,5,0]].

% Edge case

test(edge_1)       :- vector_nth0([],[],R), R=[].

% At least one of Indexes and Elements must be nonvar(_).

test(free,[fail])  :- vector_nth0(_I,[a,b,c],_E).

% Out-of-bound tests

test(oob_1,[fail]) :- vector_nth0([1],[],_).
test(oob_2,[fail]) :- vector_nth0([-1],[a,b,c],_).
test(oob_3,[fail]) :- vector_nth0([3],[a,b,c],_).

:- end_tests(vector_nth0).

rt :- run_tests(vector_nth0).

