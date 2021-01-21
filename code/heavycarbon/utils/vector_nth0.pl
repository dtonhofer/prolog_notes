% ==============================================================================
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
% ==============================================================================
% Running the tests: There should be a file "vector_nth0.plt" nearby.
% Then, if the root directory for "code" is on the library path:
%
% ?- use_module(library('heavycarbon/utils/vector_nth0.pl')).
% ?- load_test_files([]).
% ?- run_tests.
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% Changes:
% First version Sun 12 Apr 21:39:05 UTC 2020
% =============================================================================

:- module(vector_nth0,
          [
          vector_nth0/3
          ]).

vector_nth0(Indexes,List,Elements) :-
   % Make sure at least one of "Indexes" and "Elements" is a "nonvar"
   % and that lengths are equal. This actually **constructs** lists of
   % fresh variables if "Indexes" or "Elements" is "var"
   (nonvar(Elements)
       -> (length(Elements,L),length(Indexes,L))
    ;
    nonvar(Indexes)
       -> (length(Indexes,L),length(Elements,L))
   ),
   maplist(
      ({List}/[Index,Element]>>nth0(Index,List,Element)), % library(yall) use!
      Indexes,
      Elements).

