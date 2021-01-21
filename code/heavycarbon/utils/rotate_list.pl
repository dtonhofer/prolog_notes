% =============================================================================
% rotate_list(+List,+N,?Rotated): Rotates "List" by "N" positions.
%
% N=0 : no rotation
% N>0 : Rotate "leftwards": move a prefix of (N mod Length) list items to
%       the back of the list to form the result.
% N<0 : Rotate "rightwards": move a suffix of (abs(N) mod Length) list items
%       to the front of the list to form the result (however, this is done
%       completely the same way as for the case N>0)

% Inspired by a discussion on
%
%   http://computer-programming-forum.com/55-prolog/358ecf5f07f2de46.htm
%
% although the solution below, which uses two difference lists and
% a single scan of the original list (and no append/3), was not proposed.
%
% Furthermore, from
%
%   https://stackoverflow.com/questions/10255703/how-to-rotate-lists-in-prolog
%
% When you just want to rotate by 1 position:
%
%   rotatelist([H|T], R) :- append(T, [H], R).
%
% Works in "both directions"
%
%   ?- rotatelist([a,b,c],R).
%   R = [b, c, a].
%   ?- once(rotatelist(R,[a,b,c])).
%   R = [c, a, b].
%
% =============================================================================
% Running the tests: There should be a file "rotate_list.plt" nearby.
% Then, if the root directory for "code" is on the library path:
%
% ?- use_module(library('heavycarbon/utils/rotate_lits.pl')).
% ?- load_test_files([]).
% ?- run_tests.
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% 2020-04: Version 1
% 2021-01: Code reviewed to apply my skills in open difference lists
% =============================================================================

:- module(rotate_list,
          [
          rotate_list/3
          ]).

% rotate the empty list

rotate_list([],_,[]) :- !.

% rotate the nonempty list

rotate_list(List,N,Rotated) :-
   length(List,Length),
   Length>0,
   !,
   PrefixLength is N mod Length,
   Tip1=Fin1, % Difflist of open list to collect prefix to become suffix
   Tip2=Fin2, % Difflist of open list to collect suffix to become prefix (and solution)
   copy_list(List,PrefixLength,Tip1,Fin1,Tip2,Fin2),
   Rotated=Tip2. % this is either output or an "accepting" of a provided Rotated


% If the "PrefixCounter" is > 0, continue moving down the
% (input) "List", while append every head element of "List" to the
% first open difference list given by "Tip1-Fin1" (it will become
% the new suffix), leaving the second difference list (as yet
% empty, it will become the new prefix), given by "Tip2-Fin2", alone.

                           %    Collecting       Empty open difflist
                           %       prefix         to collect suffix
                           %   to become suffix   to become prefix
                           %    Tip1     Fin1       Tip2   Fin2
copy_list([L|Ls],PrefixCounter, Tip1, [L|NewFin1] , Tip2 , Fin2) :-
   PrefixCounter>0,
   !,
   succ(PrefixCounterNext,PrefixCounter),
   copy_list(Ls,PrefixCounterNext,Tip1,NewFin1,Tip2,Fin2).

% If the "PrefixCounter" is 0, continue moving down the
% (input) "List", but now append every head element of "List" to the
% second open difference list given by "Tip2-Fin2" (it will become
% the new prefix), leaving the first open difference list (the
% collected prefix), given by "Tip1-Fin1", alone.

                 %  Collected       Becomes new
                 %    Prefix          Prefix
                 %  Tip1   Fin1   Tip2      Fin2
copy_list([L|Ls],0, Tip1 , Fin1 , Tip2 , [L|NewFin2]) :-
   !,
   copy_list(Ls,0,Tip1,Fin1,Tip2,NewFin2).

% If we hit the end of the (input) "List", then we are done!
% The tip of the first open difference list (the collected prefix),
% given by "Tip1", is unified with the fin of the second open
% difference list, "Fin2" (here, already unified), thus appending
% the collected prefix to the list rooted in "Tip2". Simultaneously,
% the fin of the collected prefix "Fin1" is unified with [], closing
% the prefix, and closing the final result list, now rooted in Tip2.

             %  Collected    Becomes new
             %    Prefix       Prefix
             %  Tip1  Fin1   Tip2   Fin2
copy_list([],0, Tip1 , []  ,  _   , Tip1).


