% ============================================================================
% Randomly select a key from a dict, where the dict values indicate
% the relative probability of selection by length-of-atom. 
% ============================================================================
%
% For example, with this dict, the keys will appear with a probability
% derived from the number of 'x' that appears in the atom defined as
% value:
%
% randomly_select(
%     sel{
%            alfa     : ''             % selection probability = 0
%           ,bravo    : xxxxx          % selection probability = 5/24
%           ,charlie  : xxx            % selection probability = 3/24
%           ,echo     : xxxxxxxxxx     % selection probability = 10/24
%           ,foxtrott : xxxxxx         % selection probability = 6/24
%        },
%     Selected).
%
% Switch on debugging output with:
%
% ?- debug(randomly_select).
%
% =============================================================================
% Running the tests: There should be a file "between_with_step.plt" nearby.
% Then, if the root directory for "code" is on the library path:
%
% ?- use_module(library('heavycarbon/utils/randomly_select.pl')).
% ?- load_test_files([]).
% ?- run_tests.
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================

:- module(heavycarbon_randomly_select,
   [
   randomly_select/2   % randomly_select(+SelDict,-Selected)
   ]).

:- use_module(library('heavycarbon/support/meta_helpers.pl')).      % unless, if_then_else
:- include(library('heavycarbon/support/throwme_nonmodular.pl')).   % better exceptions

% :- debug(randomly_select).

% ---
% Determining length of a list of '*' and summing over a list of such strings
% ---

nonspaces(N) --> [' '],!,nonspaces(N).                % skip spaces
nonspaces(N) --> [_]  ,!,nonspaces(Nm),{succ(Nm,N)}.  % anything else goes towards a total count
nonspaces(0) --> [].                                  % be greedy, terminate in last clause

% ---
% Transform a dict with "Keyword: xxxxxxx" entries into a list
% with pairs "Keyword-Cumul" where "Cumul" is the cumulative
% number of non-spaces found in all dict entry values that come
% before the dict entry "Keyword", plus the number of non-spaces
% for that entry itself. This gives an unnormalized cumulative
% probability function. The order in which the dict entries
% are inspected is unimportant!
% ---

build_cumulative_list(SelDict,CumulPairs,Total) :-
   dict_pairs(SelDict,_,Pairs), % order unimportant
   build_cumulative_list_2(Pairs,0,Fin,Total),
   CumulPairs = Fin.

count_nonspaces(S,Count) :-
   atom_chars(S,Chars), % S could also be a string
   phrase(nonspaces(Count),Chars).

build_cumulative_list_2([K-V|Pairs],TotalSoFar,Fin,TotalOut) :-
   count_nonspaces(V,Count),
   if_then_else(
      (Count > 0),
      (TotalNow is Count + TotalSoFar,
       Fin=[K-TotalNow|NewFin],
       build_cumulative_list_2(Pairs,TotalNow,NewFin,TotalOut)),
      (build_cumulative_list_2(Pairs,TotalSoFar,Fin,TotalOut))).

build_cumulative_list_2([],Total,[],Total).

% ---
% Once a random integer R has been chosen, just get first entry
% whose cumulative probability is higher or equal to R
% ---

retrieve([K-Cumul|CumulPairs],R,Selected) :-
   debug(randomly_select,"Now checking R = ~q against ~q-~q (i.e. R =< ~q?)",[R,K,Cumul,Cumul]),
   if_then_else(
      (R =< Cumul),
      (K = Selected),
      retrieve(CumulPairs,R,Selected)).

% ---
% The selection
% ---

randomly_select(SelDict,Selected) :-
   must_be(dict,SelDict),
   must_be(var,Selected),
   build_cumulative_list(SelDict,CumulPairs,Total), % TODO: this should be cached or tabled
   debug(randomly_select,"CumulPairs is ~q and the Total is ~q",[CumulPairs,Total]),
   unless(Total > 0,throwme(randomly_select,total_is_zero(SelDict))),
   random_between(1,Total,R),
   debug(randomly_select,"The random value is ~q (from [1,~q])",[R,Total]),
   retrieve(CumulPairs,R,Selected).

% ---
% Exception descriptor for throwme
% ---

exc_desc(randomly_select,total_is_zero(SelDict),
         _,
         contract(total_is_0,SelDict),
         "The total un-normalized probability over all cases is 0 - cannot select").


