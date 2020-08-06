:- module(heavycarbon_randomly_select,
          [
             randomly_select/2   % randomly_select(+SelDict,?Selected)
          ]).

:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).
:- include(library('heavycarbon/support/throwme_nonmodular.pl')).

% ============================================================================
% Randomly select a key from a dict, where the dict values indicate
% relative probability of selection by a string of stars
%
% Use this module with:
%
% :- use_module(library('heavycarbon/utils/randomly_select.pl')).
%
% Switch on debugging output:
%
% ?- debug(randomly_select).
%
% For example:
/*

randomly_select(
     _{
       alfa     : ''               % selection probability = 0
      ,bravo    : '*****'          % selection probability = 5/24
      ,charlie  : '***'            % selection probability = 3/24
      ,echo     : '**********'     % selection probability = 10/24
      ,foxtrott : '******'},       % selection probability = 6/24
     Selected).

*/
%
% with printout:
%
% CumulPairs is [borked_list-3,compound-13,dict-19,open_list-24] and the Total is 24
% The random value is 15 (from [1,24])
% Now checking R = 15 against borked_list-3 (i.e. R =< 3?)
% Now checking R = 15 against compound-13 (i.e. R =< 13?)
% Now checking R = 15 against dict-19 (i.e. R =< 19?)
% ============================================================================

% ---
% Determining length of a list of '*' and summing over a list of such strings
% ---

stars(N) --> ['*'],!,stars(Nm),{succ(Nm,N)}.
stars(N) --> [_],!,stars(N).
stars(0) --> [].

% ---
% Transform a dict with "keyword:'*****'" entries into a list
% with "keyword-cumul" entries where "cumul" is the cumulative
% number of stars up and including "keyword".
% ---

build_cumulative_list(SelDict,CumulPairs,TotalStars) :-
   dict_pairs(SelDict,_,Pairs), % order unimportant
   build_cumulative_list_2(Pairs,0,Fin,TotalStars),
   CumulPairs = Fin.

count_stars(S,Count) :-
   atom_chars(S,Chars),
   phrase(stars(Count),Chars).

build_cumulative_list_2([K-V|Pairs],TotalSoFar,Fin,TotalOut) :-
   count_stars(V,Count),
   if_then_else(
      (Count > 0),
      (TotalNow is Count + TotalSoFar,
       Fin=[K-TotalNow|NewFin],
       build_cumulative_list_2(Pairs,TotalNow,NewFin,TotalOut)),
      (build_cumulative_list_2(Pairs,TotalSoFar,Fin,TotalOut))).

build_cumulative_list_2([],TotalShunt,[],TotalShunt).

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
   assertion(is_dict(SelDict)),
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


