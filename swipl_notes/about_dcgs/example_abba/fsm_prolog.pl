:- use_module(library(clpfd)).
:- use_module(library(pcre)).
:- debug(dcg).

% ---
% merge(+XX, +Chars, +PiecesIn, -PiecesOut)
% ---
% XX            = 'ab' or 'ba', it will be prepended to 'PiecesIn' to give 'PiecesOut'
% Chars         = List of characters that have been discarded looking for 'ab' or 'ba',
%                 possibly empty. The characters come in reverse order of encounter.
%                 If nonempty, the list is reversed, fsed into an atom, and that atom 
%                 in prepended to 'PiecesIn' to give 'PiecesOut'.
% FuturePieces  = All the "pieces" (i.e. the 'ab', 'ba' and discarded character runs)
%                 that will be found in the future.
% PiecesOut     = The completed "FuturePieces" given to the caller ("the return value")

merge_cs(XX,[C|Cs],FPs,[A,XX|FPs]) :- reverse([C|Cs],Csr), atom_chars(A,Csr).
merge_cs(XX,[]    ,FPs,[  XX|FPs]).

merge_cs([C|Cs],[A]) :- reverse([C|Cs],Csr), atom_chars(A,Csr).
merge_cs([]    ,[]).

% ---
% The DCG
% ---

start(AB,BA,Pieces)          --> [], anything(AB,BA,Pieces,[]).

% The "!" both commits, dumping unneeded choicepoints and 
% makes sure that rule 3, as the fallback rule, doesn't eat an 'a'
% that is actually followed by a 'b' or a 'b' that is actually 
% followed by an 'a'.
% Finally, rule 4 is the rule only chosen if there is no 
% input left, but it also matches the case where there is 
% input left, but nothing is used. If rule 3 had no "!", ruke
% 4 might be chosen on backtracking even with nonempty input,
% but as phrase/3 demands that the remaining input be [] for
% parse success, that would not lead to success.

anything(AB,BA,PiecesOut,Cs) --> [a,b],!, {AB #= ABn+1, merge_cs(ab,Cs,FuturePieces,PiecesOut)}, start(ABn,BA,FuturePieces).
anything(AB,BA,PiecesOut,Cs) --> [b,a],!, {BA #= BAn+1, merge_cs(ba,Cs,FuturePieces,PiecesOut)}, start(AB,BAn,FuturePieces).
anything(AB,BA,Pieces,Cs)    --> [C]  ,!, anything(AB,BA,Pieces,[C|Cs]).
anything(0,0,Pieces,Cs)      --> [], {merge_cs(Cs,Pieces)}.

% ---
% Calling the DCG
% ---

fsm_parse(Str,AB,BA,Dashed,Rest) :- 
   atom_chars(Str,Cs),
   phrase(start(AB,BA,Pieces),Cs,Rest),
   atomic_list_concat(Pieces, '-', Dashed).

% ---
% Tests
% ---

:- begin_tests(dcg_chars).

test(fsm_00,[true(T),nondet]) :- fsm_parse(''                ,AB,BA,D,[]), T = ([AB,BA,D] == [0,0,'']).
test(fsm_01,[true(T),nondet]) :- fsm_parse('yyabyybayy'      ,AB,BA,D,[]), T = ([AB,BA,D] == [1,1,'yy-ab-yy-ba-yy']).
test(fsm_02,[true(T),nondet]) :- fsm_parse('yyabbayyabaabaab',AB,BA,D,[]), T = ([AB,BA,D] == [4,1,'yy-ab-ba-yy-ab-a-ab-a-ab']).
test(fsm_03,[true(T),nondet]) :- fsm_parse('abbaayybbaba'    ,AB,BA,D,[]), T = ([AB,BA,D] == [1,3,'ab-ba-ayyb-ba-ba']).
test(fsm_04,[true(T),nondet]) :- fsm_parse('abbaabbaba'      ,AB,BA,D,[]), T = ([AB,BA,D] == [2,3,'ab-ba-ab-ba-ba']).
test(fsm_05,[true(T),nondet]) :- fsm_parse('abbayyabbaba'    ,AB,BA,D,[]), T = ([AB,BA,D] == [2,3,'ab-ba-yy-ab-ba-ba']).
test(fsm_06,[true(T),nondet]) :- fsm_parse('abbaabbaba'      ,AB,BA,D,[]), T = ([AB,BA,D] == [2,3,'ab-ba-ab-ba-ba']).
test(fsm_07,[true(T),nondet]) :- fsm_parse('yabybayyyy'      ,AB,BA,D,[]), T = ([AB,BA,D] == [1,1,'y-ab-y-ba-yyyy']).
test(fsm_08,[true(T),nondet]) :- fsm_parse('yyyyyyyyya'      ,AB,BA,D,[]), T = ([AB,BA,D] == [0,0,'yyyyyyyyya']).
test(fsm_09,[true(T),nondet]) :- fsm_parse('yyyyyyybaa'      ,AB,BA,D,[]), T = ([AB,BA,D] == [0,1,'yyyyyyy-ba-a']).
test(fsm_10,[true(T),nondet]) :- fsm_parse('yyyyyyyba'       ,AB,BA,D,[]), T = ([AB,BA,D] == [0,1,'yyyyyyy-ba']).
test(fsm_11,[true(T),nondet]) :- fsm_parse('yaybyayba'       ,AB,BA,D,[]), T = ([AB,BA,D] == [0,1,'yaybyay-ba']).
test(fsm_12,[true(T),nondet]) :- fsm_parse('yaabby'          ,AB,BA,D,[]), T = ([AB,BA,D] == [1,0,'ya-ab-by']).

:- end_tests(dcg_chars). 

rt(dcg_chars) :- run_tests(dcg_chars).
