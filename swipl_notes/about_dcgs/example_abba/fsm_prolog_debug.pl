:- use_module(library(clpfd)).

:- debug(dcg). % switch on debugging output for topic "dcg"

% ---
% merge(+XX, +Chars, +PiecesIn, -PiecesOut)
% ---

merge_cs(XX,[C|Cs],FPs,[A,XX|FPs]) :- reverse([C|Cs],Csr), atom_chars(A,Csr).
merge_cs(XX,[]    ,FPs,[  XX|FPs]).

merge_cs([C|Cs],[A]) :- reverse([C|Cs],Csr), atom_chars(A,Csr).
merge_cs([]    ,[]).

ex_debug(Format,Args,HiddenLeft,HiddenRight). 
/*
 :-  
   atomic_list_concat(["~q ~q :", Format],'',NewFormat),
   debug(dcg,NewFormat,[HiddenLeft,HiddenRight|Args]).
*/

% ---
% The DCG
% ---

start(AB,BA,Pieces) --> 
   [], 
   % call(ex_debug("start->anything",[])),
   { 
      debug(dcg,"start->anything",[])
   }, 
   anything(AB,BA,Pieces,[]).

anything(AB,BA,PiecesOut,Cs) --> 
   [a,b], !,
   { 
      debug(dcg,"ab: anything->start",[]), 
      AB #= ABn+1, 
      merge_cs(ab,Cs,FuturePieces,PiecesOut)
   }, 
   start(ABn,BA,FuturePieces).

anything(AB,BA,PiecesOut,Cs) --> 
   [b,a], !,
   {
      debug(dcg,"ba: anything->start",[]), 
      BA #= BAn+1, 
      merge_cs(ba,Cs,FuturePieces,PiecesOut)
   },
   start(AB,BAn,FuturePieces).

anything(AB,BA,Pieces,Cs) -->
   [C], !,
   {
      debug(dcg,"~q: anything->anything",[C])
   },
   anything(AB,BA,Pieces,[C|Cs]).

anything(0,0,Pieces,Cs) -->
   [],
   {
      debug(dcg,"anything->end",[]),
      merge_cs(Cs,Pieces)
   }.

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
test(fsm_01,[true(T),nondet]) :- fsm_parse('bab'             ,AB,BA,D,[]), T = ([AB,BA,D] == [0,1,'ba-b']).
test(fsm_02,[true(T),nondet]) :- fsm_parse('aba'             ,AB,BA,D,[]), T = ([AB,BA,D] == [1,0,'ab-a']).
test(fsm_03,[true(T),nondet]) :- fsm_parse('yyabyybayy'      ,AB,BA,D,[]), T = ([AB,BA,D] == [1,1,'yy-ab-yy-ba-yy']).
test(fsm_04,[true(T),nondet]) :- fsm_parse('yyabbayyabaabaab',AB,BA,D,[]), T = ([AB,BA,D] == [4,1,'yy-ab-ba-yy-ab-a-ab-a-ab']).
test(fsm_05,[true(T),nondet]) :- fsm_parse('abbaayybbaba'    ,AB,BA,D,[]), T = ([AB,BA,D] == [1,3,'ab-ba-ayyb-ba-ba']).
test(fsm_06,[true(T),nondet]) :- fsm_parse('abbaabbaba'      ,AB,BA,D,[]), T = ([AB,BA,D] == [2,3,'ab-ba-ab-ba-ba']).
test(fsm_07,[true(T),nondet]) :- fsm_parse('abbayyabbaba'    ,AB,BA,D,[]), T = ([AB,BA,D] == [2,3,'ab-ba-yy-ab-ba-ba']).
test(fsm_08,[true(T),nondet]) :- fsm_parse('abbaabbaba'      ,AB,BA,D,[]), T = ([AB,BA,D] == [2,3,'ab-ba-ab-ba-ba']).
test(fsm_09,[true(T),nondet]) :- fsm_parse('yabybayyyy'      ,AB,BA,D,[]), T = ([AB,BA,D] == [1,1,'y-ab-y-ba-yyyy']).
test(fsm_10,[true(T),nondet]) :- fsm_parse('yyyyyyyyya'      ,AB,BA,D,[]), T = ([AB,BA,D] == [0,0,'yyyyyyyyya']).
test(fsm_11,[true(T),nondet]) :- fsm_parse('yyyyyyybaa'      ,AB,BA,D,[]), T = ([AB,BA,D] == [0,1,'yyyyyyy-ba-a']).
test(fsm_12,[true(T),nondet]) :- fsm_parse('yyyyyyyba'       ,AB,BA,D,[]), T = ([AB,BA,D] == [0,1,'yyyyyyy-ba']).
test(fsm_13,[true(T),nondet]) :- fsm_parse('yaybyayba'       ,AB,BA,D,[]), T = ([AB,BA,D] == [0,1,'yaybyay-ba']).
test(fsm_14,[true(T),nondet]) :- fsm_parse('yaabby'          ,AB,BA,D,[]), T = ([AB,BA,D] == [1,0,'ya-ab-by']).

:- end_tests(dcg_chars). 

rt(dcg_chars) :- run_tests(dcg_chars).
