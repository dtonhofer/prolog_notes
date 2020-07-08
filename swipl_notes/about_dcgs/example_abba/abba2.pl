:- use_module(library(clpfd)).
:- use_module(library(pcre)).
:- debug(dcg).

start(AB,BA,[a_eater|Path])    --> [a], a_eater(AB,BA,Path).
start(AB,BA,[b_eater|Path])    --> [b], b_eater(AB,BA,Path).
start(AB,BA,[start(C)|Path])   --> [C], { \+ memberchk(C,[a,b]) }, start(AB,BA,Path).
start(0,0,[end])               --> []. % not "must be empty" but "can be empty"

a_eater(AB,BA,[a_eater|Path])  --> [a],a_eater(AB,BA,Path).
a_eater(AB,BA,[ab_found|Path]) --> [b],ab_found(AB,BA,Path).
a_eater(AB,BA,[start(C)|Path]) --> [C], { \+ memberchk(C,[a,b]) }, start(AB,BA,Path).
a_eater(0,0,[end])             --> []. % not "must be empty" but "can be empty"

b_eater(AB,BA,[b_eater|Path])  --> [b],a_eater(AB,BA,Path).
b_eater(AB,BA,[ba_found|Path]) --> [a],ba_found(AB,BA,Path).
b_eater(AB,BA,[start(C)|Path]) --> [C], { \+ memberchk(C,[a,b]) }, start(AB,BA,Path).
b_eater(0,0,[end])             --> []. % not "must be empty" but "can be empty"

ab_found(AB,BA,[start|Path])   --> [], start(ABn,BA,Path),{AB #= ABn+1}. 
ba_found(AB,BA,[start|Path])   --> [], start(AB,BAn,Path),{BA #= BAn+1}. 

automaton_1_parse(X,AB,BA,Path,Rest) :- atom_chars(X,Cs),phrase(start(AB,BA,Path),Cs,Rest).

:- begin_tests(dcg_chars).

test(a1_0,[true(T)])        :- automaton_1_parse(''          ,AB,BA,_Path,[]),T = ([AB,BA] == [0,0]).
test(a1_1,[true(T),nondet]) :- automaton_1_parse('abbaabbaba',AB,BA,_Path,[]),T = ([AB,BA] == [2,3]).
test(a1_2,[true(T),nondet]) :- automaton_1_parse('yabybayyyy',AB,BA,_Path,[]),T = ([AB,BA] == [1,1]).
test(a1_3,[true(T),nondet]) :- automaton_1_parse('yyyyyyyyya',AB,BA,_Path,[]),T = ([AB,BA] == [0,0]).
test(a1_4,[true(T),nondet]) :- automaton_1_parse('yyyyyyybaa',AB,BA,_Path,[]),T = ([AB,BA] == [0,1]).
test(a1_5,[true(T),nondet]) :- automaton_1_parse('yyyyyyyba' ,AB,BA,_Path,[]),T = ([AB,BA] == [0,1]).
test(a1_6,[true(T),nondet]) :- automaton_1_parse('yaybyayba' ,AB,BA,_Path,[]),T = ([AB,BA] == [0,1]).
test(a1_7,[true(T),nondet]) :- automaton_1_parse('yaabby'    ,AB,BA,_Path,[]),T = ([AB,BA] == [1,0]).

test(0,[true(T)])        :- automaton_1_parse(''          ,AB,BA,_Path,[]),T = ([AB,BA] == [0,0]).
test(1,[true(T),nondet]) :- automaton_1_parse('abbaabbaba',AB,BA,_Path,[]),T = ([AB,BA] == [2,3]).
test(2,[true(T),nondet]) :- automaton_1_parse('yabybayyyy',AB,BA,_Path,[]),T = ([AB,BA] == [1,1]).
test(3,[true(T),nondet]) :- automaton_1_parse('yyyyyyyyya',AB,BA,_Path,[]),T = ([AB,BA] == [0,0]).
test(4,[true(T),nondet]) :- automaton_1_parse('yyyyyyybaa',AB,BA,_Path,[]),T = ([AB,BA] == [0,1]).
test(5,[true(T),nondet]) :- automaton_1_parse('yyyyyyyba' ,AB,BA,_Path,[]),T = ([AB,BA] == [0,1]).
test(6,[true(T),nondet]) :- automaton_1_parse('yaybyayba' ,AB,BA,_Path,[]),T = ([AB,BA] == [0,1]).
test(7,[true(T),nondet]) :- automaton_1_parse('yaabby'    ,AB,BA,_Path,[]),T = ([AB,BA] == [1,0]).

:- end_tests(dcg_chars). 

rt(dcg_chars) :- run_tests(dcg_chars).
