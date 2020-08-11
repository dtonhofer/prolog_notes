:- use_module('trivials.pl').

:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).
:- use_module(library('heavycarbon/support/fold_support.pl')).

recognize(In,Rest,DcgGoal,Out) :- 
   assertion(nonvar(In)),
   atom_codes(In,InCodes),
   % TODO: Direct if Out is instantiated
   compound_name_arguments(DcgGoalCompleted,DcgGoal,[OutCodes]),
   phrase(DcgGoalCompleted,InCodes,RestCodes),
   atom_codes(Out,OutCodes),   % this instantiates or unifies/compares
   atom_codes(Rest,RestCodes). % this instantiates or unifies/compares

maplist_java_id_start_char(ListIn,ListOut) :-
   maplist([C,T]>>reify(jpl_java_identifier_start_char(C),T),ListIn,ListOut).

maplist_java_id_part_char(ListIn,ListOut) :-
   maplist([C,T]>>reify(jpl_java_identifier_part_char(C),T),ListIn,ListOut).



:- begin_tests(digits).

test("digits 12345",true(Out == '12345'))
   :- recognize('12345','',jpl_nonempty_digits,Out).

test("digits 0",true(Out == '0'))
   :- recognize('0','',jpl_nonempty_digits,Out).

test("no digits",fail)
   :- recognize('',_,jpl_nonempty_digits,_).

test("not digits",fail)
   :- recognize('foo',_,jpl_nonempty_digits,_).

test("digits with rest",true([Out,Rest] == ['0123','foo']))
   :- recognize('0123foo',Rest,jpl_nonempty_digits,Out).

:- end_tests(digits).



:- debug(identifier_chars).



:- begin_tests(identifier_chars).

test("identifier start chars") :- 
   maplist_java_id_start_char(`$abcdefghijklöüä`,R),
   debug(identifier_chars,"Result: ~q",[R]),
   all_true(R).

test("identifier nonstart chars") :- 
   maplist_java_id_start_char(`.0123456789`,R),
   debug(identifier_chars,"Result: ~q",[R]),
   all_false(R).

test("identifier part chars") :- 
   maplist_java_id_part_char(`_0123456789$abcdefghijklöüä`,R),
   debug(identifier_chars,"Result: ~q",[R]),
   all_true(R).

:- end_tests(identifier_chars).


:- debug(java_identifier).


:- begin_tests(java_identifier).

test("id1") :- 
   recognize('an_identifier','',jpl_java_identifier,Out),
   debug("Recognized: ~q",[Out]).
 

:- end_tests(java_identifier).



