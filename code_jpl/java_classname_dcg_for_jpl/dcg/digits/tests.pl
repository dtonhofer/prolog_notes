:- use_module('trivials.pl').
:- use_module('entityname.pl').

:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).
:- use_module(library('heavycarbon/support/fold_support.pl')).

recognize_atom_to_atom(In,Rest,DcgGoal,Out) :- 
   assertion(nonvar(In)),
   var(Out), % Out not yet set; transfer it back to caller in the end
   !,
   atom_codes(In,InCodes),
   compound_name_arguments(DcgGoalCompleted,DcgGoal,[Out]), % will fill Out with atom
   phrase(DcgGoalCompleted,InCodes,RestCodes),
   atom_codes(Rest,RestCodes), % this instantiates or unifies/compares
   assertion(atom(Out)).
 
recognize_atom_to_atom(In,Rest,DcgGoal,Out) :- 
   assertion(nonvar(In)),
   nonvar(Out), % Out already set; pass it to recognizer goal for checking 
   assertion(atom(Out)),
   !,
   atom_codes(In,InCodes),
   compound_name_arguments(DcgGoalCompleted,DcgGoal,[Out]), % will consult Out
   phrase(DcgGoalCompleted,InCodes,RestCodes),   
   atom_codes(Rest,RestCodes). % this instantiates or unifies/compares

recognize_atom_to_compound(In,Rest,DcgGoal,Out) :- 
   assertion(nonvar(In)),
   var(Out), % Out not yet set; transfer it back to caller in the end
   !,
   atom_codes(In,InCodes),
   compound_name_arguments(DcgGoalCompleted,DcgGoal,[Out]), % will fill Out with atom
   phrase(DcgGoalCompleted,InCodes,RestCodes),
   atom_codes(Rest,RestCodes). % this instantiates or unifies/compares
 
recognize_atom_to_compound(In,Rest,DcgGoal,Out) :- 
   assertion(nonvar(In)),
   nonvar(Out), % Out already set; pass OutCodes to recognizing goal
   !,
   atom_codes(In,InCodes),
   compound_name_arguments(DcgGoalCompleted,DcgGoal,[Out]), % will consult Out
   phrase(DcgGoalCompleted,InCodes,RestCodes),   
   atom_codes(Rest,RestCodes). % this instantiates or unifies/compares




% ---------

:- begin_tests(digits).

test("digits 12345",true(Out == '12345'))
   :- recognize_atom_to_atom('12345','',jpl_nonempty_atom_of_digits,Out).

test("digits 0",true(Out == '0'))
   :- recognize_atom_to_atom('0','',jpl_nonempty_atom_of_digits,Out).

test("no digits",fail)
   :- recognize_atom_to_atom('',_,jpl_nonempty_atom_of_digits,_).

test("not digits",fail)
   :- recognize_atom_to_atom('foo',_,jpl_nonempty_atom_of_digits,_).

test("digits with rest",true([Out,Rest] == ['0123','foo']))
   :- recognize_atom_to_atom('0123foo',Rest,jpl_nonempty_atom_of_digits,Out).

:- end_tests(digits).

% ---------

% :- debug(identifier_chars).

:- begin_tests(identifier_chars).

% create a list of true/false atoms, one for each position of the input list
% of character codes

maplist_java_id_start_char(ListIn,ListOut) :-
   maplist([C,T]>>reify(jpl_java_identifier_start_char(C),T),ListIn,ListOut).

maplist_java_id_part_char(ListIn,ListOut) :-
   maplist([C,T]>>reify(jpl_java_identifier_part_char(C),T),ListIn,ListOut).

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

% ---------

% :- debug(java_identifier).

:- begin_tests(java_identifier).

test("recognize Java identifier (unconstrained Out), no rest",true([Out,Rest] == [my_identifier,''])) :- 
   recognize_atom_to_atom('my_identifier',Rest,jpl_java_identifier,Out),
   debug(java_identifier,"Recognized: ~q with rest: ~q",[Out,Rest]).

test("recognize Java identifier (unconstrained Out), with rest",true([Out,Rest] == [my_identifier,'.dodododo'])) :- 
   recognize_atom_to_atom('my_identifier.dodododo',Rest,jpl_java_identifier,Out),
   debug(java_identifier,"Recognized: ~q with rest: ~q",[Out,Rest]).

test("recognize Java identifier of length 1, no rest",true([Out,Rest] == [m,''])) :- 
   recognize_atom_to_atom('m',Rest,jpl_java_identifier,Out).

test("recognize Java identifier (Out already set to result), no rest",true(Rest == '')) :- 
   recognize_atom_to_atom('my_identifier',Rest,jpl_java_identifier,'my_identifier').

test("recognize Java identifier (Out already set to result), with rest",true(Rest == '.dodododo')) :- 
   recognize_atom_to_atom('my_identifier.dodododo',Rest,jpl_java_identifier,'my_identifier').

test("not a Java identifier",fail) :- 
   recognize_atom_to_atom('-my',_,jpl_java_identifier,_).

test("empty atom is not a Java identifier",fail) :- 
   recognize_atom_to_atom('',_,jpl_java_identifier,_).

test("valid identifier with differing Out",fail) :- 
   recognize_atom_to_atom('my',_,jpl_java_identifier,'notmy').

:- end_tests(java_identifier).

% --------

:- begin_tests(java_type_identifier).

test("recognize Java type identifier",true([Out,Rest] == [my_identifier,''])) :- 
   recognize_atom_to_atom('my_identifier',Rest,jpl_java_type_identifier,Out).

test("reject bad Java type identifier 'var'",fail) :-
   recognize_atom_to_atom('var',_,jpl_java_type_identifier,_).

test("java type identifier DOES NOT stop at '$'",true([Out,Rest] == ['foo$bar',''])) :-
   recognize_atom_to_atom('foo$bar',Rest,jpl_java_type_identifier,Out).

:- end_tests(java_type_identifier).

% -------

:- begin_tests(jpl_typeterm_entityname).

test("entityname is just 'int': integer primitive",true(Out == primitive(int))) :-
   recognize_atom_to_compound('int','',jpl_typeterm_entityname,Out).

test("entityname is just 'void': void primitive",true(Out == primitive(void))) :-
   recognize_atom_to_compound('void','',jpl_typeterm_entityname,Out).

test("entityname is actually 'integer', which is a class called 'integer', which is ok!",true(Out == class([],[integer]))) :-
   recognize_atom_to_compound('integer','',jpl_typeterm_entityname,Out).

:- end_tests(jpl_typeterm_entityname).

% ------

:- begin_tests(messy_dollar_split).

test(1,true(Runs == [alfa])) :-
   messy_dollar_split(alfa,Runs).

test(2,true(Runs == [a])) :-
   messy_dollar_split(a,Runs).

test(3,true(Runs == ['$'])) :-
   messy_dollar_split('$',Runs).

test(4,true(Runs == ['alfa$'])) :-
   messy_dollar_split('alfa$',Runs).

test(5,true(Runs == [alfa,bravo])) :-
   messy_dollar_split('alfa$bravo',Runs).

test(6,true(Runs == ['$alfa'])) :-
   messy_dollar_split('$alfa',Runs).

test(7,true(Runs == ['alfa','$bravo'])) :-
   messy_dollar_split('alfa$$bravo',Runs).

test(8,true(Runs == ['$alfa','bravo','charlie$'])) :-
   messy_dollar_split('$alfa$bravo$charlie$',Runs).

test(9,true(Runs == ['$$alfa','$bravo','$$charlie','$$$'])) :-
   messy_dollar_split('$$alfa$$bravo$$$charlie$$$$',Runs).

:- end_tests(messy_dollar_split).

% ------

:- begin_tests(jpl_binary_classname_without_dollar).

test("simple classname",true(Out == class([],[foo]))) :-
   recognize_atom_to_compound('foo','',jpl_tt_en_binary_classname,Out).

test("qualified classname",true(Out == class([alfa,bravo,charlie],[foo]))) :-
   recognize_atom_to_compound('alfa.bravo.charlie.foo','',jpl_tt_en_binary_classname,Out).

:- end_tests(jpl_binary_classname_without_dollar).

% ------

% Note that "splitting at a dollar is ill-defined and pointless and
% should eventually disappear

:- begin_tests(jpl_binary_classname_with_dollar).

test("qualified inner member type",true(Out == class([alfa,bravo,charlie],[foo,bar]))) :-
   recognize_atom_to_compound('alfa.bravo.charlie.foo$bar','',jpl_tt_en_binary_classname,Out).

test("qualified inner anonymous type",true(Out == class([alfa,bravo,charlie],[foo,'01234']))) :-
   recognize_atom_to_compound('alfa.bravo.charlie.foo$01234','',jpl_tt_en_binary_classname,Out).

test("qualified inner local class",true(Out == class([alfa,bravo,charlie],[foo,'01234bar']))) :-
   recognize_atom_to_compound('alfa.bravo.charlie.foo$01234bar','',jpl_tt_en_binary_classname,Out).

test("qualified inner member type, deep",true(Out == class([alfa,bravo,charlie],[foo,bar,baz,quux]))) :-
   recognize_atom_to_compound('alfa.bravo.charlie.foo$bar$baz$quux','',jpl_tt_en_binary_classname,Out).

:- end_tests(jpl_binary_classname_with_dollar).

% ------

:- begin_tests(jpl_entity_is_array).

test("array of double",true(Out == array(primitive(double)))) :-
   recognize_atom_to_compound('[D','',jpl_typeterm_entityname,Out).

test("array of array of integer",true(Out == array(array(primitive(int))))) :-
   recognize_atom_to_compound('[[I','',jpl_typeterm_entityname,Out).

test("array of void",fail) :-
   recognize_atom_to_compound('[[V','',jpl_typeterm_entityname,_).

test("array of java.lang.String",true(Out == array(array(class([java, lang], ['String']))))) :-
   recognize_atom_to_compound('[[Ljava.lang.String;','',jpl_typeterm_entityname,Out).


:- end_tests(jpl_entity_is_array).



