:- use_module('trivials.pl').
:- use_module('entityname.pl').
:- use_module('orig_jpl.pl').

% :- debug(identifier_chars).
% :- debug(java_identifier).
% :- debug(run_both).

% ===========================================================================
% Direct copy of some helper functions in my code toolbox
% ===========================================================================

andify_rightwards(true,true,true).
andify_rightwards(true,false,false).
andify_rightwards(false,true,false).
andify_rightwards(false,false,false).

andify_negation_rightwards(true,false,true).
andify_negation_rightwards(true,true,false).
andify_negation_rightwards(false,true,false).
andify_negation_rightwards(false,false,false).

all_true(List) :-
   foldl([E,FromLeft,ToRight]>>once(andify_rightwards(FromLeft,E,ToRight)),List,true,Out),
   Out == true.

all_false(List) :-
   foldl([E,FromLeft,ToRight]>>once(andify_negation_rightwards(FromLeft,E,ToRight)),List,true,Out),
   Out == true.

% ---
% An implementation of ->/2. Pass three goals.
% ---

if_then_else(Condition,Then,Else) :- 
   call(Condition) -> call(Then) ; call(Else).

% ---
% Reification of truth value
% ---

reify(Goal,Truth) :-
   if_then_else(call(Goal),(Truth=true),(Truth=false)).

% ---
% An implementation of ->/2 with an "else" that's true. Pass two goals.
% ---

if_then(Condition,Then) :- 
   call(Condition) -> call(Then) ; true.

% ===========================================================================
% Recognize the descriptor in "In" (an atom)
% - by calling "DcgGoal" augmented with the two arguments "TypeTerm" 
%   (generally a freshvar that captures the result of recognition) 
%   and "Mode" (one of slashy or dotty).
% - leaving "Rest" (an atom) as leftover characters (or unify-comparing it if set)
% - instantiating "TypeTerm" to the strutured result (or unify-comnparing it if set)
% ===========================================================================

recognize(In,Rest,DcgGoal,TypeTerm,Mode) :- 
   assertion(nonvar(In)),
   assertion(memberchk(Mode,[slashy,dotty,typedesc])),
   atom_codes(In,InCodes),
   compound_name_arguments(
      DcgGoalCompleted,
      DcgGoal,
      [TypeTerm,Mode]),
   phrase(DcgGoalCompleted,InCodes,RestCodes),
   atom_codes(Rest,RestCodes).

recognize(In,Rest,DcgGoal,TypeTerm) :- 
   assertion(nonvar(In)),
   atom_codes(In,InCodes),
   compound_name_arguments(
      DcgGoalCompleted,
      DcgGoal,
      [TypeTerm]),
   phrase(DcgGoalCompleted,InCodes,RestCodes),
   atom_codes(Rest,RestCodes).

% ===========================================================================
% Code used to compare the results of the old calls and the new calls
% ===========================================================================

% ---
% Select which old DCG predicate to call
% ---

old_goal_by_mode(slashy   , jpl_type_findclassname ). % jpl_type_findclassname//1
old_goal_by_mode(dotty    , jpl_type_classname_1   ). % jpl_type_classname_1//1
old_goal_by_mode(typedesc , jpl_type_descriptor_1  ). % jpl_type_descriptor_1//1

new_goal_by_mode(slashy   , new_slashy).
new_goal_by_mode(dotty    , new_dotty).
new_goal_by_mode(typedesc , new_typedesc).

% Construct the returnable term

outcome(true ,X,success(X)).
outcome(false,_,failure).

% Indirection for new calls; easier than constructing the goal

new_slashy(T)   --> jpl_typeterm_entityname(T,slashy).
new_dotty(T)    --> jpl_typeterm_entityname(T,dotty).
new_typedesc(T) --> jpl_slashy_type_descriptor(T).

% Run the old call and the new call with input "In"

run_both(In,OutNew,OutOld,Mode) :- 
   new_goal_by_mode(Mode,NewGoal),
   reify(
      recognize(In,'',NewGoal,TypeTermNew),
      SuccessNew),
   old_goal_by_mode(Mode,OldGoal),
   reify(
      recognize(In,'',OldGoal,TypeTermOld),
      SuccessOld),
   if_then(
      call(SuccessNew),
      (deprimitive(TypeTermNew,TypeTermNewDP))), % rip out the "primitive/1" tags
   outcome(SuccessNew,TypeTermNewDP,OutNew),
   outcome(SuccessOld,TypeTermOld  ,OutOld),
   if_then_else(
      call(SuccessNew),
      (debug(run_both,"~q : New   : ~q",[In,TypeTermNew]),
       debug(run_both,"~q : NewDP : ~q",[In,TypeTermNewDP])),
      (debug(run_both,"~q : New failed",[In]))),
   if_then_else(
      call(SuccessOld),
      (debug(run_both,"~q : Old   : ~q",[In,TypeTermOld])),
      (debug(run_both,"~q : Old failed",[In]))),
   call(once(SuccessNew;SuccessOld)). % at least one must have succeeded!

% ===========================================================================
% Testing characters of Java identifiers
% ===========================================================================

% ---
% Helper
% Create a list of true/false atoms, one for each position of the input list
% of character codes, as the results of applying the predicates under test
% ---

maplist_java_id_start_char(ListIn,ListOut) :-
   maplist([C,T]>>reify(jpl_java_identifier_start_char(C),T),ListIn,ListOut).

maplist_java_id_part_char(ListIn,ListOut) :-
   maplist([C,T]>>reify(jpl_java_identifier_part_char(C),T),ListIn,ListOut).

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

% ===========================================================================
% Testing Java identifiers via "jpl_java_identifier//1"
% ===========================================================================

:- begin_tests(java_identifier).

test("recognize Java identifier (unconstrained Out), no rest", true([Out,Rest] == [my_identifier,''])) :- 
   recognize('my_identifier',Rest,jpl_java_identifier,Out),
   debug(java_identifier,"Recognized: ~q with rest: ~q",[Out,Rest]).

test("recognize Java identifier (unconstrained Out), with rest", true([Out,Rest] == [my_identifier,'.dodododo'])) :- 
   recognize('my_identifier.dodododo',Rest,jpl_java_identifier,Out),
   debug(java_identifier,"Recognized: ~q with rest: ~q",[Out,Rest]).

test("recognize Java identifier of length 1, no rest", true([Out,Rest] == [m,''])) :- 
   recognize('m',Rest,jpl_java_identifier,Out).

test("recognize Java identifier (Out already set to result), no rest", true(Rest == '')) :- 
   recognize('my_identifier',Rest,jpl_java_identifier,'my_identifier').

test("recognize Java identifier (Out already set to result), with rest", true(Rest == '.dodododo')) :- 
   recognize('my_identifier.dodododo',Rest,jpl_java_identifier,'my_identifier').

test("starts with dash: not a Java identifier", fail) :- 
   recognize('-my',_,jpl_java_identifier,_).

test("contains dash and thus is broken up", true([Out,Rest] == ['my','-my'])) :- 
   recognize('my-my',Rest,jpl_java_identifier,Out).

test("empty atom is not a Java identifier", fail) :- 
   recognize('',_,jpl_java_identifier,_).

test("valid identifier with differing Out", fail) :- 
   recognize('my',_,jpl_java_identifier,'notmy').

:- end_tests(java_identifier).

% ===========================================================================
% Testing Java type identifiers via "jpl_java_type_identifier//1"
% This is practically the same as testing "jpl_java_identifier";
% here only the keywords "var" and "yield" are additionally disallowed.
% ===========================================================================

:- begin_tests(java_type_identifier).

test("recognize Java type identifier",true([Out,Rest] == [my_identifier,''])) :- 
   recognize('my_identifier',Rest,jpl_java_type_identifier,Out).

test("reject bad Java type identifier 'var'",fail) :-
   recognize('var',_,jpl_java_type_identifier,_).

test("java type identifier DOES NOT stop at '$'",true([Out,Rest] == ['foo$bar',''])) :-
   recognize('foo$bar',Rest,jpl_java_type_identifier,Out).

:- end_tests(java_type_identifier).

% ===========================================================================
% Testing the "messy dollar split" which is used to split Java classnames
% but is actually of dubious value
% ===========================================================================

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

% ===========================================================================
% Testing recognition of the "binary classname", i.e. the classname
% as it appears in binaries (in its 'dotty' form)
% ===========================================================================

:- begin_tests(jpl_binary_classname_without_dollar).

test("simple classname",true(Out == class([],[foo]))) :-
   recognize('foo','',jpl_binary_classname,Out,dotty).

test("qualified classname",true(Out == class([alfa,bravo,charlie],[foo]))) :-
   recognize('alfa.bravo.charlie.foo','',jpl_binary_classname,Out,dotty).

:- end_tests(jpl_binary_classname_without_dollar).

% ===========================================================================
% Testing recognition of the "binary classname" with "$" inside.
% Note that "splitting at a dollar is ill-defined and pointless and
% should eventually disappear.
% ===========================================================================

:- begin_tests(jpl_binary_classname_with_dollar).

test("qualified inner member type",true(Out == class([alfa,bravo,charlie],[foo,bar]))) :-
   recognize('alfa.bravo.charlie.foo$bar','',jpl_binary_classname,Out,dotty).

test("qualified inner anonymous type",true(Out == class([alfa,bravo,charlie],[foo,'01234']))) :-
   recognize('alfa.bravo.charlie.foo$01234','',jpl_binary_classname,Out,dotty).

test("qualified inner local class",true(Out == class([alfa,bravo,charlie],[foo,'01234bar']))) :-
   recognize('alfa.bravo.charlie.foo$01234bar','',jpl_binary_classname,Out,dotty).

test("qualified inner member type, deep",true(Out == class([alfa,bravo,charlie],[foo,bar,baz,quux]))) :-
   recognize('alfa.bravo.charlie.foo$bar$baz$quux','',jpl_binary_classname,Out,dotty).

:- end_tests(jpl_binary_classname_with_dollar).

% ===========================================================================
% Testing Java entityname <-> typeterm mapping for some primitives
% ===========================================================================

:- begin_tests(jpl_entity_is_primitive).

test("entityname is just 'int': integer primitive",true(Out == primitive(int))) :-
   recognize('int','',jpl_typeterm_entityname,Out,dotty).

test("entityname is just 'void': void primitive",true(Out == primitive(void))) :-
   recognize('void','',jpl_typeterm_entityname,Out,dotty).

test("entityname is actually 'integer', which is a class called 'integer', which is ok!",true(Out == class([],[integer]))) :-
   recognize('integer','',jpl_typeterm_entityname,Out,dotty).

:- end_tests(jpl_entity_is_primitive).

% ===========================================================================
% Testing Java entityname <-> typeterm mapping for arrays
% ===========================================================================

:- begin_tests(jpl_entity_is_array).

test("array of double",true(Out == array(primitive(double)))) :-
   recognize('[D','',jpl_typeterm_entityname,Out,dotty).

test("array of array of integer",true(Out == array(array(primitive(int))))) :-
   recognize('[[I','',jpl_typeterm_entityname,Out,dotty).

test("array of void",fail) :-
   recognize('[[V','',jpl_typeterm_entityname,_,dotty).

test("array of java.lang.String",true(Out == array(array(class([java, lang], ['String']))))) :-
   recognize('[[Ljava.lang.String;','',jpl_typeterm_entityname,Out,dotty).

:- end_tests(jpl_entity_is_array).

% ===========================================================================
% Directly comparing old and new entityname <-> typeterm mapping
% for "dotty" entity names
% ===========================================================================

:- begin_tests(compare_both_dotty).

test("dotty/comparing #1" ,[blocked("Old response bad"),true(OutNew == OutOld)]) :- 
   run_both('int',OutNew,OutOld,dotty).    % Old: class([],[int])   ???

test("dotty/comparing #2" ,[blocked("Old response bad"),true(OutNew == OutOld)]) :- 
   run_both('float',OutNew,OutOld,dotty).  % Old: class([],[float]) ???

test("dotty/comparing #3" ,[blocked("Old response bad"),true(OutNew == OutOld)]) :- 
   run_both('void',OutNew,OutOld,dotty).   % Old: class([],[void])  ???

test("old call gives wrong result #1") :- 
   run_both('foo.bar.baz.Foo$',success(OutNew),success(OutOld),dotty),
   OutNew == class([foo,bar,baz],['Foo$']), 
   OutOld == class([foo,bar,baz],['Foo','']). % OLD IS WRONG

test("failure on old call #2") :- 
   run_both('foo.bar.baz.$Foo',success(OutNew),failure,dotty), % OLD FAILS
   OutNew == class([foo,bar,baz],['$Foo']).

test("dotty/comparing 01" , true(OutNew == OutOld)) :- 
   run_both('java.lang.Integer',OutNew,OutOld,dotty).

test("dotty/comparing 02" , true(OutNew == OutOld)) :- 
   run_both('integer',OutNew,OutOld,dotty). % The class called "integer" (not the primitive!)

test("dotty/comparing 03" , true(OutNew == OutOld)) :- 
   run_both('[D',OutNew,OutOld,dotty).

test("dotty/comparing 04" , true(OutNew == OutOld)) :- 
   run_both('[[[[[I',OutNew,OutOld,dotty).

test("dotty/comparing 05" , true(OutNew == OutOld)) :- 
   run_both('[[J',OutNew,OutOld,dotty).

test("dotty/comparing 06" , true(OutNew == OutOld)) :- 
   run_both('[[Ljava.lang.String;',OutNew,OutOld,dotty).

test("dotty/comparing 07" , true(OutNew == OutOld)) :- 
   run_both('java.lang.String',OutNew,OutOld,dotty).

test("dotty/comparing 08" , true(OutNew == OutOld)) :- 
   run_both('Foo',OutNew,OutOld,dotty).

test("dotty/comparing 09" , true(OutNew == OutOld)) :- 
   run_both('foo.bar.baz.Foo',OutNew,OutOld,dotty).

test("dotty/comparing 10" , true(OutNew == OutOld)) :- 
   run_both('foo.bar.baz.Foo$Quux',OutNew,OutOld,dotty).

:- end_tests(compare_both_dotty).

% ===========================================================================
% Directly comparing old and new entityname <-> typeterm mapping
% for "slashy" entity names
% ===========================================================================

:- begin_tests(compare_both_slashy).

test("slashy/comparing 01" , true(OutNew == OutOld)) :- 
   run_both('java/lang/Integer',OutNew,OutOld,slashy).

test("slashy/comparing 02" , true(OutNew == OutOld)) :- 
   run_both('integer',OutNew,OutOld,slashy). % The class called "integer"

test("slashy/comparing 03" , true(OutNew == OutOld)) :- 
   run_both('[D',OutNew,OutOld,slashy).

test("slashy/comparing 04" , true(OutNew == OutOld)) :- 
   run_both('[[[[[I',OutNew,OutOld,slashy).

test("slashy/comparing 05" , true(OutNew == OutOld)) :- 
   run_both('[[J',OutNew,OutOld,slashy).

test("slashy/comparing 06" , true(OutNew == OutOld)) :- 
   run_both('[[Ljava/lang/String;',OutNew,OutOld,slashy).

test("slashy/comparing 07" , true(OutNew == OutOld)) :- 
   run_both('java/lang/String',OutNew,OutOld,slashy).

test("slashy/comparing 08" , true(OutNew == OutOld)) :- 
   run_both('Foo',OutNew,OutOld,slashy).

test("slashy/comparing 09" , true(OutNew == OutOld)) :- 
   run_both('foo/bar/baz/Foo',OutNew,OutOld,slashy).

test("slashy/comparing 10" , true(OutNew == OutOld)) :- 
   run_both('foo/bar/baz/Foo$Quux',OutNew,OutOld,slashy).

:- end_tests(compare_both_slashy).

% ===========================================================================
% Directly comparing old and new 
% ===========================================================================

:- begin_tests(compare_both_typedesc).

test("typedesc/comparing 03" , true(OutNew == OutOld)) :-
   run_both('[D',OutNew,OutOld,typedesc).

test("typedesc/comparing 04" , true(OutNew == OutOld)) :-
   run_both('[[[[[I',OutNew,OutOld,typedesc).

test("typedesc/comparing 05" , true(OutNew == OutOld)) :-
   run_both('[[J',OutNew,OutOld,typedesc).

test("typedesc/comparing 06" , true(OutNew == OutOld)) :-
   run_both('[[Ljava/lang/String;',OutNew,OutOld,typedesc).

test("typedesc/comparing 07" , true(OutNew == OutOld)) :-
   run_both('Ljava/lang/String;',OutNew,OutOld,typedesc).

test("typedesc/comparing 08" , true(OutNew == OutOld)) :-
   run_both('LFoo;',OutNew,OutOld,typedesc).

test("typedesc/comparing 09" , true(OutNew == OutOld)) :-
   run_both('Lfoo/bar/baz/Foo;',OutNew,OutOld,typedesc).

test("typedesc/comparing 10" , true(OutNew == OutOld)) :-
   run_both('Lfoo/bar/baz/Foo$Quux;',OutNew,OutOld,typedesc).

test("typedesc/comparing 11" , true(OutNew == OutOld)) :-
   run_both('([[Ljava/lang/String;Ljava/lang/Integer;JJ[D)D',OutNew,OutOld,typedesc).


:- end_tests(compare_both_typedesc).


