:- use_module(library(jpl)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- debug(jpl_tests).

java_version(V,SV,SSV) --> integer(V), `.`, integer(SV), `.`, integer(SSV).
java_localdate(Y,M,D)  --> integer(Y), `-`, integer(M), `-`, integer(D).

inheritance_path_of_object(JObj,Path) :-
    assertion(jpl_is_object(JObj)),
    jpl_object_to_class(JObj, JClass),
    debug(jpl_tests,"Object is of Class ~q",[JClass]),
    inheritance_path_of_class(JClass,[],Path).
    
inheritance_path_of_class(JClass,Path,PathOut) :-
    assertion(jpl_is_object(JClass)),                               % it's an object
    assertion((jpl_object_to_class(JClass,JXClass),
               jpl_class_to_classname(JXClass,'java.lang.Class'))), % of type java.lang.Class
    jpl_class_to_classname(JClass,JClassName),
    debug(jpl_tests,"Class ~q has name ~q",[JClass,JClassName]),
    jpl_call(JClass,'getSuperclass',[],JSuperClass),
    inheritance_path_of_class_sub(JSuperClass,[JClassName|Path],PathOut).

inheritance_path_of_class_sub(JSuperClass,Path,Path) :- 
   jpl_null(JSuperClass),!.

inheritance_path_of_class_sub(JSuperClass,Path,PathOut) :- 
   \+ jpl_null(JSuperClass),!,
   inheritance_path_of_class(JSuperClass,Path,PathOut).

:- begin_tests(jpl).

test(get_java_version,true) :-
   jpl_call('java.lang.System','getProperty',['java.version'],TxtAtom),
   atom_codes(TxtAtom,TxtCodes),
   phrase(java_version(V,SV,SSV),TxtCodes,[]),
   debug(jpl_tests,"Java Version: ~q.~q.~q",[V,SV,SSV]),
   member(V,[7,8,9,10,11,12,13,14]).

test(create_java_string,true(TxtAtom == 'Hello, World')) :-
   jpl_new('java.lang.String',['Hello, World'],TxtAtom).

test(create_localdate_and_call,true) :-
   jpl_call('java.time.LocalDate','now',[],JInst),
   jpl_call(JInst,'toString',[],TxtAtom),
   atom_codes(TxtAtom,TxtCodes),
   phrase(java_localdate(_Y,_M,_D),TxtCodes,[]).

test(create_localdate_and_store_then_retrieve_and_call,true) :-
   jpl_call('java.time.LocalDate','now',[],JInst),
   assertz(storage(java_localdate,JInst)),     % store in Prolog DB
   storage(java_localdate,JInst2),!,           % retrieve from Prolog DB
   jpl_call(JInst2,'toString',[],TxtAtom),
   atom_codes(TxtAtom,TxtCodes),
   phrase(java_localdate(_Y,_M,_D),TxtCodes,[]).

test(gregorian_calendar_class_hierarchy,
     true(Path == ['java.lang.Object','java.util.Calendar','java.util.GregorianCalendar'])) :-
   jpl_new('java.util.GregorianCalendar',[],JGC),
   debug(jpl_tests,"Obtained Java GregorianCalendar = ~q",[JGC]),
   inheritance_path_of_object(JGC,Path).

test(java_string_is_not_an_object_but_an_atom,true) :-
   jpl_new('java.lang.String',['Hello, World'],JS),
   \+ jpl_is_object(JS),
   atom(JS).

:- end_tests(jpl).

rt(jpl) :- run_tests(jpl).

:- initialization(rt(jpl)).

