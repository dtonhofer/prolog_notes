% Referenced from https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/jpl.html%27)

% The instructions may be added to the init.pl file, probably ~/.config/swi-prolog/init.pl

:- use_module(library(jpl)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).

% Switch on debug printing for topic "jpl_test", used in this code

:- debug(jpl_tests).

% Parse Java version string "X.Y.Z" using a DCG clause

java_version(V,SV,SSV) --> integer(V), `.`, integer(SV), `.`, integer(SSV).

% Parse "Y-M-D" string using a DCG clause

java_localdate(Y,M,D)  --> integer(Y), `-`, integer(M), `-`, integer(D).

% ---
% Given an object, print its inheritance path
% ---

inheritance_path_of_object(JObj,Path) :-
    assertion(jpl_is_object(JObj)),
    jpl_object_to_class(JObj, JClass),
    debug(jpl_tests,"Object is of Class ~q",[JClass]),
    inheritance_path_of_class(JClass,[],Path).

% ---
% Given an object, Class its inheritance path
% ---

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

% ---
% plunit tests
% Run them by executing ?- run_tests.
% ---

:- begin_tests(jpl).

test("Get the version of your JVM") :-
   jpl_call('java.lang.System','getProperty',['java.version'],TxtAtom),
   atom_codes(TxtAtom,TxtCodes),
   phrase(java_version(V,SV,SSV),TxtCodes,[]),
   debug(jpl_tests,"Java Version: ~q.~q.~q",[V,SV,SSV]),
   member(V,[7,8,9,10,11,12,13,14]).

test("Create a java.time.LocalDate and call its toString() method",true) :-
   jpl_call('java.time.LocalDate','now',[],JInst),
   jpl_call(JInst,'toString',[],TxtAtom),
   atom_codes(TxtAtom,TxtCodes),
   phrase(java_localdate(_Y,_M,_D),TxtCodes,[]).

test("Create  LocalDate, store it in the Prolog database, retrieve it, then call it") :-
   jpl_call('java.time.LocalDate','now',[],JInst),
   assertz(storage(java_localdate,JInst)),     % store in Prolog DB
   storage(java_localdate,JInst2),!,           % retrieve from Prolog DB
   jpl_call(JInst2,'toString',[],TxtAtom),
   atom_codes(TxtAtom,TxtCodes),
   phrase(java_localdate(_Y,_M,_D),TxtCodes,[]).

test("Print the inheritance path of a GregorianCalendar",
     true(Path == ['java.lang.Object','java.util.Calendar','java.util.GregorianCalendar'])) :-
   jpl_new('java.util.GregorianCalendar',[],JGC),
   debug(jpl_tests,"Obtained Java GregorianCalendar = ~q",[JGC]),
   inheritance_path_of_object(JGC,Path).

test("A new java.lang.String is directly mapped to an atom (1)",true(TxtAtom == 'Hello, World')) :-
   jpl_new('java.lang.String',['Hello, World'],TxtAtom).

test("A new java.lang.String is directly mapped to an atom (2)") :-
   jpl_new('java.lang.String',['Hello, World'],JS),
   \+ jpl_is_object(JS),
   atom(JS).

:- end_tests(jpl).
