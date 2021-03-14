% ----
% Some simple starter test for the SWI-Prolog-to-Java bridge ("JPL")
%
% Referenced from:
%
% https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/jpl.html%27)
% ----

% The following module-loading instructions may be added to the init.pl
% file, which should be ~/.config/swi-prolog/init.pl

:- use_module(library(jpl)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).

% Switch on debug printing for debug topic "jpl_test", used in this file

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
    inheritance_path_of_class(JClass,Path).

% ---
% Given an object, print its inheritance path
% ---

inheritance_path_of_class(JClass,Path) :-
    assertion(jpl_is_object(JClass)),                        % it's an object (i.e. not "null")
    assertion(
       (jpl_object_to_class(JClass,JXClass),
        jpl_class_to_classname(JXClass,'java.lang.Class'))), % of type java.lang.Class
    jpl_class_to_classname(JClass,JClassName),               % the "classname" indicates what object this is about
    debug(jpl_tests,"Class ~q has name ~q",[JClass,JClassName]),
    (class_superclass(JClass,JSuperClass)
     ->
     (Path=[JClassName|MorePath],
      inheritance_path_of_class(JSuperClass,MorePath))
     ;
     Path=[JClassName]).

% this only works in the direction JClass -> JSuperClass
% fails if there is no superclass

class_superclass(JClass,JSuperClass) :-
   jpl_call(JClass,'getSuperclass',[],JSuperClass),
   \+ jpl_null(JSuperClass).

% ---
% plunit tests
% Run them by executing ?- run_tests.
% ---

:- begin_tests(jpl).

test("Get the version of your JVM") :-
   jpl_call('java.lang.System','getProperty',['java.version'],TxtAtom),
   atom_codes(TxtAtom,TxtCodes),
   phrase(java_version(V,SV,SSV),TxtCodes),
   debug(jpl_tests,"Java Version: ~q.~q.~q",[V,SV,SSV]),
   between(7,15,V).

test("Create a java.time.LocalDate and call its toString() method",true) :-
   jpl_call('java.time.LocalDate','now',[],JInst),
   jpl_call(JInst,'toString',[],TxtAtom),
   atom_codes(TxtAtom,TxtCodes),
   phrase(java_localdate(_Y,_M,_D),TxtCodes).

test("Create  LocalDate, store it in the Prolog database, retrieve it, then call it") :-
   jpl_call('java.time.LocalDate','now',[],JInst),
   assertz(storage(java_localdate,JInst)),     % store in Prolog DB
   storage(java_localdate,JInst2),!,           % retrieve from Prolog DB
   jpl_call(JInst2,'toString',[],TxtAtom),
   atom_codes(TxtAtom,TxtCodes),
   phrase(java_localdate(_Y,_M,_D),TxtCodes).

test("Print the inheritance path of a GregorianCalendar",
     true(Path == ['java.util.GregorianCalendar','java.util.Calendar','java.lang.Object'])) :-
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
