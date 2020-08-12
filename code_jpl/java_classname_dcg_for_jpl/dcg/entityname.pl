:- module(entityname, [
    jpl_typeterm_entityname//2
   ,jpl_binary_classname//2
   ,jpl_slashy_type_descriptor//1
   ,messy_dollar_split/2
   ,deprimitive/2
   ]).

:- use_module('trivials.pl').

% ===========================================================================
% Rip out the "primitive/1" tag which exists in the new version but not the 
% old. This is called for every recognize operation and a bit costly.
% Going forward, one can either leave it out or (better) adapt JPL to
% consider it on the same level as class/2, array/1 etc.
% ===========================================================================

deprimitive(primitive(X),X) :- !.

deprimitive(X,X) :- 
   atomic(X),!.

deprimitive(X,Y) :- 
   compound(X), 
   compound_name_arguments(X,N,Args), 
   maplist([I,O]>>deprimitive(I,O),Args,ArgsNew), 
   compound_name_arguments(Y,N,ArgsNew).

% ===========================================================================
%! jpl_typeterm_entityname(TypeTerm)//2
%
% Map an "entityname" (a classname as returned from Class.getName())
% into a Prolog-side "type term" and vice-versa. The second argument indicates
% whether it's:
%
% The "dotty" form, for which package names contain dots. This is the usual
% form, also used in binaries and as returned by Class.getName()
%
% The "slashy" form, for which package names contain slashes. This is used
% by JNI's FindClass.
%
% ~~~
%         Entityname                      Prolog-side Type Term
%           Atom  <---------------------------> Term
%       java.util.Date                class([java,util],['Date'])
% ~~~
%
% Examples from the Java Documentation at Oracle for the "dotty" form:
%
% https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/Class.html#getName()
%
% ~~~
% String.class.getName()
%   returns "java.lang.String"
% byte.class.getName()
%   returns "byte"
% (new Object[3]).getClass().getName()
%   returns "[Ljava.lang.Object;"
% (new int[3][4][5][6][7][8][9]).getClass().getName()
%   returns "[[[[[[[I"
% ~~~
%
% For the "slashy form", see
%
% https://docs.oracle.com/en/java/javase/14/docs/specs/jni/functions.html#findclass
%
% Previously (pre 2020-08) called: `jpl_type_classname_1//1`    for the "dotty form"
%                                  `jpl_type_findclassname//1`  for the "slashy form"
% Now called:                      `jpl_typeterm_entityname//1`
%
% The dotty form is called from:
%
% * `jpl_classname_chars_to_type/2` - to process `java.lang.Class.getName()` ouput
% * `jpl_classname_to_type/2`       - to process an atom passed in from Prolog in `jpl_new/2`, `jpl_call/2`, etc.
% * `jpl_type_to_nicename/2`        - Transform a Term to a stringy thing for printing
% * `jpl_type_to_classname/2`       - Transform a Term to a stringy thing for printing
%
% The slashy form is called from:
%
% * `jpl_type_to_findclassname/2`
% ===========================================================================

% ---
% THE TOP
% We can be pretty precise here regarding what will be found as 1st arg term
% instead of just "T" as argument. This also helps in documentation.
% ---

jpl_typeterm_entityname(class(Ps,Cs),Mode)  --> jpl_binary_classname(class(Ps,Cs),Mode),!.
jpl_typeterm_entityname(array(T),Mode)      --> jpl_array_of_entityname(array(T),Mode),!.

% do the following ever occur? primitives not inside arrays?

jpl_typeterm_entityname(primitive(P),_)     --> jpl_primitive_at_toplevel(primitive(P)),!.  
jpl_typeterm_entityname(primitive(void),_)  --> jpl_void_at_toplevel(primitive(void)).

% ---
% The "binary classname" (i.e. the classname as it appears in binaries) as
% specified in The Java™ Language Specification.
% See "Binary Compatibility" - "The Form of a Binary"
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-13.html#jls-13.1
% which points to the "fully qualified name" and "canonical name"
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-6.html#jls-6.7
%
% For JNI, we can switch to "slashy" mode instead of the "dotty" mode, which
% technically makes this NOT the "binary classname", but we keep the predicate name.
% ---

jpl_binary_classname(class(Ps,Cs),Mode) --> jpl_package_parts(Ps,Mode), jpl_class_parts(Cs).

% ---
% The qualified name of the package (which may be empty if it is the
% unnamed package). This is a series of Java identifiers separated by dots.
% "The fully qualified name of a named package that is not a subpackage of a
% named package is its simple name." ... "A simple name is a single identifier."
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-6.html#jls-6.7
% Note that the last '.' is not considered a separator towards the subsequent
% class parts but as a terminator of the package parts sequence (it's a view 
% less demanding of backtracking)
% ---

jpl_package_parts([A|As],dotty)  --> jpl_java_identifier(A), `.`, !, jpl_package_parts(As,dotty).
jpl_package_parts([A|As],slashy) --> jpl_java_identifier(A), `/`, !, jpl_package_parts(As,slashy).
jpl_package_parts([],_)          --> [].

% ---
% The class parts of a class name (everything beyond the last dot
% of the package prefix, if it exists). This comes from "13.1 - The form of
% a binary", where it is laid out a bit confusingly.
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-13.html#jls-13.1
%
% PROBLEM 2020-08:
%
% Here is an ambiguity that I haven't been able to resolve: '$' is a perfectly
% legitimate character both at the start and in the middle of a classname,
% in fact you can create classes with '$' inside the classname and they compile
% marvelously (try it!). However it is also used as separator for inner class
% names ... but not really! In fact, it is just a concatentation character for
% a _generated class name_ (that makes sense - an inner class is a syntactic
% construct of Java the Language, but of no concern to the JVM, not even for
% access checking because the compiler is supposed to have bleached out any
% problemtic code).
% Parsing such a generated class name can go south in several different ways:
% '$' at the begging, '$' at the end, multiple runs of '$$$' .. one should not
% attempt to do it! 
% But the original JPL code does, so we keep this practice for now.
% ---

jpl_class_parts(Parts) --> jpl_java_type_identifier(A),{ messy_dollar_split(A,Parts) }.

% Heuristic: Only a '$' flanked to the left by a valid character
% that is a non-dollar and to the right by a valid character that
% may or may not be a dollar gives rise to split.

messy_dollar_split(A,Out) :-
   assertion(A \== ''),
   atom_chars(A,Chars),
   append([''|Chars],[''],GAChars), % GA is a "guarded A char list" flanked by empties and contains at least 3 chars
   triple_process(GAChars,[],[],RunsOut),
   postprocess_messy_dollar_split_runs(RunsOut,Out).

postprocess_messy_dollar_split_runs(Runs,Out) :-
   reverse(Runs,R1),
   maplist([Rin,Rout]>>reverse(Rin,Rout),R1,O1),
   maplist([Chars,Atom]>>atom_chars(Atom,Chars),O1,Out).

% Split only between P and N, dropping C, when:
% 1) C is a $ and P is not a dollar and not a start of line
% 2) N is not the end of line

triple_process([P,'$',N|Rest],Run,Runs,Out) :-
   N \== '', P \== '$' , P \== '',!,
   triple_process(['',N|Rest],[],[Run|Runs],Out).

triple_process(['','$',N|Rest],Run,Runs,Out) :-
   !,
   triple_process(['',N|Rest],['$'|Run],Runs,Out).

triple_process([_,C,N|Rest],Run,Runs,Out) :-
   C \== '$',!,
   triple_process([C,N|Rest],[C|Run],Runs,Out).

triple_process([_,C,''],Run,Runs,[[C|Run]|Runs]) :- !.

triple_process([_,''],Run,Runs,[Run|Runs]).

% ---
% jpl_array_of_entityname//1
% Described informally at Javadoc for Class.getName()
% ---

jpl_array_of_entityname(array(T),Mode) --> `[`, jpl_entityname_in_array(T,Mode).

% ---
% jpl_array_of_entityname//1
% Described informally at Javadoc for Class.getName()
% ---

jpl_entityname_in_array(class(Ps,Cs),Mode)  --> `L`, jpl_binary_classname(class(Ps,Cs),Mode), `;`.
jpl_entityname_in_array(array(T),Mode)      --> jpl_array_of_entityname(array(T),Mode).
jpl_entityname_in_array(primitive(T),_)     --> jpl_primitive_in_array(primitive(T)).

% ===========================================================================
% This is the replacement for the old `jpl_type_descriptor_1//1`
% It basically seems to be using the same serialized format as the
% one for arrays (but in slashy mode), so we use that directly.
% It can also understand a method descriptor.
% ===========================================================================

jpl_slashy_type_descriptor(class(Ps,Cs)) --> jpl_entityname_in_array(class(Ps,Cs),slashy).
jpl_slashy_type_descriptor(array(T))     --> jpl_entityname_in_array(array(T),slashy).
jpl_slashy_type_descriptor(primitive(T)) --> jpl_entityname_in_array(primitive(T),slashy).
jpl_slashy_type_descriptor(method(Ts,T)) --> jpl_method_descriptor(method(Ts,T)).

jpl_method_descriptor(method(Ts,T)) --> `(`, jpl_method_descriptor_args(Ts), `)`, jpl_method_descriptor_retval(T).

jpl_method_descriptor_args([T|Ts]) --> jpl_slashy_type_descriptor(T), !, jpl_method_descriptor_args(Ts).
jpl_method_descriptor_args([]) --> [].

jpl_method_descriptor_retval(primitive(void)) --> `V`.  
jpl_method_descriptor_retval(T) --> jpl_slashy_type_descriptor(T).

