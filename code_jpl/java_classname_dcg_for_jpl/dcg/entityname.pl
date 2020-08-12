:- module(entityname, [
    jpl_typeterm_entityname//1
   ,jpl_tt_en_binary_classname//1 
   ,messy_dollar_split/2
   ]).

:- use_module('trivials.pl').

% ===
%! jpl_typeterm_entityname(TypeTerm)//1
%
% Map an "entityname" (a classname as returned from Class.getName())
% into a Prolog-side "type term" and vice-versa.
%
% ~~~
%         Entityname                      Prolog-side Type Term
%           Atom  <---------------------------> Term
%       java.util.Date                class([java,util],['Date'])
% ~~~
% Examples from the Java Documentation at Oracle:
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
% Previously (pre 2020-08) called: `jpl_type_classname_1//1`
%
% Now called: `jpl_typeterm_entityname//1`
%
% Called from:
%
% * `jpl_classname_chars_to_type/2` - to process `java.lang.Class.getName()` ouput
% * `jpl_classname_to_type/2`       - to process an atom passed in from Prolog in `jpl_new/2`, `jpl_call/2`, etc.
% * `jpl_type_to_nicename/2`        - Transform a Term to a stringy thing for printing
% * `jpl_type_to_classname/2`       - Transform a Term to a stringy thing for printing
%
% Note this testcode:
%
% class TestPrint {
%   
%    public static void main(String[] argv) {
%       System.out.println(boolean.class.getName());
%       System.out.println(Boolean.class.getName());
%       System.out.println(byte.class.getName());
%       System.out.println(Byte.class.getName());
%       System.out.println(char.class.getName());
%       System.out.println(Character.class.getName());
%       System.out.println(double.class.getName());
%       System.out.println(Double.class.getName());
%       System.out.println(float.class.getName());
%       System.out.println(Float.class.getName());
%       System.out.println(int.class.getName());
%       // System.out.println(integer.class.getName()); does not exist but *could* be a user-defined class
%       System.out.println(Integer.class.getName());
%       System.out.println(long.class.getName());
%       System.out.println(Long.class.getName());
%       System.out.println(short.class.getName());
%       System.out.println(Short.class.getName());
%       System.out.println(void.class.getName());
%       System.out.println(Void.class.getName());
%    }
% }
%
% Which outputs:
%
% boolean
% java.lang.Boolean
% byte
% java.lang.Byte
% char
% java.lang.Character
% double
% java.lang.Double
% float
% java.lang.Float
% int
% java.lang.Integer
% long
% java.lang.Long
% short
% java.lang.Short
% void
% java.lang.Void
% ===

% We can be pretty precise here regarding what will be found as term
% instead of "just T" as argument. This also helps in documentation.

jpl_typeterm_entityname(class(Ps,Cs))    --> jpl_tt_en_binary_classname(class(Ps,Cs)),!.
jpl_typeterm_entityname(array(T))        --> jpl_tt_en_array_of_entityname(array(T)),!.
jpl_typeterm_entityname(primitive(P))    --> jpl_tt_en_primitive_at_toplevel(primitive(P)),!.  
jpl_typeterm_entityname(primitive(void)) --> jpl_tt_en_void_at_toplevel(primitive(void)).

% ---
% The "binary classname" (i.e. the classname as it appears in binaries) as
% specified in The Java™ Language Specification.
% See "Binary Compatibility" - "The Form of a Binary"
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-13.html#jls-13.1
% which points to the "fully qualified name" and "canonical name"
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-6.html#jls-6.7
% ---

% possibly empty dotted package parts followed by nonempty class parts
jpl_tt_en_binary_classname(class(Ps,Cs)) --> jpl_tt_en_dotted_package_parts(Ps), jpl_tt_en_class_parts(Cs).

% ---
% The fully qualified name of the package (which may be empty if it is the
% unnamed package). This is a series of Java identifiers separated by dots.
% Here, the dot separating the package parts from the class parts is included.
% "The fully qualified name of a named package that is not a subpackage of a
% named package is its simple name." ... "A simple name is a single identifier."
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-6.html#jls-6.7
% ---

% a possibly empty sequence of dotted package parts
% note that the last '.' is not considered a separator towards the subsequent class parts
% but as a terminator of the package parts sequence (it's a view less demanding of backtracking)
jpl_tt_en_dotted_package_parts([A|As]) --> jpl_java_identifier(A), `.`, !, jpl_tt_en_dotted_package_parts(As).
jpl_tt_en_dotted_package_parts([])     --> [].

% ---
% The class parts of a class name (everything beyond the last dot
% of the package prefix, if it exists). This comes from "13.1 - The form of
% a binary", where it is laid out a bit confusingly.
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-13.html#jls-13.1
% PROBLEM 2020-08:
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
% But the original JPL code does, so we keep this practice and hope it works.
% ---

jpl_tt_en_class_parts(Parts) --> jpl_java_type_identifier(A),{ messy_dollar_split(A,Parts) }.

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


/*
jpl_tt_en_class_parts([A|As]) --> `$`, jpl_java_type_identifier(A), `$`, !, jpl_tt_en_inner_class_parts(As).
jpl_tt_en_class_parts([A])    --> jpl_java_type_identifier(A).

jpl_tt_en_inner_class_parts([A|As]) --> jpl_tt_en_inner_class_part(A), `$`, !, jpl_tt_en_inner_class_parts(As).
jpl_tt_en_inner_class_parts([A])    --> jpl_tt_en_inner_class_part(A).

% subclassing the inner class part
% to be done: the binary name of a type variable declared by a generic method
% to be done: the binary name of a type variable declared by a generic constructor
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-8.html#jls-8.4.4
jpl_tt_en_inner_class_part(A) --> jpl_tt_en_member_type(A),!.
jpl_tt_en_inner_class_part(A) --> jpl_tt_en_local_class(A),!.
jpl_tt_en_inner_class_part(A) --> jpl_tt_en_anonymous_type(A).

jpl_tt_en_member_type(A)     --> jpl_java_type_identifier(A).
jpl_tt_en_local_class(A)     --> jpl_nonempty_atom_of_digits(DG), jpl_java_type_identifier(TI), { atom_concat(DG,TI,A) }.
jpl_tt_en_anonymous_type(A)  --> jpl_nonempty_atom_of_digits(A).
*/

% ---
% jpl_tt_en_array_of_entityname//1
% Described informally at Javadoc for Class.getName()
% ---

jpl_tt_en_array_of_entityname(array(T)) --> `[`, jpl_tt_en_entityname_in_array(T).

% ---
% jpl_tt_en_array_of_entityname//1
% Described informally at Javadoc for Class.getName()
% ---

jpl_tt_en_entityname_in_array(T) --> `L`, jpl_tt_en_binary_classname(T), `;`.
jpl_tt_en_entityname_in_array(T) --> jpl_tt_en_array_of_entityname(T).
jpl_tt_en_entityname_in_array(T) --> jpl_tt_en_primitive_in_array(T).
% array of void is actually rejected by the compiler; I tested it
% jpl_tt_en_entityname_in_array(T) --> jpl_tt_en_void_in_array(T).

