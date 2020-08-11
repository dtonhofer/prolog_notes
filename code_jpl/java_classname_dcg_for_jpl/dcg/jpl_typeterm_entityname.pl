:- module(jpl_typeterm_entityname, [jpl_typeterm_entityname//1]).

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
% ===

jpl_typeterm_entityname(T) --> jpl_tt_en_binary_classname(T),!.
jpl_typeterm_entityname(T) --> jpl_tt_en_array_of_entityname(T),!.
jpl_typeterm_entityname(T) --> jpl_tt_en_primitive(T),!.
jpl_typeterm_entityname(T) --> jpl_tt_en_void(T).


% ---
% The "binary classname" (i.e. the classname as it appears in binaries) as
% specified in The Java™ Language Specification.
% See "Binary Compatibility" - "The Form of a Binary"
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-13.html#jls-13.1
% which points to the "fully qualified name" and "canonical name"
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-6.html#jls-6.7
% ---

jpl_tt_en_binary_classname(class(Ps,Cs)) --> jpl_tt_en_dotted_package_parts(Ps), jpl_tt_en_class_parts(Cs).

% ---
% The fully qualified name of the package (which may be empty if it is the
% unnamed package). This is a series of Java identifiers separated by dots.
% Here, the dot separating the package parts from the class parts is included.
% "The fully qualified name of a named package that is not a subpackage of a
% named package is its simple name." ... "A simple name is a single identifier."
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-6.html#jls-6.7
% ---

jpl_tt_en_dotted_package_parts([P|Ps]) --> jpl_tt_en_java_identifier(P), '.', !, jpl_tt_en_dotted_package_parts(Ps).
jpl_tt_en_dotted_package_parts([])     --> [].

% ---
% The class parts of a class name (everything beyond the last dot
% of the package prefix, if it exists). This comes from "13.1 - The form of
% a binary", where it is laid out a bit confusingly.
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-13.html#jls-13.1
% ---

jpl_tt_en_class_parts([C|Cs]) --> jpl_tt_en_type_identifier(C), '$', !, jpl_tt_en_inner_class_parts(Cs).
jpl_tt_en_class_parts([C])    --> jpl_tt_en_type_identifier(C).

jpl_tt_en_inner_class_parts([C|Cs]) --> jpl_tt_en_inner_class_part(C), '$', !, jpl_tt_en_inner_class_parts(Cs).
jpl_tt_en_inner_class_parts([C])    --> jpl_tt_en_inner_class_part(C).

jpl_tt_en_inner_class_part(C) --> jpl_tt_en_member_type(C),!.
jpl_tt_en_inner_class_part(C) --> jpl_tt_en_local_class(C),!.
jpl_tt_en_inner_class_part(C) --> jpl_tt_en_anonymous_type(C).

jpl_tt_en_member_type(C)     --> jpl_tt_en_type_identifier(C).
jpl_tt_en_local_class(C)     --> nonempty_digits(Ds), jpl_tt_en_type_identifier(TI), { atom_codes(Pfx,Ds), atom_concat(Pfx,T1,C) }.
jpl_tt_en_anonymous_type(C)  --> nonempty_digits(Ds), { atom_codes(C,Ds) }.

% ---
% We mostly end up here
% ---

jpl_tt_en_type_identifier(C)  --> jpl_java_identifier(C), { \+memberchhk(C,[var,yield]) }.

% ---
% The Java identifier is described at
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-3.html#jls-Identifier
% ---

jpl_tt_en_identifier(I) --> jpl_tt_en_identifier_raw(I), 
                            { \+java_keyword(I), 
                              \+java_boolean_literal(I), 
                              \+java_null_literal(I) }.

jpl_tt_en_java_identifier_raw(I) --> { if_then(nonvar(I),atom_codes(I,[C|Cs])) },
                                     { java_identifier_start_char(C) },
                                     jpl_tt_identifier_part_char(Cs),
                                     { if_then(var(I),atom_codes(I,[C|Cs])) }.

jpl_tt_en_identifier_part_chars([C|Cs]) --> [C], { java_identifier_part_char(C) } , jpl_tt_en_identifier_part_chars(Cs).
jpl_tt_en_identifier_part_chars([])     --> [].

% ---
% jpl_tt_en_array_of_entityname//1
% Described informally at Javadoc for Class.getName()
% ---

jpl_tt_en_array_of_entityname(array(T)) --> '[', jpl_tt_en_entityname_inside_array(T).

% ---
% jpl_tt_en_array_of_entityname//1
% Described informally at Javadoc for Class.getName()
% ---

jpl_tt_en_entityname_inside_array(T) --> 'L', jpl_tt_en_binary_classname(T), ';'.
jpl_tt_en_entityname_inside_array(T) --> jpl_tt_en_array_of_entityname(T).
jpl_tt_en_entityname_inside_array(T) --> jpl_tt_en_primitive(T).
jpl_tt_en_entityname_inside_array(T) --> jpl_tt_en_void(T). % array of void sounds really weird
