:- module(jpl_java_name_parsing, 
          [jpl_parse_entityname//1,
           jpl_parse_java_type_descriptor//1,
           jpl_parse_findclassname_type_descriptor//1]).

:- use_module(library(jpl_java_identifier_chars)).

% ===
% Some tests
% ===

:- begin_tests(parsing_java_names).

test(char_1s)      :- codepoint_call('A',java_identifier_start_char).
test(char_2s,fail) :- codepoint_call('0',java_identifier_start_char).
test(char_3s,fail) :- codepoint_call('\n',java_identifier_start_char).
test(char_4s)      :- codepoint_call('$',java_identifier_start_char).
test(char_5s)      :- codepoint_call('$',java_identifier_start_char).
test(char_6s)      :- codepoint_call('\u3047',java_identifier_start_char). % Hiragana letter small e

test(char_1p)      :- codepoint_call('A',java_identifier_part_char).
test(char_2p)      :- codepoint_call('0',java_identifier_part_char).
test(char_3p,fail) :- codepoint_call('\n',java_identifier_part_char).
test(char_4p)      :- codepoint_call('$',java_identifier_part_char).
test(char_5p)      :- codepoint_call('$',java_identifier_part_char).
test(char_6p)      :- codepoint_call('\u3047',java_identifier_part_char). % Hiragana letter small e

codepoint_call(A,G) :- 
   atom_codes(A,Codes),
   (\+ length(Codes,1) -> throw("Length should be 1") ; true),
   nth0(0,Codes,Code),
   call(G,Code).
   
:- end_tests(parsing_java_names).

rt(parsing_java_names) :- run_tests(parsing_java_names).

% ===
% Parse a classname as returned from Class.getName()
% previously called "jpl_type_classname_1//1"
% now called        "jpl_parse_entityname//1"
% https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/Class.html#getName()
% ===

jpl_parse_entityname(T) --> jpl_parse_binary_classname(T),!.
jpl_parse_entityname(T) --> jpl_parse_array_of_entityname(T),!.
jpl_parse_entityname(T) --> jpl_parse_primitive(T),!.
jpl_parse_entityname(T) --> jpl_parse_void(T).

jpl_parse_java_type_descriptor(T) --> jpl_parse_primitive(T),!.
jpl_parse_java_type_descriptor(T) --> jpl_parse_class_descriptor(T),!.
jpl_parse_java_type_descriptor(T) --> jpl_parse_array_descriptor(T),!.
jpl_parse_java_type_descriptor(T) --> jpl_parse_method_descriptor(T).

jpl_parse_findclassname_type_descriptor(T) --> jpl_type_bare_class_descriptor(T).
jpl_parse_findclassname_type_descriptor(T) --> jpl_type_array_descriptor(T).


% ===
% The binary classname as specified in The Java™ Language Specification. 
% ===

jpl_parse_binary_classname(class(Ps,Cs)) --> jpl_parse_dotted_package_parts(Ps), jpl_parse_class_parts(Cs).

% ===
% The package name: 
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-7.html#jls-7.4
% ===

jpl_parse_dotted_package_parts([P|Ps]) --> jpl_parse_java_identifier(P), ".", !, jpl_parse_dotted_package_parts(Ps).
jpl_parse_dotted_package_parts([])     --> [].

% ===
% The Java identifier is composed of identifiers:
% https://docs.oracle.com/javase/specs/jls/se14/html/jls-3.html#jls-Identifier
% ===

jpl_parse_java_identifier(A)           --> jpl_parse_java_identifier_chars(A),
                                          { \+keyword(A), \+boolean_literal(A), \+null_literal(A) }.

jpl_parse_java_identifier_chars(A)     --> { nonvar(A) -> atom_codes(A,[C|Cs]) ; true }, 
                                          jpl_parse_java_letter(C),
                                          jpl_parse_java_letter_or_digits(Cs),
                                          { atom_codes(A,[C|Cs]) }.

jpl_parse_java_letter_or_digits(A)     --> { nonvar(A) -> atom_codes(A,[C|Cs]) ; true }, 
                                          jpl_parse_java_letter_or_digit(C),!
                                          jpl_parse_java_letter_or_digits(C),
                                          { atom_codes(A,[C|Cs]) }.
jpl_parse_java_letter_or_digits([])    --> [].
 
% A "Java letter" is a character for which the method Character.isJavaIdentifierStart(int) returns true.
% A "Java letter-or-digit" is a character for which the method Character.isJavaIdentifierPart(int) returns true. 
% The "Java letters" include uppercase and lowercase ASCII Latin letters A-Z (\u0041-\u005a), and a-z (\u0061-\u007a),
% and, for historical reasons, the ASCII dollar sign ($, or \u0024) and underscore (_, or \u005f). The dollar 
% sign should be used only in mechanically generated source code or, rarely, to access pre-existing names on
% legacy systems. The underscore may be used in identifiers formed of two or more characters, but it cannot
% be used as a one-character identifier due to being a keyword.
%
% The "Java digits" include the ASCII digits 0-9 (\u0030-\u0039).
% 
% Letters and digits may be drawn from the entire Unicode character set, which supports most writing scripts in 
% use in the world today, including the large sets for Chinese, Japanese, and Korean. This allows programmers to
% use identifiers in their programs that are written in their native languages. 

% This is not going to be portable between Prologs that have badly defined character code sets!

jpl_type_java_letter(0'_) --> "_",!.
jpl_type_java_letter(0'$) --> "$",!.
jpl_type_java_letter(C)   --> [C], { C>=0'a, C=<0'z },!.  
jpl_type_java_letter(C)   --> [C], { C>=0'A, C=<0'Z }.

jpl_type_java_letter_or_digit(C) --> jpl_type_java_letter(C),!.
jpl_type_java_letter_or_digit(C) -_> [C], { C>=0'0, C=<0'9 }.





jpl_type_id(A) -->  { nonvar(A) -> atom_codes(A,[C|Cs]) ; true },  jpl_type_alfa(C), jpl_type_id_rest(Cs), { atom_codes(A, [C|Cs]) }.

jpl_type_id_rest([C|Cs]) --> jpl_type_alfa_num(C), !, jpl_type_id_rest(Cs).
jpl_type_id_rest([])     --> [].



% ---

jpl_type_class_parts([C|Cs]) -->  jpl_type_class_part(C), jpl_type_inner_class_parts(Cs).

% ---

jpl_type_inner_class_parts([C|Cs]) --> "$", jpl_type_inner_class_part(C), !, jpl_type_inner_class_parts(Cs).
jpl_type_inner_class_parts([])     --> [].

% ===
%
% ===

% ===
%
% ===

% ===
% Parse an array name returned from Class.getName()
% https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/Class.html#getName()
% OK
% ===

jpl_type_array_entityname(array(T)) --> "[", jpl_type_entityname_inside_array(T).

jpl_type_entityname_inside_array(T) --> "L", jpl_type_binary_classname(Class), ";".
jpl_type_entityname_inside_array(T) --> jpl_type_array_entityname(T).
jpl_type_entityname_inside_array(T) --> jpl_type_primitive(T) ; jpl_type_void(T).







jpl_type_array_descriptor(array(T)) --> "[", jpl_type_descriptor_1(T).

jpl_type_bare_class_descriptor(class(Ps,Cs)) --> jpl_type_slashed_package_parts(Ps), jpl_type_class_parts(Cs).
jpl_type_class_descriptor(class(Ps,Cs))      --> "L", jpl_type_bare_class_descriptor(class(Ps,Cs)), ";".



jpl_type_slashed_package_parts([P|Ps]) --> jpl_type_package_part(P), "/", !, jpl_type_slashed_package_parts(Ps).
jpl_type_slashed_package_parts([])     --> [].

% ===
%
% ===

jpl_type_method_descriptor(method(Ts,T)) --> "(", jpl_type_method_descriptor_args(Ts), ")", jpl_type_method_descriptor_return(T).

jpl_type_method_descriptor_args([T|Ts]) --> jpl_type_descriptor_1(T), !, jpl_type_method_descriptor_args(Ts).
jpl_type_method_descriptor_args([])     -->  [].

jpl_type_method_descriptor_return(T) -->  jpl_type_void(T).
jpl_type_method_descriptor_return(T) -->  jpl_type_descriptor_1(T).

% ===
%
% ===

jpl_type_id(A) -->  { nonvar(A) -> atom_codes(A,[C|Cs]) ; true },  jpl_type_alfa(C), jpl_type_id_rest(Cs), { atom_codes(A, [C|Cs]) }.

jpl_type_id_rest([C|Cs]) --> jpl_type_alfa_num(C), !, jpl_type_id_rest(Cs).
jpl_type_id_rest([])     --> [].

% inner class name parts (empirically)

jpl_type_id_v2(A) --> { nonvar(A) -> atom_codes(A,Cs) ; true }, jpl_type_id_rest(Cs), { atom_codes(A, Cs) }.

% ===
%
% ===

jpl_type_void(void) --> "V".

% ===
% Parse a primitive name as returned from Class.getName()
% https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/Class.html#getName()
% OK
% ===

jpl_type_primitive(boolean) --> "Z",!.
jpl_type_primitive(byte)    --> "B",!.
jpl_type_primitive(char)    --> "C",!.
jpl_type_primitive(double)  --> "D",!.
jpl_type_primitive(float)   --> "F",!.
jpl_type_primitive(int)     --> "I",!.
jpl_type_primitive(long)    --> "J",!.
jpl_type_primitive(short)   --> "S".


