:- module(orig_jpl, 
   [ jpl_type_classname_1//1
   ]).

% jpl_type_alfa(0'$) -->        % presumably not allowed
%   "$".                        % given the "inner class" syntax?

jpl_type_alfa(0'_) -->
    "_",
    !.
jpl_type_alfa(C) -->
    [C], { C>=0'a, C=<0'z },
    !.
jpl_type_alfa(C) -->
    [C], { C>=0'A, C=<0'Z }.


jpl_type_alfa_num(C) -->
    jpl_type_alfa(C),
    !.
jpl_type_alfa_num(C) -->
    [C], { C>=0'0, C=<0'9 }.


jpl_type_array_classname(array(T)) -->
    "[", jpl_type_classname_2(T).


jpl_type_array_descriptor(array(T)) -->
    "[", jpl_type_descriptor_1(T).


jpl_type_bare_class_descriptor(class(Ps,Cs)) -->
    jpl_type_slashed_package_parts(Ps), jpl_type_class_parts(Cs).


jpl_type_bare_classname(class(Ps,Cs)) -->
    jpl_type_dotted_package_parts(Ps), jpl_type_class_parts(Cs).


jpl_type_class_descriptor(class(Ps,Cs)) -->
    "L", jpl_type_bare_class_descriptor(class(Ps,Cs)), ";".


jpl_type_class_part(N) -->
    jpl_type_id(N).


jpl_type_class_parts([C|Cs]) -->
    jpl_type_class_part(C), jpl_type_inner_class_parts(Cs).


jpl_type_classname_1(T) -->
    jpl_type_bare_classname(T),
    !.
jpl_type_classname_1(T) -->
    jpl_type_array_classname(T),
    !.
jpl_type_classname_1(T) -->
    jpl_type_primitive(T).


jpl_type_classname_2(T) -->
    jpl_type_delimited_classname(T).
jpl_type_classname_2(T) -->
    jpl_type_array_classname(T).
jpl_type_classname_2(T) -->
    jpl_type_primitive(T).



jpl_type_delimited_classname(Class) -->
    "L", jpl_type_bare_classname(Class), ";".



jpl_type_descriptor_1(T) -->
    jpl_type_primitive(T),
    !.
jpl_type_descriptor_1(T) -->
    jpl_type_class_descriptor(T),
    !.
jpl_type_descriptor_1(T) -->
    jpl_type_array_descriptor(T),
    !.
jpl_type_descriptor_1(T) -->
    jpl_type_method_descriptor(T).



jpl_type_dotted_package_parts([P|Ps]) -->
    jpl_type_package_part(P), ".", !, jpl_type_dotted_package_parts(Ps).
jpl_type_dotted_package_parts([]) -->
    [].



jpl_type_findclassname(T) -->
    jpl_type_bare_class_descriptor(T).
jpl_type_findclassname(T) -->
    jpl_type_array_descriptor(T).



jpl_type_id(A) -->
    { nonvar(A) -> atom_codes(A,[C|Cs]) ; true },
    jpl_type_alfa(C), jpl_type_id_rest(Cs),
    { atom_codes(A, [C|Cs]) }.



jpl_type_id_rest([C|Cs]) -->
    jpl_type_alfa_num(C), !, jpl_type_id_rest(Cs).
jpl_type_id_rest([]) -->
    [].



jpl_type_id_v2(A) -->                   % inner class name parts (empirically)
    { nonvar(A) -> atom_codes(A,Cs) ; true },
    jpl_type_id_rest(Cs),
    { atom_codes(A, Cs) }.



jpl_type_inner_class_part(N) -->
    jpl_type_id_v2(N).



jpl_type_inner_class_parts([C|Cs]) -->
    "$", jpl_type_inner_class_part(C), !, jpl_type_inner_class_parts(Cs).
jpl_type_inner_class_parts([]) -->
    [].



jpl_type_method_descriptor(method(Ts,T)) -->
    "(", jpl_type_method_descriptor_args(Ts), ")", jpl_type_method_descriptor_return(T).



jpl_type_method_descriptor_args([T|Ts]) -->
    jpl_type_descriptor_1(T), !, jpl_type_method_descriptor_args(Ts).
jpl_type_method_descriptor_args([]) -->
    [].



jpl_type_method_descriptor_return(T) -->
    jpl_type_void(T).
jpl_type_method_descriptor_return(T) -->
    jpl_type_descriptor_1(T).



jpl_type_package_part(N) -->
    jpl_type_id(N).



jpl_type_primitive(boolean) -->
    "Z",
    !.
jpl_type_primitive(byte) -->
    "B",
    !.
jpl_type_primitive(char) -->
    "C",
    !.
jpl_type_primitive(short) -->
    "S",
    !.
jpl_type_primitive(int) -->
    "I",
    !.
jpl_type_primitive(long) -->
    "J",
    !.
jpl_type_primitive(float) -->
    "F",
    !.
jpl_type_primitive(double) -->
    "D".



jpl_type_slashed_package_parts([P|Ps]) -->
    jpl_type_package_part(P), "/", !, jpl_type_slashed_package_parts(Ps).
jpl_type_slashed_package_parts([]) -->
    [].



jpl_type_void(void) -->
    "V".


