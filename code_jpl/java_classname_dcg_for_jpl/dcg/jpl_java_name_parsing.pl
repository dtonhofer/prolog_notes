:- module(jpl_java_name_grammar,
          [jpl_typeterm_entityname//1]).



% ===
%! jpl_findclassname_type_descriptor/1
%
% Map a name used by "findclass" into a Prolog-side "type term" and vice-versa.
%
% ~~~
%     Findclassname                   Prolog-side Type Term
%         Atom <-----------------------------> Term
%     java/util/Date                 class([java,util],['Date'])
% ~~~
%
% Previously (pre 2020-08) called: `jpl_type_findclassname//1`
%
% Now called: `jpl_findclassname_typedescriptor//1`
%
% Called from:
%
% * `jpl_type_to_findclassname/2`
% ===

jpl_findclassname_typedescriptor(T) --> jpl_fcl_td__bare_class_descriptor(T).
jpl_findclassname_typedescriptor(T) --> jpl_fcl_td_array_descriptor(T).

% ---
% Undigested
% ---


jpl_ttXtd_array_desc(array(T))           --> "[", jpl_typeterm_typedesc(T).

jpl_ttXtd_class_desc(class(Ps,Cs))       --> "L", jpl_ttXtd_bare_class_desc(class(Ps,Cs)), ";".

jpl_ttXtd_bare_class_desc(class(Ps,Cs))  --> jpl_ttXtd_slashed_package_parts(Ps), jpl_tt2d_class_parts(Cs).

jpl_ttXtd_slashed_package_parts([P|Ps])  --> jpl_ttXtd_package_part(P), "/", !, jpl_tt2d_slashed_package_parts(Ps).
jpl_ttXtd_slashed_package_parts([])      --> [].


jpl_ttXtd_method_desc(method(Ts,T))      --> "(", jpl_type_method_desc_args(Ts), ")", jpl_type_method_desc_return(T).

jpl_ttXtd_method_desc_args([T|Ts])       --> jpl_typeterm_typedesc(T), !, jpl_ttXtd_method_desc_args(Ts).
jpl_ttXtd_method_desc_args([])           --> [].

jpl_ttXtd_method_desc_return(T)          --> jpl_void(T).
jpl_ttXtd_method_desc_return(T)          --> jpl_typeterm_typedesc(T).

jpl_ttXtd_package_part(T)                --> .....


