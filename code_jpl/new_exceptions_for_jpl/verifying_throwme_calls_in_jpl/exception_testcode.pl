% ===
% Testing code
% ===
% This is built by grepping the throwme/2 calls out of the "jpl.pl" file
% and adding them below, so that all the throwme/2 call can be tested as
% they appear in the code (within limits).
% ===

:- load_files(exception_thrower).
:- load_files(exception_descriptors).

:- debug(throwme_plunit).

% ---
% Which "context" terms and "formal" terms are acceptable?
% Note that the arguments of the terms are generally NOT those prescribed by
% the ISO Standard.
% ---

context_ok(context(Location,MsgTxt)) :-
  Location = (Name/Arity),
  atom(Name),
  integer(Arity),
  (var(MsgTxt);atom(MsgTxt)).

formal_ok(instantiation_error).
formal_ok(uninstantiation_error(_Cuplrit)).
formal_ok(type_error(_ValidDomain,_Culprit)).
formal_ok(domain_error(_ValidDomain,_Culprit)).
formal_ok(existence_error(_ObjectType,_Culprit)).
formal_ok(permission_error(_Operation,_PermissionType,_Culprit)).
formal_ok(representation_error(_Flag)).
formal_ok(resource_error(_Resource)).
formal_ok(syntax_error(_ImplDepAtom)).

% ---
% "exc_test/1" receives a Goal which is a call to "throwme/2".
% It calls that Goal and catches any exception, which, if it arrives here and
% was not caught earlier, indicates a test failure.
% ---

exc_test(Goal) :-
   % the recover is just "print & fail" because we expect a catch in "throw_and_catch_iso_exception/1"
   catch(throw_and_catch_iso_exception(Goal),
         Caught,
         (log_exception_term(Caught),fail)).

log_exception_term(Caught) :-
   debug(throwme_plunit,"Generated a non-ISO-Standard exception term: ~q",[Caught]).

% ---
% "throw_and_catch_iso_exception/1" receives a Goal which is a call to throwme/2.
% It calls that Goal and catches any exception with an exception term matching at
% least the form of an ISO-Standard, then performs more checks and possibly fails.
% ---

throw_and_catch_iso_exception(Goal) :-
   catch(Goal,error(Formal,Context),true),
   (context_ok(Context) -> true ; (debug(throwme_plunit, "Bad 'context' term: ~q", [Context]),fail)),
   (formal_ok(Formal)   -> true ; (debug(throwme_plunit, "Bad 'formal' term:  ~q", [Formal]),fail)),
   debug(throwme_plunit,"Good: Formal = ~q, Context = ~q", [Formal,Context]).

:- begin_tests(throwme).

test( 1) :- exc_test(throwme(jpl_new,x_is_var)).
test( 2) :- exc_test(throwme(jpl_new,x_not_classname(_X))).
test( 3) :- exc_test(throwme(jpl_new,x_not_instantiable(_X))).
test( 4) :- exc_test(throwme(jpl_new,not_a_jpl_term(_Vx))).
test( 5) :- exc_test(throwme(jpl_new_class,params_is_var)).
test( 6) :- exc_test(throwme(jpl_new_class,params_is_not_list(_Params))).
test( 7) :- exc_test(throwme(jpl_new_class,class_is_interface(_Cn))).
test( 8) :- exc_test(throwme(jpl_new_class,class_without_constructor(_Cn/_A))).
test( 9) :- exc_test(throwme(jpl_new_class,acyclic(_Te,_Msg))).
test(10) :- exc_test(throwme(jpl_new_class,bad_jpl_datum(_Params))).
test(11) :- exc_test(throwme(jpl_new_class,single_constructor_mismatch(_Tx/_A))).
test(12) :- exc_test(throwme(jpl_new_class,any_constructor_mismatch(_Params))).
test(13) :- exc_test(throwme(jpl_new_class,constructor_multimatch(_Params))).
test(14) :- exc_test(throwme(jpl_new_class,class_is_abstract(_Cn))).
test(15) :- exc_test(throwme(jpl_new_array,params_is_var)).
test(16) :- exc_test(throwme(jpl_new_array,params_is_negative(_Params))).
test(17) :- exc_test(throwme(jpl_new_primitive,primitive_type_requested(_T))).
test(18) :- exc_test(throwme(jpl_new_primitive,params_is_var)).
test(19) :- exc_test(throwme(jpl_new_primitive,params_is_bad(_Params))).
test(20) :- exc_test(throwme(jpl_new_catchall,catchall(_T))).
test(21) :- exc_test(throwme(jpl_call,arg1_is_var)).
test(22) :- exc_test(throwme(jpl_call,no_such_class(_X))).
test(23) :- exc_test(throwme(jpl_call,arg1_is_array(_X))).
test(24) :- exc_test(throwme(jpl_call,arg1_is_bad(_X))).
test(25) :- exc_test(throwme(jpl_call,mspec_is_var)).
test(26) :- exc_test(throwme(jpl_call,mspec_is_bad(_Mspec))).
test(27) :- exc_test(throwme(jpl_call,acyclic(_Te,_Msg))).
test(28) :- exc_test(throwme(jpl_call,nonconvertible_params(_Params))).
test(29) :- exc_test(throwme(jpl_call,arg3_is_var)).
test(30) :- exc_test(throwme(jpl_call,arg3_is_bad(_Params))).
test(31) :- exc_test(throwme(jpl_call,not_a_jpl_term(_Rx))).
test(32) :- exc_test(throwme(jpl_call_instance,no_such_method(_Mname/_A))).
test(33) :- exc_test(throwme(jpl_call_instance,param_not_assignable(_Params))).
test(34) :- exc_test(throwme(jpl_call_instance,multiple_most_specific(_Mname/_Params))).
test(35) :- exc_test(throwme(jpl_call_static,no_such_method(_M))).
test(36) :- exc_test(throwme(jpl_call_static,param_not_assignable(_Params))).
test(37) :- exc_test(throwme(jpl_call_instance,multiple_most_specific(_Mname/_Params))).
test(38) :- exc_test(throwme(jpl_get,arg1_is_var)).
test(39) :- exc_test(throwme(jpl_get,named_class_not_found(_Classname))).
test(40) :- exc_test(throwme(jpl_get,named_class_not_found(_X))).
test(41) :- exc_test(throwme(jpl_get,arg1_is_bad(_X))).
test(42) :- exc_test(throwme(jpl_get,arg1_is_bad_2(_X))).
test(43) :- exc_test(throwme(jpl_get,not_a_jpl_term(_X))).
test(44) :- exc_test(throwme(jpl_get_static,arg2_is_var)).
test(45) :- exc_test(throwme(jpl_get_static,arg2_is_bad(_Fname))).
test(46) :- exc_test(throwme(jpl_get_static,no_such_field(_Fname))).
test(47) :- exc_test(throwme(jpl_get_static,multiple_fields(_Fname))).
test(48) :- exc_test(throwme(jpl_get_instance,arg2_is_var)).
test(49) :- exc_test(throwme(jpl_get_instance,arg2_is_bad(_Fname))).
test(50) :- exc_test(throwme(jpl_get_instance,no_such_field(_Fname))).
test(51) :- exc_test(throwme(jpl_get_instance,multiple_fields(_Fname))).
test(52) :- exc_test(throwme(jpl_get_instance_array,arg2_is_var)).
test(53) :- exc_test(throwme(jpl_get_instance_array,arg2_is_bad(_Fspec))).
test(54) :- exc_test(throwme(jpl_get_instance_array,arg2_is_too_large(_Fspec))).
test(55) :- exc_test(throwme(jpl_get_instance_array,bad_range_low(_N-_M))).
test(56) :- exc_test(throwme(jpl_get_instance_array,bad_range_high(_N-_M))).
test(57) :- exc_test(throwme(jpl_get_instance_array,bad_range_pair_values(_N-_M))).
test(58) :- exc_test(throwme(jpl_get_instance_array,bad_range_pair_types(_N-_M))).
test(59) :- exc_test(throwme(jpl_get_instance_array,no_such_field(_Fspec))).
test(60) :- exc_test(throwme(jpl_get_instance_array,wrong_spec(_Fspec))).
test(61) :- exc_test(throwme(jpl_set,acyclic(_Te,_Msg))).
test(62) :- exc_test(throwme(jpl_set,arg1_is_var)).
test(63) :- exc_test(throwme(jpl_set,classname_does_not_resolve(_X))).
test(64) :- exc_test(throwme(jpl_set,class_not_found(_Classname))).
test(65) :- exc_test(throwme(jpl_set,acyclic(_Te,_Msg))).
test(66) :- exc_test(throwme(jpl_set,arg1_is_bad(_X))).
test(67) :- exc_test(throwme(jpl_set_instance_class,arg2_is_var)).
test(68) :- exc_test(throwme(jpl_set_instance_class,arg2_is_bad(_Fname))).
test(69) :- exc_test(throwme(jpl_set_instance_class,no_such_field(_Fname))).
test(70) :- exc_test(throwme(jpl_set_instance_class,field_is_final(_Fname))).
test(71) :- exc_test(throwme(jpl_set_instance_class,incompatible_value(_NNF,_V))).
test(72) :- exc_test(throwme(jpl_set_instance_class,arg3_is_bad(_V))).
test(73) :- exc_test(throwme(jpl_set_instance_class,multiple_fields(_Fname))).
test(74) :- exc_test(throwme(jpl_set_instance_array,arg3_is_var)).
test(75) :- exc_test(throwme(jpl_set_instance_array,arg2_is_var)).
test(76) :- exc_test(throwme(jpl_set_instance_array,arg2_is_bad(_Fspec))).
test(77) :- exc_test(throwme(jpl_set_instance_array,no_values(_Fspec,_Vs))).
test(78) :- exc_test(throwme(jpl_set_instance_array,more_than_one_value(_Fspec,_Vs))).
test(79) :- exc_test(throwme(jpl_set_instance_array,too_few_values(_N-_M,_Vs))).
test(80) :- exc_test(throwme(jpl_set_instance_array,too_many_values(_N-_M,_Vs))).
test(81) :- exc_test(throwme(jpl_set_instance_array,bad_range_pair_values(_N-_M))).
test(82) :- exc_test(throwme(jpl_set_instance_array,bad_range_pair_types(_N-_M))).
test(83) :- exc_test(throwme(jpl_set_instance_array,cannot_assign_to_final_field)).
test(84) :- exc_test(throwme(jpl_set_instance_array,no_such_field(_Fspec))).
test(85) :- exc_test(throwme(jpl_set_instance_array,arg2_is_bad_2(_Fspec))).
test(86) :- exc_test(throwme(jpl_set_static,arg2_is_unbound)).
test(87) :- exc_test(throwme(jpl_set_static,arg2_is_bad(_Fname))).
test(88) :- exc_test(throwme(jpl_set_static,no_such_public_static_field(field,_Fname))).
test(89) :- exc_test(throwme(jpl_set_static,cannot_assign_final_field(_Fname))).
test(90) :- exc_test(throwme(jpl_set_static,value_not_assignable(_NNF,_V))).
test(91) :- exc_test(throwme(jpl_set_static,arg3_is_bad(field_value,_V))).
test(92) :- exc_test(throwme(jpl_set_static,multiple_matches(field,_Fname))).
test(93) :- exc_test(throwme(jpl_set_array,not_all_values_assignable(_T,_Ds))).
test(94) :- exc_test(throwme(jpl_set_array,not_all_values_convertible(_T,_Ds))).
test(95) :- exc_test(throwme(jpl_set_array,element_type_unknown(array_element_type,_T))).
test(96) :- exc_test(throwme(jpl_type_to_class,arg1_is_var)).
test(97) :- exc_test(throwme(check_lib,lib_not_found(_Name,_Msg))).

:- end_tests(throwme).



