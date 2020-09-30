% The exception descriptors from jpl.pl
% Mon 10 Aug 20:24:10 CEST 2020

exc_desc(jpl_new,x_is_var,
         jpl_new/3,
         instantiation_error,
         '1st arg must be bound to a classname, descriptor or object type').

exc_desc(jpl_new,x_not_classname(X),
         jpl_new/3,
         domain_error(classname,X),
         'if 1st arg is an atom, it must be a classname or descriptor').

exc_desc(jpl_new,x_not_instantiable(X),
         jpl_new/3,
         type_error(instantiable,X),
         '1st arg must be a classname, descriptor or object type').

exc_desc(jpl_new,not_a_jpl_term(X),
         jpl_new/3,
         type_error(term,X),
         'result is not a org.jpl7.Term instance as required').

% ---

exc_desc(jpl_new_class,params_is_var,
         jpl_new/3,
         instantiation_error,
         '2nd arg must be a proper list of valid parameters for a constructor').

exc_desc(jpl_new_class,params_is_not_list(Params),
         jpl_new/3,
         type_error(list,Params),
         '2nd arg must be a proper list of valid parameters for a constructor').

exc_desc(jpl_new_class,class_is_interface(C),
         jpl_new/3,
         type_error(concrete_class,C),
         'cannot create instance of an interface').

exc_desc(jpl_new_class,class_without_constructor(Co),
         jpl_new/3,
         existence_error(constructor,Co),
         'no constructor found with the corresponding quantity of parameters').

exc_desc(jpl_new_class,acyclic(X,Msg),
         jpl_new/3,
         type_error(acyclic,X),
         Msg).

exc_desc(jpl_new_class,bad_jpl_datum(Params),
         jpl_new/3,
         domain_error(list(jpl_datum),Params),
         'one or more of the actual parameters is not a valid representation of any Java value or object').

exc_desc(jpl_new_class,single_constructor_mismatch(Co),
         jpl_new/3,
         existence_error(constructor,Co),
         'the actual parameters are not assignable to the formal parameter types of the only constructor which takes this qty of parameters').

exc_desc(jpl_new_class,any_constructor_mismatch(Params),
         jpl_new/3,
         type_error(constructor_args,Params),
         'the actual parameters are not assignable to the formal parameter types of any of the constructors which take this qty of parameters').

exc_desc(jpl_new_class,constructor_multimatch(Params),
         jpl_new/3,
         type_error(constructor_params,Params),
         'more than one most-specific matching constructor (shouldn''t happen)').

exc_desc(jpl_new_class,class_is_abstract(C),
         jpl_new/3,
         type_error(concrete_class,C),
         'cannot create instance of an abstract class').

% ---

exc_desc(jpl_new_array,params_is_var,
         jpl_new/3,
         instantiation_error,
         'when constructing a new array, 2nd arg must either be a non-negative integer (denoting the required array length) or a proper list of valid element values').

exc_desc(jpl_new_array,params_is_negative(Params),
         jpl_new/3,
         domain_error(array_length,Params),
         'when constructing a new array, if the 2nd arg is an integer (denoting the required array length) then it must be non-negative').

% ---

exc_desc(jpl_new_primitive,primitive_type_requested(T),
         jpl_new/3,
         domain_error(object_type,T),
         'cannot construct an instance of a primitive type').

% the call to this is commented out in jpl.pl
exc_desc(jpl_new_primitive,params_is_var,
         jpl_new/3,
         instantiation_error,
         'when constructing a new instance of a primitive type, 2nd arg must be bound (to a representation of a suitable value)').

% the call to this is commented out in jpl.pl
exc_desc(jpl_new_primitive,params_is_bad(Params),
         jpl_new/3,
         domain_error(constructor_args,Params),
         'when constructing a new instance of a primitive type, 2nd arg must either be an empty list (indicating that the default value of that type is required) or a list containing exactly one representation of a suitable value)').

% ---

exc_desc(jpl_new_catchall,catchall(T),
         jpl_new/3,
         domain_error(jpl_type,T),
         '1st arg must denote a known or plausible type').

% ---

exc_desc(jpl_call,arg1_is_var,
         jpl_call/4,
         instantiation_error,
         '1st arg must be bound to an object, classname, descriptor or type').

exc_desc(jpl_call,no_such_class(X),
         jpl_call/4,
         existence_error(class,X),
         'the named class cannot be found').

exc_desc(jpl_call,arg1_is_bad(X),
         jpl_call/4,
         type_error(class_name_or_descriptor,X),
         '1st arg must be an object, classname, descriptor or type').

exc_desc(jpl_call,arg1_is_array(X),
         jpl_call/4,
         type_error(object_or_class,X),
         'cannot call a static method of an array type, as none exists').

exc_desc(jpl_call,arg1_is_bad_2(X),
         jpl_call/4,
         domain_error(object_or_class,X),
         '1st arg must be an object, classname, descriptor or type').

exc_desc(jpl_call,mspec_is_var,
         jpl_call/4,
         instantiation_error,
         '2nd arg must be an atom naming a public method of the class or object').

exc_desc(jpl_call,mspec_is_bad(Mspec),
         jpl_call/4,
         type_error(method_name,Mspec),
         '2nd arg must be an atom naming a public method of the class or object').

exc_desc(jpl_call,acyclic(Te,Msg),
         jpl_call/4,
         type_error(acyclic,Te),
         Msg).

exc_desc(jpl_call,nonconvertible_params(Params),
         jpl_call/4,
         type_error(method_params,Params),
         'not all actual parameters are convertible to Java values or references').

exc_desc(jpl_call,arg3_is_var,
         jpl_call/4,
         instantiation_error,
         '3rd arg must be a proper list of actual parameters for the named method').

exc_desc(jpl_call,arg3_is_bad(Params),
         jpl_call/4,
         type_error(method_params,Params),
         '3rd arg must be a proper list of actual parameters for the named method').

exc_desc(jpl_call,not_a_jpl_term(X),
         jpl_call/4,
         type_error(jni_jref,X),
         'result is not a org.jpl7.Term instance as required').

% ---

exc_desc(jpl_call_instance,no_such_method(M),
 	 jpl_call/4,
	 existence_error(method,M),
         'the class or object has no public methods with the given name and quantity of parameters').

exc_desc(jpl_call_instance,param_not_assignable(P),
	 jpl_call/4,
	 type_error(method_params,P),
         'the actual parameters are not assignable to the formal parameters of any of the named methods').

exc_desc(jpl_call_instance,multiple_most_specific(M),
	 jpl_call/4,
	 existence_error(most_specific_method,M),
         'more than one most-specific method is found for the actual parameters (this should not happen)').

% ---

exc_desc(jpl_call_static,no_such_method(M),
         jpl_call/4,
 	 existence_error(method,M),
         'the class has no public static methods with the given name and quantity of parameters').

exc_desc(jpl_call_static,param_not_assignable(P),
 	 jpl_call/4,
	 type_error(method_params,P),
         'the actual parameters are not assignable to the formal parameters of any of the named methods').

exc_desc(jpl_call_static,multiple_most_specific(M),
	 jpl_call/4,
	 existence_error(most_specific_method,M),
         'more than one most-specific method is found for the actual parameters (this should not happen)').

% ---

exc_desc(jpl_get,arg1_is_var,
	 jpl_get/3,
         instantiation_error,
         '1st arg must be bound to an object, classname, descriptor or type').

exc_desc(jpl_get,named_class_not_found(Classname),
	 jpl_get/3,
         existence_error(class,Classname),
         'the named class cannot be found').

exc_desc(jpl_get,arg1_is_bad(X),
	 jpl_get/3,
         type_error(class_name_or_descriptor,X),
         '1st arg must be an object, classname, descriptor or type').

exc_desc(jpl_get,arg1_is_bad_2(X),
	 jpl_get/3,
         domain_error(object_or_class,X),
         '1st arg must be an object, classname, descriptor or type').

exc_desc(jpl_get,not_a_jpl_term(X),
         jpl_get/3,
         type_error(jni_ref,X),
         'result is not a org.jpl7.Term instance as required').

% ---

exc_desc(jpl_get_static,arg2_is_var,
	 jpl_get/3,
	 instantiation_error,
         '2nd arg must be bound to an atom naming a public field of the class').

exc_desc(jpl_get_static,arg2_is_bad(F),
	 jpl_get/3,
 	 type_error(field_name,F),
         '2nd arg must be an atom naming a public field of the class').

exc_desc(jpl_get_static,no_such_field(F),
	 jpl_get/3,
	 existence_error(field,F),
         'the class or object has no public static field with the given name').

exc_desc(jpl_get_static,multiple_fields(F),
	 jpl_get/3,
	 existence_error(unique_field,F),
         'more than one field is found with the given name').

% ---

exc_desc(jpl_get_instance,arg2_is_var,
	 jpl_get/3,
	 instantiation_error,
         '2nd arg must be bound to an atom naming a public field of the class or object').

exc_desc(jpl_get_instance,arg2_is_bad(X),
	 jpl_get/3,
	 type_error(field_name,X),
         '2nd arg must be an atom naming a public field of the class or object').

exc_desc(jpl_get_instance,no_such_field(Fname),
	 jpl_get/3,
	 existence_error(field,Fname),
         'the class or object has no public field with the given name').

exc_desc(jpl_get_instance,multiple_fields(Fname),
	 jpl_get/3,
	 existence_error(unique_field,Fname),
         'more than one field is found with the given name').

% ---

exc_desc(jpl_get_instance_array,arg2_is_var,
	 jpl_get/3,
	 instantiation_error,
         'when 1st arg is an array, 2nd arg must be bound to an index, an index range, or ''length''').

exc_desc(jpl_get_instance_array,arg2_is_bad(X),
	 jpl_get/3,
	 domain_error(array_index,X),
         'when 1st arg is an array, integral 2nd arg must be non-negative').

exc_desc(jpl_get_instance_array,arg2_is_too_large(X),
	 jpl_get/3,
 	 domain_error(array_index,X),
         'when 1st arg is an array, integral 2nd arg must not exceed upper bound of array').

exc_desc(jpl_get_instance_array,bad_range_low(R),
	 jpl_get/3,
	 domain_error(array_index_range,R),
         'lower bound of array index range must not exceed upper bound of array').

exc_desc(jpl_get_instance_array,bad_range_high(R),
	 jpl_get/3,
	 domain_error(array_index_range,R),
         'upper bound of array index range must not exceed upper bound of array').

exc_desc(jpl_get_instance_array,bad_range_pair_values(R),
	 jpl_get/3,
	 domain_error(array_index_range,R),
         'array index range must be a non-decreasing pair of non-negative integers').

exc_desc(jpl_get_instance_array,bad_range_pair_types(R),
	 jpl_get/3,
	 type_error(array_index_range,R),
         'array index range must be a non-decreasing pair of non-negative integers').

exc_desc(jpl_get_instance_array,no_such_field(F),
	 jpl_get/3,
	 domain_error(array_field_name,F),
         'the array has no public field with the given name').

exc_desc(jpl_get_instance_array,wrong_spec(F),
	 jpl_get/3,
	 type_error(array_lookup_spec,F),
         'when 1st arg is an array, 2nd arg must be an index, an index range, or ''length''').

% ---

exc_desc(jpl_set,arg1_is_var,
	 jpl_set/3,
	 instantiation_error,
         '1st arg must be an object, classname, descriptor or type').

exc_desc(jpl_set,classname_does_not_resolve(X),
 	 jpl_set/3,
	 existence_error(class,X),
         'the named class cannot be found').

exc_desc(jpl_set,class_not_found(Classname),
         jpl_set/3,
	 existence_error(class,Classname),
         'the class cannot be found').

exc_desc(jpl_set,acyclic(X,Msg),
         jpl_set/3,
         type_error(acyclic,X),
         Msg).

exc_desc(jpl_set,arg1_is_bad(X),
 	 jpl_set/3,
	 domain_error(object_or_class,X),
         '1st arg must be an object, classname, descriptor or type').

% ---

exc_desc(jpl_set_instance_class,arg2_is_var,
	 jpl_set/3,
	 instantiation_error,
	 '2nd arg must be bound to the name of a public, non-final field').

exc_desc(jpl_set_instance_class,arg2_is_bad(Fname),
	 jpl_set/3,
	 type_error(field_name,Fname),
	 '2nd arg must be the name of a public, non-final field').

exc_desc(jpl_set_instance_class,no_such_field(Fname),
	 jpl_set/3,
	 existence_error(field,Fname),
	 'no public fields of the object have this name').

exc_desc(jpl_set_instance_class,field_is_final(Fname),
	 jpl_set/3,
	 permission_error(modify,final_field,Fname),
	 'cannot assign a value to a final field (actually you could but I''ve decided not to let you)').

exc_desc(jpl_set_instance_class,incompatible_value(NNf,V),
	 jpl_set/3,
	 type_error(NNf,V),
	 'the value is not assignable to the named field of the class').

exc_desc(jpl_set_instance_class,arg3_is_bad(V),
	 jpl_set/3,
	 type_error(field_value,V),
	 '3rd arg does not represent any Java value or object').

exc_desc(jpl_set_instance_class,multiple_fields(Fname),
	 jpl_set/3,
	 existence_error(field,Fname),
	 'more than one public field of the object has this name (this should not happen)').

% ---

exc_desc(jpl_set_instance_array,arg3_is_var,
 	 jpl_set/3,
	 instantiation_error,
	 'when 1st arg is an array, 3rd arg must be bound to a suitable element value or list of values').

exc_desc(jpl_set_instance_array,arg2_is_var,
 	 jpl_set/3,
	 instantiation_error,
	 'when 1st arg is an array, 2nd arg must be bound to an index or index range').

exc_desc(jpl_set_instance_array,arg2_is_bad(FSpec),
 	 jpl_set/3,
	 domain_error(array_index,FSpec),
	 'when 1st arg is an array, an integral 2nd arg must be a non-negative index').

exc_desc(jpl_set_instance_array,no_values(Fspec,Vs),
 	 jpl_set/3,
	 domain_error(array_element(Fspec),Vs),
	 'no values for array element assignment: needs one').

exc_desc(jpl_set_instance_array,more_than_one_value(Fspec,Vs),
 	 jpl_set/3,
	 domain_error(array_element(Fspec),Vs),
	 'too many values for array element assignment: needs one').

exc_desc(jpl_set_instance_array,too_few_values(N-M,Vs),
 	 jpl_set/3,
	 domain_error(array_elements(N-M),Vs),
	 'too few values for array range assignment').

exc_desc(jpl_set_instance_array,too_many_values(N-M,Vs),
 	 jpl_set/3,
	 domain_error(array_elements(N-M),Vs),
	 'too many values for array range assignment').

exc_desc(jpl_set_instance_array,bad_range_pair_values(N-M),
 	 jpl_set/3,
	 domain_error(array_index_range,N-M),
	 'array index range must be a non-decreasing pair of non-negative integers').

exc_desc(jpl_set_instance_array,bad_range_pair_types(N-M),
 	 jpl_set/3,
	 type_error(array_index_range,N-M),
	 'array index range must be a non-decreasing pair of non-negative integers').

exc_desc(jpl_set_instance_array,cannot_assign_to_final_field,
 	 jpl_set/3,
	 permission_error(modify,final_field,length),
	 'cannot assign a value to a final field').

exc_desc(jpl_set_instance_array,no_such_field(Fspec),
 	 jpl_set/3,
	 existence_error(field,Fspec),
	 'array has no field with that name').

exc_desc(jpl_set_instance_array,arg2_is_bad_2(Fspec),
 	 jpl_set/3,
	 domain_error(array_index,Fspec),
	 'when 1st arg is an array object, 2nd arg must be a non-negative index or index range').

% ---

exc_desc(jpl_set_static,arg2_is_unbound,
         jpl_set/3,
         instantiation_error,
         'when 1st arg denotes a class, 2nd arg must be bound to the name of a public, static, non-final field').

exc_desc(jpl_set_static,arg2_is_bad(Fname),
         jpl_set/3,
         type_error(field_name,Fname),
         'when 1st arg denotes a class, 2nd arg must be the name of a public, static, non-final field').

exc_desc(jpl_set_static,no_such_public_static_field(field,Fname),
         jpl_set/3,
         existence_error(field,Fname),
	 'class has no public static fields of this name').

exc_desc(jpl_set_static,cannot_assign_final_field(Fname),
         jpl_set/3,
         permission_error(modify,final_field,Fname),
	 'cannot assign a value to a final field').

exc_desc(jpl_set_static,value_not_assignable(NNf,V),
         jpl_set/3,
         type_error(NNf,V),
	 'the value is not assignable to the named field of the class').

exc_desc(jpl_set_static,arg3_is_bad(field_value,V),
         jpl_set/3,
         type_error(field_value,V),
	 '3rd arg does not represent any Java value or object').

exc_desc(jpl_set_static,multiple_matches(field,Fname),
         jpl_set/3,
         existence_error(field,Fname),
	 'more than one public static field of the class has this name (this should not happen)(?)').

% ---

exc_desc(jpl_set_array,not_all_values_assignable(T,Ds),
         jpl_set/3,
         type_error(array(T),Ds),
	 'not all values are assignable to the array element type').

exc_desc(jpl_set_array,not_all_values_convertible(T,Ds),
         jpl_set/3,
         type_error(array(T),Ds),
  	 'not all values are convertible to Java values or references').

exc_desc(jpl_set_array,element_type_unknown(array_element_type,T),
         jpl_set/3,
         type_error(array_element_type,T),
	 'array element type is unknown: neither a class, nor an array type, nor a primitive type').

% ---

exc_desc(jpl_datum_to_type,is_cyclic(Term),
         jpl_call/4, % I don't know why, but the tests expect jpl_call/4 here
         type_error(acyclic,Term),
         'must be acyclic').

% ---

exc_desc(jpl_type_to_class,arg1_is_var,
         jpl_type_to_class/2,
         instantiation_error,
	 '1st arg must be bound to a JPL type').

% ---

exc_desc(check_lib,lib_not_found(Name,Msg),
         check_lib/2,
         existence_error(library,Name),
         Msg).


