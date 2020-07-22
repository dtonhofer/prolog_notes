% ===
% exc_desc(+Pred,+Key,-Location,-Formal,-MsgTxt)
% Descriptors for exceptions. The lookup key are the two first arguments of exc_desc/4:
% Pred : what predicate is throwing
% Key  : a term that is both user-readable but still abstract key and a way
%        for passing values to the predicates that constructs the error term
% The three last arguments are used to construct the exception term when throwing.
% ===

% ---

exc_desc(jpl_new, x_is_var,
         jpl_new/3, 
         instantiation_error,
         '1st arg must be bound to a classname, descriptor or object type').
         
exc_desc(jpl_new, x_not_classname(X),
         jpl_new/3, 
         domain_error(classname,X),
         'if 1st arg is an atom, it must be a classname or descriptor').
         
exc_desc(jpl_new, x_not_instantiable(X),
         jpl_new/3, 
         type_error(instantiable,X),
         '1st arg must be a classname, descriptor or object type').
         
exc_desc(jpl_new, not_a_jpl_term(Vx),
         jpl_new/3, 
         type_error(term,Vx),
         'result is not a org.jpl7.Term instance as required').
 
% ---

exc_desc(jpl_new_1_class, params_is_var,
         jpl_new/3, 
         instantiation_error,
         '2nd arg must be a proper list of valid parameters for a constructor').
         
exc_desc(jpl_new_1_class, params_not_list(Params),
         jpl_new/3, 
         type_error(list,Params),
         '2nd arg must be a proper list of valid parameters for a constructor').

exc_desc(jpl_new_1_class, class_is_interface(Cn),
         jpl_new/3, 
         type_error(concrete_class,Cn),
         'cannot create instance of an interface').
         
exc_desc(jpl_new_1_class, class_sans_constructor(Cn/A),
         jpl_new/3, 
         existence_error(constructor,Cn/A),
         'no constructor found with the corresponding quantity of parameters').

exc_desc(jpl_new_1_class, acyclic(Te,Msg),
         jpl_new/3, 
         type_error(acyclic,Te),
         Msg).

exc_desc(jpl_new_1_class, bad_jpl_datum(Params),
         jpl_new/3, 
         domain_error(list(jpl_datum),Params),
         'one or more of the actual parameters is not a valid representation of any Java value or object').

exc_desc(jpl_new_1_class, single_constructor_mismatch(Tx/A),
         jpl_new/3, 
         existence_error(constructor,Tx/A),
         'the actual parameters are not assignable to the formal parameter types of the only constructor which takes this qty of parameters').
         
exc_desc(jpl_new_1_class, any_constructor_mismatch(Params),
         jpl_new/3, 
         type_error(constructor_params,Params), 
         'the actual parameters are not assignable to the formal parameter types of any of the constructors which take this qty of parameters').

exc_desc(jpl_new_1_class, constructor_multimatch(Params), 
         jpl_new/3, 
         type_error(constructor_params,Params),
         'more than one most-specific matching constructor (shouldn''t happen)').
         
exc_desc(jpl_new_1_class, class_is_abstract(Cn),
         jpl_new/3, 
         type_error(concrete_class,Cn), 
         'cannot create instance of an abstract class').

% ---

exc_desc(jpl_new_1_array, params_is_var,
         jpl_new/3, 
         instantiation_error,
         'when constructing a new array, 2nd arg must either be a non-negative integer (denoting the required array length) or a proper list of valid element values').

exc_desc(jpl_new_1_array, negative_params(Params),
         jpl_new/3, 
         domain_error(array_length,Params),
         'when constructing a new array, if the 2nd arg is an integer (denoting the required array length) then it must be non-negative').
         
% ---

exc_desc(jpl_new_1_primitive, primitive(T), 
         jpl_new/3, 
         domain_error(object_type,T),
         'cannot construct an instance of a primitive type').

exc_desc(jpl_new_1_primitive, params_is_var,
         jpl_new/3, 
         instantiation_error,     
         'when constructing a new instance of a primitive type, 2nd arg must be bound (to a representation of a suitable value)').

exc_desc(jpl_new_1_primitive, params_is_bad(Params),
         jpl_new/3, 
         domain_error(constructor_args,Params), 
         'when constructing a new instance of a primitive type, 2nd arg must either be an empty list (indicating that the default value of that type is required) or a list containing exactly one representation of a suitable value)').

% ---

exc_desc(jpl_new_1_catchall, catchall(T),
         jpl_new/3, 
         domain_error(jpl_type,T),
         '1st arg must denote a known or plausible type').         
         
% ---

exc_desc(jpl_call, x_is_var,
         jpl_call/4,
         instantiation_error,
         '1st arg must be bound to an object, classname, descriptor or type').
         
exc_desc(jpl_call, no_such_class(X), 
         jpl_call/4,    
         existence_error(class,X),
         'the named class cannot be found').

exc_desc(jpl_call, x_is_bad(X)
         jpl_call/4,
         type_error(class_name_or_descriptor,X),
         '1st arg must be an object, classname, descriptor or type').

exc_desc(jpl_call, x_is_array(X) 
         jpl_call/4,
         type_error(object_or_class,X),
         'cannot call a static method of an array type, as none exists').

exc_desc(jpl_call, x_is_bad_2(X),
         jpl_call/4,
         domain_error(object_or_class,X),
         '1st arg must be an object, classname, descriptor or type').
         
exc_desc(jpl_call, mspec_is_var,
         jpl_call/4,
         instantiation_error,
         '2nd arg must be an atom naming a public method of the class or object').

exc_desc(jpl_call, mspec_is_bad(Mspec), 
         jpl_call/4,
         type_error(method_name,Mspec),
         '2nd arg must be an atom naming a public method of the class or object').
                  
exc_desc(jpl_call, acyclic(Te,Msg),
         jpl_call/4, 
         type_error(acyclic,Te),
         Msg).
                  
exc_desc(jpl_call, nonconvertible_params(Params), 
         jpl_call/4,
         type_error(method_params,Params),
         'not all actual parameters are convertible to Java values or references').

exc_desc(jpl_call, params_is_var, 
         jpl_call/4,
         instantiation_error,
         '3rd arg must be a proper list of actual parameters for the named method').

exc_desc(jpl_call,params_is_bad(Params), 
         jpl_call/4,
         type_error(method_params,Params),
         '3rd arg must be a proper list of actual parameters for the named method').

exc_desc(jpl_call,not_a_jpl_term(Rx), 
         jpl_call/4,
         type_error(jni_jref,Rx),
         'result is not a org.jpl7.Term instance as required').

% ===
% throwme(+Who,+Key)
% Predicate called to throw an exception. 
% Who   : what predicate is throwing (we chose this to be an atom generally)
% Key   : a term that is both user-readable but still abstract key and a way
%         for passing values to the predicates that constructs the error term
% ===

% Lookup an excpetion term by "Pred" and "Key" and construct an explicitly non-ISO
% exception term if this lookup fails (because that means there is a programming
% error)

throwme(Who,Key) :- 
   ((\+ exc_desc(Who,Key,_,_,_)) ->
      (with_output_to(atom(T),format("Unmatched exc_desc/5(~q,~q,_,_,_)", [Who,Key])),
      throw(programming_error(T)))
      ;
      true),
   bagof(_,exc_desc(Who,Key,_,_,_),Bag),
   length(BagLength,Bag),
   ((BagLength > 1) -> 
      (with_output_to(atom(T),format("Multiple solutions for exc_desc/5(~q,~q,_,_,_)", [Who,Key])),
      throw(programming_error(T)))
      ;
      true),
   % there is exactly one!   
   exc_desc(Who,Key,Location,IsoFormal,Msg),
   throw(error(IsoFormal,context(Location,Msg))).
      
% ===
% Testing code
% This is built by grepping the throwme/2 have out of the jpl.pl file
% and adding them below, so that all the throwme/2 can be tested as they
% appear in the code (within limits).
% ===
    
:- debug(throwme_plunit).

rt(throwme_plunit) :- run_tests(throwme_plunit).


% ---
% Which "context" terms and "formal" terms are acceptable?
% Note that the arguments of the terms are generally NOT those prescribed by 
% the ISO Standard.
% ---

context_ok(context(_Pred,_Msg)).

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
% exc_test/1 receives a Goal which is a call to throwme/2.
% It calls that Goal and catches any exception, which, if it arrives here and
% was not caught earlier, indicates a test failure.
% ---

exc_test(ThrowmeGoal) :-
   % the recover is just "print & fail" because we expect a catch in inner_throw
   catch(inner_throw(ThrowmeGoal),Any,(log_exception_term(Any),fail)).
   
log_exception_term(Any) :-   
   with_output_to(atom(T),format("Generated a non-ISO-Standard exception term ~q", [Any])),
   debug(throwme_plunit,T,[]).

% ---
% inner_throw/1 receives a Goal which is a call to throwme/2.
% It calls that Goal and catches any exception with an exception term matching at
% least the form of an ISO-Standard, then performs more checks.
% ---
   
inner_throw(ThrowmeGoal) :-
   catch(ThrowmeGoal,error(Formal,Context),true),
   (context_ok(Context) -> true ; (debug(throwme_plunit, "Bad context term ~q", [Context]),fail)),
   (formal_ok(Formal)   -> true ; (debug(throwme_plunit, "Bad formal term ~q", [Formal]),fail)),
   debug(throwme_plunit,"Good: Formal = ~q, Context = ~q", [Formal,Context]).

:- begin_tests(throwme_plunit).

test(1)  :- exc_test(throwme(jpl_new/3,x_is_var)).
test(2)  :- exc_test(throwme(jpl_new/3,x_not_classname(_X))).
test(3)  :- exc_test(throwme(jpl_new/3,x_not_instantiable(_X))).
test(4)  :- exc_test(throwme(jpl_new/3,vx_has_no_ref(_Vx))).
test(5)  :- exc_test(throwme(jpl_new/3,params_is_var)).
test(6)  :- exc_test(throwme(jpl_new/3,params_not_list(_Params))).
test(7)  :- exc_test(throwme(jpl_new/3,class_is_interface(_Cn))).
test(8)  :- exc_test(throwme(jpl_new/3,class_sans_constructor(_Cn/_A))).
test(9)  :- exc_test(throwme(jpl_new/3,acyclic(_Te,_Msg))).
test(10) :- exc_test(throwme(jpl_new/3,bad_jpl_datum(_Params))).
test(11) :- exc_test(throwme(jpl_new/3,single_constructor_mismatch(_Tx/_A))).
test(12) :- exc_test(throwme(jpl_new/3,any_constructor_mismatch(_Params))).
test(13) :- exc_test(throwme(jpl_new/3,class_is_abstract(_Cn))).

:- end_tests(throwme_plunit).



