:- module(jpl_exceptions,[
      throw_instantiation_error/2,
      throw_domain_error/4,
      throw_type_error/4,
      throw_existence_error/4,
      throw_permission_error/5,
      throw_illegal_state_error/2,
      catch_any_known/2,            % used for testing: catch_any_known(Goal,S)
      rt/1]).                       % call rt(jpl_exceptions) for a cheap test

% ===
% Messages, retrieved by code
% ===

exception_code(ea01,'1st arg must be bound to a classname, descriptor or object type').
exception_code(ea02,'if 1st arg is an atom, it must be a classname or descriptor').
exception_code(ea03,'1st arg must be a classname, descriptor or object type').
exception_code(ea04,'result is not a org.jpl7.Term instance as required').

% ---

exception_code(eb01,'2nd arg must be a proper list of valid parameters for a constructor').
exception_code(eb03,'cannot create instance of an interface').
exception_code(eb04,'no constructor found with the corresponding quantity of parameters').
exception_code(eb05,'one or more of the actual parameters is not a valid representation of any Java value or object').
exception_code(eb06,'the actual parameters are not assignable to the formal parameter types of the only constructor which takes this qty of parameters').
exception_code(eb07,'the actual parameters are not assignable to the formal parameter types of any of the constructors which take this qty of parameters').
exception_code(eb08,'more than one most-specific matching constructor (shouldn''t happen)').
exception_code(eb09,'cannot create instance of an abstract class').

% ---

exception_code(ec01,'when constructing a new array, 2nd arg must either be a non-negative integer (denoting the required array length) or a proper list of valid element values').
exception_code(ec02,'when constructing a new array, if the 2nd arg is an integer (denoting the required array length) then it must be non-negative').

% ---

exception_code(ed01,'cannot construct an instance of a primitive type').
exception_code(ed02,'when constructing a new instance of a primitive type, 2nd arg must be bound (to a representation of a suitable value)').
exception_code(ed03,'when constructing a new instance of a primitive type, 2nd arg must either be an empty list (indicating that the default value of that type is required) or a list containing exactly one representation of a suitable value)').

% ---

exception_code(ee01,'1st arg must denote a known or plausible type').

% ---

exception_code(ef01,'1st arg must be bound to an object, classname, descriptor or type').
exception_code(ef02,'the named class cannot be found').
exception_code(ef03,'1st arg must be an object, classname, descriptor or type').
exception_code(ef04,'cannot call a static method of an array type, as none exists').
exception_code(ef05,'2nd arg must be an atom naming a public method of the class or object').
exception_code(ef06,'not all actual parameters are convertible to Java values or references').
exception_code(ef07,'3rd arg must be a proper list of actual parameters for the named method').
exception_code(ef08,'result is not a org.jpl7.Term instance as required').

% ---

exception_code(eg01,'the class or object has no public methods with the given name and quantity of parameters').
exception_code(eg02,'the actual parameters are not assignable to the formal parameters of any of the named methods').
exception_code(eg03,'more than one most-specific method is found for the actual parameters (this should not happen)').

% ---

exception_code(eh01,'the class has no public static methods with the given name and quantity of parameters').
exception_code(eh02,'the actual parameters are not assignable to the formal parameters of any of the named methods').
exception_code(eh03,'more than one most-specific method is found for the actual parameters (this should not happen)').

% ---

exception_code(ei01,'1st arg must be bound to an object, classname, descriptor or type').
exception_code(ei02,'the named class cannot be found').
exception_code(ei03,'1st arg must be an object, classname, descriptor or type').
exception_code(ei04,'result is not a org.jpl7.Term instance as required').

% ---

exception_code(ej01,'2nd arg must be bound to an atom naming a public field of the class').
exception_code(ej02,'2nd arg must be an atom naming a public field of the class').
exception_code(ej03,'the class or object has no public static field with the given name').
exception_code(ej04,'more than one field is found with the given name').

% ---

exception_code(ek01,'2nd arg must be bound to an atom naming a public field of the class or object').
exception_code(ek02,'2nd arg must be an atom naming a public field of the class or object').
exception_code(ek03,'the class or object has no public field with the given name').
exception_code(ek04,'more than one field is found with the given name').

% ---

exception_code(el01,'when 1st arg is an array, 2nd arg must be bound to an index, an index range, or ''length''').
exception_code(el02,'when 1st arg is an array, integral 2nd arg must be non-negative').
exception_code(el03,'when 1st arg is an array, integral 2nd arg must not exceed upper bound of array').
exception_code(el04,'lower bound of array index range must not exceed upper bound of array').
exception_code(el05,'upper bound of array index range must not exceed upper bound of array').
exception_code(el06,'array index range must be a non-decreasing pair of non-negative integers').
exception_code(el07,'the array has no public field with the given name').
exception_code(el08,'when 1st arg is an array, 2nd arg must be an index, an index range, or ''length''').

% ---

exception_code(em01,'1st arg must be an object, classname, descriptor or type').
exception_code(em02,'the named class cannot be found').
exception_code(em03,'the class cannot be found').
exception_code(em04,'1st arg must be an object, classname, descriptor or type').

% ---

exception_code(en01,'2nd arg must be bound to the name of a public, non-final field').
exception_code(en02,'2nd arg must be the name of a public, non-final field').
exception_code(en03,'no public fields of the object have this name').
exception_code(en04,'cannot assign a value to a final field (actually you could but I''ve decided not to let you)').
exception_code(en05,'the value is not assignable to the named field of the class').
exception_code(en06,'3rd arg does not represent any Java value or object').
exception_code(en07,'more than one public field of the object has this name (this should not happen)').

% ---

exception_code(eo01,'when 1st arg is an array, 3rd arg must be bound to a suitable element value or list of values').
exception_code(eo02,'when 1st arg is an array, 2nd arg must be bound to an index or index range').
exception_code(eo03,'when 1st arg is an array, an integral 2nd arg must be a non-negative index').
exception_code(eo04,'no values for array element assignment: needs one').
exception_code(eo05,'too many values for array element assignment: needs one').
exception_code(eo06,'too few values for array range assignment').
exception_code(eo07,'too many values for array range assignment').
exception_code(eo08,'array index range must be a non-decreasing pair of non-negative integers').
exception_code(eo09,'cannot assign a value to a final field').
exception_code(eo10,'array has no field with that name').
exception_code(eo11,'when 1st arg is an array object, 2nd arg must be a non-negative index or index range').

% ---

exception_code(ep01,'when 1st arg denotes a class, 2nd arg must be bound to the name of a public, static, non-final field').
exception_code(ep02,'when 1st arg denotes a class, 2nd arg must be the name of a public, static, non-final field').
exception_code(ep03,'class has no public static fields of this name').
exception_code(ep04,'cannot assign a value to a final field').
exception_code(ep05,'the value is not assignable to the named field of the class').
exception_code(ep06,'3rd arg does not represent any Java value or object').
exception_code(ep07,'more than one public static field of the class has this name (this should not happen)(?)').

% ---

exception_code(eq01,'not all values are assignable to the array element type').
exception_code(eq02,'not all values are convertible to Java values or references').
exception_code(eq03,'array element type is unknown (this should not happen)').

% ---

exception_code(er01,'must be acyclic').

% ---

exception_code(es01,'1st arg must be bound to a JPL type').

% ===
% Predicates that throw. "Pred" is generally the predicate descriptor of the
% predicate from where the throw originates.
% ===

throw_instantiation_error(Pred,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(instantiation_error,context(Pred,ExText))).

throw_domain_error(Pred,Type,Term,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(domain_error(Type,Term),context(Pred,ExText))).

throw_type_error(Pred,Type,Term,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(type_error(Type,Term),context(Pred,ExText))).

throw_existence_error(Pred,Type,Term,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(existence_error(Type,Term),context(Pred,ExText))).
   
throw_permission_error(Pred,Action,Type,Term,ExCode) :-   
   (exception_code(ExCode,ExText) -> true ; (ExText = ExCode)),
   throw(error(permission_error(Action,Type,Term),context(Pred,ExText))).

% This is not ISO but we need it.
% It replaces throwing a system_error with parameters which is
% also not ISO, and also incorrect: system_error should be used when
% the harddisk crashes, not when the assertion fails.

throw_illegal_state_error(Pred,ExCode) :-
   (exception_code(ExCode,ExText) -> true ; throw(ExText = ExCode)),
   throw(error(illegal_state_error,context(Pred,ExText))).

% ===
% Catch any of the above. This predicate is only used during editing
% to see whether all exceptions are correctly written down.
% ===

catch_any_known(Goal,S) :-
   catch(Goal,H,true),
   (
   (H = error(instantiation_error,context(Pred,ExText))
   ->  S=[[error,instantiation],-(pred,Pred),-(msg,ExText)])
   ;
   (H = error(domain_error(Type,Term),context(Pred,ExText))
   ->  S=[[error,domain],-(type,Type),-(term,Term),-(pred,Pred),-(msg,ExText)])
   ;
   (H = error(type_error(Type,Term),context(Pred,ExText))
   ->  S=[[error,type],-(type,Type),-(term,Term),-(pred,Pred),-(msg,ExText)])
   ;
   (H = error(existence_error(Type,Term),context(Pred,ExText))
   ->  S=[[error,existence],-(type,Type),-(term,Term),-(pred,Pred),-(msg,ExText)])
   ;
   (H = error(permission_error(Action,Type,Term),context(Pred,ExText))
   ->  S=[[error,permission],-(action,Action),-(type,Type),-(term,Term),-(pred,Pred),-(msg,ExText)])
   ;
   (H = error(illegal_state_error,context(Pred,ExText))
   ->  S=[[error,illegal_state],-(pred,Pred),-(msg,ExText)])
   ;
   throw(H)
   ).

% ===
% Minor testing
% ===

:- begin_tests(jpl_exceptions).

test(a1) :- catch(throw_instantiation_error(hello/3,'game over'),K,true),
            K == error(instantiation_error, context(hello/3, 'game over')).

test(a2) :- catch(throw_domain_error(hello/3,int,8,'game over'),K,true),
            K = error(domain_error(int, 8), context(hello/3, 'game over')).

test(a3) :- catch(throw_type_error(hello/3,int,8,'game over'),K,true),
            K = error(type_error(int, 8), context(hello/3, 'game over')).
            
test(a4) :- catch(throw_existence_error(hello/3,int,8,'game over'),K,true),
            K = error(existence_error(int, 8), context(hello/3, 'game over')).

test(a5) :- catch(throw_permission_error(hello/3,write,int,8,'game over'),K,true),
            K = error(permission_error(write, int, 8), context(hello/3, 'game over')).
           
test(a6) :- catch(throw_illegal_state_error(hello/3,'game over'),K,true),
            K = error(illegal_state_error, context(hello/3, 'game over')).


test(b1) :- catch_any_known(throw_instantiation_error(hello/3,'game over'),S),
            S == [[error, instantiation], pred-hello/3, msg-'game over'].

test(b2) :- catch_any_known(throw_domain_error(hello/3,int,8,'game over'),S),
            S == [[error, domain], type-int, term-8, pred-hello/3, msg-'game over'].

test(b3) :- catch_any_known(throw_type_error(hello/3,int,8,'game over'),S),
            S == [[error, type], type-int, term-8, pred-hello/3, msg-'game over'].
           
test(b4) :- catch_any_known(throw_existence_error(hello/3,int,8,'game over'),S),
            S == [[error, existence], type-int, term-8, pred-hello/3, msg-'game over'].

test(b5) :- catch_any_known(throw_permission_error(hello/3,write,int,8,'game over'),S),
            S = [[error, permission], action-write, type-int, term-8, pred-hello/3, msg-'game over'].
           
test(b6) :- catch_any_known(throw_illegal_state_error(hello/3,'game over'),S),
            S = [[error, illegal_state], pred-hello/3, msg-'game over'].
           
:- end_tests(jpl_exceptions).

rt(jpl_exceptions) :- run_tests(jpl_exceptions).

