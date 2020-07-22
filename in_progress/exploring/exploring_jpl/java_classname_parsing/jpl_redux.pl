/*  Part of JPL -- SWI-Prolog/Java interface

    Author:        Paul Singleton, Fred Dushin and Jan Wielemaker
    E-mail:        paul@jbgb.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2020, Paul Singleton
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(jpl,
    [   jpl_get_default_jvm_opts/1,
        jpl_set_default_jvm_opts/1,
        jpl_get_actual_jvm_opts/1,
        jpl_pl_lib_version/1,
        jpl_c_lib_version/1,
        jpl_pl_syntax/1,
        jpl_new/3,
        jpl_call/4,
        jpl_get/3,
        jpl_set/3,
        jpl_servlet_byref/3,
        jpl_servlet_byval/3,
        jpl_class_to_classname/2,
        jpl_class_to_type/2,
        jpl_classname_to_class/2,
        jpl_classname_to_type/2,
        jpl_datum_to_type/2,
        jpl_false/1,
        jpl_is_class/1,
        jpl_is_false/1,
        jpl_is_null/1,
        jpl_is_object/1,
        jpl_is_object_type/1,
        jpl_is_ref/1,
        jpl_is_true/1,
        jpl_is_type/1,
        jpl_is_void/1,
        jpl_null/1,
        jpl_object_to_class/2,
        jpl_object_to_type/2,
        jpl_primitive_type/1,
        jpl_ref_to_type/2,
        jpl_true/1,
        jpl_type_to_class/2,
        jpl_type_to_classname/2,
        jpl_void/1,
        jpl_array_to_length/2,
        jpl_array_to_list/2,
        jpl_datums_to_array/2,
        jpl_enumeration_element/2,
        jpl_enumeration_to_list/2,
        jpl_hashtable_pair/2,
        jpl_iterator_element/2,
        jpl_list_to_array/2,
        jpl_terms_to_array/2,
        jpl_array_to_terms/2,
        jpl_map_element/2,
        jpl_set_element/2
    ]).
:- autoload(library(apply),[maplist/2]).
:- autoload(library(debug),[debugging/1,debug/3]).
:- autoload(library(lists),
	    [member/2,nth0/3,nth1/3,append/3,flatten/2,select/3]).
:- autoload(library(shlib),[load_foreign_library/1]).


% Parsing facilities have been moved to a separate file

:- use_module(library(jpl_java_name_parsing)).

% Exception-generation facilities have been moved to a separate file

:- use_module(library(jpl_exceptions)).

/** <module> A Java interface for SWI Prolog 7.x

The library(jpl) provides a bidirectional interface to a Java Virtual Machine.

@see http://jpl7.org/
*/

% suppress debugging this library
:- set_prolog_flag(generate_debug_info, false).


%! jpl_new(+X, +Params, -V) is det.
%
% X can be:
%  * an atomic classname, e.g. =|'java.lang.String'|=
%  * or an atomic descriptor, e.g. =|'[I'|= or =|'Ljava.lang.String;'|=
%  * or a suitable type, i.e. any class(_,_) or array(_), e.g. class([java,util],['Date'])
%
% If X is an object (non-array)  type   or  descriptor and Params is a
% list of values or references, then V  is the result of an invocation
% of  that  type's  most  specifically-typed    constructor  to  whose
% respective formal parameters the actual   Params are assignable (and
% assigned).
%
% If X is an array type or descriptor   and Params is a list of values
% or references, each of which is   (independently)  assignable to the
% array element type, then V is a  new   array  of as many elements as
% Params has members,  initialised  with   the  respective  members of
% Params.
%
% If X is an array type  or   descriptor  and Params is a non-negative
% integer N, then V is a new array of that type, with N elements, each
% initialised to Java's appropriate default value for the type.
%
% If V is {Term} then we attempt to convert a new org.jpl7.Term instance to
% a corresponding term; this is of  little   obvious  use here, but is
% consistent with jpl_call/4 and jpl_get/3.

jpl_new(X, Params, V) :-
    Pred=jpl_new/3,
    (   var(X)
    ->  throw_instantiation_error(Pred,ea01)
    ;   jpl_is_type(X)                  % NB only class(_,_) or array(_)
    ->  Type = X
    ;   atom(X)                 % e.g. 'java.lang.String', '[L', 'boolean'
    ->  (   jpl_classname_to_type(X, Type)
        ->  true
        ;   throw_domain_error(Pred,classname,X,ea02)
        )
    ;   throw_type_error(Pred,instantiable,X,ea03)
    ),
    jpl_new_1(Type, Params, Vx),
    (   nonvar(V),
        V = {Term}  % yucky way of requesting Term->term conversion
    ->  (   jni_jref_to_term(Vx, TermX)    % fails if Vx is not a JRef to a org.jpl7.Term
        ->  Term = TermX
        ;   throw_type_error(Pred,term,Vx,ea04)
        )
    ;   V = Vx
    ).


%! jpl_new_1(+Tx, +Params, -Vx)
%
% (serves only jpl_new/3)
%
% Tx can be a class(_,_) or array(_) type.
%
% Params must be a proper list of constructor parameters.
%
% At exit, Vx is bound to a JPL reference to a new, initialised instance of Tx

jpl_new_1(class(Ps,Cs), Params, Vx) :-
    Pred=jpl_new/3,
    !,                                      % green (see below)
    Tx = class(Ps,Cs),
    (   var(Params)
    ->  throw_instantiation_error(Pred,eb01)
    ;   \+ is_list(Params)
    ->  throw_type_error(Pred,list,Params,eb01)
    ;   true
    ),
    length(Params, A),          % the "arity" of the required constructor
    jpl_type_to_class(Tx, Cx),  % throws Java exception if class is not found
    N = '<init>',               % JNI's constructor naming convention for GetMethodID()
    Tr = void,                  % all constructors have this return "type"
    findall(
        z3(I,MID,Tfps),
        jpl_method_spec(Tx, I, N, A, _Mods, MID, Tr, Tfps), % cached
        Z3s
    ),
    (   Z3s == []               % no constructors which require the given qty of parameters?
    ->  jpl_type_to_classname(Tx, Cn),
        (   jpl_call(Cx, isInterface, [], @(true))
        ->  throw_type_error(Pred,concrete_class,Cn,eb03)
        ;   throw_existence_error(Pred,constructor,Cn/A,eb04)
        )
    ;   (   catch(
                jpl_datums_to_types(Params, Taps),  % infer actual parameter types
                error(type_error(acyclic,Te),context(jpl_datum_to_type/2,Msg)),
                throw_type_error(Pred,acyclic,Te,Msg) % rethrow
            )
        ->  true
        ;   throw_domain_error(Pred,list(jpl_datum),Params,eb05)
        ),
        findall(
            z3(I,MID,Tfps),                 % select constructors to which actual parameters are assignable
            (   member(z3(I,MID,Tfps), Z3s),
                jpl_types_fit_types(Taps, Tfps) % assignability test: actual parameter types "fit" formal parameter types?
            ),
            Z3sA
        ),
        (   Z3sA == []                      % no type-assignable constructors?
        ->  (   Z3s = [_]
            ->  throw_existence_error(Pred,constructor,Tx/A,eb06)
            ;   throw_type_error(Pred,constructor_args,Params,eb07)
            )
        ;   Z3sA = [z3(I,MID,Tfps)]
        ->  true
        ;   jpl_z3s_to_most_specific_z3(Z3sA, z3(I,MID,Tfps))
        ->  true
        ;   throw_type_error(Pred,constructor_params,Params,eb08)
        )
    ),
    catch(
        jNewObject(Cx, MID, Tfps, Params, Vx),
        error(java_exception(_), 'java.lang.InstantiationException'),
        (   jpl_type_to_classname(Tx, Cn),
            throw_type_error(Pred,concrete_class,Cn,eb09) % Rethrow
        )
    ),
    jpl_cache_type_of_ref(Tx, Vx).          % since we know it

jpl_new_1(array(T), Params, Vx) :-
    Pred=jpl_new/3,
    !,
    (   var(Params)
    ->  throw_instantiation_error(Pred,ec01)
    ;   integer(Params)         % integer I -> array[0..I-1] of default values
    ->  (   Params >= 0
        ->  Len is Params
        ;   throw_domain_error(Pred,array_length,Params,ec02)
        )
    ;   is_list(Params)     % [V1,..VN] -> array[0..N-1] of respective values
    ->  length(Params, Len)
    ),
    jpl_new_array(T, Len, Vx), % NB may throw out-of-memory exception
    (   nth0(I, Params, Param),     % nmember fails silently when Params is integer
        jpl_set(Vx, I, Param),
        fail
    ;   true
    ),
    jpl_cache_type_of_ref(array(T), Vx).   % since we know it

jpl_new_1(T, _Params, _Vx) :-       % doomed attempt to create new primitive type instance (formerly a dubious completist feature :-)
    Pred=jpl_new/3,
    jpl_primitive_type(T),
    !,
    throw_domain_error(Pred,object_type,T,ed01).
  % (   var(Params)
  % ->  throw_instantiation_error(Pred,ed02)
  % ;   Params == []
  % ->  jpl_primitive_type_default_value(T, Vx)
  % ;   Params = [Param]
  % ->  jpl_primitive_type_term_to_value(T, Param, Vx)
  % ;   throw_domain_error(Pred,constructor_args,Params,ed03)
  % ).

jpl_new_1(T, _, _) :-
    Pred=jpl_new/3,
    throw_domain_error(Pred,jpl_type,T,ee01).


%! jpl_new_array(+ElementType, +Length, -NewArray) is det
%
% binds NewArray to a jref to a newly created Java array of ElementType and Length

jpl_new_array(boolean, Len, A) :-
    jNewBooleanArray(Len, A).
jpl_new_array(byte, Len, A) :-
    jNewByteArray(Len, A).
jpl_new_array(char, Len, A) :-
    jNewCharArray(Len, A).
jpl_new_array(short, Len, A) :-
    jNewShortArray(Len, A).
jpl_new_array(int, Len, A) :-
    jNewIntArray(Len, A).
jpl_new_array(long, Len, A) :-
    jNewLongArray(Len, A).
jpl_new_array(float, Len, A) :-
    jNewFloatArray(Len, A).
jpl_new_array(double, Len, A) :-
    jNewDoubleArray(Len, A).
jpl_new_array(array(T), Len, A) :-
    jpl_type_to_class(array(T), C),
    jNewObjectArray(Len, C, @(null), A).        % initialise each element to null
jpl_new_array(class(Ps,Cs), Len, A) :-
    jpl_type_to_class(class(Ps,Cs), C),
    jNewObjectArray(Len, C, @(null), A).


%! jpl_call(+X, +MethodName:atom, +Params:list(datum), -Result:datum) is det
%
% X should be either
%  * an object reference, e.g. =|<jref>(1552320)|= (for static or instance methods)
%  * or a classname, e.g. =|'java.util.Date'|= (for static methods only)
%  * or a descriptor, e.g. =|'Ljava.util.Date;'|= (for static methods only)
%  * or type, e.g. =|class([java,util],['Date'])|= (for static methods only)
%
% MethodName should be a method name (as an atom) (may involve dynamic overload resolution based on inferred types of params)
%
% Params should be a proper list (perhaps empty) of suitable actual parameters for the named method.
%
% The class or object may have several methods with the given name;
% JPL will resolve (per call) to the most appropriate method based on the quantity and inferred types of Params.
% This resolution mimics the corresponding static resolution performed by Java compilers.
%
% Finally, an attempt will be made to unify Result with the method's returned value,
% or with =|@(void)|= if it has none.

jpl_call(X, Mspec, Params, R) :-
    Pred=jpl_call/4

    (   jpl_object_to_type(X, Type)         % the usual case (goal fails safely if X is var or rubbish)
    ->  Obj = X,
        Kind = instance
    ;   var(X)
    ->  throw_instantiation_error(Pred,ef01)
    ;   atom(X)
    ->  (   jpl_classname_to_type(X, Type)     % does this attempt to load the class?
        ->  (   jpl_type_to_class(Type, ClassObj)
            ->  Kind = static
            ;   throw_existence_error(Pred,class,X,ef02)
            )
        ;   throw_type_error(Pred,class_name_or_descriptor,X,ef03)
        )
    ;   X = class(_,_)
    ->  Type = X,
        jpl_type_to_class(Type, ClassObj),
        Kind = static
    ;   X = array(_)
    ->  throw_type_error(Pred,object_or_class,X,ef04)
    ;   throw_domain_error(Pred,object_or_class,ef03)
    ),
    (   atom(Mspec)                 % the usual case, i.e. a method name
    ->  true
    ;   var(Mspec)
    ->  throw_instantiation_error(Pred,ef05)
    ;   throw_type_error(Pred,method_name,Mspec,ef05)
    ),
    (   is_list(Params)
    ->  (   catch(
                jpl_datums_to_types(Params, Taps),
                error(type_error(acyclic,Te),context(jpl_datum_to_type/2,Msg)),
                throw_type_error(Pred,acyclic,Te,Msg) % Rethrow
            )
        ->  true
        ;   throw_type_error(Pred,method_params,Params,ef06)
        ),
        length(Params, A)
    ;   var(Params)
    ->  throw_instantiation_error(ef07)
    ;   throw_type_error(Pred,method_params,Params,ef07)
    ),
    (   Kind == instance
    ->  jpl_call_instance(Type, Obj, Mspec, Params, Taps, A, Rx)
    ;   jpl_call_static(Type, ClassObj, Mspec, Params, Taps, A, Rx)
    ),
    (   nonvar(R),
        R = {Term}  % yucky way of requesting Term->term conversion
    ->  (   jni_jref_to_term(Rx, TermX)    % fails if Rx isn't a JRef to a org.jpl7.Term
        ->  Term = TermX
        ;   throw_type_error(Pred,ef08)
        )
    ;   R = Rx
    ).


%! jpl_call_instance(+ObjectType, +Object, +MethodName, +Params, +ActualParamTypes, +Arity, -Result)
%
% calls the MethodName-d method (instance or static) of Object (which is of ObjectType),
% which most specifically applies to Params,
% which we have found to be (respectively) of ActualParamTypes,
% and of which there are Arity, yielding Result.

jpl_call_instance(Type, Obj, Mname, Params, Taps, A, Rx) :-
    Pred=jpl_call/4,
    findall(                    % get remaining details of all accessible methods of Obj's class (as denoted by Type)
        z5(I,Mods,MID,Tr,Tfps),
        jpl_method_spec(Type, I, Mname, A, Mods, MID, Tr, Tfps),
        Z5s
    ),
    (   Z5s = []
    ->  throw_existence_error(Pred,method,Mname/A,eg01)
    ;   findall(
            z5(I,Mods,MID,Tr,Tfps),             % those to which Params is assignable
            (   member(z5(I,Mods,MID,Tr,Tfps), Z5s),
                jpl_types_fit_types(Taps, Tfps) % assignability test: actual param types "fit" formal param types
            ),
            Z5sA                                % Params-assignable methods
        ),
        (   Z5sA == []
        ->  throw_type_error(Pred,method_params,Params,eg02)
        ;   Z5sA = [z5(I,Mods,MID,Tr,Tfps)]
        ->  true                                % exactly one applicable method
        ;   jpl_z5s_to_most_specific_z5(Z5sA, z5(I,Mods,MID,Tr,Tfps))
        ->  true                                % exactly one most-specific applicable method
        ;   throw_existence_error(Pred,most_specific_method,Mname/Params,eg03)
        )
    ),
    (   member(static, Mods)                                        % if the chosen method is static
    ->  jpl_object_to_class(Obj, ClassObj),                         % get a java.lang.Class instance which personifies Obj's class
        jpl_call_static_method(Tr, ClassObj, MID, Tfps, Params, Rx) % call static method w.r.t. associated Class object
    ;   jpl_call_instance_method(Tr, Obj, MID, Tfps, Params, Rx)    % else call (non-static) method w.r.t. object itself
    ).


%! jpl_call_static(+ClassType, +ClassObject, +MethodName, +Params, +ActualParamTypes, +Arity, -Result)
%
% calls the MethodName-d static method of the class (which is of ClassType,
% and which is represented by the java.lang.Class instance ClassObject)
% which most specifically applies to Params,
% which we have found to be (respectively) of ActualParamTypes,
% and of which there are Arity, yielding Result.

jpl_call_static(Type, ClassObj, Mname, Params, Taps, A, Rx) :-
    Pred=jpl_call/4,
    findall(                    % get all accessible static methods of the class denoted by Type and ClassObj
        z5(I,Mods,MID,Tr,Tfps),
        (   jpl_method_spec(Type, I, Mname, A, Mods, MID, Tr, Tfps),
            member(static, Mods)
        ),
        Z5s
    ),
    (   Z5s = []
    ->  throw_existence_error(Pred,method,Mname/A,eh01)
    ;   findall(
            z5(I,Mods,MID,Tr,Tfps),
            (   member(z5(I,Mods,MID,Tr,Tfps), Z5s),
                jpl_types_fit_types(Taps, Tfps) % assignability test: actual param types "fit" formal param types
            ),
            Z5sA                                % Params-assignable methods
        ),
        (   Z5sA == []
        ->  throw_type_error(Pred,method_params,Params,eh02)
        ;   Z5sA = [z5(I,Mods,MID,Tr,Tfps)]
        ->  true                % exactly one applicable method
        ;   jpl_z5s_to_most_specific_z5(Z5sA, z5(I,Mods,MID,Tr,Tfps))
        ->  true                % exactly one most-specific applicable method
        ;   throw_existence_error(Pred,most_specific_method,Mname/Params,eh03)
        )
    ),
    jpl_call_static_method(Tr, ClassObj, MID, Tfps, Params, Rx).


%! jpl_call_instance_method(+Type, +ClassObject, +MethodID, +FormalParamTypes, +Params, -Result)

jpl_call_instance_method(void, Class, MID, Tfps, Ps, R) :-
    jCallVoidMethod(Class, MID, Tfps, Ps),
    jpl_void(R).
jpl_call_instance_method(boolean, Class, MID, Tfps, Ps, R) :-
    jCallBooleanMethod(Class, MID, Tfps, Ps, R).
jpl_call_instance_method(byte, Class, MID, Tfps, Ps, R) :-
    jCallByteMethod(Class, MID, Tfps, Ps, R).
jpl_call_instance_method(char, Class, MID, Tfps, Ps, R) :-
    jCallCharMethod(Class, MID, Tfps, Ps, R).
jpl_call_instance_method(short, Class, MID, Tfps, Ps, R) :-
    jCallShortMethod(Class, MID, Tfps, Ps, R).
jpl_call_instance_method(int, Class, MID, Tfps, Ps, R) :-
    jCallIntMethod(Class, MID, Tfps, Ps, R).
jpl_call_instance_method(long, Class, MID, Tfps, Ps, R) :-
    jCallLongMethod(Class, MID, Tfps, Ps, R).
jpl_call_instance_method(float, Class, MID, Tfps, Ps, R) :-
    jCallFloatMethod(Class, MID, Tfps, Ps, R).
jpl_call_instance_method(double, Class, MID, Tfps, Ps, R) :-
    jCallDoubleMethod(Class, MID, Tfps, Ps, R).
jpl_call_instance_method(array(_), Class, MID, Tfps, Ps, R) :-
    jCallObjectMethod(Class, MID, Tfps, Ps, R).
jpl_call_instance_method(class(_,_), Class, MID, Tfps, Ps, R) :-
    jCallObjectMethod(Class, MID, Tfps, Ps, R).


%! jpl_call_static_method(+Type, +ClassObject, +MethodID, +FormalParamTypes, +Params, -Result)

jpl_call_static_method(void, Class, MID, Tfps, Ps, R) :-
    jCallStaticVoidMethod(Class, MID, Tfps, Ps),
    jpl_void(R).
jpl_call_static_method(boolean, Class, MID, Tfps, Ps, R) :-
    jCallStaticBooleanMethod(Class, MID, Tfps, Ps, R).
jpl_call_static_method(byte, Class, MID, Tfps, Ps, R) :-
    jCallStaticByteMethod(Class, MID, Tfps, Ps, R).
jpl_call_static_method(char, Class, MID, Tfps, Ps, R) :-
    jCallStaticCharMethod(Class, MID, Tfps, Ps, R).
jpl_call_static_method(short, Class, MID, Tfps, Ps, R) :-
    jCallStaticShortMethod(Class, MID, Tfps, Ps, R).
jpl_call_static_method(int, Class, MID, Tfps, Ps, R) :-
    jCallStaticIntMethod(Class, MID, Tfps, Ps, R).
jpl_call_static_method(long, Class, MID, Tfps, Ps, R) :-
    jCallStaticLongMethod(Class, MID, Tfps, Ps, R).
jpl_call_static_method(float, Class, MID, Tfps, Ps, R) :-
    jCallStaticFloatMethod(Class, MID, Tfps, Ps, R).
jpl_call_static_method(double, Class, MID, Tfps, Ps, R) :-
    jCallStaticDoubleMethod(Class, MID, Tfps, Ps, R).
jpl_call_static_method(array(_), Class, MID, Tfps, Ps, R) :-
    jCallStaticObjectMethod(Class, MID, Tfps, Ps, R).
jpl_call_static_method(class(_,_), Class, MID, Tfps, Ps, R) :-
    jCallStaticObjectMethod(Class, MID, Tfps, Ps, R).


%! jpl_get(+X, +Fspec, -V:datum) is det
%
% X can be
%
%  * a classname
%  * or a descriptor
%  * or an (object or array) type (for static fields)
%  * or a non-array object (for static and non-static fields)
%  * or an array (for 'length' pseudo field, or indexed element retrieval)
%
% Fspec can be
%
%  * an atomic field name
%  * or an integral array index (to get an element from an array)
%  * or a pair I-J of integers (to get a subrange of an array).
%
% Finally, an attempt will be made to unify V with the retrieved value or object reference.
%
% Examples
%
%  ==
%  jpl_get('java.awt.Cursor', 'NE_RESIZE_CURSOR', Q).
%  Q = 7.
%
%  jpl_new(array(class([java,lang],['String'])), [for,while,do,if,then,else,try,catch,finally], A),
%  jpl_get(A, 3-5, B).
%  B = [if, then, else].
%  ==

jpl_get(X, Fspec, V) :-
    Pred=jpl_get/3,
    (   jpl_object_to_type(X, Type)
    ->  Obj = X,
        jpl_get_instance(Type, Type, Obj, Fspec, Vx)   % pass Type twice for FAI
    ;   var(X)
    ->  throw_instantiation_error(Pred,ei01)
    ;   jpl_is_type(X)          % e.g. class([java,lang],['String']), array(int)
    ->  Type = X,
        (   jpl_type_to_class(Type, ClassObj)
        ->  jpl_get_static(Type, ClassObj, Fspec, Vx)
        ;   jpl_type_to_classname(Type, Classname),
            throw_existence_error(Pred,class,Classname,ei02)
        )
    ;   atom(X)
    ->  (   jpl_classname_to_type(X, Type)     % does this attempt to load the class?
        ->  (   jpl_type_to_class(Type, ClassObj)
            ->  jpl_get_static(Type, ClassObj, Fspec, Vx)
            ;   throw_existence_error(Pred,class,X,ei02)
            )
        ;   throw_type_error(Pred,class_name_or_descriptor,X,ei03)
        )
    ;   throw_domain_error(Pred,object_or_class,X,ei03)
    ),
    (   nonvar(V),
        V = {Term}  % yucky way of requesting Term->term conversion
    ->  (   jni_jref_to_term(Vx, TermX)    % fails if Rx is not a JRef to a org.jpl7.Term
        ->  Term = TermX
        ;   throw_type_error(Pred,ei04)
        )
    ;   V = Vx
    ).


%! jpl_get_static(+Type:type, +ClassObject:jref, +FieldName:atom, -Value:datum) is det
%
% ClassObject is an instance of   java.lang.Class which represents
% the same class as Type; Value   (Vx below) is guaranteed unbound
% on entry, and will, before exit,   be unified with the retrieved
% value

jpl_get_static(Type, ClassObj, Fname, Vx) :-
    Pred=jpl_get/3,
    (   atom(Fname)             % assume it's a field name
    ->  true
    ;   var(Fname)
    ->  throw_instantiation_error(Pred,ej01)
    ;   throw_type_error(Pred,field_name,Fname,ej02)
    ),
  % get static fields of the denoted class
    findall(
        z4(I,Mods,FID,Tf),
        (   jpl_field_spec(Type, I, Fname, Mods, FID, Tf),
            member(static, Mods)
        ),
        Z4s
    ),
    (   Z4s = []
    ->  throw_existence_error(Pred,field,Fname,ej03)
    ;   Z4s = [z4(I,_Mods,FID,Tf)]
    ->  jpl_get_static_field(Tf, ClassObj, FID, Vx)
    ;   throw_existence_error(Pred,unique_field,Fname,ej04)
    ).


%! jpl_get_instance(+Type, +Type, +Object, +FieldSpecifier, -Value) is det

jpl_get_instance(class(_,_), Type, Obj, Fname, Vx) :-
    Pred=jpl_get/3,
    (   atom(Fname)                 % the usual case
    ->  true
    ;   var(Fname)
    ->  throw_instantiation_error(Pred,ek01)
    ;   throw_type_error(Pred,field_name,Fname,ek02)
    ),
    findall(
        z4(I,Mods,FID,Tf),
        jpl_field_spec(Type, I, Fname, Mods, FID, Tf),
        Z4s
    ),
    (   Z4s = []
    ->  throw_existence_error(Pred,field,Fname,ek03)
    ;   Z4s = [z4(I,Mods,FID,Tf)]
    ->  (   member(static, Mods)
        ->  jpl_object_to_class(Obj, ClassObj),
            jpl_get_static_field(Tf, ClassObj, FID, Vx)
        ;   jpl_get_instance_field(Tf, Obj, FID, Vx)
        )
    ;   throw_existence_error(Pred,unique_field,Fname,ek04)
    ).

jpl_get_instance(array(ElementType), _, Array, Fspec, Vx) :-
    Pred=jpl_get/3
    (   var(Fspec)
    ->  throw_instantiation_error(Pred,el01)
    ;   integer(Fspec)
    ->  (   Fspec < 0       % lo bound check
        ->  throw_domain_error(Pred,array_index,Fspec,el02)
        ;   jGetArrayLength(Array, Len),
            Fspec >= Len    % hi bound check
        ->  throw_domain_error(Pred,array_index,Fspec,el03)
        ;   jpl_get_array_element(ElementType, Array, Fspec, Vx)
        )
    ;   Fspec = N-M     % NB should we support e.g. 3-2 -> [] ?
    ->  (   integer(N),
            integer(M)
        ->  (   N >= 0,
                M >= N
            ->  jGetArrayLength(Array, Len),
                (   N >= Len
                ->  throw_domain_error(Pred,array_index_range,N-M,el04)
                ;   M >= Len
                ->  throw_domain_error(Pred,array_index_range,N-M,el05)
                ;   jpl_get_array_elements(ElementType, Array, N, M, Vx)
                )
            ;   throw_domain_error(Pred,array_index_range,N-M,el06)
            )
        ;   throw_type_error(Pred,array_index_range,N-M,el06)
        )
    ;   atom(Fspec)
    ->  (   Fspec == length             % special-case for this solitary array "method"
        ->  jGetArrayLength(Array, Vx)
        ;   throw_domain_error(Pred,array_field_name,Fspec,el07)
        )
    ;   throw_type_error(Pred,array_lookup_spec,Fspec,el08)
    ).


%! jpl_get_array_element(+ElementType:type, +Array:jref, +Index, -Vc) is det
%
% Array is a JPL reference to a Java array of ElementType;  Vc is
% (unified with a JPL repn  of)   its  Index-th  (numbered from 0)
% element Java values are now  converted   to  Prolog terms within
% foreign code
%
% @tbd more of this could be done within foreign code

jpl_get_array_element(Type, Array, Index, Vc) :-
    (   (   Type = class(_,_)
        ;   Type = array(_)
        )
    ->  jGetObjectArrayElement(Array, Index, Vr)
    ;   jpl_primitive_type(Type)
    ->  jni_type_to_xput_code(Type, Xc),
        jni_alloc_buffer(Xc, 1, Bp),        % one-element buf for a Type
        jpl_get_primitive_array_region(Type, Array, Index, 1, Bp),
        jni_fetch_buffer_value(Bp, 0, Vr, Xc),    % zero-th element
        jni_free_buffer(Bp)
    ),
    Vr = Vc.    % redundant since Vc is always (?) unbound at call


%! jpl_get_array_elements(+ElementType, +Array, +N, +M, -Vs)
%
% serves only jpl_get_instance/5
%
% Vs will always be unbound on entry

jpl_get_array_elements(ElementType, Array, N, M, Vs) :-
    (   (   ElementType = class(_,_)
        ;   ElementType = array(_)
        )
    ->  jpl_get_object_array_elements(Array, N, M, Vs)
    ;   jpl_get_primitive_array_elements(ElementType, Array, N, M, Vs)
    ).


jpl_get_instance_field(boolean, Obj, FieldID, V) :-
    jGetBooleanField(Obj, FieldID, V).
jpl_get_instance_field(byte, Obj, FieldID, V) :-
    jGetByteField(Obj, FieldID, V).
jpl_get_instance_field(char, Obj, FieldID, V) :-
    jGetCharField(Obj, FieldID, V).
jpl_get_instance_field(short, Obj, FieldID, V) :-
    jGetShortField(Obj, FieldID, V).
jpl_get_instance_field(int, Obj, FieldID, V) :-
    jGetIntField(Obj, FieldID, V).
jpl_get_instance_field(long, Obj, FieldID, V) :-
    jGetLongField(Obj, FieldID, V).
jpl_get_instance_field(float, Obj, FieldID, V) :-
    jGetFloatField(Obj, FieldID, V).
jpl_get_instance_field(double, Obj, FieldID, V) :-
    jGetDoubleField(Obj, FieldID, V).
jpl_get_instance_field(class(_,_), Obj, FieldID, V) :-
    jGetObjectField(Obj, FieldID, V).
jpl_get_instance_field(array(_), Obj, FieldID, V) :-
    jGetObjectField(Obj, FieldID, V).


%!  jpl_get_object_array_elements(+Array, +LoIndex, +HiIndex, -Vcs) is det
%
%   Array should be a  (zero-based)  array   of  some  object  (array or
%   non-array)  type;  LoIndex  is  an   integer,    0   =<   LoIndex  <
%   length(Array);  HiIndex  is  an  integer,  LoIndex-1  =<  HiIndex  <
%   length(Array); at call, Vcs will be unbound;  at exit, Vcs will be a
%   list of (references to)  the   array's  elements  [LoIndex..HiIndex]
%   inclusive

jpl_get_object_array_elements(Array, Lo, Hi, Vcs) :-
    (   Lo =< Hi
    ->  Vcs = [Vc|Vcs2],
        jGetObjectArrayElement(Array, Lo, Vc),
        Next is Lo+1,
        jpl_get_object_array_elements(Array, Next, Hi, Vcs2)
    ;   Vcs = []
    ).


%!  jpl_get_primitive_array_elements(+ElementType, +Array, +LoIndex, +HiIndex, -Vcs) is det.
%
%   Array  should  be  a  (zero-based)  Java  array  of  (primitive)
%   ElementType; Vcs should be unbound on entry, and on exit will be
%   a list of (JPL representations of   the  values of) the elements
%   [LoIndex..HiIndex] inclusive

jpl_get_primitive_array_elements(ElementType, Array, Lo, Hi, Vcs) :-
    Size is Hi-Lo+1,
    (   Size == 0
    ->  Vcs = []
    ;   jni_type_to_xput_code(ElementType, Xc),
        jni_alloc_buffer(Xc, Size, Bp),
        jpl_get_primitive_array_region(ElementType, Array, Lo, Size, Bp),
        jpl_primitive_buffer_to_array(ElementType, Xc, Bp, 0, Size, Vcs),
        jni_free_buffer(Bp)
    ).


jpl_get_primitive_array_region(boolean, Array, Lo, S, I) :-
    jGetBooleanArrayRegion(Array, Lo, S, jbuf(I,boolean)).
jpl_get_primitive_array_region(byte, Array, Lo, S, I) :-
    jGetByteArrayRegion(Array, Lo, S, jbuf(I,byte)).
jpl_get_primitive_array_region(char, Array, Lo, S, I) :-
    jGetCharArrayRegion(Array, Lo, S, jbuf(I,char)).
jpl_get_primitive_array_region(short, Array, Lo, S, I) :-
    jGetShortArrayRegion(Array, Lo, S, jbuf(I,short)).
jpl_get_primitive_array_region(int, Array, Lo, S, I) :-
    jGetIntArrayRegion(Array, Lo, S, jbuf(I,int)).
jpl_get_primitive_array_region(long, Array, Lo, S, I) :-
    jGetLongArrayRegion(Array, Lo, S, jbuf(I,long)).
jpl_get_primitive_array_region(float, Array, Lo, S, I) :-
    jGetFloatArrayRegion(Array, Lo, S, jbuf(I,float)).
jpl_get_primitive_array_region(double, Array, Lo, S, I) :-
    jGetDoubleArrayRegion(Array, Lo, S, jbuf(I,double)).


jpl_get_static_field(boolean, Array, FieldID, V) :-
    jGetStaticBooleanField(Array, FieldID, V).
jpl_get_static_field(byte, Array, FieldID, V) :-
    jGetStaticByteField(Array, FieldID, V).
jpl_get_static_field(char, Array, FieldID, V) :-
    jGetStaticCharField(Array, FieldID, V).
jpl_get_static_field(short, Array, FieldID, V) :-
    jGetStaticShortField(Array, FieldID, V).
jpl_get_static_field(int, Array, FieldID, V) :-
    jGetStaticIntField(Array, FieldID, V).
jpl_get_static_field(long, Array, FieldID, V) :-
    jGetStaticLongField(Array, FieldID, V).
jpl_get_static_field(float, Array, FieldID, V) :-
    jGetStaticFloatField(Array, FieldID, V).
jpl_get_static_field(double, Array, FieldID, V) :-
    jGetStaticDoubleField(Array, FieldID, V).
jpl_get_static_field(class(_,_), Array, FieldID, V) :-
    jGetStaticObjectField(Array, FieldID, V).
jpl_get_static_field(array(_), Array, FieldID, V) :-
    jGetStaticObjectField(Array, FieldID, V).


%! jpl_set(+X, +Fspec, +V) is det.
%
% sets the Fspec-th field of (class or object) X to value V iff it is assignable
%
% X can be
%  * a class instance (for static or non-static fields)
%  * or an array (for indexed element or subrange assignment)
%  * or a classname, or a class(_,_) or array(_) type (for static fields)
%  * but not a String (no fields to retrieve)
%
% Fspec can be
%  * an atomic field name (overloading through shadowing has yet to be handled properly)
%  * or an array index I (X must be an array object: V is assigned to X[I])
%  * or a pair I-J of integers (X must be an array object, V must be a list of values: successive members of V are assigned to X[I..J])
%
% V must be a suitable value or object.

jpl_set(X, Fspec, V) :-
    Pred=jpl_set/3,
    (   jpl_object_to_type(X, Type)         % the usual case (test is safe if X is var or rubbish)
    ->  Obj = X,
        catch(
            jpl_set_instance(Type, Type, Obj, Fspec, V),    % first 'Type' is for FAI
            error(type_error(acyclic,Te),context(jpl_datum_to_type/2,Msg)),
            throw_type_error(Pred,acyclic,Te,Msg) % rethrow
        )
    ;   var(X)
    ->  throw_instantiation_error(Pred,em01)
    ;   (   atom(X)
        ->  (   jpl_classname_to_type(X, Type)          % it's a classname or descriptor...
            ->  true
            ;   throw_existence_error(Pred,class,X,em02)
            )
        ;   (   X = class(_,_)                          % it's a class type...
            ;   X = array(_)                            % ...or an array type
            )
        ->  Type = X
        ),
        (   jpl_type_to_class(Type, ClassObj)      % ...whose Class object is available
        ->  true
        ;   jpl_type_to_classname(Type, Classname),
            throw_existence_error(Pred,class,Classname,em03)
        )
    ->  catch(
            jpl_set_static(Type, ClassObj, Fspec, V),
            error(type_error(acyclic,Te),context(jpl_datum_to_type/2,Msg)),
            throw_type_error(Pred,acyclic,Te,Msg) % rethrow
        )
    ;   throw_domain_error(Pred,object_or_class,X,em04)
    ).


%! jpl_set_instance(+Type, +Type, +ObjectReference, +FieldName, +Value) is det.
%
%   ObjectReference is a JPL reference to a Java object
%   of the class denoted by Type (which is passed twice for first agument indexing);
%
%   FieldName should name a public, non-final (static or non-static) field of this object,
%   but could be anything, and is validated here;
%
%   Value should be assignable to the named field, but could be anything, and is validated here

jpl_set_instance(class(_,_), Type, Obj, Fname, V) :-    % a non-array object
    Pred=jpl_set/3,
    (   atom(Fname)                 % the usual case
    ->  true
    ;   var(Fname)
    ->  throw_instantiation_error(Pred,en01)
    ;   throw_type_error(Pred,field_name,Fname,en02)
    ),
    findall(
        z4(I,Mods,FID,Tf),
        jpl_field_spec(Type, I, Fname, Mods, FID, Tf),  % public fields of class denoted by Type
        Z4s
    ),
    (   Z4s = []
    ->  throw_existence_error(Pred,field,Fname,en03)
    ;   Z4s = [z4(I,Mods,FID,Tf)]
    ->  (   member(final, Mods)
        ->  throw_permission_error(Pred,modify,final_field,Fname,en04)
        ;   jpl_datum_to_type(V, Tv)
        ->  (   jpl_type_fits_type(Tv, Tf)
            ->  (   member(static, Mods)
                ->  jpl_object_to_class(Obj, ClassObj),
                    jpl_set_static_field(Tf, ClassObj, FID, V)
                ;   jpl_set_instance_field(Tf, Obj, FID, V)         % oughta be jpl_set_instance_field?
                )
            ;   jpl_type_to_nicename(Tf, NNf),
                throw_type_error(Pred,NNf,V,en05)
            )
        ;   throw_type_error(Pred,field_value,V,en06)
        )
    ;   throw_existence_error(Pred,field,Fname,en07)   % 'existence'? or some other sort of error maybe?
    ).

jpl_set_instance(array(Type), _, Obj, Fspec, V) :-
    Pred=jpl_set/3
    (   is_list(V)                  % a list of array element values
    ->  Vs = V
    ;   var(V)
    ->  throw_instantiation_error(Pred,eo01)
    ;   Vs = [V]                    % a single array element value
    ),
    length(Vs, Iv),
    (   var(Fspec)
    ->  throw_instantiation_error(Pred,eo02)
    ;   integer(Fspec)          % single-element assignment
    ->  (   Fspec < 0
        ->  throw_domain_error(Pred,array_index,Fspec,eo03)
        ;   Iv is 1
        ->  N is Fspec
        ;   Iv is 0
        ->  throw_domain_error(Pred,array_element(Fspec),Vs,eo04)
        ;   throw_domain_error(Pred,array_element(Fspec),Vs,eo05)
        )
    ;   Fspec = N-M             % element-sequence assignment
    ->  (   integer(N),
            integer(M)
        ->  (   N >= 0,
                Size is (M-N)+1,
                Size >= 0
            ->  (   Size == Iv
                ->  true
                ;   Size < Iv
                ->  throw_domain_error(Pred,array_elements(N-M),Vs,eo06)
                ;   throw_domain_error(Pred,array_elements(N-M),Vs,eo07)
                )
            ;   throw_domain_error(Pred,array_index_range,N-M,eo08)
            )
        ;   throw_type_error(Pred,array_index_range,N-M,eo08)
        )
    ;   atom(Fspec)
    ->  (   Fspec == length
        ->  throw_permission_error(Pred,modify,final_field,length,eo09)
        ;   throw_existence_error(Pred,field,Fspec,eo10)
        )
    ;   throw_domain_error(Pred,array_index,Fspec,eo11)
    ),
    jpl_set_array(Type, Obj, N, Iv, Vs).


%! jpl_set_static(+Type, +ClassObj, +FieldName, +Value) is det.
%
% We can rely on:
%  * Type being a class/2 type representing some accessible class
%  * ClassObj being an instance of java.lang.Class which represents the same class as Type
%
%   but FieldName could be anything, so we validate it here,
%   look for a suitable (static) field of the target class,
%   then call jpl_set_static_field/4 to attempt to assign Value (which could be anything) to it
%
% NB this does not yet handle shadowed fields correctly.

jpl_set_static(Type, ClassObj, Fname, V) :-
    Pred=jpl_set/3,
    (   atom(Fname)                     % the usual case
    ->  true
    ;   var(Fname)
    ->  throw_instantiation_error(Pred,pl_set/3,ep01)
    ;   throw_type_error(field_name,Fname,ep02)
    ),
    findall(  % get all static fields of the denoted class
        z4(I,Mods,FID,Tf),
        (   jpl_field_spec(Type, I, Fname, Mods, FID, Tf),
            member(static, Mods)
        ),
        Z4s
    ),
    (   Z4s = []
    ->  throw_existence_error(Pred,field,Fname,ep03)
    ;   Z4s = [z4(I,Mods,FID,Tf)]       % exactly one synonymous field?
    ->  (   member(final, Mods)
        ->  throw_permission_error(Pred,modify,final_field,Fname,ep04)
        ;   jpl_datum_to_type(V, Tv)
        ->  (   jpl_type_fits_type(Tv, Tf)
            ->  jpl_set_static_field(Tf, ClassObj, FID, V)
            ;   jpl_type_to_nicename(Tf, NNf),
                throw_type_error(Pred,NNf,V,ep05)
            )
        ;   throw_type_error(Pred,field_value,V,ep06)
        )
    ;   throw_existence_error(Pred,field,Fname,ep07)
    ).


%! jpl_set_array(+ElementType, +Array, +Offset, +DatumQty, +Datums) is det.
%
%   Datums, of which there are DatumQty,   are stashed in successive
%   elements of Array which is an   array of ElementType starting at
% the Offset-th (numbered from 0)
% throws error(type_error(acyclic,_),context(jpl_datum_to_type/2,_))

jpl_set_array(T, A, N, I, Ds) :-
    Pred=jpl_set/3,
    (   jpl_datums_to_types(Ds, Tds)        % most specialised types of given values
    ->  (   jpl_types_fit_type(Tds, T)      % all assignable to element type?
        ->  true
        ;   throw_type_error(Pred,array(T),Ds,eq01)
        )
    ;   throw_type_error(Pred,array(T),Ds,eq02)
    ),
    (   (   T = class(_,_)
        ;   T = array(_)                    % array elements are objects
        )
    ->  (   nth0(J, Ds, D),                 % for each datum
            Nd is N+J,                      % compute array index
            (   D = {Tq}                    % quoted term?
            ->  jni_term_to_jref(Tq, D2)    % convert to a JPL reference to a corresponding org.jpl7.Term object
            ;   D = D2
            ),
            jSetObjectArrayElement(A, Nd, D2),
            fail                            % iterate
        ;   true
        )
    ;   jpl_primitive_type(T)               % array elements are primitive values
    ->  jni_type_to_xput_code(T, Xc),
        jni_alloc_buffer(Xc, I, Bp),        % I-element buf of required primitive type
        jpl_set_array_1(Ds, T, 0, Bp),
        jpl_set_elements(T, A, N, I, Bp),
        jni_free_buffer(Bp)
    ;
        % throw_system_error(Pred,array_element_type,T,eq03)        % a system error with args is not ISO and this is not a system error
        throw_illegal_state_error(Pred,array_element_type,T,eq03)   % this is not ISO either, but at least it's meaningful
    ).


%! jpl_set_array_1(+Values, +Type, +BufferIndex, +BufferPointer) is det.
%
%   successive members of Values  are   stashed  as (primitive) Type
%   from the BufferIndex-th element (numbered from 0) onwards of the
%   buffer indicated by BufferPointer
%
%   NB  this   could  be done more efficiently (?) within foreign code...

jpl_set_array_1([], _, _, _).
jpl_set_array_1([V|Vs], Tprim, Ib, Bp) :-
    jni_type_to_xput_code(Tprim, Xc),
    jni_stash_buffer_value(Bp, Ib, V, Xc),
    Ibnext is Ib+1,
    jpl_set_array_1(Vs, Tprim, Ibnext, Bp).


jpl_set_elements(boolean, Obj, N, I, Bp) :-
    jSetBooleanArrayRegion(Obj, N, I, jbuf(Bp,boolean)).
jpl_set_elements(char, Obj, N, I, Bp) :-
    jSetCharArrayRegion(Obj, N, I, jbuf(Bp,char)).
jpl_set_elements(byte, Obj, N, I, Bp) :-
    jSetByteArrayRegion(Obj, N, I, jbuf(Bp,byte)).
jpl_set_elements(short, Obj, N, I, Bp) :-
    jSetShortArrayRegion(Obj, N, I, jbuf(Bp,short)).
jpl_set_elements(int, Obj, N, I, Bp) :-
    jSetIntArrayRegion(Obj, N, I, jbuf(Bp,int)).
jpl_set_elements(long, Obj, N, I, Bp) :-
    jSetLongArrayRegion(Obj, N, I, jbuf(Bp,long)).
jpl_set_elements(float, Obj, N, I, Bp) :-
    jSetFloatArrayRegion(Obj, N, I, jbuf(Bp,float)).
jpl_set_elements(double, Obj, N, I, Bp) :-
    jSetDoubleArrayRegion(Obj, N, I, jbuf(Bp,double)).


%! jpl_set_instance_field(+Type, +Obj, +FieldID, +V) is det.
%
% We can rely on Type, Obj and FieldID being valid, and on V being
%   assignable (if V is a quoted term then it is converted here)

jpl_set_instance_field(boolean, Obj, FieldID, V) :-
    jSetBooleanField(Obj, FieldID, V).
jpl_set_instance_field(byte, Obj, FieldID, V) :-
    jSetByteField(Obj, FieldID, V).
jpl_set_instance_field(char, Obj, FieldID, V) :-
    jSetCharField(Obj, FieldID, V).
jpl_set_instance_field(short, Obj, FieldID, V) :-
    jSetShortField(Obj, FieldID, V).
jpl_set_instance_field(int, Obj, FieldID, V) :-
    jSetIntField(Obj, FieldID, V).
jpl_set_instance_field(long, Obj, FieldID, V) :-
    jSetLongField(Obj, FieldID, V).
jpl_set_instance_field(float, Obj, FieldID, V) :-
    jSetFloatField(Obj, FieldID, V).
jpl_set_instance_field(double, Obj, FieldID, V) :-
    jSetDoubleField(Obj, FieldID, V).
jpl_set_instance_field(class(_,_), Obj, FieldID, V) :-  % also handles byval term assignments
    (   V = {T}                     % quoted term?
    ->  jni_term_to_jref(T, V2)     % convert to a JPL reference to a corresponding org.jpl7.Term object
    ;   V = V2
    ),
    jSetObjectField(Obj, FieldID, V2).
jpl_set_instance_field(array(_), Obj, FieldID, V) :-
    jSetObjectField(Obj, FieldID, V).


%! jpl_set_static_field(+Type, +ClassObj, +FieldID, +V)
%
% We can rely on Type, ClassObj and FieldID being valid,
% and on V being assignable (if V is a quoted term then it is converted here).

jpl_set_static_field(boolean, Obj, FieldID, V) :-
    jSetStaticBooleanField(Obj, FieldID, V).
jpl_set_static_field(byte, Obj, FieldID, V) :-
    jSetStaticByteField(Obj, FieldID, V).
jpl_set_static_field(char, Obj, FieldID, V) :-
    jSetStaticCharField(Obj, FieldID, V).
jpl_set_static_field(short, Obj, FieldID, V) :-
    jSetStaticShortField(Obj, FieldID, V).
jpl_set_static_field(int, Obj, FieldID, V) :-
    jSetStaticIntField(Obj, FieldID, V).
jpl_set_static_field(long, Obj, FieldID, V) :-
    jSetStaticLongField(Obj, FieldID, V).
jpl_set_static_field(float, Obj, FieldID, V) :-
    jSetStaticFloatField(Obj, FieldID, V).
jpl_set_static_field(double, Obj, FieldID, V) :-
    jSetStaticDoubleField(Obj, FieldID, V).
jpl_set_static_field(class(_,_), Obj, FieldID, V) :-    % also handles byval term assignments
    (   V = {T}                         % quoted term?
    ->  jni_term_to_jref(T, V2)         % convert to a JPL reference to a corresponding org.jpl7.Term object
    ;   V = V2
    ),
    jSetStaticObjectField(Obj, FieldID, V2).
jpl_set_static_field(array(_), Obj, FieldID, V) :-
    jSetStaticObjectField(Obj, FieldID, V).


%! jpl_get_default_jvm_opts(-Opts:list(atom)) is det
%
% Returns (as a list of atoms) the options which will be passed to the JVM when it is initialised,
% e.g. =|['-Xrs']|=

jpl_get_default_jvm_opts(Opts) :-
    jni_get_default_jvm_opts(Opts).


%! jpl_set_default_jvm_opts(+Opts:list(atom)) is det
%
%   Replaces the default JVM initialisation options with those supplied.

jpl_set_default_jvm_opts(Opts) :-
    is_list(Opts),
    length(Opts, N),
    jni_set_default_jvm_opts(N, Opts).


%! jpl_get_actual_jvm_opts(-Opts:list(atom)) is semidet
%
% Returns (as a list of atoms) the options with which the JVM was initialised.
%
% Fails silently if a JVM has not yet been started, and can thus be used to test for this.

jpl_get_actual_jvm_opts(Opts) :-
    jni_get_actual_jvm_opts(Opts).


jpl_assert(Fact) :-
    (   jpl_assert_policy(Fact, yes)
    ->  assert(Fact)
    ;   true
    ).


jpl_assert_policy(jpl_field_spec_cache(_,_,_,_,_,_), yes).
jpl_assert_policy(jpl_method_spec_cache(_,_,_,_,_,_,_,_), yes).
jpl_assert_policy(jpl_class_tag_type_cache(_,_), yes).
jpl_assert_policy(jpl_classname_type_cache(_,_), yes).
jpl_assert_policy(jpl_iref_type_cache(_,_), no).   % must correspond to JPL_CACHE_TYPE_OF_REF in jpl.c
jpl_assert_policy(jpl_field_spec_is_cached(_), YN) :-
    jpl_assert_policy(jpl_field_spec_cache(_,_,_,_,_,_), YN).
jpl_assert_policy(jpl_method_spec_is_cached(_), YN) :-
    jpl_assert_policy(jpl_method_spec_cache(_,_,_,_,_,_,_,_), YN).


%! jpl_tidy_iref_type_cache(+Iref) is det.
%
% Delete the cached type info, if any, under Iref.
%
% Called from jpl.c's jni_free_iref() via jni_tidy_iref_type_cache()

jpl_tidy_iref_type_cache(Iref) :-
  % write('[decaching types for iref='), write(Iref), write(']'), nl,
    retractall(jpl_iref_type_cache(Iref,_)),
    true.


jpl_fergus_find_candidate([], Candidate, Candidate, []).
jpl_fergus_find_candidate([X|Xs], Candidate0, Candidate, Rest) :-
    (   jpl_fergus_greater(X, Candidate0)
    ->  Candidate1 = X,
        Rest = [Candidate0|Rest1]
    ;   Candidate1 = Candidate0,
        Rest = [X|Rest1]
    ),
    jpl_fergus_find_candidate(Xs, Candidate1, Candidate, Rest1).


jpl_fergus_greater(z5(_,_,_,_,Tps1), z5(_,_,_,_,Tps2)) :-
    jpl_types_fit_types(Tps1, Tps2).
jpl_fergus_greater(z3(_,_,Tps1), z3(_,_,Tps2)) :-
    jpl_types_fit_types(Tps1, Tps2).


%! jpl_fergus_is_the_greatest(+Xs:list(T), -GreatestX:T)
%
% Xs is a list of things  for which jpl_fergus_greater/2 defines a
% partial ordering; GreatestX is one of  those, than which none is
% greater; fails if there is more   than  one such; this algorithm
% was contributed to c.l.p by Fergus   Henderson in response to my
% "there must be a better way" challenge: there was, this is it

jpl_fergus_is_the_greatest([X|Xs], Greatest) :-
    jpl_fergus_find_candidate(Xs, X, Greatest, Rest),
    forall(
        member(R, Rest),
        jpl_fergus_greater(Greatest, R)
    ).


%! jpl_z3s_to_most_specific_z3(+Zs, -Z)
%
% Zs is a list of arity-matching, type-suitable z3(I,MID,Tfps).
%
% Z is the single most specific element of Zs,
% i.e. that than which no other z3/3 has a more specialised signature (fails if there is more than one such).

jpl_z3s_to_most_specific_z3(Zs, Z) :-
    jpl_fergus_is_the_greatest(Zs, Z).


%! jpl_z5s_to_most_specific_z5(+Zs, -Z)
%
% Zs is a list of arity-matching, type-suitable z5(I,Mods,MID,Tr,Tfps)
%
% Z is the single most specific element of Zs,
% i.e. that than which no other z5/5 has a more specialised signature (fails if there is more than one such)

jpl_z5s_to_most_specific_z5(Zs, Z) :-
    jpl_fergus_is_the_greatest(Zs, Z).


%! jpl_pl_lib_version(-Version)
%
% Version is the fully qualified version identifier of the in-use Prolog component (jpl.pl) of JPL.
%
% It should exactly match the version identifiers of JPL's C (jpl.c) and Java (jpl.jar) components.
%
% Example
%
%  ==
%  ?- jpl_pl_lib_version(V).
%  V = '7.6.0-alpha'.
%  ==

jpl_pl_lib_version(VersionString) :-
    jpl_pl_lib_version(Major, Minor, Patch, Status),
    atomic_list_concat([Major,'.',Minor,'.',Patch,'-',Status], VersionString).


%! jpl_pl_lib_version(-Major, -Minor, -Patch, -Status)
%
% Major, Minor, Patch and Status are the respective components of the version identifier of the in-use C component (jpl.c) of JPL.
%
% Example
%
%  ==
%  ?- jpl:jpl_pl_lib_version(Major, Minor, Patch, Status).
%  Major = 7,
%  Minor = 4,
%  Patch = 0,
%  Status = alpha.
%  ==

jpl_pl_lib_version(7, 6, 0, alpha).  % jref as blob

%! jpl_c_lib_version(-Version)
%
% Version is the fully qualified version identifier of the in-use C component (jpl.c) of JPL.
%
% It should exactly match the version identifiers of JPL's Prolog (jpl.pl) and Java (jpl.jar) components.
%
% Example
%
%  ==
%  ?- jpl_c_lib_version(V).
%  V = '7.4.0-alpha'.
%  ==


%! jpl_java_lib_version(-Version)
%
% Version is the fully qualified version identifier of the in-use Java component (jpl.jar) of JPL.
%
% Example
%
%  ==
%  ?- jpl:jpl_java_lib_version(V).
%  V = '7.4.0-alpha'.
%  ==

%! jpl_java_lib_version(V)

jpl_java_lib_version(V) :-
    jpl_call('org.jpl7.JPL', version_string, [], V).


%! jpl_pl_lib_path(-Path:atom)

jpl_pl_lib_path(Path) :-
    module_property(jpl, file(Path)).


%! jpl_c_lib_path(-Path:atom)

jpl_c_lib_path(Path) :-
    shlib:current_library(_, _, Path, jpl, _),
    !.


%! jpl_java_lib_path(-Path:atom)

jpl_java_lib_path(Path) :-
    jpl_call('org.jpl7.JPL', jarPath, [], Path).


%! jCallBooleanMethod(+Obj:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rbool:boolean)

jCallBooleanMethod(Obj, MethodID, Types, Params, Rbool) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(39, Obj, MethodID, ParamBuf, Rbool).



%! jCallByteMethod(+Obj:jref, +MethodID:methodId, +Types, +Params:list(datum), -Rbyte:byte)

jCallByteMethod(Obj, MethodID, Types, Params, Rbyte) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(42, Obj, MethodID, ParamBuf, Rbyte).



%! jCallCharMethod(+Obj:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rchar:char)

jCallCharMethod(Obj, MethodID, Types, Params, Rchar) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(45, Obj, MethodID, ParamBuf, Rchar).


%! jCallDoubleMethod(+Obj:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rdouble:double)

jCallDoubleMethod(Obj, MethodID, Types, Params, Rdouble) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(60, Obj, MethodID, ParamBuf, Rdouble).


%! jCallFloatMethod(+Obj:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rfloat:float)

jCallFloatMethod(Obj, MethodID, Types, Params, Rfloat) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(57, Obj, MethodID, ParamBuf, Rfloat).


%! jCallIntMethod(+Obj:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rint:int)

jCallIntMethod(Obj, MethodID, Types, Params, Rint) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(51, Obj, MethodID, ParamBuf, Rint).


%! jCallLongMethod(+Obj:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rlong:long)

jCallLongMethod(Obj, MethodID, Types, Params, Rlong) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(54, Obj, MethodID, ParamBuf, Rlong).


%! jCallObjectMethod(+Obj:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Robj:jref)

jCallObjectMethod(Obj, MethodID, Types, Params, Robj) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(36, Obj, MethodID, ParamBuf, Robj).


%! jCallShortMethod(+Obj:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rshort:short)

jCallShortMethod(Obj, MethodID, Types, Params, Rshort) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(48, Obj, MethodID, ParamBuf, Rshort).


%! jCallStaticBooleanMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rbool:boolean)

jCallStaticBooleanMethod(Class, MethodID, Types, Params, Rbool) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(119, Class, MethodID, ParamBuf, Rbool).


%! jCallStaticByteMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rbyte:byte)

jCallStaticByteMethod(Class, MethodID, Types, Params, Rbyte) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(122, Class, MethodID, ParamBuf, Rbyte).


%! jCallStaticCharMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rchar:char)

jCallStaticCharMethod(Class, MethodID, Types, Params, Rchar) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(125, Class, MethodID, ParamBuf, Rchar).


%! jCallStaticDoubleMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rdouble:double)

jCallStaticDoubleMethod(Class, MethodID, Types, Params, Rdouble) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(140, Class, MethodID, ParamBuf, Rdouble).


%! jCallStaticFloatMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rfloat:float)

jCallStaticFloatMethod(Class, MethodID, Types, Params, Rfloat) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(137, Class, MethodID, ParamBuf, Rfloat).


%! jCallStaticIntMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rint:int)

jCallStaticIntMethod(Class, MethodID, Types, Params, Rint) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(131, Class, MethodID, ParamBuf, Rint).


%! jCallStaticLongMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rlong:long)

jCallStaticLongMethod(Class, MethodID, Types, Params, Rlong) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(134, Class, MethodID, ParamBuf, Rlong).


%! jCallStaticObjectMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Robj:jref)

jCallStaticObjectMethod(Class, MethodID, Types, Params, Robj) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(116, Class, MethodID, ParamBuf, Robj).


%! jCallStaticShortMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Rshort:short)

jCallStaticShortMethod(Class, MethodID, Types, Params, Rshort) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(128, Class, MethodID, ParamBuf, Rshort).


%! jCallStaticVoidMethod(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum))

jCallStaticVoidMethod(Class, MethodID, Types, Params) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_void(143, Class, MethodID, ParamBuf).


%! jCallVoidMethod(+Obj:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum))

jCallVoidMethod(Obj, MethodID, Types, Params) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_void(63, Obj, MethodID, ParamBuf).


%! jFindClass(+ClassName:findclassname, -Class:jref)

jFindClass(ClassName, Class) :-
    jni_func(6, ClassName, Class).


%! jGetArrayLength(+Array:jref, -Size:int)

jGetArrayLength(Array, Size) :-
    jni_func(171, Array, Size).


%! jGetBooleanArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:boolean_buf)

jGetBooleanArrayRegion(Array, Start, Len, Buf) :-
    jni_void(199, Array, Start, Len, Buf).


%! jGetBooleanField(+Obj:jref, +FieldID:fieldId, -Rbool:boolean)

jGetBooleanField(Obj, FieldID, Rbool) :-
    jni_func(96, Obj, FieldID, Rbool).


%! jGetByteArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:byte_buf)

jGetByteArrayRegion(Array, Start, Len, Buf) :-
    jni_void(200, Array, Start, Len, Buf).


%! jGetByteField(+Obj:jref, +FieldID:fieldId, -Rbyte:byte)

jGetByteField(Obj, FieldID, Rbyte) :-
    jni_func(97, Obj, FieldID, Rbyte).


%! jGetCharArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:char_buf)

jGetCharArrayRegion(Array, Start, Len, Buf) :-
    jni_void(201, Array, Start, Len, Buf).


%! jGetCharField(+Obj:jref, +FieldID:fieldId, -Rchar:char)

jGetCharField(Obj, FieldID, Rchar) :-
    jni_func(98, Obj, FieldID, Rchar).


%! jGetDoubleArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:double_buf)

jGetDoubleArrayRegion(Array, Start, Len, Buf) :-
    jni_void(206, Array, Start, Len, Buf).


%! jGetDoubleField(+Obj:jref, +FieldID:fieldId, -Rdouble:double)

jGetDoubleField(Obj, FieldID, Rdouble) :-
    jni_func(103, Obj, FieldID, Rdouble).


%! jGetFieldID(+Class:jref, +Name:fieldName, +Type:type, -FieldID:fieldId)

jGetFieldID(Class, Name, Type, FieldID) :-
    jpl_type_to_descriptor(Type, TD),
    jni_func(94, Class, Name, TD, FieldID).


%! jGetFloatArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:float_buf)

jGetFloatArrayRegion(Array, Start, Len, Buf) :-
    jni_void(205, Array, Start, Len, Buf).


%! jGetFloatField(+Obj:jref, +FieldID:fieldId, -Rfloat:float)

jGetFloatField(Obj, FieldID, Rfloat) :-
    jni_func(102, Obj, FieldID, Rfloat).


%! jGetIntArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:int_buf)

jGetIntArrayRegion(Array, Start, Len, Buf) :-
    jni_void(203, Array, Start, Len, Buf).


%! jGetIntField(+Obj:jref, +FieldID:fieldId, -Rint:int)

jGetIntField(Obj, FieldID, Rint) :-
    jni_func(100, Obj, FieldID, Rint).


%! jGetLongArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:long_buf)

jGetLongArrayRegion(Array, Start, Len, Buf) :-
    jni_void(204, Array, Start, Len, Buf).


%! jGetLongField(+Obj:jref, +FieldID:fieldId, -Rlong:long)

jGetLongField(Obj, FieldID, Rlong) :-
    jni_func(101, Obj, FieldID, Rlong).


%! jGetMethodID(+Class:jref, +Name:atom, +Type:type, -MethodID:methodId)

jGetMethodID(Class, Name, Type, MethodID) :-
    jpl_type_to_descriptor(Type, TD),
    jni_func(33, Class, Name, TD, MethodID).


%! jGetObjectArrayElement(+Array:jref, +Index:int, -Obj:jref)

jGetObjectArrayElement(Array, Index, Obj) :-
    jni_func(173, Array, Index, Obj).


%! jGetObjectClass(+Object:jref, -Class:jref)

jGetObjectClass(Object, Class) :-
    jni_func(31, Object, Class).


%! jGetObjectField(+Obj:jref, +FieldID:fieldId, -RObj:jref)

jGetObjectField(Obj, FieldID, Robj) :-
    jni_func(95, Obj, FieldID, Robj).


%! jGetShortArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:short_buf)

jGetShortArrayRegion(Array, Start, Len, Buf) :-
    jni_void(202, Array, Start, Len, Buf).


%! jGetShortField(+Obj:jref, +FieldID:fieldId, -Rshort:short)

jGetShortField(Obj, FieldID, Rshort) :-
    jni_func(99, Obj, FieldID, Rshort).


%! jGetStaticBooleanField(+Class:jref, +FieldID:fieldId, -Rbool:boolean)

jGetStaticBooleanField(Class, FieldID, Rbool) :-
    jni_func(146, Class, FieldID, Rbool).


%! jGetStaticByteField(+Class:jref, +FieldID:fieldId, -Rbyte:byte)

jGetStaticByteField(Class, FieldID, Rbyte) :-
    jni_func(147, Class, FieldID, Rbyte).


%! jGetStaticCharField(+Class:jref, +FieldID:fieldId, -Rchar:char)

jGetStaticCharField(Class, FieldID, Rchar) :-
    jni_func(148, Class, FieldID, Rchar).


%! jGetStaticDoubleField(+Class:jref, +FieldID:fieldId, -Rdouble:double)

jGetStaticDoubleField(Class, FieldID, Rdouble) :-
    jni_func(153, Class, FieldID, Rdouble).


%! jGetStaticFieldID(+Class:jref, +Name:fieldName, +Type:type, -FieldID:fieldId)

jGetStaticFieldID(Class, Name, Type, FieldID) :-
    jpl_type_to_descriptor(Type, TD),               % cache this?
    jni_func(144, Class, Name, TD, FieldID).


%! jGetStaticFloatField(+Class:jref, +FieldID:fieldId, -Rfloat:float)

jGetStaticFloatField(Class, FieldID, Rfloat) :-
    jni_func(152, Class, FieldID, Rfloat).


%! jGetStaticIntField(+Class:jref, +FieldID:fieldId, -Rint:int)

jGetStaticIntField(Class, FieldID, Rint) :-
    jni_func(150, Class, FieldID, Rint).


%! jGetStaticLongField(+Class:jref, +FieldID:fieldId, -Rlong:long)

jGetStaticLongField(Class, FieldID, Rlong) :-
    jni_func(151, Class, FieldID, Rlong).


%! jGetStaticMethodID(+Class:jref, +Name:methodName, +Type:type, -MethodID:methodId)

jGetStaticMethodID(Class, Name, Type, MethodID) :-
    jpl_type_to_descriptor(Type, TD),
    jni_func(113, Class, Name, TD, MethodID).


%! jGetStaticObjectField(+Class:jref, +FieldID:fieldId, -RObj:jref)

jGetStaticObjectField(Class, FieldID, Robj) :-
    jni_func(145, Class, FieldID, Robj).


%! jGetStaticShortField(+Class:jref, +FieldID:fieldId, -Rshort:short)

jGetStaticShortField(Class, FieldID, Rshort) :-
    jni_func(149, Class, FieldID, Rshort).


%! jGetSuperclass(+Class1:jref, -Class2:jref)

jGetSuperclass(Class1, Class2) :-
    jni_func(10, Class1, Class2).


%! jIsAssignableFrom(+Class1:jref, +Class2:jref)

jIsAssignableFrom(Class1, Class2) :-
    jni_func(11, Class1, Class2, @(true)).


%! jNewBooleanArray(+Length:int, -Array:jref)

jNewBooleanArray(Length, Array) :-
    jni_func(175, Length, Array).


%! jNewByteArray(+Length:int, -Array:jref)

jNewByteArray(Length, Array) :-
    jni_func(176, Length, Array).


%! jNewCharArray(+Length:int, -Array:jref)

jNewCharArray(Length, Array) :-
    jni_func(177, Length, Array).


%! jNewDoubleArray(+Length:int, -Array:jref)

jNewDoubleArray(Length, Array) :-
    jni_func(182, Length, Array).


%! jNewFloatArray(+Length:int, -Array:jref)

jNewFloatArray(Length, Array) :-
    jni_func(181, Length, Array).


%! jNewIntArray(+Length:int, -Array:jref)

jNewIntArray(Length, Array) :-
    jni_func(179, Length, Array).


%! jNewLongArray(+Length:int, -Array:jref)

jNewLongArray(Length, Array) :-
    jni_func(180, Length, Array).


%! jNewObject(+Class:jref, +MethodID:methodId, +Types:list(type), +Params:list(datum), -Obj:jref)

jNewObject(Class, MethodID, Types, Params, Obj) :-
    jni_params_put(Params, Types, ParamBuf),
    jni_func(30, Class, MethodID, ParamBuf, Obj).


%! jNewObjectArray(+Len:int, +Class:jref, +InitVal:jref, -Array:jref)

jNewObjectArray(Len, Class, InitVal, Array) :-
    jni_func(172, Len, Class, InitVal, Array).


%! jNewShortArray(+Length:int, -Array:jref)

jNewShortArray(Length, Array) :-
    jni_func(178, Length, Array).


%! jSetBooleanArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:boolean_buf)

jSetBooleanArrayRegion(Array, Start, Len, Buf) :-
    jni_void(207, Array, Start, Len, Buf).


%! jSetBooleanField(+Obj:jref, +FieldID:fieldId, +Rbool:boolean)

jSetBooleanField(Obj, FieldID, Rbool) :-
    jni_void(105, Obj, FieldID, Rbool).


%! jSetByteArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:byte_buf)

jSetByteArrayRegion(Array, Start, Len, Buf) :-
    jni_void(208, Array, Start, Len, Buf).


%! jSetByteField(+Obj:jref, +FieldID:fieldId, +Rbyte:byte)

jSetByteField(Obj, FieldID, Rbyte) :-
    jni_void(106, Obj, FieldID, Rbyte).


%! jSetCharArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:char_buf)

jSetCharArrayRegion(Array, Start, Len, Buf) :-
    jni_void(209, Array, Start, Len, Buf).


%! jSetCharField(+Obj:jref, +FieldID:fieldId, +Rchar:char)

jSetCharField(Obj, FieldID, Rchar) :-
    jni_void(107, Obj, FieldID, Rchar).


%! jSetDoubleArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:double_buf)

jSetDoubleArrayRegion(Array, Start, Len, Buf) :-
    jni_void(214, Array, Start, Len, Buf).


%! jSetDoubleField(+Obj:jref, +FieldID:fieldId, +Rdouble:double)

jSetDoubleField(Obj, FieldID, Rdouble) :-
    jni_void(112, Obj, FieldID, Rdouble).


%! jSetFloatArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:float_buf)

jSetFloatArrayRegion(Array, Start, Len, Buf) :-
    jni_void(213, Array, Start, Len, Buf).


%! jSetFloatField(+Obj:jref, +FieldID:fieldId, +Rfloat:float)

jSetFloatField(Obj, FieldID, Rfloat) :-
    jni_void(111, Obj, FieldID, Rfloat).


%! jSetIntArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:int_buf)

jSetIntArrayRegion(Array, Start, Len, Buf) :-
    jni_void(211, Array, Start, Len, Buf).


%! jSetIntField(+Obj:jref, +FieldID:fieldId, +Rint:int)

jSetIntField(Obj, FieldID, Rint) :-
    jni_void(109, Obj, FieldID, Rint).


%! jSetLongArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:long_buf)

jSetLongArrayRegion(Array, Start, Len, Buf) :-
    jni_void(212, Array, Start, Len, Buf).


%! jSetLongField(+Obj:jref, +FieldID:fieldId, +Rlong:long)

jSetLongField(Obj, FieldID, Rlong) :-
    jni_void(110, Obj, FieldID, Rlong).


%! jSetObjectArrayElement(+Array:jref, +Index:int, +Obj:jref)

jSetObjectArrayElement(Array, Index, Obj) :-
    jni_void(174, Array, Index, Obj).


%! jSetObjectField(+Obj:jref, +FieldID:fieldId, +RObj:jref)

jSetObjectField(Obj, FieldID, Robj) :-
    jni_void(104, Obj, FieldID, Robj).


%! jSetShortArrayRegion(+Array:jref, +Start:int, +Len:int, +Buf:short_buf)

jSetShortArrayRegion(Array, Start, Len, Buf) :-
    jni_void(210, Array, Start, Len, Buf).


%! jSetShortField(+Obj:jref, +FieldID:fieldId, +Rshort:short)

jSetShortField(Obj, FieldID, Rshort) :-
    jni_void(108, Obj, FieldID, Rshort).


%! jSetStaticBooleanField(+Class:jref, +FieldID:fieldId, +Rbool:boolean)

jSetStaticBooleanField(Class, FieldID, Rbool) :-
    jni_void(155, Class, FieldID, Rbool).


%! jSetStaticByteField(+Class:jref, +FieldID:fieldId, +Rbyte:byte)

jSetStaticByteField(Class, FieldID, Rbyte) :-
    jni_void(156, Class, FieldID, Rbyte).


%! jSetStaticCharField(+Class:jref, +FieldID:fieldId, +Rchar:char)

jSetStaticCharField(Class, FieldID, Rchar) :-
    jni_void(157, Class, FieldID, Rchar).


%! jSetStaticDoubleField(+Class:jref, +FieldID:fieldId, +Rdouble:double)

jSetStaticDoubleField(Class, FieldID, Rdouble) :-
    jni_void(162, Class, FieldID, Rdouble).


%! jSetStaticFloatField(+Class:jref, +FieldID:fieldId, +Rfloat:float)

jSetStaticFloatField(Class, FieldID, Rfloat) :-
    jni_void(161, Class, FieldID, Rfloat).


%! jSetStaticIntField(+Class:jref, +FieldID:fieldId, +Rint:int)

jSetStaticIntField(Class, FieldID, Rint) :-
    jni_void(159, Class, FieldID, Rint).


%! jSetStaticLongField(+Class:jref, +FieldID:fieldId, +Rlong)

jSetStaticLongField(Class, FieldID, Rlong) :-
    jni_void(160, Class, FieldID, Rlong).


%! jSetStaticObjectField(+Class:jref, +FieldID:fieldId, +Robj:jref)

jSetStaticObjectField(Class, FieldID, Robj) :-
    jni_void(154, Class, FieldID, Robj).


%! jSetStaticShortField(+Class:jref, +FieldID:fieldId, +Rshort:short)

jSetStaticShortField(Class, FieldID, Rshort) :-
    jni_void(158, Class, FieldID, Rshort).


%! jni_params_put(+Params:list(datum), +Types:list(type), -ParamBuf:paramBuf)
%
% The old form used a static buffer, hence was not re-entrant;
% the new form allocates a buffer of one jvalue per arg,
% puts the (converted) args into respective elements, then returns it
% (the caller is responsible for freeing it).

jni_params_put(As, Ts, ParamBuf)     :-
    jni_ensure_jvm,                     % in case e.g. NewStringUTF() is called
    length(As, N),
    jni_type_to_xput_code(jvalue, Xc), % Xc will be 15
    jni_alloc_buffer(Xc, N, ParamBuf),
    jni_params_put_1(As, 0, Ts, ParamBuf).


%! jni_params_put_1(+Params:list(datum), +N:integer, +JPLTypes:list(type), +ParamBuf:paramBuf)
%
% Params is a (full or partial) list of args-not-yet-stashed.
%
% Types are their (JPL) types (e.g. 'boolean').
%
% N is the arg and buffer index (0+) at which the head of Params is to be stashed.
%
% The old form used a static buffer and hence was non-reentrant;
% the new form uses a dynamically allocated buffer (which oughta be freed after use).
%
% NB if the (user-provided) actual params were to be unsuitable for conversion
% to the method-required types, this would fail silently (without freeing the buffer);
% it's not clear whether the overloaded-method-resolution ensures that all args
% are convertible

jni_params_put_1([], _, [], _).
jni_params_put_1([A|As], N, [Tjni|Ts], ParamBuf) :-     % type checking?
    (   jni_type_to_xput_code(Tjni, Xc)
    ->  (   A = {Term}                                  % a quoted general term?
        ->  jni_term_to_jref(Term, Ax)                  % convert it to a @(Tag) ref to a new Term instance
        ;   A = Ax
        ),
        jni_param_put(N, Xc, Ax, ParamBuf)              % foreign
    ;   fail                                            % oughta raise an exception?
    ),
    N2 is N+1,
    jni_params_put_1(As, N2, Ts, ParamBuf).             % stash remaining params (if any)


%! jni_type_to_xput_code(+JspType, -JniXputCode)
%
%   NB JniXputCode determines widening and casting in foreign code
%
%   NB the codes could be compiled into jni_method_spec_cache etc.
%   instead of, or as well as, types (for - small - efficiency gain)

jni_type_to_xput_code(boolean,      1).     % JNI_XPUT_BOOLEAN
jni_type_to_xput_code(byte,         2).     % JNI_XPUT_BYTE
jni_type_to_xput_code(char,         3).     % JNI_XPUT_CHAR
jni_type_to_xput_code(short,        4).     % JNI_XPUT_SHORT
jni_type_to_xput_code(int,          5).     % JNI_XPUT_INT
jni_type_to_xput_code(long,         6).     % JNI_XPUT_LONG
jni_type_to_xput_code(float,        7).     % JNI_XPUT_FLOAT
jni_type_to_xput_code(double,       8).     % JNI_XPUT_DOUBLE
jni_type_to_xput_code(class(_,_),   12).    % JNI_XPUT_REF
jni_type_to_xput_code(array(_),     12).    % JNI_XPUT_REF
jni_type_to_xput_code(jvalue,       15).    % JNI_XPUT_JVALUE


%! jpl_class_to_constructor_array(+Class:jref, -MethodArray:jref)
%
% NB might this be done more efficiently in foreign code? or in Java?

jpl_class_to_constructor_array(Cx, Ma) :-
    jpl_classname_to_class('java.lang.Class', CC),      % cacheable?
    jGetMethodID( CC, getConstructors, method([],array(class([java,lang,reflect],['Constructor']))), MID), % cacheable?
    jCallObjectMethod(Cx, MID, [], [], Ma).


%! jpl_class_to_constructors(+Class:jref, -Methods:list(jref))

jpl_class_to_constructors(Cx, Ms) :-
    jpl_class_to_constructor_array(Cx, Ma),
    jpl_object_array_to_list(Ma, Ms).


%! jpl_class_to_field_array(+Class:jref, -FieldArray:jref)

jpl_class_to_field_array(Cx, Fa) :-
    jpl_classname_to_class('java.lang.Class', CC),      % cacheable?
    jGetMethodID(CC, getFields, method([],array(class([java,lang,reflect],['Field']))), MID),  % cacheable?
    jCallObjectMethod(Cx, MID, [], [], Fa).


%! jpl_class_to_fields(+Class:jref, -Fields:list(jref))
%
% NB do this in Java (ditto for methods)?

jpl_class_to_fields(C, Fs) :-
    jpl_class_to_field_array(C, Fa),
    jpl_object_array_to_list(Fa, Fs).


%! jpl_class_to_method_array(+Class:jref, -MethodArray:jref)
%
% NB migrate into foreign code for efficiency?

jpl_class_to_method_array(Cx, Ma) :-
    jpl_classname_to_class('java.lang.Class', CC),      % cacheable?
    jGetMethodID(CC, getMethods, method([],array(class([java,lang,reflect],['Method']))), MID),  % cacheable?
    jCallObjectMethod(Cx, MID, [], [], Ma).


%! jpl_class_to_methods(+Class:jref, -Methods:list(jref))
%
% NB also used for constructors.
%
% NB do this in Java (ditto for fields)?

jpl_class_to_methods(Cx, Ms) :-
    jpl_class_to_method_array(Cx, Ma),
    jpl_object_array_to_list(Ma, Ms).


%! jpl_constructor_to_modifiers(+Method, -Modifiers)
%
% NB migrate into foreign code for efficiency?

jpl_constructor_to_modifiers(X, Ms) :-
    jpl_classname_to_class('java.lang.reflect.Constructor', Cx),   % cached?
    jpl_method_to_modifiers_1(X, Cx, Ms).


%! jpl_constructor_to_name(+Method:jref, -Name:atom)
%
% It is a JNI convention that each constructor behaves (at least,
% for reflection), as a method whose name is '<init>'.

jpl_constructor_to_name(_X, '<init>').


%! jpl_constructor_to_parameter_types(+Method:jref, -ParameterTypes:list(type))
%
% NB migrate to foreign code for efficiency?

jpl_constructor_to_parameter_types(X, Tfps) :-
    jpl_classname_to_class('java.lang.reflect.Constructor', Cx),   % cached?
    jpl_method_to_parameter_types_1(X, Cx, Tfps).


%! jpl_constructor_to_return_type(+Method:jref, -Type:type)
%
% It is a JNI convention that, for the purposes of retrieving a MethodID,
% a constructor has a return type of 'void'.

jpl_constructor_to_return_type(_X, void).


%! jpl_field_spec(+Type:type, -Index:integer, -Name:atom, -Modifiers, -MID:mId, -FieldType:type)
%
% I'm unsure whether arrays have fields, but if they do, this will handle them correctly.

jpl_field_spec(T, I, N, Mods, MID, Tf) :-
    (   jpl_field_spec_is_cached(T)
    ->  jpl_field_spec_cache(T, I, N, Mods, MID, Tf)
    ;   jpl_type_to_class(T, C),
        jpl_class_to_fields(C, Fs),
        (   T = array(_BaseType)    % regardless of base type...
        ->  Tci = array(_)          % ...the "cache index" type is this
        ;   Tci = T
        ),
        jpl_field_spec_1(C, Tci, Fs),
        jpl_assert(jpl_field_spec_is_cached(Tci)),
        jpl_field_spec_cache(Tci, I, N, Mods, MID, Tf)
    ).


jpl_field_spec_1(C, Tci, Fs) :-
    (   nth1(I, Fs, F),
        jpl_field_to_name(F, N),
        jpl_field_to_modifiers(F, Mods),
        jpl_field_to_type(F, Tf),
        (   member(static, Mods)
        ->  jGetStaticFieldID(C, N, Tf, MID)
        ;   jGetFieldID(C, N, Tf, MID)
        ),
        jpl_assert(jpl_field_spec_cache(Tci,I,N,Mods,MID,Tf)),
        fail
    ;   true
    ).


:- dynamic jpl_field_spec_cache/6.      % document this...


:- dynamic jpl_field_spec_is_cached/1.  % document this...


%! jpl_field_to_modifiers(+Field:jref, -Modifiers:ordset(modifier))

jpl_field_to_modifiers(F, Ms) :-
    jpl_classname_to_class('java.lang.reflect.Field', Cf),
    jpl_method_to_modifiers_1(F, Cf, Ms).


%! jpl_field_to_name(+Field:jref, -Name:atom)

jpl_field_to_name(F, N) :-
    jpl_classname_to_class('java.lang.reflect.Field', Cf),
    jpl_member_to_name_1(F, Cf, N).


%! jpl_field_to_type(+Field:jref, -Type:type)

jpl_field_to_type(F, Tf) :-
    jpl_classname_to_class('java.lang.reflect.Field', Cf),
    jGetMethodID(Cf, getType, method([],class([java,lang],['Class'])), MID),
    jCallObjectMethod(F, MID, [], [], Cr),
    jpl_class_to_type(Cr, Tf).


%! jpl_method_spec(+Type:type, -Index:integer, -Name:atom, -Arity:integer, -Modifiers:ordset(modifier), -MID:methodId, -ReturnType:type, -ParameterTypes:list(type))
%
% Generates pertinent details of all accessible methods of Type (class/2 or array/1),
% populating or using the cache as appropriate.

jpl_method_spec(T, I, N, A, Mods, MID, Tr, Tfps) :-
    (   jpl_method_spec_is_cached(T)
    ->  jpl_method_spec_cache(T, I, N, A, Mods, MID, Tr, Tfps)
    ;   jpl_type_to_class(T, C),
        jpl_class_to_constructors(C, Xs),
        jpl_class_to_methods(C, Ms),
        (   T = array(_BaseType)    % regardless of base type...
        ->  Tci = array(_)          % ...the "cache index" type is this
        ;   Tci = T
        ),
        jpl_method_spec_1(C, Tci, Xs, Ms),
        jpl_assert(jpl_method_spec_is_cached(Tci)),
        jpl_method_spec_cache(Tci, I, N, A, Mods, MID, Tr, Tfps)
    ).


%! jpl_method_spec_1(+Class:jref, +CacheIndexType:partialType, +Constructors:list(method), +Methods:list(method))
%
% If the original type is e.g. array(byte) then CacheIndexType is array(_) else it is that type.

jpl_method_spec_1(C, Tci, Xs, Ms) :-
    (   (   nth1(I, Xs, X),     % generate constructors, numbered from 1
            jpl_constructor_to_name(X, N),
            jpl_constructor_to_modifiers(X, Mods),
            jpl_constructor_to_return_type(X, Tr),
            jpl_constructor_to_parameter_types(X, Tfps)
        ;   length(Xs, J0),
            nth1(J, Ms, M),     % generate members, continuing numbering
            I is J0+J,
            jpl_method_to_name(M, N),
            jpl_method_to_modifiers(M, Mods),
            jpl_method_to_return_type(M, Tr),
            jpl_method_to_parameter_types(M, Tfps)
        ),
        length(Tfps, A), % arity
        (   member(static, Mods)
        ->  jGetStaticMethodID(C, N, method(Tfps,Tr), MID)
        ;   jGetMethodID(C, N, method(Tfps,Tr), MID)
        ),
        jpl_assert(jpl_method_spec_cache(Tci,I,N,A,Mods,MID,Tr,Tfps)),
        fail
    ;   true
    ).


:- dynamic jpl_method_spec_cache/8.


:- dynamic jpl_method_spec_is_cached/1.


%! jpl_method_to_modifiers(+Method:jref, -ModifierSet:ordset(modifier))

jpl_method_to_modifiers(M, Ms) :-
    jpl_classname_to_class('java.lang.reflect.Method', Cm),
    jpl_method_to_modifiers_1(M, Cm, Ms).


%! jpl_method_to_modifiers_1(+Method:jref, +ConstructorClass:jref, -ModifierSet:ordset(modifier))

jpl_method_to_modifiers_1(XM, Cxm, Ms) :-
    jGetMethodID(Cxm, getModifiers, method([],int), MID),
    jCallIntMethod(XM, MID, [], [], I),
    jpl_modifier_int_to_modifiers(I, Ms).


%! jpl_method_to_name(+Method:jref, -Name:atom)

jpl_method_to_name(M, N) :-
    jpl_classname_to_class('java.lang.reflect.Method', CM),
    jpl_member_to_name_1(M, CM, N).


%! jpl_member_to_name_1(+Member:jref, +CM:jref, -Name:atom)

jpl_member_to_name_1(M, CM, N) :-
    jGetMethodID(CM, getName, method([],class([java,lang],['String'])), MID),
    jCallObjectMethod(M, MID, [], [], N).


%! jpl_method_to_parameter_types(+Method:jref, -Types:list(type))

jpl_method_to_parameter_types(M, Tfps) :-
    jpl_classname_to_class('java.lang.reflect.Method', Cm),
    jpl_method_to_parameter_types_1(M, Cm, Tfps).


%! jpl_method_to_parameter_types_1(+XM:jref, +Cxm:jref, -Tfps:list(type))
%
% XM is (a JPL ref to) an instance of java.lang.reflect.[Constructor|Method]

jpl_method_to_parameter_types_1(XM, Cxm, Tfps) :-
    jGetMethodID(Cxm, getParameterTypes, method([],array(class([java,lang],['Class']))), MID),
    jCallObjectMethod(XM, MID, [], [], Atp),
    jpl_object_array_to_list(Atp, Ctps),
    jpl_classes_to_types(Ctps, Tfps).


%! jpl_method_to_return_type(+Method:jref, -Type:type)

jpl_method_to_return_type(M, Tr) :-
    jpl_classname_to_class('java.lang.reflect.Method', Cm),
    jGetMethodID(Cm, getReturnType, method([],class([java,lang],['Class'])), MID),
    jCallObjectMethod(M, MID, [], [], Cr),
    jpl_class_to_type(Cr, Tr).


jpl_modifier_bit(public,        0x001).
jpl_modifier_bit(private,       0x002).
jpl_modifier_bit(protected,     0x004).
jpl_modifier_bit(static,        0x008).
jpl_modifier_bit(final,         0x010).
jpl_modifier_bit(synchronized,  0x020).
jpl_modifier_bit(volatile,      0x040).
jpl_modifier_bit(transient,     0x080).
jpl_modifier_bit(native,        0x100).
jpl_modifier_bit(interface,     0x200).
jpl_modifier_bit(abstract,      0x400).


%! jpl_modifier_int_to_modifiers(+Int:integer, -ModifierSet:ordset(modifier))
%
% ModifierSet is an ordered (hence canonical) list,
% possibly empty (although I suspect never in practice?),
% of modifier atoms, e.g. [public,static]

jpl_modifier_int_to_modifiers(I, Ms) :-
    setof(
        M,                                  %  should use e.g. set_of_all/3
        B^( jpl_modifier_bit(M, B),
            (B /\ I) =\= 0
        ),
        Ms
    ).


%! jpl_cache_type_of_ref(+Type:type, +Ref:jref)
%
% Type must be a proper (concrete) JPL type
%
% Ref must be a proper JPL reference (not void)
%
% Type is memoed (if policy so dictates) as the type of the referenced object (unless it's null)
% by iref (so as not to disable atom-based GC)
%
% NB obsolete lemmas must be watched-out-for and removed

jpl_cache_type_of_ref(T, Ref) :-
    (   jpl_assert_policy(jpl_iref_type_cache(_,_), no)
    ->  true
    ;   \+ ground(T)                            % shouldn't happen (implementation error)
    ->  write('[jpl_cache_type_of_ref/2: arg 1 is not ground]'), nl,    % oughta throw an exception
        fail
    ;   Ref == @(null)                          % a null ref? (this is valid)
    ->  true                                    % silently ignore it
    ;   (   jpl_iref_type_cache(Ref, TC)        % we expect TC == T
        ->  (   T == TC
            ->  true
            ; % write('[JPL: found obsolete tag-type lemma...]'), nl,   % or keep statistics? (why?)
                retractall(jpl_iref_type_cache(Ref,_)),
                jpl_assert(jpl_iref_type_cache(Ref,T))
            )
        ;   jpl_assert(jpl_iref_type_cache(Ref,T))
        )
    ).


%! jpl_class_tag_type_cache(-Ref:jref, -ClassType:type)
%
% Ref is a reference to a JVM instance of java.lang.Class which denotes ClassType.
%
% We index on Ref so as to keep these objects around
% even after an atom garbage collection
% (if needed once, they are likely to be needed again)

:- dynamic jpl_class_tag_type_cache/2.


%! jpl_class_to_ancestor_classes(+Class:jref, -AncestorClasses:list(jref))
%
% AncestorClasses will be a list of (JPL references to) instances of java.lang.Class
% denoting the "implements" lineage (?), nearest first
% (the first member denotes the class which Class directly implements,
% the next (if any) denotes the class which *that* class implements,
% and so on to java.lang.Object)

jpl_class_to_ancestor_classes(C, Cas) :-
    (   jpl_class_to_super_class(C, Ca)
    ->  Cas = [Ca|Cas2],
        jpl_class_to_ancestor_classes(Ca, Cas2)
    ;   Cas = []
    ).


%! jpl_class_to_classname(+Class:jref, -ClassName:dottedName)
%
% Class is a reference to a class object.
%
% ClassName is its canonical (?) source-syntax (dotted) name,
% e.g. =|'java.util.Date'|=
%
% NB not used outside jni_junk and jpl_test (is this (still) true?)
%
% NB oughta use the available caches (but their indexing doesn't suit)

jpl_class_to_classname(C, CN) :-
    jpl_call(C, getName, [], CN).


%! jpl_class_to_raw_classname(+Class:jref, -ClassName:rawName)
%
% The "raw classname" is the "name of the entity" as returned by
% Class.getName()
%
% For example: "java.lang.String", "byte", "[Ljava.lang.Object;", "[[[[[[[I"
%
% See
% https://docs.oracle.com/javase/7/docs/api/java/lang/Class.html#getName()
% https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/Class.html#getName()

jpl_class_to_raw_classname(Cobj, CN) :-
    jpl_classname_to_class('java.lang.Class', CC),      % cached?
    jGetMethodID(CC, getName, method([],class([java,lang],['String'])), MIDgetName),
    jCallObjectMethod(Cobj, MIDgetName, [], [], S),
    S = CN.


%! jpl_class_to_raw_classname_chars(+Class:jref, -ClassnameChars:codes)
%
% Class is a reference to a class object
%
% ClassnameChars is a codes representation of its dotted name

jpl_class_to_raw_classname_chars(Cobj, CsCN) :-
    jpl_class_to_raw_classname(Cobj, CN),
    atom_codes(CN, CsCN).


jpl_class_to_super_class(C, Cx) :-
    jGetSuperclass(C, Cx),
    Cx \== @(null),         % as returned when C is java.lang.Object, i.e. no superclass
    jpl_cache_type_of_ref(class([java,lang],['Class']), Cx).


%! jpl_class_to_type(+ClassObject:jref, -Type:type)
%
% ClassObject is a reference to a class object of Type.
%
% NB should ensure that, if not found in cache, then cache is updated.
%
% Intriguingly, getParameterTypes returns class objects (undocumented AFAIK) with names
% 'boolean', 'byte' etc. and even 'void' (?!)

jpl_class_to_type(Ref, Type) :-
    (   jpl_class_tag_type_cache(Ref, Tx)
    ->  true
    ;   jpl_class_to_raw_classname_chars(Ref, Cs),   % uncached
        jpl_classname_chars_to_type(Cs, Tr),
        jpl_type_to_canonical_type(Tr, Tx),             % map e.g. class([],[byte]) -> byte
        jpl_assert(jpl_class_tag_type_cache(Ref,Tx))
    ->  true    % the elseif goal should be determinate, but just in case...
    ),
    Type = Tx.


jpl_classes_to_types([], []).
jpl_classes_to_types([C|Cs], [T|Ts]) :-
    jpl_class_to_type(C, T),
    jpl_classes_to_types(Cs, Ts).


jpl_classname_chars_to_type(Cs, Type) :-
    (   phrase(jpl_parse_entityname(Type), Cs)
    ->  true
    ).


%! jpl_classname_to_class(+ClassName:className, -Class:jref)
%
% ClassName unambiguously represents a class, e.g. =|'java.lang.String'|=
%
% Class is a (canonical) reference to the corresponding class object.
%
% NB uses caches where the class is already encountered.

jpl_classname_to_class(N, C) :-
    jpl_classname_to_type(N, T),    % cached
    jpl_type_to_class(T, C).        % cached


%! jpl_classname_to_type(+Classname:className, -Type:type)
%
% Classname is any of: a source-syntax (dotted) class name, e.g. 'java.util.Date', '[java.util.Date' or '[L'
%
% Type is its corresponding JPL type structure, e.g. =|class([java,util],['Date'])|=, =|array(class([java,util],['Date']))|=, =|array(long)|=
%
% NB by "classname" do I mean "typename"?
%
% NB should this throw an exception for unbound CN? is this public API?

jpl_classname_to_type(CN, T) :-
    (   jpl_classname_type_cache(CN, Tx)
    ->  Tx = T
    ;   atom_codes(CN, CsCN),
        phrase(jpl_parse_entityname(T), CsCN)
    ->  jpl_assert(jpl_classname_type_cache(CN,T)),
        true
    ).


%! jpl_classname_type_cache( -Classname:className, -Type:type)
%
% Classname is the atomic name of Type.
%
% NB may denote a class which cannot be found.

:- dynamic jpl_classname_type_cache/2.


%! jpl_datum_to_type(+Datum:datum, -Type:type)
%
% Datum must be a JPL representation of an instance of one (or more) Java types;
%
% Type is the unique most specialised type of which Datum denotes an instance;
%
% NB 3 is an instance of byte, char, short, int and long,
% of which byte and char are the joint, overlapping most specialised types,
% so this relates 3 to the pseudo subtype 'char_byte';
%
% @see jpl_type_to_preferred_concrete_type/2 for converting inferred types to instantiable types

jpl_datum_to_type(D, T) :-
    (   jpl_value_to_type(D, T)
    ->  true
    ;   jpl_ref_to_type(D, T)
    ->  true
    ;   nonvar(D),
        D = {Term}
    ->  (   cyclic_term(Term)
        ->  throw_type_error(jpl_datum_to_type/2,acyclic,Term,er01)
        ;   atom(Term)
        ->  T = class([org,jpl7],['Atom'])
        ;   integer(Term)
        ->  T = class([org,jpl7],['Integer'])
        ;   float(Term)
        ->  T = class([org,jpl7],['Float'])
        ;   var(Term)
        ->  T = class([org,jpl7],['Variable'])
        ;   T = class([org,jpl7],['Compound'])
        )
    ).


jpl_datums_to_most_specific_common_ancestor_type([D], T) :-
    jpl_datum_to_type(D, T).
jpl_datums_to_most_specific_common_ancestor_type([D1,D2|Ds], T0) :-
    jpl_datum_to_type(D1, T1),
    jpl_type_to_ancestor_types(T1, Ts1),
    jpl_datums_to_most_specific_common_ancestor_type_1([D2|Ds], [T1|Ts1], [T0|_]).


jpl_datums_to_most_specific_common_ancestor_type_1([], Ts, Ts).
jpl_datums_to_most_specific_common_ancestor_type_1([D|Ds], Ts1, Ts0) :-
    jpl_datum_to_type(D, Tx),
    jpl_lineage_types_type_to_common_lineage_types(Ts1, Tx, Ts2),
    jpl_datums_to_most_specific_common_ancestor_type_1(Ds, Ts2, Ts0).


%! jpl_datums_to_types(+Datums:list(datum), -Types:list(type))
%
% Each member of Datums is a JPL value or reference,
% denoting an instance of some Java type,
% and the corresponding member of Types denotes the most specialised type
% of which it is an instance (including some I invented for the overlaps
% between e.g. char and short).

jpl_datums_to_types([], []).
jpl_datums_to_types([D|Ds], [T|Ts]) :-
    jpl_datum_to_type(D, T),
    jpl_datums_to_types(Ds, Ts).


%! jpl_ground_is_type(+X:term)
%
% X, known to be ground, is (or at least superficially resembles :-) a JPL type.

jpl_ground_is_type(X) :-
    jpl_primitive_type(X),
    !.
jpl_ground_is_type(array(X)) :-
    jpl_ground_is_type(X).
jpl_ground_is_type(class(_,_)).
jpl_ground_is_type(method(_,_)).


:- dynamic jpl_iref_type_cache/2.


jpl_lineage_types_type_to_common_lineage_types(Ts, Tx, Ts0) :-
    (   append(_, [Tx|Ts2], Ts)
    ->  [Tx|Ts2] = Ts0
    ;   jpl_type_to_super_type(Tx, Tx2)
    ->  jpl_lineage_types_type_to_common_lineage_types(Ts, Tx2, Ts0)
    ).


jpl_non_var_is_object_type(class(_,_)).

jpl_non_var_is_object_type(array(_)).


%! jpl_object_array_to_list(+Array:jref, -Values:list(datum))
%
% Values is a list of JPL values (primitive values or object references)
% representing the respective elements of Array.

jpl_object_array_to_list(A, Vs) :-
    jpl_array_to_length(A, N),
    jpl_object_array_to_list_1(A, 0, N, Vs).


%! jpl_object_array_to_list_1(+A, +I, +N, -Xs)

jpl_object_array_to_list_1(A, I, N, Xs) :-
    (   I == N
    ->  Xs = []
    ;   jGetObjectArrayElement(A, I, X),
        Xs = [X|Xs2],
        J is I+1,
        jpl_object_array_to_list_1(A, J, N, Xs2)
    ).


%! jpl_object_to_class(+Object:jref, -Class:jref)
%
% fails silently if Object is not a valid reference to a Java object
%
% Class is a (canonical) reference to the (canonical) class object
% which represents the class of Object
%
% NB what's the point of caching the type if we don't look there first?

jpl_object_to_class(Obj, C) :-
    jpl_is_object(Obj),
    jGetObjectClass(Obj, C),
    jpl_cache_type_of_ref(class([java,lang],['Class']), C).


%! jpl_object_to_type(+Object:jref, -Type:type)
%
% Object must be a proper JPL reference to a Java object
% (i.e. a class or array instance, but not null, void or String).
%
% Type is the JPL type of that object.

jpl_object_to_type(Ref, Type) :-
    jpl_is_object(Ref),
    (   jpl_iref_type_cache(Ref, T)
    ->  true                                % T is Tag's type
    ;   jpl_object_to_class(Ref, Cobj),     % else get ref to class obj
        jpl_class_to_type(Cobj, T),         % get type of class it denotes
        jpl_assert(jpl_iref_type_cache(Ref,T))
    ),
    Type = T.


jpl_object_type_to_super_type(T, Tx) :-
    (   (   T = class(_,_)
        ;   T = array(_)
        )
    ->  jpl_type_to_class(T, C),
        jpl_class_to_super_class(C, Cx),
        Cx \== @(null),
        jpl_class_to_type(Cx, Tx)
    ).


%! jpl_primitive_buffer_to_array(+Type, +Xc, +Bp, +I, +Size, -Vcs)
%
% Bp points to a buffer of (sufficient) Type values.
%
% Vcs will be unbound on entry,
% and on exit will be a list of Size of them, starting at index I
% (the buffer is indexed from zero)

jpl_primitive_buffer_to_array(T, Xc, Bp, I, Size, [Vc|Vcs]) :-
    jni_fetch_buffer_value(Bp, I, Vc, Xc),
    Ix is I+1,
    (   Ix < Size
    ->  jpl_primitive_buffer_to_array(T, Xc, Bp, Ix, Size, Vcs)
    ;   Vcs = []
    ).


%! jpl_primitive_type(-Type:atom) is nondet
%
% Type is an atomic JPL representation of one of Java's primitive types.
%
%  ==
%  ?- setof(Type, jpl_primitive_type(Type), Types).
%  Types = [boolean, byte, char, double, float, int, long, short].
%  ==

jpl_primitive_type(boolean).
jpl_primitive_type(char).
jpl_primitive_type(byte).
jpl_primitive_type(short).
jpl_primitive_type(int).
jpl_primitive_type(long).
jpl_primitive_type(float).
jpl_primitive_type(double).


%! jpl_primitive_type_default_value(-Type:type, -Value:datum)
%
% Each element of any array of (primitive) Type created by jpl_new/3,
% or any instance of (primitive) Type created by jpl_new/3,
% will be initialised to Value (to mimic Java semantics).

jpl_primitive_type_default_value(boolean, @(false)).
jpl_primitive_type_default_value(char,    0).
jpl_primitive_type_default_value(byte,    0).
jpl_primitive_type_default_value(short,   0).
jpl_primitive_type_default_value(int,     0).
jpl_primitive_type_default_value(long,    0).
jpl_primitive_type_default_value(float,   0.0).
jpl_primitive_type_default_value(double,  0.0).


jpl_primitive_type_super_type(T, Tx) :-
    (   jpl_type_fits_type_direct_prim(T, Tx)
    ;   jpl_type_fits_type_direct_xtra(T, Tx)
    ).


%! jpl_primitive_type_term_to_value(+Type, +Term, -Val)
%
% Term, after widening iff appropriate, represents an instance of Type.
%
% Val is the instance of Type which it represents (often the same thing).
%
% NB currently used only by jpl_new_1 when creating an "instance"
% of a primitive type (which may be misguided completism - you can't
% do that in Java)

jpl_primitive_type_term_to_value(Type, Term, Val) :-
    (   jpl_primitive_type_term_to_value_1(Type, Term, Val)
    ->  true
    ).


%! jpl_primitive_type_term_to_value_1(+Type, +RawValue, -WidenedValue)
%
% I'm not worried about structure duplication here.
%
% NB this oughta be done in foreign code.

jpl_primitive_type_term_to_value_1(boolean, @(false), @(false)).
jpl_primitive_type_term_to_value_1(boolean, @(true), @(true)).
jpl_primitive_type_term_to_value_1(char, I, I) :-
    integer(I),
    I >= 0,
    I =< 65535.         %  (2**16)-1.
jpl_primitive_type_term_to_value_1(byte, I, I) :-
    integer(I),
    I >= 128,           % -(2**7)
    I =< 127.           %  (2**7)-1
jpl_primitive_type_term_to_value_1(short, I, I) :-
    integer(I),
    I >= -32768,        % -(2**15)
    I =<  32767.        %  (2**15)-1
jpl_primitive_type_term_to_value_1(int, I, I) :-
    integer(I),
    I >= -2147483648,   % -(2**31)
    I =<  2147483647.   %  (2**31)-1
jpl_primitive_type_term_to_value_1(long, I, I) :-
    integer(I),
    I >= -9223372036854775808,  % -(2**63)
    I =<  9223372036854775807.  %  (2**63)-1
jpl_primitive_type_term_to_value_1(float, V, F) :-
    (   integer(V)
    ->  F is float(V)
    ;   float(V)
    ->  F = V
    ).
jpl_primitive_type_term_to_value_1(double, V, F) :-
    (   integer(V)
    ->  F is float(V)
    ;   float(V)
    ->  F = V
    ).


jpl_primitive_type_to_ancestor_types(T, Ts) :-
    (   jpl_primitive_type_super_type(T, Ta)
    ->  Ts = [Ta|Tas],
        jpl_primitive_type_to_ancestor_types(Ta, Tas)
    ;   Ts = []
    ).


jpl_primitive_type_to_super_type(T, Tx) :-
    jpl_primitive_type_super_type(T, Tx).


%! jpl_ref_to_type(+Ref:jref, -Type:type)
%
% Ref must be a proper JPL reference (to an object, null or void).
%
% Type is its type.

jpl_ref_to_type(Ref, T) :-
    (   Ref == @(null)
    ->  T = null
    ;   Ref == @(void)
    ->  T = void
    ;   jpl_object_to_type(Ref, T)
    ).


%! jpl_tag_to_type(+Tag:tag, -Type:type)
%
% Tag must be an (atomic) object tag.
%
% Type is its type (either from the cache or by reflection).
% OBSOLETE

jpl_tag_to_type(Tag, Type) :-
    jni_tag_to_iref(Tag, Iref),
    (   jpl_iref_type_cache(Iref, T)
    ->  true                                % T is Tag's type
    ;   jpl_object_to_class(@(Tag), Cobj), % else get ref to class obj
        jpl_class_to_type(Cobj, T),         % get type of class it denotes
        jpl_assert(jpl_iref_type_cache(Iref,T))
    ),
    Type = T.


%! jpl_type_fits_type(+TypeX:type, +TypeY:type) is semidet
%
% TypeX and TypeY must each be proper JPL types.
%
% This succeeds iff TypeX is assignable to TypeY.

jpl_type_fits_type(Tx, Ty) :-
    (   jpl_type_fits_type_1(Tx, Ty)
    ->  true
    ).


%! jpl_type_fits_type_1(+T1:type, +T2:type)
%
% NB it doesn't matter that this leaves choicepoints; it serves only jpl_type_fits_type/2

jpl_type_fits_type_1(T, T).
jpl_type_fits_type_1(class(Ps1,Cs1), class(Ps2,Cs2)) :-
    jpl_type_to_class(class(Ps1,Cs1), C1),
    jpl_type_to_class(class(Ps2,Cs2), C2),
    jIsAssignableFrom(C1, C2).
jpl_type_fits_type_1(array(T1), class(Ps2,Cs2)) :-
    jpl_type_to_class(array(T1), C1),
    jpl_type_to_class(class(Ps2,Cs2), C2),
    jIsAssignableFrom(C1, C2).
jpl_type_fits_type_1(array(T1), array(T2)) :-
    jpl_type_to_class(array(T1), C1),
    jpl_type_to_class(array(T2), C2),
    jIsAssignableFrom(C1, C2).
jpl_type_fits_type_1(null, class(_,_)).
jpl_type_fits_type_1(null, array(_)).
jpl_type_fits_type_1(T1, T2) :-
    jpl_type_fits_type_xprim(T1, T2).


jpl_type_fits_type_direct_prim(float, double).
jpl_type_fits_type_direct_prim(long,  float).
jpl_type_fits_type_direct_prim(int,   long).
jpl_type_fits_type_direct_prim(char,  int).
jpl_type_fits_type_direct_prim(short, int).
jpl_type_fits_type_direct_prim(byte,  short).


jpl_type_fits_type_direct_xprim(Tp, Tq) :-
    jpl_type_fits_type_direct_prim(Tp, Tq).
jpl_type_fits_type_direct_xprim(Tp, Tq) :-
    jpl_type_fits_type_direct_xtra(Tp, Tq).


%! jpl_type_fits_type_direct_xtra(-PseudoType:type, -ConcreteType:type)
%
% This defines the direct subtype-supertype relationships
% which involve the intersection pseudo types =|char_int|=, =|char_short|= and =|char_byte|=

jpl_type_fits_type_direct_xtra(char_int,   int).    % char_int is a direct subtype of int
jpl_type_fits_type_direct_xtra(char_int,   char).   % etc.
jpl_type_fits_type_direct_xtra(char_short, short).
jpl_type_fits_type_direct_xtra(char_short, char).
jpl_type_fits_type_direct_xtra(char_byte,  byte).
jpl_type_fits_type_direct_xtra(char_byte,  char).
jpl_type_fits_type_direct_xtra(overlong,   float).  % 6/Oct/2006 experiment


%! jpl_type_fits_type_xprim(-Tp, -T) is nondet
%
% NB serves only jpl_type_fits_type_1/2

jpl_type_fits_type_xprim(Tp, T) :-
    jpl_type_fits_type_direct_xprim(Tp, Tq),
    (   Tq = T
    ;   jpl_type_fits_type_xprim(Tq, T)
    ).


%! jpl_type_to_ancestor_types(+T:type, -Tas:list(type))
%
% This does not accommodate the assignability of null,
% but that's OK (?) since "type assignability" and "type ancestry" are not equivalent.

jpl_type_to_ancestor_types(T, Tas) :-
    (   (   T = class(_,_)
        ;   T = array(_)
        )
    ->  jpl_type_to_class(T, C),
        jpl_class_to_ancestor_classes(C, Cas),
        jpl_classes_to_types(Cas, Tas)
    ;   jpl_primitive_type_to_ancestor_types(T, Tas)
    ->  true
    ).


%! jpl_type_to_canonical_type(+Type:type, -CanonicalType:type)
%
% Type must be a type, not necessarily canonical.
%
% CanonicalType will be equivalent and canonical.
%
% Example
%  ==
%  ?- jpl:jpl_type_to_canonical_type(class([],[byte]), T).
%  T = byte.
%  ==

jpl_type_to_canonical_type(array(T), array(Tc)) :-
    !,
    jpl_type_to_canonical_type(T, Tc).
jpl_type_to_canonical_type(class([],[void]), void) :-
    !.
jpl_type_to_canonical_type(class([],[N]), N) :-
    jpl_primitive_type(N),
    !.
jpl_type_to_canonical_type(class(Ps,Cs), class(Ps,Cs)) :-
    !.
jpl_type_to_canonical_type(void, void) :-
    !.
jpl_type_to_canonical_type(P, P) :-
    jpl_primitive_type(P).


%! jpl_type_to_class(+Type:type, -Class:jref)
%
% Incomplete types are now never cached (or otherwise passed around).
%
% jFindClass throws an exception if FCN can't be found.

jpl_type_to_class(T, RefA) :-
    (   ground(T)
	->  (   jpl_class_tag_type_cache(RefB, T)
	    ->  true
	    ;   (   jpl_type_to_findclassname(T, FCN)   % peculiar syntax for FindClass()
	        ->  jFindClass(FCN, RefB),       % which caches type of RefB
	            jpl_cache_type_of_ref(class([java,lang],['Class']), RefB)    % 9/Nov/2004 bugfix (?)
	        ),
	        jpl_assert(jpl_class_tag_type_cache(RefB,T))
	    ),
	    RefA = RefB
    ;   throw_instantiation_error(jpl_type_to_class/2,es01)
    ).


%! jpl_type_to_nicename(+Type:type, -NiceName:dottedName)
%
% Type, which is a class or array type (not sure about the others...),
% is denoted by ClassName in dotted syntax.
%
% NB is this used? is "nicename" well defined and necessary?
%
% NB this could use caching if indexing were amenable.
%
% Examples
%  ==
%  ?- jpl:jpl_type_to_nicename(class([java,util],['Date']), Name).
%  Name = 'java.util.Date'.
%
%  ?- jpl:jpl_type_to_nicename(boolean, Name).
%  Name = boolean.
%  ==
%
% @see jpl_type_to_classname/2

jpl_type_to_nicename(T, NN) :-
    (   jpl_primitive_type(T)
    ->  NN = T
    ;   (   phrase(jpl_parse_entityname(T), Cs)
        ->  atom_codes(CNx, Cs),                                % green commit to first solution
            NN = CNx
        )
    ).


%! jpl_type_to_classname(+Type:type, -ClassName:dottedName)
%
% Type, which is a class or array type (not sure about the others...),
% is denoted by ClassName in dotted syntax.
%
% e.g. jpl_type_to_classname(class([java,util],['Date']), 'java.util.Date')
%
% @see jpl_type_to_nicename/2

jpl_type_to_classname(T, CN) :-
    (   phrase(jpl_parse_entityname(T), Cs)
    ->  atom_codes(CNx, Cs),                                % green commit to first solution
        CN = CNx
    ).


%! jpl_type_to_descriptor(+Type:type, -Descriptor:descriptor)
%
% Type (denoting any Java type)
% (can also be a JPL method/2 structure (?!))
% is represented by Descriptor (JVM internal syntax)
%
% Example
%  ==
%  ?- jpl:jpl_type_to_descriptor(class([java,util],['Date']), Descriptor).
%  Descriptor = 'Ljava/util/Date;'.
%  ==
%
% I'd cache this, but I'd prefer more efficient indexing on types (hashed?)

jpl_type_to_descriptor(T, D) :-
    (   phrase(jpl_parse_java_type_descriptor(T), Cs)
    ->  atom_codes(Dx, Cs),
        D = Dx
    ).


%! jpl_type_to_findclassname(+Type:type, -FindClassName:findClassName)
%
% FindClassName denotes Type (class or array only)
% in the syntax required peculiarly by JNI's FindClass().
%
% Example
%  ==
%  ?- jpl:jpl_type_to_findclassname(class([java,util],['Date']), FindClassName).
%  FindClassName = 'java/util/Date'.
%  ==

jpl_type_to_findclassname(T, FCN) :-
    (   phrase(jpl_parse_findclassname_type_descriptor(T), Cs)
    ->  atom_codes(FCNx, Cs),
        FCN = FCNx
    ).


%! jpl_type_to_super_type(+Type:type, -SuperType:type)
%
% Type should be a proper JPL type.
%
% SuperType is the (at most one) type which it directly implements (if it's a class).
%
% If Type denotes a class, this works only if that class can be found.

jpl_type_to_super_type(T, Tx) :-
    (   jpl_object_type_to_super_type(T, Tx)
    ->  true
    ;   jpl_primitive_type_to_super_type(T, Tx)
    ->  true
    ).


%! jpl_type_to_preferred_concrete_type(+Type:type, -ConcreteType:type)
%
% Type must be a canonical JPL type,
% possibly an inferred pseudo type such as =|char_int|= or =|array(char_byte)|=
%
% ConcreteType is the preferred concrete (Java-instantiable) type.
%
% Example
%  ==
%  ?- jpl_type_to_preferred_concrete_type(array(char_byte), T).
%  T = array(byte).
%  ==
%
% NB introduced 16/Apr/2005 to fix bug whereby jpl_list_to_array([1,2,3],A) failed
% because the lists's inferred type of array(char_byte) is not Java-instantiable

jpl_type_to_preferred_concrete_type(T, Tc) :-
    (   jpl_type_to_preferred_concrete_type_1(T, TcX)
    ->  Tc = TcX
    ).


jpl_type_to_preferred_concrete_type_1(char_int, int).
jpl_type_to_preferred_concrete_type_1(char_short, short).
jpl_type_to_preferred_concrete_type_1(char_byte, byte).
jpl_type_to_preferred_concrete_type_1(array(T), array(Tc)) :-
    jpl_type_to_preferred_concrete_type_1(T, Tc).
jpl_type_to_preferred_concrete_type_1(T, T).


%! jpl_types_fit_type(+Types:list(type), +Type:type)
%
% Each member of Types is (independently) (if that means anything) assignable to Type.
%
% Used in dynamic type check when attempting to e.g. assign list of values to array.

jpl_types_fit_type([], _).
jpl_types_fit_type([T1|T1s], T2) :-
    jpl_type_fits_type(T1, T2),
    jpl_types_fit_type(T1s, T2).


%! jpl_types_fit_types(+Types1:list(type), +Types2:list(type))
%
% Each member type of Types1 "fits" the respective member type of Types2.

jpl_types_fit_types([], []).
jpl_types_fit_types([T1|T1s], [T2|T2s]) :-
    jpl_type_fits_type(T1, T2),
    jpl_types_fit_types(T1s, T2s).


%! jpl_value_to_type(+Value:datum, -Type:type)
%
% Value must be a proper JPL datum other than a ref
% i.e. primitive, String or void
%
% Type is its unique most specific type,
% which may be one of the pseudo types =|char_byte|=, =|char_short|= or =|char_int|=.

jpl_value_to_type(V, T) :-
    ground(V),                          % critically assumed by jpl_value_to_type_1/2
    (   jpl_value_to_type_1(V, Tv)      % 2nd arg must be unbound
    ->  T = Tv
    ).


%! jpl_value_to_type_1(+Value:datum, -Type:type) is semidet
%
% Type is the unique most specific JPL type of which Value represents an instance.
%
% Called solely by jpl_value_to_type/2, which commits to first solution.
%
% NB  some  integer  values  are  of  JPL-peculiar  uniquely  most
% specific subtypes, i.e. char_byte, char_short,  char_int but all
% are understood by JPL's internal utilities which call this proc.
%
% NB we regard float as subtype of double.
%
% NB objects and refs always have straightforward types.

jpl_value_to_type_1(@(false), boolean) :- !.
jpl_value_to_type_1(@(true), boolean) :- !.
jpl_value_to_type_1(A, class([java,lang],['String'])) :-   % yes it's a "value"
    atom(A),
    !.
jpl_value_to_type_1(I, T) :-
    integer(I),
    !,
    (   I >= 0
    ->  (   I  < 128                 ->  T = char_byte
        ;   I  < 32768               ->  T = char_short
        ;   I  < 65536               ->  T = char_int
        ;   I  < 2147483648          ->  T = int
        ;   I =< 9223372036854775807 ->  T = long
        ;   T = overlong
        )
    ;   I >= -128                 ->  T = byte
    ;   I >= -32768               ->  T = short
    ;   I >= -2147483648          ->  T = int
    ;   I >= -9223372036854775808 ->  T = long
    ;   T = overlong
    ).
jpl_value_to_type_1(F, float) :-
    float(F).


%! jpl_is_class(@Term)
%
% True if Term is a JPL reference to an instance of =|java.lang.Class|=.

jpl_is_class(X) :-
    jpl_is_object(X),
    jpl_object_to_type(X, class([java,lang],['Class'])).


%! jpl_is_false(@Term)
%
% True if Term is =|@(false)|=, the JPL representation of the Java boolean value 'false'.

jpl_is_false(X) :-
    X == @(false).


%! jpl_is_fieldID(-X)
%
% X is a JPL field ID structure (jfieldID/1)..
%
% NB JPL internal use only.
%
% NB applications should not be messing with these.
%
% NB a var arg may get bound.

jpl_is_fieldID(jfieldID(X)) :-
    integer(X).


%! jpl_is_methodID(-X)
%
% X is a JPL method ID structure (jmethodID/1).
%
% NB JPL internal use only.
%
% NB applications should not be messing with these.
%
% NB a var arg may get bound.

jpl_is_methodID(jmethodID(X)) :-   % NB a var arg may get bound...
    integer(X).


%! jpl_is_null(@Term)
%
% True if Term is =|@(null)|=, the JPL representation of Java's 'null' reference.

jpl_is_null(X) :-
    X == @(null).


%! jpl_is_object(@Term)
%
% True if Term is a well-formed JPL object reference.
%
% NB this checks only syntax, not whether the object exists.

jpl_is_object(X) :-
	blob(X, jref).


%! jpl_is_object_type(@Term)
%
% True if Term is an object (class or array) type, not e.g. a primitive, null or void.

jpl_is_object_type(T) :-
    \+ var(T),
    jpl_non_var_is_object_type(T).


%! jpl_is_ref(@Term)
%
% True if Term is a well-formed JPL reference,
% either to a Java object
% or to Java's notional but important 'null' non-object.

jpl_is_ref(Term) :-
    (	jpl_is_object(Term)
    ->	true
    ;	jpl_is_null(Term)
    ->	true
    ).


%! jpl_is_true(@Term)
%
%  True if Term is  =|@(true)|=,  the   JPL  representation  of the Java
%  boolean value 'true'.

jpl_is_true(X) :-
    X == @(true).

%! jpl_is_type(@Term)
%
%  True if Term is a well-formed JPL type structure.

jpl_is_type(X) :-
    ground(X),
    jpl_ground_is_type(X).

%! jpl_is_void(@Term)
%
%  True if Term is =|@(void)|=,  the   JPL  representation of the pseudo
%  Java value 'void' (which is returned   by  jpl_call/4 when invoked on
%  void methods).
%
%  NB you can try passing 'void' back  to   Java,  but  it won't ever be
%  interested.

jpl_is_void(X) :-
    X == @(void).

%! jpl_false(-X:datum) is semidet
%
%  X is =|@(false)|=, the JPL representation of the Java boolean value
%  'false'.
%
%  @see jpl_is_false/1

jpl_false(@(false)).

%! jpl_null(-X:datum) is semidet
%
%  X is =|@(null)|=, the JPL representation of Java's 'null' reference.
%
%  @see jpl_is_null/1

jpl_null(@(null)).

%! jpl_true(-X:datum) is semidet
%
%  X is =|@(true)|=, the JPL representation   of  the Java boolean value
%  'true'.
%
%  @see jpl_is_true/1

jpl_true(@(true)).


%! jpl_void(-X:datum) is semidet
%
%  X is =|@(void)|=, the JPL  representation   of  the pseudo Java value
%  'void'.
%
%  @see jpl_is_void/1

jpl_void(@(void)).


%! jpl_array_to_length(+Array:jref, -Length:integer)
%
%  Array should be a JPL reference to a Java array of any type.
%
%  Length is the length of that  array.   This  is  a utility predicate,
%  defined thus:
%
%  ```
%  jpl_array_to_length(A, N) :-
%      (   jpl_ref_to_type(A, array(_))
%      ->  jGetArrayLength(A, N)
%      ).
%  ```

jpl_array_to_length(A, N) :-
    (   jpl_ref_to_type(A, array(_))    % can this be done cheaper e.g. in foreign code?
    ->  jGetArrayLength(A, N)           % *must* be array, else undefined (crash?)
    ).


%! jpl_array_to_list(+Array:jref, -Elements:list(datum))
%
%  Array should be a JPL reference to a Java array of any type.
%
%  Elements is a Prolog  list  of   JPL  representations  of the array's
%  elements (values or references, as appropriate).   This  is a utility
%  predicate, defined thus:
%
%  ```
%  jpl_array_to_list(A, Es) :-
%      jpl_array_to_length(A, Len),
%      (   Len > 0
%      ->  LoBound is 0,
%          HiBound is Len-1,
%          jpl_get(A, LoBound-HiBound, Es)
%      ;   Es = []
%      ).
%  ```

jpl_array_to_list(A, Es) :-
    jpl_array_to_length(A, Len),
    (   Len > 0
    ->  LoBound is 0,
        HiBound is Len-1,
        jpl_get(A, LoBound-HiBound, Es)
    ;   Es = []
    ).


%! jpl_datums_to_array(+Datums:list(datum), -A:jref)
%
%  A will be a JPL reference to a new Java array, whose base type is the
%  most specific Java type of which each   member of Datums is (directly
%  or indirectly) an instance.
%
%  NB this fails silently if
%
%   - Datums is an empty list (no base type can be inferred)
%   - Datums contains both a primitive value and an object (including
%     array) reference (no common supertype)

jpl_datums_to_array(Ds, A) :-
    ground(Ds),
    jpl_datums_to_most_specific_common_ancestor_type(Ds, T),    % T may be pseudo e.g. char_byte
    jpl_type_to_preferred_concrete_type(T, Tc),    % bugfix added 16/Apr/2005
    jpl_new(array(Tc), Ds, A).


%! jpl_enumeration_element(+Enumeration:jref, -Element:datum)
%
%  Generates each Element from Enumeration.
%
%  - if the element is a java.lang.String then Element will be an atom
%  - if the element is null then Element will (oughta) be null
%  - otherwise I reckon it has to be an object ref

jpl_enumeration_element(En, E) :-
    (   jpl_call(En, hasMoreElements, [], @(true))
    ->  jpl_call(En, nextElement, [], Ex),
        (   E = Ex
        ;   jpl_enumeration_element(En, E)
        )
    ).


%! jpl_enumeration_to_list(+Enumeration:jref, -Elements:list(datum))
%
%  Enumeration should be a JPL reference   to an object which implements
%  the =|Enumeration|= interface.
%
%  Elements is a  Prolog  list  of   JPL  references  to  the enumerated
%  objects. This is a utility predicate, defined thus:
%
%  ```
%  jpl_enumeration_to_list(Enumeration, Es) :-
%      (   jpl_call(Enumeration, hasMoreElements, [], @(true))
%      ->  jpl_call(Enumeration, nextElement, [], E),
%          Es = [E|Es1],
%          jpl_enumeration_to_list(Enumeration, Es1)
%      ;   Es = []
%      ).
%  ```

jpl_enumeration_to_list(Enumeration, Es) :-
    (   jpl_call(Enumeration, hasMoreElements, [], @(true))
    ->  jpl_call(Enumeration, nextElement, [], E),
        Es = [E|Es1],
        jpl_enumeration_to_list(Enumeration, Es1)
    ;   Es = []
    ).


%! jpl_hashtable_pair(+HashTable:jref, -KeyValuePair:pair(datum,datum)) is nondet
%
%  Generates Key-Value pairs from the given HashTable.
%
%  NB String is converted to atom but Integer is presumably returned as
%  an object ref (i.e. as elsewhere, no auto unboxing);
%
%  NB this is anachronistic: the Map interface is preferred.

jpl_hashtable_pair(HT, K-V) :-
    jpl_call(HT, keys, [], Ek),
    jpl_enumeration_to_list(Ek, Ks),
    member(K, Ks),
    jpl_call(HT, get, [K], V).


%! jpl_iterator_element(+Iterator:jref, -Element:datum)
%
%  Iterator should be a JPL reference to  an object which implements the
%  =|java.util.Iterator|= interface.
%
%  Element is the  JPL  representation  of   the  next  element  in  the
%  iteration. This is a utility predicate, defined thus:
%
%  ```
%  jpl_iterator_element(I, E) :-
%      (   jpl_call(I, hasNext, [], @(true))
%      ->  (   jpl_call(I, next, [], E)
%          ;   jpl_iterator_element(I, E)
%          )
%      ).
%  ```

jpl_iterator_element(I, E) :-
    (   jpl_call(I, hasNext, [], @(true))
    ->  (   jpl_call(I, next, [], E)
        ;   jpl_iterator_element(I, E)
        )
    ).


%! jpl_list_to_array(+Datums:list(datum), -Array:jref)
%
%  Datums should be a proper  Prolog  list   of  JPL  datums  (values or
%  references).
%
%  If Datums have a most specific common  supertype, then Array is a JPL
%  reference to a new  Java  array,  whose   base  type  is  that common
%  supertype, and whose respective  elements  are   the  Java  values or
%  objects represented by Datums.

jpl_list_to_array(Ds, A) :-
    jpl_datums_to_array(Ds, A).


%! jpl_terms_to_array(+Terms:list(term), -Array:jref) is semidet
%
%  Terms should be a proper Prolog list of arbitrary terms.
%
%  Array is a JPL reference to a   new  Java array of ``org.jpl7.Term``,
%  whose elements represent the respective members of the list.

jpl_terms_to_array(Ts, A) :-
    jpl_terms_to_array_1(Ts, Ts2),
    jpl_new(array(class([org,jpl7],['Term'])), Ts2, A).


jpl_terms_to_array_1([], []).
jpl_terms_to_array_1([T|Ts], [{T}|Ts2]) :-
    jpl_terms_to_array_1(Ts, Ts2).


%! jpl_array_to_terms(+JRef:jref, -Terms:list(term))
%
%  JRef should be a JPL  reference  to   a  Java  array of org.jpl7.Term
%  instances (or ots subtypes); Terms will be  a list of the terms which
%  the respective array elements represent.

jpl_array_to_terms(JRef, Terms) :-
    jpl_call('org.jpl7.Util', termArrayToList, [JRef], {Terms}).


%! jpl_map_element(+Map:jref, -KeyValue:pair(datum,datum)) is nondet
%
%  Map must be a  JPL  Reference  to   an  object  which  implements the
%  =|java.util.Map|= interface
%
%  This generates each Key-Value pair from the Map, e.g.
%
%  ```
%  ?- jpl_call('java.lang.System', getProperties, [], Map), jpl_map_element(Map, E).
%  Map = @<jref>(0x20b5c38),
%  E = 'java.runtime.name'-'Java(TM) SE Runtime Environment' ;
%  Map = @<jref>(0x20b5c38),
%  E = 'sun.boot.library.path'-'C:\\Program Files\\Java\\jre7\\bin'
%  etc.
%  ```
%
%  This is a utility predicate, defined thus:
%
%  ```
%  jpl_map_element(Map, K-V) :-
%      jpl_call(Map, entrySet, [], ES),
%      jpl_set_element(ES, E),
%      jpl_call(E, getKey, [], K),
%      jpl_call(E, getValue, [], V).
%  ```

jpl_map_element(Map, K-V) :-
    jpl_call(Map, entrySet, [], ES),
    jpl_set_element(ES, E),
    jpl_call(E, getKey, [], K),
    jpl_call(E, getValue, [], V).


%! jpl_set_element(+Set:jref, -Element:datum) is nondet
%
%  Set must be a  JPL  reference  to   an  object  which  implements the
%  =|java.util.Set|= interface.
%
%  On backtracking, Element is bound  to   a  JPL representation of each
%  element of Set. This is a utility predicate, defined thus:
%
%  ```
%  jpl_set_element(S, E) :-
%      jpl_call(S, iterator, [], I),
%      jpl_iterator_element(I, E).
%  ```

jpl_set_element(S, E) :-
    jpl_call(S, iterator, [], I),
    jpl_iterator_element(I, E).


%! jpl_servlet_byref(+Config, +Request, +Response)
%
%  This serves the _byref_ servlet  demo,   exemplifying  one tactic for
%  implementing a servlet  in  Prolog  by   accepting  the  Request  and
%  Response objects as JPL references and   accessing  their members via
%  JPL as required;
%
%  @see jpl_servlet_byval/3

jpl_servlet_byref(Config, Request, Response) :-
    jpl_call(Config, getServletContext, [], Context),
    jpl_call(Response, setStatus, [200], _),
    jpl_call(Response, setContentType, ['text/html'], _),
    jpl_call(Response, getWriter, [], W),
    jpl_call(W, println, ['<html><head></head><body><h2>jpl_servlet_byref/3 says:</h2><pre>'], _),
    jpl_call(W, println, ['\nservlet context stuff:'], _),
    jpl_call(Context, getInitParameterNames, [], ContextInitParameterNameEnum),
    jpl_enumeration_to_list(ContextInitParameterNameEnum, ContextInitParameterNames),
    length(ContextInitParameterNames, NContextInitParameterNames),
    atomic_list_concat(['\tContext.InitParameters = ',NContextInitParameterNames], NContextInitParameterNamesMsg),
    jpl_call(W, println, [NContextInitParameterNamesMsg], _),
    (   member(ContextInitParameterName, ContextInitParameterNames),
        jpl_call(Context, getInitParameter, [ContextInitParameterName], ContextInitParameter),
        atomic_list_concat(['\t\tContext.InitParameter[',ContextInitParameterName,'] = ',ContextInitParameter], ContextInitParameterMsg),
        jpl_call(W, println, [ContextInitParameterMsg], _),
        fail
    ;   true
    ),
    jpl_call(Context, getMajorVersion, [], MajorVersion),
    atomic_list_concat(['\tContext.MajorVersion = ',MajorVersion], MajorVersionMsg),
    jpl_call(W, println, [MajorVersionMsg], _),
    jpl_call(Context, getMinorVersion, [], MinorVersion),
    atomic_list_concat(['\tContext.MinorVersion = ',MinorVersion], MinorVersionMsg),
    jpl_call(W, println, [MinorVersionMsg], _),
    jpl_call(Context, getServerInfo, [], ServerInfo),
    atomic_list_concat(['\tContext.ServerInfo = ',ServerInfo], ServerInfoMsg),
    jpl_call(W, println, [ServerInfoMsg], _),
    jpl_call(W, println, ['\nservlet config stuff:'], _),
    jpl_call(Config, getServletName, [], ServletName),
    (   ServletName == @(null)
    ->  ServletNameAtom = null
    ;   ServletNameAtom = ServletName
    ),
    atomic_list_concat(['\tConfig.ServletName = ',ServletNameAtom], ServletNameMsg),
    jpl_call(W, println, [ServletNameMsg], _),
    jpl_call(Config, getInitParameterNames, [], ConfigInitParameterNameEnum),
    jpl_enumeration_to_list(ConfigInitParameterNameEnum, ConfigInitParameterNames),
    length(ConfigInitParameterNames, NConfigInitParameterNames),
    atomic_list_concat(['\tConfig.InitParameters = ',NConfigInitParameterNames], NConfigInitParameterNamesMsg),
    jpl_call(W, println, [NConfigInitParameterNamesMsg], _),
    (   member(ConfigInitParameterName, ConfigInitParameterNames),
        jpl_call(Config, getInitParameter, [ConfigInitParameterName], ConfigInitParameter),
        atomic_list_concat(['\t\tConfig.InitParameter[',ConfigInitParameterName,'] = ',ConfigInitParameter], ConfigInitParameterMsg),
        jpl_call(W, println, [ConfigInitParameterMsg], _),
        fail
    ;   true
    ),
    jpl_call(W, println, ['\nrequest stuff:'], _),
    jpl_call(Request, getAttributeNames, [], AttributeNameEnum),
    jpl_enumeration_to_list(AttributeNameEnum, AttributeNames),
    length(AttributeNames, NAttributeNames),
    atomic_list_concat(['\tRequest.Attributes = ',NAttributeNames], NAttributeNamesMsg),
    jpl_call(W, println, [NAttributeNamesMsg], _),
    (   member(AttributeName, AttributeNames),
        jpl_call(Request, getAttribute, [AttributeName], Attribute),
        jpl_call(Attribute, toString, [], AttributeString),
        atomic_list_concat(['\t\tRequest.Attribute[',AttributeName,'] = ',AttributeString], AttributeMsg),
        jpl_call(W, println, [AttributeMsg], _),
        fail
    ;   true
    ),
    jpl_call(Request, getCharacterEncoding, [], CharacterEncoding),
    (   CharacterEncoding == @(null)
    ->  CharacterEncodingAtom = ''
    ;   CharacterEncodingAtom = CharacterEncoding
    ),
    atomic_list_concat(['\tRequest.CharacterEncoding',' = ',CharacterEncodingAtom], CharacterEncodingMsg),
    jpl_call(W, println, [CharacterEncodingMsg], _),
    jpl_call(Request, getContentLength, [], ContentLength),
    atomic_list_concat(['\tRequest.ContentLength',' = ',ContentLength], ContentLengthMsg),
    jpl_call(W, println, [ContentLengthMsg], _),
    jpl_call(Request, getContentType, [], ContentType),
    (   ContentType == @(null)
    ->  ContentTypeAtom = ''
    ;   ContentTypeAtom = ContentType
    ),
    atomic_list_concat(['\tRequest.ContentType',' = ',ContentTypeAtom], ContentTypeMsg),
    jpl_call(W, println, [ContentTypeMsg], _),
    jpl_call(Request, getParameterNames, [], ParameterNameEnum),
    jpl_enumeration_to_list(ParameterNameEnum, ParameterNames),
    length(ParameterNames, NParameterNames),
    atomic_list_concat(['\tRequest.Parameters = ',NParameterNames], NParameterNamesMsg),
    jpl_call(W, println, [NParameterNamesMsg], _),
    (   member(ParameterName, ParameterNames),
        jpl_call(Request, getParameter, [ParameterName], Parameter),
        atomic_list_concat(['\t\tRequest.Parameter[',ParameterName,'] = ',Parameter], ParameterMsg),
        jpl_call(W, println, [ParameterMsg], _),
        fail
    ;   true
    ),
    jpl_call(Request, getProtocol, [], Protocol),
    atomic_list_concat(['\tRequest.Protocol',' = ',Protocol], ProtocolMsg),
    jpl_call(W, println, [ProtocolMsg], _),
    jpl_call(Request, getRemoteAddr, [], RemoteAddr),
    atomic_list_concat(['\tRequest.RemoteAddr',' = ',RemoteAddr], RemoteAddrMsg),
    jpl_call(W, println, [RemoteAddrMsg], _),
    jpl_call(Request, getRemoteHost, [], RemoteHost),
    atomic_list_concat(['\tRequest.RemoteHost',' = ',RemoteHost], RemoteHostMsg),
    jpl_call(W, println, [RemoteHostMsg], _),
    jpl_call(Request, getScheme, [], Scheme),
    atomic_list_concat(['\tRequest.Scheme',' = ',Scheme], SchemeMsg),
    jpl_call(W, println, [SchemeMsg], _),
    jpl_call(Request, getServerName, [], ServerName),
    atomic_list_concat(['\tRequest.ServerName',' = ',ServerName], ServerNameMsg),
    jpl_call(W, println, [ServerNameMsg], _),
    jpl_call(Request, getServerPort, [], ServerPort),
    atomic_list_concat(['\tRequest.ServerPort',' = ',ServerPort], ServerPortMsg),
    jpl_call(W, println, [ServerPortMsg], _),
    jpl_call(Request, isSecure, [], @(Secure)),
    atomic_list_concat(['\tRequest.Secure',' = ',Secure], SecureMsg),
    jpl_call(W, println, [SecureMsg], _),
    jpl_call(W, println, ['\nHTTP request stuff:'], _),
    jpl_call(Request, getAuthType, [], AuthType),
    (   AuthType == @(null)
    ->  AuthTypeAtom = ''
    ;   AuthTypeAtom = AuthType
    ),
    atomic_list_concat(['\tRequest.AuthType',' = ',AuthTypeAtom], AuthTypeMsg),
    jpl_call(W, println, [AuthTypeMsg], _),
    jpl_call(Request, getContextPath, [], ContextPath),
    (   ContextPath == @(null)
    ->  ContextPathAtom = ''
    ;   ContextPathAtom = ContextPath
    ),
    atomic_list_concat(['\tRequest.ContextPath',' = ',ContextPathAtom], ContextPathMsg),
    jpl_call(W, println, [ContextPathMsg], _),
    jpl_call(Request, getCookies, [], CookieArray),
    (   CookieArray == @(null)
    ->  Cookies = []
    ;   jpl_array_to_list(CookieArray, Cookies)
    ),
    length(Cookies, NCookies),
    atomic_list_concat(['\tRequest.Cookies',' = ',NCookies], NCookiesMsg),
    jpl_call(W, println, [NCookiesMsg], _),
    (   nth0(NCookie, Cookies, Cookie),
        atomic_list_concat(['\t\tRequest.Cookie[',NCookie,']'], CookieMsg),
        jpl_call(W, println, [CookieMsg], _),
        jpl_call(Cookie, getName, [], CookieName),
        atomic_list_concat(['\t\t\tRequest.Cookie.Name = ',CookieName], CookieNameMsg),
        jpl_call(W, println, [CookieNameMsg], _),
        jpl_call(Cookie, getValue, [], CookieValue),
        atomic_list_concat(['\t\t\tRequest.Cookie.Value = ',CookieValue], CookieValueMsg),
        jpl_call(W, println, [CookieValueMsg], _),
        jpl_call(Cookie, getPath, [], CookiePath),
        (   CookiePath == @(null)
        ->  CookiePathAtom = ''
        ;   CookiePathAtom = CookiePath
        ),
        atomic_list_concat(['\t\t\tRequest.Cookie.Path = ',CookiePathAtom], CookiePathMsg),
        jpl_call(W, println, [CookiePathMsg], _),
        jpl_call(Cookie, getComment, [], CookieComment),
        (   CookieComment == @(null)
        ->  CookieCommentAtom = ''
        ;   CookieCommentAtom = CookieComment
        ),
        atomic_list_concat(['\t\t\tRequest.Cookie.Comment = ',CookieCommentAtom], CookieCommentMsg),
        jpl_call(W, println, [CookieCommentMsg], _),
        jpl_call(Cookie, getDomain, [], CookieDomain),
        (   CookieDomain == @(null)
        ->  CookieDomainAtom = ''
        ;   CookieDomainAtom = CookieDomain
        ),
        atomic_list_concat(['\t\t\tRequest.Cookie.Domain = ',CookieDomainAtom], CookieDomainMsg),
        jpl_call(W, println, [CookieDomainMsg], _),
        jpl_call(Cookie, getMaxAge, [], CookieMaxAge),
        atomic_list_concat(['\t\t\tRequest.Cookie.MaxAge = ',CookieMaxAge], CookieMaxAgeMsg),
        jpl_call(W, println, [CookieMaxAgeMsg], _),
        jpl_call(Cookie, getVersion, [], CookieVersion),
        atomic_list_concat(['\t\t\tRequest.Cookie.Version = ',CookieVersion], CookieVersionMsg),
        jpl_call(W, println, [CookieVersionMsg], _),
        jpl_call(Cookie, getSecure, [], @(CookieSecure)),
        atomic_list_concat(['\t\t\tRequest.Cookie.Secure',' = ',CookieSecure], CookieSecureMsg),
        jpl_call(W, println, [CookieSecureMsg], _),
        fail
    ;   true
    ),
    jpl_call(W, println, ['</pre></body></html>'], _),
    true.


%! jpl_servlet_byval(+MultiMap, -ContentType:atom, -Body:atom)
%
%  This exemplifies an alternative  (to   jpl_servlet_byref)  tactic for
%  implementing a servlet in Prolog; most   Request fields are extracted
%  in Java before this is called, and passed   in  as a multimap (a map,
%  some of whose values are maps).

jpl_servlet_byval(MM, CT, Ba) :-
    CT = 'text/html',
    multimap_to_atom(MM, MMa),
    atomic_list_concat(['<html><head></head><body>','<h2>jpl_servlet_byval/3 says:</h2><pre>', MMa,'</pre></body></html>'], Ba).


%! is_pair(?T:term)
%
%  I define a half-decent "pair" as having a ground key (any val).

is_pair(Key-_Val) :-
    ground(Key).


is_pairs(List) :-
    is_list(List),
    maplist(is_pair, List).


multimap_to_atom(KVs, A) :-
    multimap_to_atom_1(KVs, '', Cz, []),
    flatten(Cz, Cs),
    atomic_list_concat(Cs, A).


multimap_to_atom_1([], _, Cs, Cs).
multimap_to_atom_1([K-V|KVs], T, Cs1, Cs0) :-
    Cs1 = [T,K,' = '|Cs2],
    (   is_list(V)
    ->  (   is_pairs(V)
        ->  V = V2
        ;   findall(N-Ve, nth1(N, V, Ve), V2)
        ),
        T2 = ['    ',T],
        Cs2 = ['\n'|Cs2a],
        multimap_to_atom_1(V2, T2, Cs2a, Cs3)
    ;   to_atom(V, AV),
        Cs2 = [AV,'\n'|Cs3]
    ),
    multimap_to_atom_1(KVs, T, Cs3, Cs0).


%! to_atom(+Term, -Atom)
%
%  Unifies Atom with a printed representation of Term.
%
%  @tbd Sort of quoting requirements and use format(codes(Codes),...)

to_atom(Term, Atom) :-
    (   atom(Term)
    ->  Atom = Term                % avoid superfluous quotes
    ;   term_to_atom(Term, Atom)
    ).

%! jpl_pl_syntax(-Syntax:atom)
%
%  Unifies Syntax with 'traditional' or 'modern'   according to the mode
%  in which SWI Prolog 7.x was started

jpl_pl_syntax(Syntax) :-
	(	[] == '[]'
	->	Syntax = traditional
	;	Syntax = modern
	).

         /*******************************
         *            MESSAGES          *
         *******************************/

:- multifile
    prolog:error_message/3.

prolog:error_message(java_exception(Ex)) -->
    (   { jpl_call(Ex, toString, [], Msg)
        }
    ->  [ 'Java exception: ~w'-[Msg] ]
    ;   [ 'Java exception: ~w'-[Ex] ]
    ).


         /*******************************
         *             PATHS            *
         *******************************/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(jar, swi(lib)).

classpath(DirOrJar) :-
    getenv('CLASSPATH', ClassPath),
    search_path_separator(Sep),
    atomic_list_concat(Elems, Sep, ClassPath),
    member(DirOrJar, Elems).

%!  add_search_path(+Var, +Value) is det.
%
%   Add value to the  end  of  search-path   Var.  Value  is  normally a
%   directory. Does not change the environment if Dir is already in Var.
%
%   @param Value    Path to add in OS notation.

add_search_path(Path, Dir) :-
    (   getenv(Path, Old)
    ->  search_path_separator(Sep),
        (   atomic_list_concat(Current, Sep, Old),
            memberchk(Dir, Current)
        ->  true            % already present
        ;   atomic_list_concat([Old, Sep, Dir], New),
            (   debugging(jpl(path))
            ->  env_var_separators(A,Z),
                debug(jpl(path), 'Set ~w~w~w to ~p', [A,Path,Z,New])
            ;   true
            ),
            setenv(Path, New)
        )
    ;   setenv(Path, Dir)
    ).

%! search_path_separator(-Sep:atom)
%
%  Separator  used  the  the  OS    in  =PATH=,  =LD_LIBRARY_PATH=,
%  =CLASSPATH=, etc.

search_path_separator((;)) :-
    current_prolog_flag(windows, true),
    !.
search_path_separator(:).

env_var_separators('%','%') :-
    current_prolog_flag(windows, true),
    !.
env_var_separators($,'').


         /*******************************
         *         LOAD THE JVM         *
         *******************************/

%!  check_java_environment
%
%   Verify the Java environment.  Preferably   we  would create, but
%   most Unix systems do not   allow putenv("LD_LIBRARY_PATH=..." in
%   the current process. A suggesting found on  the net is to modify
%   LD_LIBRARY_PATH right at startup and  next execv() yourself, but
%   this doesn't work if we want to load Java on demand or if Prolog
%   itself is embedded in another application.
%
%   So, after reading lots of pages on   the web, I decided checking
%   the environment and producing a sensible   error  message is the
%   best we can do.
%
%   Please not that Java2 doesn't require   $CLASSPATH to be set, so
%   we do not check for that.

check_java_environment :-
    current_prolog_flag(apple, true),
    !,
    print_message(error, jpl(run(jpl_config_dylib))).
check_java_environment :-
    check_lib(jvm).

check_lib(Name) :-
    check_shared_object(Name, File, EnvVar, Absolute),
    (   Absolute == (-)
    ->  env_var_separators(A, Z),
        format(string(Msg), 'Please add directory holding ~w to ~w~w~w',
               [ File, A, EnvVar, Z ]),
        throw_existence_error(check_lib/1,library,Name,Msg)
    ;   true
    ).

%! check_shared_object(+Lib, -File, -EnvVar, -AbsFile) is semidet.
%
%  True if AbsFile is existing .so/.dll file for Lib.
%
%  @arg File    Full name of Lib (i.e. libjpl.so or jpl.dll)
%  @arg EnvVar  Search-path for shared objects.

check_shared_object(Name, File, EnvVar, Absolute) :-
    libfile(Name, File),
    library_search_path(Path, EnvVar),
    (   member(Dir, Path),
        atomic_list_concat([Dir, File], /, Absolute),
        exists_file(Absolute)
    ->  true
    ;   Absolute = (-)
    ).

libfile(Base, File) :-
    current_prolog_flag(unix, true),
    !,
    atom_concat(lib, Base, F0),
    current_prolog_flag(shared_object_extension, Ext),
    file_name_extension(F0, Ext, File).
libfile(Base, File) :-
    current_prolog_flag(windows, true),
    !,
    current_prolog_flag(shared_object_extension, Ext),
    file_name_extension(Base, Ext, File).


%! library_search_path(-Dirs:list, -EnvVar) is det.
%
%  Dirs is the list of  directories   searched  for shared objects/DLLs.
%  EnvVar is the variable in which the search path os stored.

library_search_path(Path, EnvVar) :-
    current_prolog_flag(shared_object_search_path, EnvVar),
    search_path_separator(Sep),
    (   getenv(EnvVar, Env),
        atomic_list_concat(Path, Sep, Env)
    ->  true
    ;   Path = []
    ).


%!  add_jpl_to_classpath
%
%   Add jpl.jar to =CLASSPATH= to facilitate  callbacks. If `jpl.jar` is
%   already in CLASSPATH, do nothing. Note that   this may result in the
%   user picking up a different version   of `jpl.jar`. We'll assume the
%   user is right in this case.
%
%   @tbd Should we warn if both `classpath`   and  `jar` return a result
%   that is different? What is different?   According  to same_file/2 or
%   content?

add_jpl_to_classpath :-
    classpath(Jar),
    file_base_name(Jar, 'jpl.jar'),
    !.
add_jpl_to_classpath :-
    classpath(Dir),
    (   sub_atom(Dir, _, _, 0, /)
    ->  atom_concat(Dir, 'jpl.jar', File)
    ;   atom_concat(Dir, '/jpl.jar', File)
    ),
    access_file(File, read),
    !.
add_jpl_to_classpath :-
    absolute_file_name(jar('jpl.jar'), JplJAR,
                       [ access(read)
                       ]),
    !,
    (   getenv('CLASSPATH', Old)
    ->  search_path_separator(Separator),
        atomic_list_concat([JplJAR, Old], Separator, New)
    ;   New = JplJAR
    ),
    setenv('CLASSPATH', New).


%!  libjpl(-Spec) is det.
%
%   Return the spec for  loading  the   JPL  shared  object. This shared
%   object must be called  libjpl.so   as  the Java System.loadLibrary()
%   call used by jpl.jar adds the lib* prefix.
%
%   In Windows we should __not__  use   foreign(jpl)  as this eventually
%   calls LoadLibrary() with an absolute path, disabling the Windows DLL
%   search process for the dependent `jvm.dll`   and possibly other Java
%   dll dependencies.

libjpl(File) :-
    (   current_prolog_flag(unix, true)
    ->  File = foreign(libjpl)
    ;   File = foreign(jpl)                                    % Windows
    ).

%!  add_jpl_to_ldpath(+JPL) is det.
%
%   Add  the  directory  holding  jpl.so  to  search  path  for  dynamic
%   libraries. This is needed for callback   from  Java. Java appears to
%   use its own search and the new value   of  the variable is picked up
%   correctly.

add_jpl_to_ldpath(JPL) :-
    absolute_file_name(JPL, File,
               [ file_type(executable),
                 access(read),
                 file_errors(fail)
               ]),
    !,
    file_directory_name(File, Dir),
    prolog_to_os_filename(Dir, OsDir),
    extend_java_library_path(OsDir),
    current_prolog_flag(shared_object_search_path, PathVar),
    add_search_path(PathVar, OsDir).
add_jpl_to_ldpath(_).

%!  add_java_to_ldpath is det.
%
%   Adds the directories holding jvm.dll to  the %PATH%. This appears to
%   work on Windows. Unfortunately most Unix   systems appear to inspect
%   the content of =LD_LIBRARY_PATH= (=DYLD_LIBRARY_PATH= on MacOS) only
%   once.

:- if(current_prolog_flag(windows,true)).
add_java_to_ldpath :-
    current_prolog_flag(windows, true),
    !,
    phrase(java_dirs, Extra),
    (   Extra \== []
    ->  print_message(informational, extend_ld_path(Extra)),
        maplist(extend_dll_search_path, Extra)
    ;   true
    ).
:- endif.
add_java_to_ldpath.


%!  extend_dll_search_path(+Dir)
%
%   Add Dir to search for DLL files. We use win_add_dll_directory/1, but
%   this doesn't seem to work on Wine,  so we also add these directories
%   to %PATH% on this platform.

:- if(current_prolog_flag(windows,true)).
extend_dll_search_path(Dir) :-
    win_add_dll_directory(Dir),
    (   current_prolog_flag(wine_version, _)
    ->  getenv('PATH', Path0),
        prolog_to_os_filename(Dir, OSDir),
        atomic_list_concat([Path0, OSDir], ';', Path),
        setenv('PATH', Path)
    ;   true
    ).
:- endif.

%!  extend_java_library_path(+OsDir)
%
%   Add Dir (in OS notation) to   the  Java =|-Djava.library.path|= init
%   options.

extend_java_library_path(OsDir) :-
    jpl_get_default_jvm_opts(Opts0),
    (   select(PathOpt0, Opts0, Rest),
        sub_atom(PathOpt0, 0, _, _, '-Djava.library.path=')
    ->  search_path_separator(Separator),
        atomic_list_concat([PathOpt0, Separator, OsDir], PathOpt),
        NewOpts = [PathOpt|Rest]
    ;   atom_concat('-Djava.library.path=', OsDir, PathOpt),
        NewOpts = [PathOpt|Opts0]
    ),
    debug(jpl(path), 'Setting Java options to ~p', [NewOpts]),
    jpl_set_default_jvm_opts(NewOpts).

%!  java_dirs// is det.
%
%   DCG  that  produces  existing  candidate  directories  holding  Java
%   related DLLs

java_dirs -->
    % JDK directories
    java_dir(jvm, '/jre/bin/client'),
    java_dir(jvm, '/jre/bin/server'),
    java_dir(java, '/jre/bin'),
    % JRE directories
    java_dir(jvm, '/bin/client'),
    java_dir(jvm, '/bin/server'),
    java_dir(java, '/bin').

java_dir(DLL, _SubPath) -->
    { check_shared_object(DLL, _, _Var, Abs),
      Abs \== (-)
    },
    !.
java_dir(_DLL, SubPath) -->
    { java_home(JavaHome),
      atom_concat(JavaHome, SubPath, SubDir),
      exists_directory(SubDir)
    },
    !,
    [SubDir].
java_dir(_, _) --> [].


%!  java_home(-Home) is semidet
%
%   Find the home location of Java.
%
%   @arg Home    JAVA home in OS notation

java_home_win_key(
    jdk,
    'HKEY_LOCAL_MACHINE/Software/JavaSoft/JDK'). % new style
java_home_win_key(
    jdk,
    'HKEY_LOCAL_MACHINE/Software/JavaSoft/Java Development Kit').
java_home_win_key(
    jre,
    'HKEY_LOCAL_MACHINE/Software/JavaSoft/JRE').
java_home_win_key(
    jre,
    'HKEY_LOCAL_MACHINE/Software/JavaSoft/Java Runtime Environment').

java_home(Home) :-
    getenv('JAVA_HOME', Home),
    exists_directory(Home),
    !.
:- if(current_prolog_flag(windows, true)).
java_home(Home) :-
    java_home_win_key(_, Key0),    % TBD: user can't choose jre or jdk
    catch(win_registry_get_value(Key0, 'CurrentVersion', Version), _, fail),
    atomic_list_concat([Key0, Version], /, Key),
    win_registry_get_value(Key, 'JavaHome', WinHome),
    prolog_to_os_filename(Home, WinHome),
    exists_directory(Home),
    !.
:- else.
java_home(Home) :-
    member(Home, [ '/usr/lib/java',
                   '/usr/local/lib/java'
                 ]),
    exists_directory(Home),
    !.
:- endif.

:- dynamic
    jvm_ready/0.
:- volatile
    jvm_ready/0.

setup_jvm :-
    jvm_ready,
    !.
setup_jvm :-
    add_jpl_to_classpath,
    add_java_to_ldpath,
    libjpl(JPL),
    catch(load_foreign_library(JPL), E, report_java_setup_problem(E)),
    add_jpl_to_ldpath(JPL),
    assert(jvm_ready).

report_java_setup_problem(E) :-
    print_message(error, E),
    check_java_environment.

         /*******************************
         *          MESSAGES        *
         *******************************/

:- multifile
    prolog:message//1.

prolog:message(extend_ld_path(Dirs)) -->
    [ 'Extended DLL search path with'-[] ],
    dir_per_line(Dirs).
prolog:message(jpl(run(Command))) -->
    [ 'Could not find libjpl.dylib dependencies.'-[],
      'Please run `?- ~p.` to correct this'-[Command]
    ].

dir_per_line([]) --> [].
dir_per_line([H|T]) -->
    [ nl, '  ~q'-[H] ],
    dir_per_line(T).

% Initialize JVM

:- initialization(setup_jvm, now).        % must be ready before export
