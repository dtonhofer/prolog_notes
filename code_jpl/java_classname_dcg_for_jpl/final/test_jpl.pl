/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2018, Paul Singleton
                              VU University Amsterdam
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

:- module(test_jpl,
          [ test_jpl/0,
            run_tests/0,
            run_tests/1
          ]).
% ensure we get the local copies

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(jpl_examples, 'examples/prolog')).
:- asserta(user:file_search_path(jar, '.')).
:- asserta(user:file_search_path(jar, foreign('src/main/java'))).
:- asserta(user:file_search_path(jar, foreign('.'))).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

% paths for Prolog embedded in Java under CTest
:- asserta(user:file_search_path(library, '../../../packages/jpl')).
:- asserta(user:file_search_path(library, '../../../packages/plunit')).

:- use_module(library(jpl)).
:- use_module(library(plunit)).

% Using cmake location
:- jpl:add_search_path('CLASSPATH', 'src/test/java/jpltest.jar').
% For before cmake
:- jpl:add_search_path('CLASSPATH', 'jpltest.jar').

test_jpl :-
    run_tests(
    [
      jpl
     ,identifier_chars
     ,java_id
     ,java_type_id
     ,messy_dollar_split
     ,jpl_classname_without_dollar
     ,jpl_classname_with_dollar
     ,jpl_entity_is_primitive
     ,jpl_entity_is_array
     ,compare_both_dotty
     ,compare_both_slashy
     ,compare_both_typedesc
    ]).

:- begin_tests(jpl).

test(
        exception_jref_1,
        [       true((
                        E = error(java_exception(JRef), 'java.lang.IllegalArgumentException'),
                        blob(JRef, jref)
                ))
        ]
) :-
    catch(jpl_new('java.util.Date', [never], _), E, true).

test(
        array_to_from_terms_1,
        [       true(
                        Terms1 == Terms2
                )
        ]
) :-
    Terms1 = [x,[1,a,7,[y,z]],k,[]],
    jpl_terms_to_array(Terms1, JRef),
    jpl_array_to_terms(JRef, Terms2).

test(
        ancestor_types_1,
        [       true(
                        Ts == [class([org,jpl7],['Term']),class([java,lang],['Object'])]
                )
        ]
) :-
    jpl:jpl_type_to_ancestor_types(class([org,jpl7],['Atom']), Ts).

test(
        call_array_equals_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A1),
                        jpl_new(array(byte), [4,5,6], A2)
                ))
        ]
) :-
    jpl_call(A1, equals, [A2], @(false)).

test(
        call_array_equals_2,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A1)
                ))
        ]
) :-
    jpl_call(A1, equals, [A1], @(true)).

test(
        call_array_hashcode_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A)
                )),
                true((
                        integer(H)
                ))
        ]
) :-
    jpl_call(A, hashCode, [], H).

test(
        call_array_hashcode_2,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A1),
                        jpl_new(array(byte), [4,5,6], A2)
                )),
                true((
                        H1 \== H2
                ))
        ]
) :-
    jpl_call(A1, hashCode, [], H1),
    jpl_call(A2, hashCode, [], H2).

test(
        call_array_to_string_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6], A)
                )),
                true((
                        atom_codes(S, [0'[, 0'B | _])
                ))
        ]
) :-
    jpl_call(A, toString, [], S).

test(
        call_instance_param_cyclic_term_1,
        [       setup((
                        T = f(T),
                        jpl_new('org.jpl7.Test', [], Test)
                )),
                throws(
                        error(type_error(acyclic,T),context(jpl_call/4,_))
                )
        ]
) :-
    jpl_call(Test, methodInstanceTerm, [{T}], @(true)).

testX(
        call_instance_param_cyclic_term_2,
        [       setup((
                        T = f(T),
                        jpl_new('org.jpl7.Test', [], Test)
                )),
                throws(
                        error(type_error(acyclic,_),context(jpl_call/4,_))
                )
        ]
) :-
    jpl_call(Test, methodInstanceTerm, [{T}], @(true)).

test(
        call_method_static_array_1,
        [       setup((
                        jpl_new(array(int), [3,4,5], IntArray)
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticArray, [IntArray], 'int[]').

test(
        call_method_static_array_2,
        [       setup((
                        jpl_new(array(byte), [3,4,5], ByteArray)
                )),
                throws(
                        error(
                                type_error(method_params,[ByteArray]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticArray, [ByteArray], _).

test(
        call_static_param_cyclic_term_1,
        [       setup((
                        T = f(T)
                )),
                throws(
                        error(type_error(acyclic,T),context(jpl_call/4,_))
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticTerm, [{T}], @(true)).

test(
        call_class_get_name_1,
        [       setup((
                        ClassName = 'java.lang.Integer',
                        jpl_classname_to_class(ClassName, ClassObject)
                )),
                true((
                        ClassName == ClassName2
                ))
        ]
) :-
    jpl_call(ClassObject, getName, [], ClassName2).

test(
        call_get_array_bad_field_name_1,
        [       setup((
                        jpl_new(array(byte), 5, A),
                        FieldName = colour
                )),
                throws(
                        error(domain_error(array_field_name,FieldName),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, FieldName, _).

test(
        call_get_array_bad_fspec_1,
        [       setup((
                        jpl_new(array(byte), 5, A),
                        Fspec = poo(77)
                )),
                throws(
                        error(type_error(array_lookup_spec,Fspec),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, Fspec, _).

test(
        call_get_array_bad_index_range_1,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(domain_error(array_index_range,(-1)-2),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, (-1)-2, _).

test(
        call_get_array_bad_index_range_2,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(domain_error(array_index_range,10-12),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, 10-12, _).

test(
        call_get_array_bad_index_range_3,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(domain_error(array_index_range,3-33),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, 3-33, _).

test(
        call_get_array_bad_index_range_4,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(type_error(array_index_range,this-that),context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, this-that, _).

test(
        get_array_element_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6,7,8], A)
                )),
                true((
                        7 == V
                ))
        ]
) :-
    jpl_get(A, 3, V). % should bind V = 7 i.e. a[3] i.e. the fourth array element counting from zero

test(
        get_array_elements_1,
        [       setup((
                        jpl_new(array(byte), [4,5,6,7,8], A)
                )),
                true((
                        [5,6] == V
                ))
        ]
) :-
    jpl_get(A, 1-2, V). % should bind V = [5,6] i.e. a[1-2] i.e. the 2nd to 3rd array elements counting from zero

test(
        get_array_length_1,
        [       setup((
                        Len1 is 5,
                        jpl_new(array(byte), Len1, A)
                )),
                true((
                        Len1 == Len2
                ))
        ]
) :-
    jpl_get(A, length, Len2). % should bind Len2 to the (integer) value of Len1

test(
        get_array_negative_index_1,
        [       setup((
                        BadIndex is -1,
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(domain_error(array_index,BadIndex), context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, BadIndex, _).

test(
        get_array_unbound_fspec_1,
        [       setup((
                        jpl_new(array(byte), 5, A)
                )),
                throws(
                        error(instantiation_error,context(jpl_get/3,_))
                )
        ]
) :-
    jpl_get(A, _, _).

test(
        get_field_static_boolean_1,
        [       true((
                        V == @(false)
                ))
        ]
) :-
    jpl_get('org.jpl7.Test', fieldStaticBoolean1, V).

test(
        get_field_static_boolean_2,
        [       true((
                        V == @(true)
                ))
        ]
) :-
    jpl_get('org.jpl7.Test', fieldStaticBoolean2, V).

test(
        get_field_static_char_1,
        [       true((
                        V == 0
                ))
        ]
) :-
    jpl_get('org.jpl7.Test', fieldStaticChar1, V).

test(
        get_field_static_char_2,
        [       true((
                        V == 65535
                ))
        ]
) :-
    jpl_get('org.jpl7.Test', fieldStaticChar2, V).

test(
        get_field_instance_byte_2,
        [       setup((
                        jpl_new('org.jpl7.Test', [], Test)
                )),
                true((
                        V == -1
                ))
        ]
) :-
    jpl_get(Test, fieldInstanceByte2, V).

test(
        list_to_array_1,
        [       true((
                        Type == array(byte)
                ))
        ]
) :-
    jpl_list_to_array([1,2,3], A),
    jpl_object_to_type(A, Type).

test(
        method_static_byte_1,
        [       throws(
                        error(
                                type_error(method_params,[-129]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoByte, [-129], _).

test(
        method_static_echo_boolean_1,
        [       setup((
                        jpl_false(V1)
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoBoolean, [V1], V2).

test(
        method_static_echo_boolean_2,
        [       setup((
                        jpl_true(V1)
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoBoolean, [V1], V2).

test(
        method_static_echo_char_1,
        [       setup((
                        V1 = 0
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], V2).

test(
        method_static_echo_char_2,
        [       setup((
                        V1 = 65535
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], V2).

test(
        method_static_char_3,
        [       setup((
                        V1 = -1
                )),
                throws(
                        error(
                                type_error(method_params,[V1]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], _).

test(
        method_static_char_4,
        [       setup((
                        V1 = 1.0
                )),
                throws(
                        error(
                                type_error(method_params,[V1]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], _).

test(
        method_static_char_5,
        [       setup((
                        V1 = a
                )),
                throws(
                        error(
                                type_error(method_params,[V1]),
                                context(jpl_call/4,_)
                        )
                )
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoChar, [V1], _).

test(
        method_static_echo_double_1,
        [       setup((
                        V1 = 1.5
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoDouble, [V1], V2).

test(
        method_static_echo_double_2,
        [       setup((
                        V1 = 2
                )),
                true((
                        V2 =:= float(V1)
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoDouble, [V1], V2).

test(
        method_static_echo_double_3,
        [       setup((
                        (   current_prolog_flag(bounded, true)
                    ->  current_prolog_flag(max_integer, V1)
                    ;   V1 is 2**63-1
                    ),
                        V2b is float(V1)
                )),
                true((
                        V2 == V2b
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoDouble, [V1], V2).

test(
        method_static_echo_float_1,
        [       setup((
                        V1 = 1.5
                )),
                true((
                        V1 == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoFloat, [V1], V2).

test(
        method_static_echo_float_2,
        [       setup((
                        V1 is 2,
                        V2b is float(V1)
                )),
                true((
                        V2 == V2b
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoFloat, [V1], V2).

test(
        method_static_echo_float_3,
        [       setup((
                        (       current_prolog_flag(bounded, true)
                    ->  current_prolog_flag(max_integer, V1)
                    ;   V1 is 2**63-1 % was 2**99
                    ),
                        V2b is float(V1)
                )),
                true((
                        V2 == V2b
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoFloat, [V1], V2).

test(
        method_static_echo_float_4,
        [       blocked('we do not yet widen unbounded integers to floats or doubles'),
                setup((
                        (   current_prolog_flag(bounded, true)
                    ->  current_prolog_flag(max_integer, V1)
                    ;   V1 is 2**99             % an unbounded integer
                    ),
                        V2b is float(V1)
                )),
                true((
                        V2 == V2b
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', methodStaticEchoFloat, [V1], V2).

test(
        new_abstract_class_1,
        [       setup((
                        Classname = 'java.util.Dictionary'
                )),
                throws(
                        error(
                                type_error(concrete_class,Classname),
                                context(jpl_new/3,_)
                        )
                )
        ]
) :-
    jpl_new(Classname, [], _).

test(
        new_array_boolean_from_val_1,
        [       setup((
                        jpl_false(V)
                )),
                true((
                        V == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', newArrayBooleanFromValue, [V], A),
    jpl_get(A, 0, V2).

test(
        new_array_double_from_val_1,
        [       setup((
                        V is 1.5
                )),
                true((
                        V == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', newArrayDoubleFromValue, [V], A),
    jpl_get(A, 0, V2).

test(
        new_array_float_from_val_1,
        [       setup((
                        V is 1.5
                )),
                true((
                        V == V2
                ))
        ]
) :-
    jpl_call('org.jpl7.Test', newArrayFloatFromValue, [V], A),
    jpl_get(A, 0, V2).

test(
        new_interface_1,
        [       setup((
                        Classname = 'java.util.Enumeration'
                )),
                throws(
                        error(
                                type_error(concrete_class,Classname),
                                context(jpl_new/3,_)
                        )
                )
        ]
) :-
    jpl_new(Classname, [], _).

test(
        new_param_cyclic_term_1,
        [       setup((
                        T = f(T)
                )),
                throws(
                        error(
                                type_error(acyclic,T),
                                context(jpl_new/3,_)
                        )
                )
        ]
) :-
    jpl_new('org.jpl7.Test', [{T}], _).

test(
        prolog_calls_java_calls_prolog_1,
        [       true((
                        V == @(true)
                ))
        ]
) :-
    jpl_new('org.jpl7.Query', ['4 is 2+2'], Q),
    jpl_call(Q, hasSolution, [], V).

test(
        set_array_element_cyclic_term_1,
        [       setup((
                        T = f(T),
                        jpl_new(array(class([org,jpl7],['Test'])), 5, A)
                )),
                throws(
                        error(
                                type_error(acyclic,T),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 0, {T}).

test(
        set_array_elements_bad_type_1,
        [       setup((
                        jpl_new(array(byte), 3, A)
                )),
                throws(
                        error(
                                type_error(array(byte),[128]),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 0, 128).

test(
        set_array_length_1,
        [       setup((
                        jpl_new(array(byte), 6, A)
                )),
                throws(
                        error(
                                permission_error(modify,final_field,length),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, length, 13).

test(
        set_field_bad_field_spec_1,
        [       setup((
                        BadFieldName = 3.7
                )),
                throws(
                        error(
                                type_error(field_name,BadFieldName),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', BadFieldName, a).

test(
        set_field_instance_cyclic_term_1,
        [       setup((
                        T = f(T),
                        jpl_new('org.jpl7.Test', [], Test)
                )),
                throws(
                        error(
                                type_error(acyclic,T),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(Test, instanceTerm, {T}).

test(
        set_field_long_array_1,
        [       setup((
                        jpl_new(array(long), [1,2,3], LongArray)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticLongArray, LongArray).

test(
        set_field_long_array_2,
        [       setup((
                        jpl_new(array(int), [1,2,3], IntArray)
                )),
                throws(
                        error(
                                type_error('[J',IntArray),      % NB '[J' is *not* how the type was specified in the failing goal
                                context(
                                        jpl_set/3,
                                        'the value is not assignable to the named field of the class'
                                )
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticLongArray, IntArray).

test(
        set_field_object_array_1,
        [       setup((
                        jpl_new('java.util.Date', [], Date),
                        jpl_new(array(class([java,lang],['Object'])), [Date,Date], ObjArray)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticObjectArray, ObjArray).

test(
        set_field_static_bad_type_1,
        [       setup((
                        BadVal = 27
                )),
                throws(
                        error(
                                type_error(boolean,BadVal),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticBoolean, BadVal).

test(
        set_field_static_boolean_1,
        [       setup((
                        jpl_true(V)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticBoolean, V).

test(
        set_field_static_boolean_2,
        [       setup((
                        jpl_false(V)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticBoolean, V).

test(
        set_field_static_boolean_bad_1,
        [       setup((
                        BadVal = foo(bar)
                )),
                throws(
                        error(
                                type_error(field_value,BadVal),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticBoolean, BadVal).

test(
        set_field_static_cyclic_term_1,
        [       setup((
                        T = f(T)
                )),
                throws(
                        error(
                                type_error(acyclic,T),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', staticTerm, {T}).

test(
        set_field_static_final_int_1,
        [       setup((
                        FieldName = fieldStaticFinalInt,
                        Value = 6
                )),
                throws(
                        error(
                                permission_error(modify,final_field,FieldName),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', FieldName, Value).

test(
        set_field_static_shadow_1,
        [       blocked('we do not yet resolve same-named shadowed fields')
        ]
) :-
    jpl_set('org.jpl7.ShadowB', fieldStaticInt, 3).

test(
        set_field_static_term_1,
        [       setup((
                        T1 = foo(bar,33),
                        T2 = bar(77,bing)
                )),
                true((
                        T1 == T1a,
                        T2 == T2a
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticTerm, {T1}),
    jpl_get('org.jpl7.Test', fieldStaticTerm, {T1a}),
    jpl_set('org.jpl7.Test', fieldStaticTerm, {T2}),
    jpl_get('org.jpl7.Test', fieldStaticTerm, {T2a}).

test(
        set_field_static_term_2,
        [       setup((
                        T1 = foo(bar,33),
                        T2 = bar(77,bing)
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticTerm, {T1}),
    jpl_get('org.jpl7.Test', fieldStaticTerm, {T1}),
    jpl_set('org.jpl7.Test', fieldStaticTerm, {T2}),
    jpl_get('org.jpl7.Test', fieldStaticTerm, {T2}).

test(
        set_get_array_element_boolean_1,
        [       setup((
                        jpl_new(array(boolean), 3, A),
                        V = @(false)
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_boolean_2,
        [       setup((
                        jpl_new(array(boolean), 3, A),
                        V = @(true)
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_boolean_3,
        [       setup((
                        jpl_new(array(boolean), 3, A),
                        V = bogus
                )),
                throws(
                        error(
                                type_error(array(boolean),[V]),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 2, V).

test(
        set_get_array_element_byte_1,
        [       setup((
                        jpl_new(array(byte), 3, A),
                        V = 33
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_byte_2,
        [       setup((
                        jpl_new(array(byte), 3, A),
                        V = 128
                )),
                throws(
                        error(
                                type_error(array(byte),[V]),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 2, V).

test(
        set_get_array_element_char_1,
        [       setup((
                        jpl_new(array(char), 3, A),
                        V = 65535
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_double_1,
        [       setup((
                        jpl_new(array(double), 3, A),
                        V = 2.5
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_float_1,
        [       setup((
                        jpl_new(array(float), 3, A),
                        V = 7.5
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_float_2,
        [       setup((
                        jpl_new(array(float), 3, A),
                        V is 2,
                        VrX is float(V)
                )),
                true((
                        VrX == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_float_3,
        [       setup((
                        jpl_new(array(float), 3, A),
                        (       current_prolog_flag(bounded, true)
                        ->      current_prolog_flag(max_integer, Imax)
                        ;       Imax is 2**63-1
                        ),
                        VrX is float(Imax)
                )),
                true((
                        VrX == Vr
                ))
        ]
) :-
    jpl_set(A, 2, Imax),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_long_1,
        [       setup((
                        jpl_new(array(long), 3, A),
                        (       current_prolog_flag(bounded, true)
                        ->      current_prolog_flag(max_integer, V)
                        ;       V is 2**63-1
                        )
                )),
                true((
                        V == Vr
                ))
        ]
) :-
    jpl_set(A, 2, V),
    jpl_get(A, 2, Vr).

test(
        set_get_array_element_long_2,
        [       setup((
                        jpl_new(array(long), 3, A),
                        (       current_prolog_flag(bounded, true)
                        ->      current_prolog_flag(max_integer, V)
                        ;       V is 2**63
                        )
                )),
                throws(
                        error(
                                type_error(array(long),[V]),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set(A, 2, V).

test(
        set_get_array_elements_boolean_1,
        [       setup((
                        jpl_new(array(boolean), 3, A),
                        Vf = @(false),
                        Vt = @(true)
                )),
                true((
                        Vf+Vt+Vf == Vr0+Vr1+Vr2
                ))
        ]
) :-
    jpl_set(A, 0, Vf),
    jpl_set(A, 1, Vt),
    jpl_set(A, 2, Vf),
    jpl_get(A, 0, Vr0),
    jpl_get(A, 1, Vr1),
    jpl_get(A, 2, Vr2).

test(
        set_get_field_static_long_1,
        [       setup((
                        (   current_prolog_flag(bounded, true)
                    ->  current_prolog_flag(max_integer, V)
                    ;   V is 2**63-1
                    )
                )),
                true((
                        V == V2
                ))
        ]
) :-
    jpl_set('org.jpl7.Test', fieldStaticLong, V),
    jpl_get('org.jpl7.Test', fieldStaticLong, V2).

test(
        set_non_accessible_field_1,
        [       throws(
                        error(
                                existence_error(field,gagaga),
                                context(jpl_set/3,_)
                        )
                )
        ]
) :-
    jpl_set('org.jpl7.Test', gagaga, 4).

test(
        terms_to_array_1,
        []
) :-
    jpl_terms_to_array([foo(bar)], A),
    jpl_object_to_type(A, array(class([org,jpl7],['Term']))),
    jpl_get(A, length, 1),
    jpl_get(A, 0, T),
    jpl_call(T, toString, [], 'foo(bar)').

test(
        throw_java_exception_1,
        [       blocked('part of the error term is nondeterministic: we need to match with _'),
                throws(
                        error(
                                java_exception(@(_)),
                                'java.lang.NumberFormatException'
                        )
                )
        ]
) :-
    jpl_call('java.lang.Integer', decode, [q], _).

test(
        versions_1,
        [       true((
                        Vpl == Vc,
                        Vc == Vjava
                ))
        ]
) :-
    jpl_pl_lib_version(Vpl),
    jpl_c_lib_version(Vc),
    jpl_call('org.jpl7.JPL', version_string, [], Vjava).

%       JW: Mutual recursion check.  Moved from jpl.pl to here.  As the
%       callback is in module user, we define it there.

user:jpl_test_fac(N, F) :-
    (       N == 1
    ->      F = 1
    ;       N > 1
    ->      N2 is N-1,
            jpl_call('org.jpl7.Test', fac, [N2], F2),  % call its Java counterpart, which does vice versa
            F is N*F2
    ;       F = 0
    ).

test(fac10,
        [       true(N==3628800)
        ]
) :-
    user:jpl_test_fac(10, N).

test(threads1,
        [       true((
                        thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
                        thread_join(ThreadId, true)
                ))
        ]
) :-
    jpl_call('java.lang.System', currentTimeMillis, [], _).

test(threads2,
        [       true(X==true)
        ]
) :-
    jpl_call('java.lang.System', currentTimeMillis, [], _),
    thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
    thread_join(ThreadId, X).

test(threads3,
        [       true((
                        length(Ss, N),
                        sort(Ss, [true])
                ))
        ]
) :-
    N is 100,  % was 1000 (ok in V6); in V7 traditional, fails with 200
    jpl_call('java.lang.System', currentTimeMillis, [], _),
    findall(
            Status,
            (       between(1, N, _),
                    thread_create(jpl_call('java.lang.System', currentTimeMillis, [], _), ThreadId, []),
                    thread_join(ThreadId, Status)
            ),
            Ss
    ).

test(jref1,
        [       true((
                        Term1 \== Term2
                ))
        ]
) :-
    length(Term1, 5),
    jpl:jni_term_to_jref(Term1, JRef),
    jpl:jni_jref_to_term(JRef, Term2).

test(jref2,
        [       true((
                        Term1 =@= Term2
                ))
        ]
) :-
    length(Term1, 5),
    jpl:jni_term_to_jref(Term1, JRef),
    jpl:jni_jref_to_term(JRef, Term2).

:- end_tests(jpl).



         /*******************************
         * Testing recognizing/parsing  *
         *******************************/

% Switch these on for some output

% :- debug(identifier_chars).
% :- debug(java_id).
% :- debug(run_both).
% :- debug(generate).

% ===========================================================================
% Some helper functions
% ===========================================================================

andify_rightwards(true,true,true).
andify_rightwards(true,false,false).
andify_rightwards(false,true,false).
andify_rightwards(false,false,false).

all_true(List) :-
   foldl([E,FromLeft,ToRight]>>once(andify_rightwards(FromLeft,E,ToRight)),List,true,Out),
   Out == true.

% ---
% An implementation of ->/2. Pass three goals.
% ---

if_then_else(Condition,Then,Else) :-
   call(Condition) -> call(Then) ; call(Else).

% ---
% Reification of truth value
% ---

reify(Goal,Truth) :-
   if_then_else(call(Goal),(Truth=true),(Truth=false)).

% ---
% An implementation of ->/2 with an "else" that's true. Pass two goals.
% ---

if_then(Condition,Then) :-
   call(Condition) -> call(Then) ; true.

% ===========================================================================
% Recognize the descriptor in "In" (an atom)
% - by calling "DcgGoal" augmented with the two arguments "TypeTerm"
%   (generally a freshvar that captures the result of recognition)
%   and "Mode" (one of slashy or dotty).
% - leaving "Rest" (an atom) as leftover characters (or unify-comparing it if set)
% - instantiating "TypeTerm" to the strutured result (or unify-comnparing it if set)
% ===========================================================================

recognize(In,Rest,DcgGoal,TypeTerm,Mode) :-
   assertion(nonvar(In)),
   assertion(memberchk(Mode,[slashy,dotty,typedesc])),
   atom_codes(In,InCodes),
   compound_name_arguments(
      DcgGoalCompleted,
      DcgGoal,
      [TypeTerm,Mode]),
   phrase(DcgGoalCompleted,InCodes,RestCodes),
   atom_codes(Rest,RestCodes).

recognize(In,Rest,DcgGoal,TypeTerm) :-
   assertion(nonvar(In)),
   atom_codes(In,InCodes),
   compound_name_arguments(
      DcgGoalCompleted,
      DcgGoal,
      [TypeTerm]),
   phrase(DcgGoalCompleted,InCodes,RestCodes),
   atom_codes(Rest,RestCodes).

generate(TypeTerm,DcgGoal,Out) :-
   assertion(nonvar(TypeTerm)),
   compound_name_arguments(
      DcgGoalCompleted,
      DcgGoal,
      [TypeTerm]),
   phrase(DcgGoalCompleted,OutCodes),
   atom_codes(Out,OutCodes),
   debug(generate,"Transformed structure '~q' to atom '~s'",[TypeTerm,Out]).

% ===========================================================================
% Code used to compare the results of the old calls and the new calls
% ===========================================================================

% ---
% Construct the returnable term
% ---

outcome(true ,X,success(X)).
outcome(false,_,failure).

% ---
% Select which "old DCG predicate" (the ones in the old jpl code embedded
% in this file) to call
% ---

old_goal_by_mode(slashy   , jpl_type_findclassname ). % jpl_type_findclassname//1
old_goal_by_mode(dotty    , jpl_type_classname_1   ). % jpl_type_classname_1//1
old_goal_by_mode(typedesc , jpl_type_descriptor_1  ). % jpl_type_descriptor_1//1

% ---
% New DCG predicates
% ---

new_goal_by_mode(slashy   , new_slashy).
new_goal_by_mode(dotty    , new_dotty).
new_goal_by_mode(typedesc , new_typedesc).

% Indirection for new calls; easier than constructing the goal

new_slashy(T)   --> jpl_typeterm_rel_entityname(T,slashy).
new_dotty(T)    --> jpl_typeterm_rel_entityname(T,dotty).
new_typedesc(T) --> jpl_typeterm_rel_slashy_typedesc(T).

% ---
% Run the old call and the new call with input "In"
% ---

run_both(In,OutNew,OutOld,Mode) :-
   new_goal_by_mode(Mode,NewGoal),
   reify(
      recognize(In,'',NewGoal,TypeTermNew),
      SuccessNew),
   old_goal_by_mode(Mode,OldGoal),
   reify(
      recognize(In,'',OldGoal,TypeTermOld),
      SuccessOld),
   outcome(SuccessNew,TypeTermNew  ,OutNew),
   outcome(SuccessOld,TypeTermOld  ,OutOld),
   if_then_else(
      call(SuccessNew),debug(run_both,"~q : New   : ~q",[In,TypeTermNew]),debug(run_both,"~q : New failed",[In])),
   if_then_else(
      call(SuccessOld),debug(run_both,"~q : Old   : ~q",[In,TypeTermOld]),debug(run_both,"~q : Old failed",[In])),
   call(once(SuccessNew;SuccessOld)). % at least one must have succeeded!

% ===========================================================================
% Testing characters of Java identifiers
% ===========================================================================

% ---
% Helper
% Create a list of true/false atoms, one for each position of the input list
% of character codes, as the results of applying the predicates under test
% ---

maplist_java_id_start_char(ListIn,ListOut) :-
   maplist([C,T]>>
      reify(jpl_java_id_start_char(C),T),
      ListIn,ListOut).

maplist_java_id_part_char(ListIn,ListOut) :-
   maplist([C,T]>>
      reify(jpl_java_id_part_char(C),T),
      ListIn,ListOut).

maplist_java_id_disallowed_char(ListIn,ListOut) :-
   maplist([C,T]>>
      reify(
         ((\+ jpl_java_id_part_char(C)),
          (\+ jpl_java_id_start_char(C))),T),
      ListIn,ListOut).

maplist_java_id_part_char_but_not_start_char(ListIn,ListOut) :-
   maplist([C,T]>>
      reify(
         ((   jpl_java_id_part_char(C)),
          (\+ jpl_java_id_start_char(C))),T),
      ListIn,ListOut).


:- begin_tests(identifier_chars).

test("characters allowed at start of an identifier") :-
   maplist_java_id_start_char(`abcdefghijklöüä`,R),
   debug(identifier_chars,"Result for 'characters allowed at start of an identifier': ~q",[R]),
   all_true(R).

test("more characters allowed at start of an identifier") :-
   maplist_java_id_start_char(`$\u3047_`,R), % \u3047 Hiragana letter small e
   debug(identifier_chars,"Result for 'more characters allowed at the start of an identifier': ~q",[R]),
   all_true(R).

test("characters disallowed in identifiers") :-
   maplist_java_id_disallowed_char(`-.`,R),
   debug(identifier_chars,"Result for 'characters disallowed in identifiers': ~q",[R]),
   all_true(R).

test("characters allowed as part but not as start of identifiers") :-
   maplist_java_id_part_char_but_not_start_char(`0123456789`,R),
   debug(identifier_chars,"Result for 'characters allowed as part but not as start of identifiers': ~q",[R]),
   all_true(R).

:- end_tests(identifier_chars).

% ===========================================================================
% Testing Java identifiers via "jpl_java_id//1"
% ===========================================================================

:- begin_tests(java_id).

test("recognize Java identifier (unconstrained Out), no rest", true([Out,Rest] == [my_identifier,''])) :-
   recognize('my_identifier',Rest,jpl_java_id,Out),
   debug(java_id,"Recognized: ~q with rest: ~q",[Out,Rest]).

test("recognize Java identifier (unconstrained Out), with rest", true([Out,Rest] == [my_identifier,'.dodododo'])) :-
   recognize('my_identifier.dodododo',Rest,jpl_java_id,Out),
   debug(java_id,"Recognized: ~q with rest: ~q",[Out,Rest]).

test("recognize Java identifier of length 1, no rest", true([Out,Rest] == [m,''])) :-
   recognize('m',Rest,jpl_java_id,Out).

test("recognize Java identifier (Out already set to result), no rest", true(Rest == '')) :-
   recognize('my_identifier',Rest,jpl_java_id,'my_identifier').

test("recognize Java identifier (Out already set to result), with rest", true(Rest == '.dodododo')) :-
   recognize('my_identifier.dodododo',Rest,jpl_java_id,'my_identifier').

test("starts with dash: not a Java identifier", fail) :-
   recognize('-my',_,jpl_java_id,_).

test("contains dash and thus is broken up", true([Out,Rest] == ['my','-my'])) :-
   recognize('my-my',Rest,jpl_java_id,Out).

test("empty atom is not a Java identifier", fail) :-
   recognize('',_,jpl_java_id,_).

test("valid identifier with differing Out", fail) :-
   recognize('my',_,jpl_java_id,'notmy').

:- end_tests(java_id).

% ===========================================================================
% Testing Java type identifiers via "jpl_java_type_id//1"
% This is practically the same as testing "jpl_java_id";
% here only the keywords "var" and "yield" are additionally disallowed.
% ===========================================================================

:- begin_tests(java_type_id).

test("recognize Java type identifier",true([Out,Rest] == [my_identifier,''])) :-
   recognize('my_identifier',Rest,jpl_java_type_id,Out).

test("reject bad Java type identifier 'var'",fail) :-
   recognize('var',_,jpl_java_type_id,_).

test("java type identifier DOES NOT stop at '$'",true([Out,Rest] == ['foo$bar',''])) :-
   recognize('foo$bar',Rest,jpl_java_type_id,Out).

:- end_tests(java_type_id).

% ===========================================================================
% Testing the "messy dollar split" which is used to split Java classnames
% but is actually of dubious value
% ===========================================================================

:- begin_tests(messy_dollar_split).

test(1,true(Runs == [alfa])) :-
   messy_dollar_split(alfa,Runs).

test(2,true(Runs == [a])) :-
   messy_dollar_split(a,Runs).

test(3,true(Runs == ['$'])) :-
   messy_dollar_split('$',Runs).

test(4,true(Runs == ['alfa$'])) :-
   messy_dollar_split('alfa$',Runs).

test(5,true(Runs == [alfa,bravo])) :-
   messy_dollar_split('alfa$bravo',Runs).

test(6,true(Runs == ['$alfa'])) :-
   messy_dollar_split('$alfa',Runs).

test(7,true(Runs == ['alfa','$bravo'])) :-
   messy_dollar_split('alfa$$bravo',Runs).

test(8,true(Runs == ['$alfa','bravo','charlie$'])) :-
   messy_dollar_split('$alfa$bravo$charlie$',Runs).

test(9,true(Runs == ['$$alfa','$bravo','$$charlie','$$$'])) :-
   messy_dollar_split('$$alfa$$bravo$$$charlie$$$$',Runs).

:- end_tests(messy_dollar_split).

% ===========================================================================
% Testing recognition of the "binary classname", i.e. the classname
% as it appears in binaries (in its 'dotty' form)
% ===========================================================================

:- begin_tests(jpl_classname_without_dollar).

test("simple classname",true(Out == class([],[foo]))) :-
   recognize('foo','',jpl_classname,Out,dotty).

test("qualified classname",true(Out == class([alfa,bravo,charlie],[foo]))) :-
   recognize('alfa.bravo.charlie.foo','',jpl_classname,Out,dotty).

:- end_tests(jpl_classname_without_dollar).

% ===========================================================================
% Testing recognition of the "binary classname" with "$" inside.
% Note that "splitting at a dollar is ill-defined and pointless and
% should eventually disappear.
% ===========================================================================

:- begin_tests(jpl_classname_with_dollar).

test("qualified inner member type",true(Out == class([alfa,bravo,charlie],[foo,bar]))) :-
   recognize('alfa.bravo.charlie.foo$bar','',jpl_classname,Out,dotty).

test("qualified inner anonymous type",true(Out == class([alfa,bravo,charlie],[foo,'01234']))) :-
   recognize('alfa.bravo.charlie.foo$01234','',jpl_classname,Out,dotty).

test("qualified inner local class",true(Out == class([alfa,bravo,charlie],[foo,'01234bar']))) :-
   recognize('alfa.bravo.charlie.foo$01234bar','',jpl_classname,Out,dotty).

test("qualified inner member type, deep",true(Out == class([alfa,bravo,charlie],[foo,bar,baz,quux]))) :-
   recognize('alfa.bravo.charlie.foo$bar$baz$quux','',jpl_classname,Out,dotty).

:- end_tests(jpl_classname_with_dollar).

% ===========================================================================
% Testing Java entityname <-> typeterm mapping for some primitives
% ===========================================================================

:- begin_tests(jpl_entity_is_primitive).

test("entityname is just 'int': integer primitive",true(Out == int)) :-
   recognize('int','',jpl_typeterm_rel_entityname,Out,dotty).

test("entityname is just 'void': void primitive",true(Out == void)) :-
   recognize('void','',jpl_typeterm_rel_entityname,Out,dotty).

test("entityname is actually 'integer', which is a class called 'integer', which is ok!",true(Out == class([],[integer]))) :-
   recognize('integer','',jpl_typeterm_rel_entityname,Out,dotty).

:- end_tests(jpl_entity_is_primitive).

% ===========================================================================
% Testing Java entityname <-> typeterm mapping for arrays
% ===========================================================================

:- begin_tests(jpl_entity_is_array).

test("array of double",true(Out == array(double))) :-
   recognize('[D','',jpl_typeterm_rel_entityname,Out,dotty).

test("array of array of integer",true(Out == array(array(int)))) :-
   recognize('[[I','',jpl_typeterm_rel_entityname,Out,dotty).

test("array of void",fail) :-
   recognize('[[V','',jpl_typeterm_rel_entityname,_,dotty).

test("array of java.lang.String",true(Out == array(array(class([java, lang], ['String']))))) :-
   recognize('[[Ljava.lang.String;','',jpl_typeterm_rel_entityname,Out,dotty).

:- end_tests(jpl_entity_is_array).

% ===========================================================================
% Directly comparing old and new entityname <-> typeterm mapping
% for "dotty" entity names
% ===========================================================================

:- begin_tests(compare_both_dotty).

test("dotty/comparing #1" ,[blocked("Old response bad"),true(OutNew == OutOld)]) :-
   run_both('int',OutNew,OutOld,dotty).    % Old: class([],[int])   ???

test("dotty/comparing #2" ,[blocked("Old response bad"),true(OutNew == OutOld)]) :-
   run_both('float',OutNew,OutOld,dotty).  % Old: class([],[float]) ???

test("dotty/comparing #3" ,[blocked("Old response bad"),true(OutNew == OutOld)]) :-
   run_both('void',OutNew,OutOld,dotty).   % Old: class([],[void])  ???

test("old call gives wrong result #1") :-
   run_both('foo.bar.baz.Foo$',success(OutNew),success(OutOld),dotty),
   OutNew == class([foo,bar,baz],['Foo$']),
   OutOld == class([foo,bar,baz],['Foo','']). % OLD IS WRONG

test("failure on old call #2") :-
   run_both('foo.bar.baz.$Foo',success(OutNew),failure,dotty), % OLD FAILS
   OutNew == class([foo,bar,baz],['$Foo']).

test("dotty/comparing 01" , true(OutNew == OutOld)) :-
   run_both('java.lang.Integer',OutNew,OutOld,dotty).

test("dotty/comparing 02" , true(OutNew == OutOld)) :-
   run_both('integer',OutNew,OutOld,dotty). % The class called "integer" (not the primitive!)

test("dotty/comparing 03" , true(OutNew == OutOld)) :-
   run_both('[D',OutNew,OutOld,dotty).

test("dotty/comparing 04" , true(OutNew == OutOld)) :-
   run_both('[[[[[I',OutNew,OutOld,dotty).

test("dotty/comparing 05" , true(OutNew == OutOld)) :-
   run_both('[[J',OutNew,OutOld,dotty).

test("dotty/comparing 06" , true(OutNew == OutOld)) :-
   run_both('[[Ljava.lang.String;',OutNew,OutOld,dotty).

test("dotty/comparing 07" , true(OutNew == OutOld)) :-
   run_both('java.lang.String',OutNew,OutOld,dotty).

test("dotty/comparing 08" , true(OutNew == OutOld)) :-
   run_both('Foo',OutNew,OutOld,dotty).

test("dotty/comparing 09" , true(OutNew == OutOld)) :-
   run_both('foo.bar.baz.Foo',OutNew,OutOld,dotty).

test("dotty/comparing 10" , true(OutNew == OutOld)) :-
   run_both('foo.bar.baz.Foo$Quux',OutNew,OutOld,dotty).

:- end_tests(compare_both_dotty).

% ===========================================================================
% Directly comparing old and new entityname <-> typeterm mapping
% for "slashy" entity names
% ===========================================================================

:- begin_tests(compare_both_slashy).

test("slashy/comparing 01" , true(OutNew == OutOld)) :-
   run_both('java/lang/Integer',OutNew,OutOld,slashy).

test("slashy/comparing 02" , true(OutNew == OutOld)) :-
   run_both('integer',OutNew,OutOld,slashy). % The class called "integer"

test("slashy/comparing 03" , true(OutNew == OutOld)) :-
   run_both('[D',OutNew,OutOld,slashy).

test("slashy/comparing 04" , true(OutNew == OutOld)) :-
   run_both('[[[[[I',OutNew,OutOld,slashy).

test("slashy/comparing 05" , true(OutNew == OutOld)) :-
   run_both('[[J',OutNew,OutOld,slashy).

test("slashy/comparing 06" , true(OutNew == OutOld)) :-
   run_both('[[Ljava/lang/String;',OutNew,OutOld,slashy).

test("slashy/comparing 07" , true(OutNew == OutOld)) :-
   run_both('java/lang/String',OutNew,OutOld,slashy).

test("slashy/comparing 08" , true(OutNew == OutOld)) :-
   run_both('Foo',OutNew,OutOld,slashy).

test("slashy/comparing 09" , true(OutNew == OutOld)) :-
   run_both('foo/bar/baz/Foo',OutNew,OutOld,slashy).

test("slashy/comparing 10" , true(OutNew == OutOld)) :-
   run_both('foo/bar/baz/Foo$Quux',OutNew,OutOld,slashy).

:- end_tests(compare_both_slashy).

% ===========================================================================
% Directly comparing old and new
% ===========================================================================

:- begin_tests(compare_both_typedesc).

test("typedesc/comparing 03" , true(OutNew == OutOld)) :-
   run_both('[D',OutNew,OutOld,typedesc).

test("typedesc/comparing 04" , true(OutNew == OutOld)) :-
   run_both('[[[[[I',OutNew,OutOld,typedesc).

test("typedesc/comparing 05" , true(OutNew == OutOld)) :-
   run_both('[[J',OutNew,OutOld,typedesc).

test("typedesc/comparing 06" , true(OutNew == OutOld)) :-
   run_both('[[Ljava/lang/String;',OutNew,OutOld,typedesc).

test("typedesc/comparing 07" , true(OutNew == OutOld)) :-
   run_both('Ljava/lang/String;',OutNew,OutOld,typedesc).

test("typedesc/comparing 08" , true(OutNew == OutOld)) :-
   run_both('LFoo;',OutNew,OutOld,typedesc).

test("typedesc/comparing 09" , true(OutNew == OutOld)) :-
   run_both('Lfoo/bar/baz/Foo;',OutNew,OutOld,typedesc).

test("typedesc/comparing 10" , true(OutNew == OutOld)) :-
   run_both('Lfoo/bar/baz/Foo$Quux;',OutNew,OutOld,typedesc).

test("typedesc/comparing 11" , true(OutNew == OutOld)) :-
   run_both('([[Ljava/lang/String;Ljava/lang/Integer;JJ[D)D',OutNew,OutOld,typedesc).

:- end_tests(compare_both_typedesc).

% ===========================================================================
% Generating "dotty classnames" (the ones found in binaries)
% ===========================================================================

:- begin_tests(generate_dotty).

test("generate dotty 01", true(Out == int)) :-
   generate(int,new_dotty,Out).

test("generate dotty 02", true(Out == void)) :-
   generate(void,new_dotty,Out).

test("generate dotty 03", true(Out == double)) :-
   generate(double,new_dotty,Out).

test("generate dotty 04", true(Out == 'java.lang.String')) :-
   generate(class([java,lang],['String']),new_dotty,Out).

test("generate dotty 05", true(Out == '[Ljava.lang.String;')) :-
   generate(array(class([java,lang],['String'])),new_dotty,Out).

test("generate dotty 06", true(Out == '[[[Ljava.util.Calendar;')) :-
   generate(array(array(array(class([java,util],['Calendar'])))),new_dotty,Out).

test("generate dotty failure 01", fail) :-
   generate(array(array(array(class([foo,bared],['-hello'])))),new_dotty,_).

:- end_tests(generate_dotty).

% ===========================================================================
% Generating "slashy classnames" (the ones for JNI findclass)
% ===========================================================================

:- begin_tests(generate_slashy).

test("generate slashy 01", true(Out == int)) :-
   generate(int,new_slashy,Out).

test("generate slashy 02", true(Out == void)) :-
   generate(void,new_slashy,Out).

test("generate slashy 03", true(Out == double)) :-
   generate(double,new_slashy,Out).

test("generate slashy 04", true(Out == 'java/lang/String')) :-
   generate(class([java,lang],['String']),new_slashy,Out).

test("generate slashy 05", true(Out == '[Ljava/lang/String;')) :-
   generate(array(class([java,lang],['String'])),new_slashy,Out).

test("generate slashy 06", true(Out == '[[[Ljava/util/Calendar;')) :-
   generate(array(array(array(class([java,util],['Calendar'])))),new_slashy,Out).

test("generate slashy failure 01", fail) :-
   generate(array(array(array(class([foo,bared],['-hello'])))),new_slashy,_).

:- end_tests(generate_slashy).

% ===========================================================================
% Generating "slashy type descritpors"
% ===========================================================================

:- begin_tests(generate_slashy_typedesc).

test("generate slashy typedesc 01", true(Out == 'I')) :-
   generate(int,new_typedesc,Out).

test("generate slashy typedesc 02", [true(Out == 'V'),blocked("No 'void' in slashy typedesc, AFAIK")]) :-
   generate(void,new_typedesc,Out).

test("generate slashy typedesc 03", true(Out == 'D')) :-
   generate(double,new_typedesc,Out).

test("generate slashy typedesc 04", true(Out == 'Ljava/lang/String;')) :-
   generate(class([java,lang],['String']),new_typedesc,Out).

test("generate slashy typedesc 05", true(Out == '[Ljava/lang/String;')) :-
   generate(array(class([java,lang],['String'])),new_typedesc,Out).

test("generate slashy typedesc 06", true(Out == '[[[Ljava/util/Calendar;')) :-
   generate(array(array(array(class([java,util],['Calendar'])))),new_typedesc,Out).

test("generate slashy typedesc failure 01", fail) :-
   generate(array(array(array(class([foo,bared],['-hello'])))),new_typedesc,_).

:- end_tests(generate_slashy_typedesc).

% ===========================================================================
% The original jpl code for recognizing entity names etc.
% Added here because we want to run direct comparisons
% ===========================================================================

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


