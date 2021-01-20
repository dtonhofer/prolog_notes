:- use_module(library('heavycarbon/strings/stringy.pl')).

:- begin_tests(stringy_tests).

test(stringy_0,fail) :- stringy(_).
test(stringy_1)      :- stringy(xxx).
test(stringy_2)      :- stringy("xxx").
test(stringy_3)      :- stringy(666.666).
test(stringy_4,fail) :- stringy([a,b,c]).
test(stringy_5,fail) :- stringy([102,111,111]).

test(stringy_is_chars_0,fail) :- stringy_is_chars(_).
test(stringy_is_chars_1,fail) :- stringy_is_chars([a,b,c|_]).
test(stringy_is_chars_2)      :- stringy_is_chars([a,b,c]).
test(stringy_is_chars_3)      :- stringy_is_chars([]).
test(stringy_is_chars_4,fail) :- stringy_is_chars(foo).

test(stringy_is_codes_0,fail) :- stringy_is_codes(_).
test(stringy_is_codes_1,fail) :- stringy_is_codes([102,111,111|_]).
test(stringy_is_codes_2)      :- stringy_is_codes([102,111,111]).
test(stringy_is_codes_3)      :- stringy_is_codes([]).
test(stringy_is_codes_4,fail) :- stringy_is_codes(foo).

test(stringy_is_atomic_0,fail) :- stringy_is_atomic(_).
test(stringy_is_atomic_1)      :- stringy_is_atomic("foo").
test(stringy_is_atomic_2)      :- stringy_is_atomic('foo').
test(stringy_is_atomic_3,fail) :- stringy_is_atomic([f,o,o]).
test(stringy_is_atomic_4,fail) :- stringy_is_atomic([102,111,111|_]).
test(stringy_is_atomic_5,fail) :- stringy_is_atomic([102,111,111]).
test(stringy_is_atomic_6,fail) :- stringy_is_atomic([]).
test(stringy_is_atomic_7,fail) :- stringy_is_atomic(666.666).

test(stringy_is_text_0,fail) :- stringy_is_text(_).
test(stringy_is_text_1)      :- stringy_is_text("foo").
test(stringy_is_text_2)      :- stringy_is_text('foo').
test(stringy_is_text_3)      :- stringy_is_text([f,o,o]).
test(stringy_is_text_4)      :- stringy_is_text([102,111,111]).
test(stringy_is_text_5,fail) :- stringy_is_text([102,111,111|_]).
test(stringy_is_text_6,fail) :- stringy_is_text(666.666).

test(stringy_is_anytext_0,fail) :- stringy_is_anytext(_).
test(stringy_is_anytext_1)      :- stringy_is_anytext("foo").
test(stringy_is_anytext_2)      :- stringy_is_anytext('foo').
test(stringy_is_anytext_3)      :- stringy_is_anytext([f,o,o]).
test(stringy_is_anytext_4)      :- stringy_is_anytext([102,111,111]).
test(stringy_is_anytext_5,fail) :- stringy_is_text([102,111,111|_]).
test(stringy_is_anytext_6)      :- stringy_is_anytext([]).
test(stringy_is_anytext_7)      :- stringy_is_anytext(666.666).
test(stringy_is_anytext_8,fail) :- stringy_is_anytext(f(g(x))).

:- end_tests(stringy_tests).

:- begin_tests(stringy_chars).

test(break_string_want_unknown,  true([Chars,Want] == [[f,o,o],string]) )      :- stringy_chars("foo", Chars, Want).
test(break_atom_want_unknown,    true([Chars,Want] == [[f,o,o],atom]) )        :- stringy_chars(foo, Chars, Want).
test(break_number_want_unknown,  true([Chars,Want] == [['1','1','3'],other]) ) :- stringy_chars(113, Chars, Want).

test(break_string_want_is_known, true(Chars == [f,o,o]))       :- stringy_chars("foo", Chars, string).
test(break_atom_want_is_known,   true(Chars == [f,o,o]))       :- stringy_chars(foo  , Chars, atom).
test(break_number_want_is_known, true(Chars == ['1','1','3'])) :- stringy_chars(113  , Chars, other).

test(accept_string)     :- stringy_chars("foo", [f,o,o], string).
test(break_atom)        :- stringy_chars(foo, [f,o,o], atom).

test(build_string, true(Text == "foo") ) :- stringy_chars(Text, [f,o,o], string).
test(build_atom,   true(Text == foo) )   :- stringy_chars(Text, [f,o,o], atom).

test(build_from_bad_stuff_1, error(type_error(_,_))) :- stringy_chars(_, ["a"], atom).
test(build_from_bad_stuff_2, error(type_error(_,_))) :- stringy_chars(_, [1.44], atom).

test(build_atom_from_what_is_actually_codes, true(Atom==o))       :- stringy_chars(Atom, [111], atom).
test(build_string_from_what_is_actually_codes, true(String=="o")) :- stringy_chars(String, [111], string).

test(build_from_messy_stuff_1, true(Text == '11a')) :- stringy_chars(Text, ['1','1','a'], atom).
test(build_from_messy_stuff_2, true(Text == "11a")) :- stringy_chars(Text, ['1','1','a'], string).

test(build_default, true([Text,Want] == ["foo",string])) :- stringy_chars(Text, [f,o,o], Want).

test(break_nonstring, error(type_error(_,_)))   :- stringy_chars(g(1), _Chars, string).
test(break_nonatom, error(type_error(_,_)))     :- stringy_chars(g(1), _Chars, atom).
test(break_nonanything, error(type_error(_,_))) :- stringy_chars(g(1), _Chars, quux).

:- end_tests(stringy_chars).


:- begin_tests(stringy_concat).

test(compute_1s,true(S=="abc"))              :- stringy_concat(a,b,c,S,string).
test(compute_2s,true(S=="c"))                :- stringy_concat('','',c,S,string).
test(compute_3s,true(S=="127what is this ")) :- stringy_concat(127,what," is this ",S,string).
test(compute_4s,true(S=="abcdefgh"))         :- stringy_concat(a,b,c,d,e,f,g,h,S,string).
test(compute_4a,true(S=="efgh"))             :- stringy_concat("",'','',"",e,f,g,h,S,string).

test(compute_1a,true(S=='abc'))              :- stringy_concat(a,b,c,S,atom).
test(compute_2a,true(S=='c'))                :- stringy_concat('','',c,S,atom).
test(compute_3a,true(S=='127what is this ')) :- stringy_concat(127,what," is this ",S,atom).
test(compute_4a,true(S=='abcdefgh'))         :- stringy_concat(a,b,c,d,e,f,g,h,S,atom).
test(compute_4a,true(S=='efgh'))             :- stringy_concat('',"","",'',e,f,g,h,S,atom).

test(exception_1,error(instantiation_error)) :- stringy_concat(a,_,c,_,atom).

:- end_tests(stringy_concat).



:- begin_tests(stringy_length).
      
test(compute_1,true(L==5)) :- stringy_length("hello",L).
test(compute_2,true(L==5)) :- stringy_length(hello,L).
test(compute_3,true(L==5)) :- stringy_length([h,e,l,l,o],L).
test(compute_4,true(L==5)) :- stringy_length([104,101,108,108,111],L).
test(compute_5,true(L==3)) :- stringy_length(123,L).

test(exception_1,error(instantiation_error))      :- stringy_length(_,_).
test(exception_2,error(type_error(text,f(x))))    :- stringy_length(f(x),_).  % thrown by string_length/2
test(exception_3,error(domain_error(_,_)))        :- stringy_length(f(_),_).
test(exception_4,error(type_error(integer,foo)))  :- stringy_length("hello",foo). % thrown by string_length/2
test(exception_5,error(type_error(integer,1.22))) :- stringy_length("hello",1.22). % thrown by string_length/2

test(accept_1)      :- stringy_length("hello",5).
test(accept_2)      :- stringy_length(123,3).
test(accept_3)      :- stringy_length([h,e,l,l,o],5).
test(accept_4,fail) :- stringy_length("hello",6).
test(accept_5,fail) :- stringy_length("hello",-1).

:- end_tests(stringy_length).


:- begin_tests(stringy_ensure).

test(string_compute_1,true(Out == "hello")) :- stringy_ensure(hello,Out,string).
test(string_compute_2,true(Out == "hello")) :- stringy_ensure("hello",Out,string).
test(string_compute_3,true(Out == "123"))   :- stringy_ensure(123,Out,string).
test(string_compute_4,true(Out == "hello")) :- stringy_ensure([h,e,l,l,o],Out,string).
test(string_compute_5,true(Out == "hello")) :- stringy_ensure([104,101,108,108,111],Out,string).

test(string_accept_1) :- stringy_ensure(hello,"hello",string).
test(string_accept_2) :- stringy_ensure("hello","hello",string).
test(string_accept_3) :- stringy_ensure(123,"123",string).
test(string_accept_4) :- stringy_ensure([h,e,l,l,o],"hello",string).
test(string_accept_5) :- stringy_ensure([104,101,108,108,111],"hello",string).

test(string_notaccept_1,fail) :- stringy_ensure(hello,hello,string).

test(atom_compute_1,true(Out == 'hello')) :- stringy_ensure(hello,Out,atom).
test(atom_compute_2,true(Out == 'hello')) :- stringy_ensure("hello",Out,atom).
test(atom_compute_3,true(Out == '123'))   :- stringy_ensure(123,Out,atom).
test(atom_compute_4,true(Out == 'hello')) :- stringy_ensure([h,e,l,l,o],Out,atom).
test(atom_compute_5,true(Out == 'hello')) :- stringy_ensure([104,101,108,108,111],Out,atom).

test(atom_accept_1) :- stringy_ensure(hello,hello,atom).
test(atom_accept_2) :- stringy_ensure("hello",hello,atom).
test(atom_accept_3) :- stringy_ensure(123,'123',atom).
test(atom_accept_4) :- stringy_ensure([h,e,l,l,o],hello,atom).
test(atom_accept_5) :- stringy_ensure([104,101,108,108,111],hello,atom).

test(atom_notaccept_2,fail) :- stringy_ensure(hello,"hello",atom).

test(exception_1,[error(type_error(string,f(x)))]) :- stringy_ensure(f(x),_,atom).  % actually thrown by atom_string/2
test(exception_2,[error(instantiation_error)])     :- stringy_ensure(_,_,atom).

:- end_tests(stringy_ensure).
