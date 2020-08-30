:- use_module(library('heavycarbon/strings/stringy.pl')).

:- begin_tests(stringy).

% ---
% Breaking mode
% ---

test("break a string", true([Chars,Want] == [[f,o,o],string]) ) :- 
   stringy_chars("foo", Chars, Want).

test("break an atom", true([Chars,Want] == [[f,o,o],atom]) ) :- 
   stringy_chars(foo, Chars, Want).

test("break a number", true([Chars,Want] == [['1','1','3'],other]) ) :- 
   stringy_chars(113, Chars, Want).

% ---
% Building mode
% ---

test("build a string", true(Text == "foo") ) :- 
   stringy_chars(Text, [f,o,o], string).

test("build an atom", true(Text == foo) ) :- 
   stringy_chars(Text, [f,o,o], atom).

test("build an atom from unclean stuff; builtin doesn't like a string 'a'", error(type_error(_,_)) ) :- 
   stringy_chars(_Text, ["a"], atom).

test("build an atom from unclean but acceptable stuff", true(Text == '11a')) :- 
   stringy_chars(Text, ['1','1','a'], atom).

test("build a default", true([Text,Want] == ["foo",string]) ) :-
   stringy_chars(Text, [f,o,o], Want).

% ---
% Failure modes
% ---

test("break a non-string", error(_)) :- 
   stringy_chars(g(1), _Chars, string).

test("break a non-atom ", error(_)) :- 
   stringy_chars(g(1), _Chars, atom).

test("break a non-anything ", error(_)) :- 
   stringy_chars(g(1), _Chars, quux).

:- end_tests(stringy).


