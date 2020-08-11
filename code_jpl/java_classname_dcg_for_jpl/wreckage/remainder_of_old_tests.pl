% ===
% Something to replace (badly named & ugly) "var(X)" and "nonvar(X)"
% ===

% is X referencing a fresh/unbound/uninstantiated term? ("is X a freshvar"?)

ff(X) :- var(X).

% is X referencing an nonfresh/bound/instantiated term? ("is X a boundvar"?)

bb(X) :- nonvar(X).

% ===
% Tests
% ===

% TODO: Is there a way to generate new test cases by named based on a findall?
% That would preclude having to copy the three test calls for every related/3

:- begin_tests(slashy).

% --- helpers

test_it(1,Code) :- related(Code,ET,EN), parse_slashy(ET,O), O == EN.
test_it(2,Code) :- related(Code,ET,EN), parse_slashy(O,EN), O == ET.
test_it(3,Code) :- related(Code,ET,EN), parse_slashy(ET,EN).

% --- tests

test(atom_vs_string_1, true(ET == boolean)) :- parse_slashy(ET,'Z').
test(atom_vs_string_2, true(ET == boolean)) :- parse_slashy(ET,"Z").

test(atom_vs_string_all_bound_1) :- parse_slashy(boolean,'Z').
test(atom_vs_string_all_bound_2) :- parse_slashy(boolean,"Z").

test(primitive_1) :- parse_slashy(boolean,'Z').
test(primitive_2) :- parse_slashy(byte   ,'B').
test(primitive_3) :- parse_slashy(char   ,'C').
test(primitive_4) :- parse_slashy(short  ,'S').
test(primitive_5) :- parse_slashy(int    ,'I').
test(primitive_6) :- parse_slashy(long   ,'J').
test(primitive_7) :- parse_slashy(float  ,'F').
test(primitive_8) :- parse_slashy(double ,'D').

related( a00, class([],['MyClass']), 
              'LMyClass;' ).

related( a01, class([p1,p2,p3],['MyClass','InnerClassA','InnerClassB']) , 
              'Lp1/p2/p3/MyClass$InnerClassA$InnerClassB;').

related( a02, class([foo],['MyClass']) ,
              'Lfoo/MyClass;').

related( a03, array(class([java,lang],['String'])) ,
              '[Ljava/lang/String;').

related( a04, method([class([java,lang],['String'])],void) ,
              '(Ljava/lang/String;)V').

related( a05, method([class([java,lang],['String']),
                 array(class([java,util],['Calendar','Day'])),
                 boolean],
                 array(class([com,example,megacorp],['Memo']))) ,
              '(Ljava/lang/String;[Ljava/util/Calendar$Day;Z)[Lcom/example/megacorp/Memo;').

test(a00_ET_to_EN) :- test_it(1,a00).
test(a00_EN_to_ET) :- test_it(2,a00).
test(a00_ET_vs_EN) :- test_it(3,a00).

test(a01_ET_to_EN) :- test_it(1,a01).
test(a01_EN_to_ET) :- test_it(2,a01).
test(a01_ET_vs_EN) :- test_it(3,a01).

test(a02_ET_to_EN) :- test_it(1,a02).
test(a02_EN_to_ET) :- test_it(2,a02).
test(a02_ET_vs_EN) :- test_it(3,a02).

test(a03_ET_to_EN) :- test_it(1,a03).
test(a03_EN_to_ET) :- test_it(2,a03).
test(a03_ET_vs_EN) :- test_it(3,a03).

test(a04_ET_to_EN) :- test_it(1,a04).
test(a04_EN_to_ET) :- test_it(2,a04).
test(a04_ET_vs_EN) :- test_it(3,a04).

test(a05_ET_to_EN) :- test_it(1,a05).
test(a05_EN_to_ET) :- test_it(2,a05).
test(a05_ET_vs_EN) :- test_it(3,a05).

:- end_tests(slashy).

:- begin_tests(findy).

% --- helpers

test_it(1,Code) :- related(Code,ET,EN), parse_findy(ET,O), O == EN.
test_it(2,Code) :- related(Code,ET,EN), parse_findy(O,EN), O == ET.
test_it(3,Code) :- related(Code,ET,EN), parse_findy(ET,EN).

% --- tests

related( b01, array(class([java,lang],['String'])) ,
              '[Ljava/lang/String;').

related( b02, array(class([com,example,megacorp],['Memo'])) ,
              '[Lcom/example/megacorp/Memo;').

related( b03, class([p1,p2,p3],['MyClass','InnerClassA','InnerClassB']) ,
              'p1/p2/p3/MyClass$InnerClassA$InnerClassB').

related( b04, class([foo],['MyClass']) ,
              'foo/MyClass').

test(b01_ET_to_EN) :- test_it(1,b01).
test(b01_EN_to_ET) :- test_it(2,b01).
test(b01_ET_vs_EN) :- test_it(3,b01).

test(b02_ET_to_EN) :- test_it(1,b02).
test(b02_EN_to_ET) :- test_it(2,b02).
test(b02_ET_vs_EN) :- test_it(3,b02).

test(b03_ET_to_EN) :- test_it(1,b03).
test(b03_EN_to_ET) :- test_it(2,b03).
test(b03_ET_vs_EN) :- test_it(3,b03).

test(b04_ET_to_EN) :- test_it(1,b04).
test(b04_EN_to_ET) :- test_it(2,b04).
test(b04_ET_vs_EN) :- test_it(3,b04).

:- end_tests(findy).

:- begin_tests(dotty).

% --- helpers

test_it(1,Code) :- related(Code,ET,EN), parse_dotty(ET,O), O == EN.
test_it(2,Code) :- related(Code,ET,EN), parse_dotty(O,EN), O == ET.
test_it(3,Code) :- related(Code,ET,EN), parse_dotty(ET,EN).

% --- tests

related( c01, array(class([java,lang],['String'])) ,
              '[Ljava.lang.String;').

related( c02, array(class([com,example,megacorp],['Memo'])) ,
              '[Lcom.example.megacorp.Memo;').

related( c03, class([p1,p2,p3],['MyClass','InnerClassA','InnerClassB']) ,
              'p1.p2.p3.MyClass$InnerClassA$InnerClassB').

related( c04, class([foo],['MyClass']) ,
              'foo.MyClass').

test(c01_ET_to_EN) :- test_it(1,c01).
test(c01_EN_to_ET) :- test_it(2,c01).
test(c01_ET_vs_EN) :- test_it(3,c01).

test(c02_ET_to_EN) :- test_it(1,c02).
test(c02_EN_to_ET) :- test_it(2,c02).
test(c02_ET_vs_EN) :- test_it(3,c02).

test(c03_ET_to_EN) :- test_it(1,c03).
test(c03_EN_to_ET) :- test_it(2,c03).
test(c03_ET_vs_EN) :- test_it(3,c03).

test(c04_ET_to_EN) :- test_it(1,c04).
test(c04_EN_to_ET) :- test_it(2,c04).
test(c04_ET_vs_EN) :- test_it(3,c04).

:- end_tests(dotty).

% ===
% Running tests
% ===

rt(findy)  :- run_tests(findy).
rt(slashy) :- run_tests(slashy).
rt(dotty)  :- run_tests(dotty).

% ===
% Helper to test whether an ETerm only contains atoms, not strings.
% Called from an assertion.
% ===

is_atomical([]).
is_atomical(T) :- atom(T).
is_atomical(T) :- compound(T),compound_name_arguments(T,_Fun,Args),maplist(is_atomical,Args).

% ===
% Parse/Generate an "Entity Name with a dotty Package Name"
%
% entity_name_dotty(?ETerm,?EName).

% ===
% Parse/Generate an "Entity Name with a slashy Package Name" (+ Method Name :-(
%
% parse_slashy(?ETerm,?EName)
% 
% Where ETerm  = Entity Term (Prolog side, a ground term with atoms)
%       ENname = Entity Name (Java side, an atom or string)
%
% Can be called as follows:
%
% parse_slashy(+ETerm,-EName)
% parse_slashy(-ETerm,+EName)
% parse_slashy(+ETerm,+EName)
%
% Note on String vs Atom:
% - - - - - - - - - - - -
% 1) An output ETerm only contains atoms.
%    An output EName is an atom.
% 2) An input ETerm must only contain atoms.
%    An input EName may be an atom or may be an SWI-Prolog string.
% 3) The DCG works on lists of character codes (integers, in SWI Prolog, unicode codepoints)
% 4) UNKOWN: Will this library run on non-SWI systems? Do we have to avoid SWI-Prolog specificities?

parse_slashy(ET,EN) :- parse_by_style(jpl_type_descriptor_1,ET,EN).

parse_findy(ET,EN)  :- parse_by_style(jpl_type_findclassname,ET,EN).

parse_dotty(ET,EN)  :- parse_by_style(jpl_type_classname_1,ET,EN).

% ---

parse_by_style(Style,ET,EN) :- bb(Style),bb(ET),ff(EN),!,
                               assertion(is_atomical(ET)),
                               compound_name_arguments(Callable,Style,[ET]), 
                               phrase(Callable, ENchs),
                               atom_chars(EN,ENchs).

parse_by_style(Style,ET,EN) :- bb(Style),bb(ET),bb(EN),!,
                               assertion(is_atomical(ET)),
                               atom_codes(EN,ENchs),
                               compound_name_arguments(Callable,Style,[ET]), 
                               phrase(Callable, ENchs).
 
parse_by_style(Style,ET,EN) :- bb(Style),ff(ET),bb(EN),!,
                               atom_codes(EN,ENchs),
                               compound_name_arguments(Callable,Style,[ET]), 
                               phrase(Callable, ENchs). 
