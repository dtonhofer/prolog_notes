:- begin_tests(compound_name_arguments).

dict_functor(F) :- compound_name_arguments(_{},F,_).

test("Disassemble an empty list", error(type_error(compound,[]))) :-
   compound_name_arguments([],_,_).

test("Disassemble a one-element list", true(T)) :-
   compound_name_arguments([a],Functor,Args),
   T = (Functor-Args == '[|]'-[a,[]]).

test("Disassemble a three-element list", true(T)) :-
   compound_name_arguments([a,b,c],Functor,Args),
   T = (Functor-Args == '[|]'-[a,[b,c]]).

test("Disassemble SWI-Prolog specific compound term of arity 0", true(T)) :-
   compound_name_arguments(a(),Functor,Args),
   T = (Functor-Args == a-[]).

test("Disassemble SWI-Prolog specific (named) empty dict", true(T)) :-
   compound_name_arguments(alpha{},Functor,Args),
   dict_functor(F),
   T = (Functor-Args == F-[alpha]).

test("Disassemble SWI-Prolog specific (named) dict with two pairs", true(T)) :-
   compound_name_arguments(alpha{x:1,y:2},Functor,Args),
   dict_functor(F),
   T = (Functor-Args == F-[alpha, 1, x, 2, y]).

test("Disassemble SWI-Prolog specific (anonymous) dict with two pairs", true(T)) :-
   compound_name_arguments(_{x:1,y:2},Functor,Args),
   dict_functor(F),
   Args = [Tag|Payload],var(Tag),
   T = (Functor-Payload == F-[1, x, 2, y]).

:- end_tests(compound_name_arguments).
