% A bunch of unit tests which explains the behaviour of compound_name_arity/3
% https://eu.swi-prolog.org/pldoc/doc_for?object=compound_name_arity/3

:- begin_tests(compound_name_arity).

dict_functor(F) :- compound_name_arguments(_{},F,_).

test("Analyze an empty list",error(type_error(compound,[]))) :-
    compound_name_arity([],_,_).

test("Analyze a one-element list",true([Name,Arity] == ['[|]',2])) :-
    compound_name_arity([a],Name,Arity).

test("Analyze a three-element list", true([Name,Arity] == ['[|]',2])) :-
   compound_name_arity([a,b,c],Name,Arity).

test("Analyze a three-element compound term",true([Name,Arity] == [p,3])) :-
   compound_name_arity(p(1,2,3),Name,Arity).

test("Analyze a three-element compound term with deeper tree structure",true([Name,Arity] == [p,3])) :-
   compound_name_arity(p(q(1),q(2),q(3)),Name,Arity).

test("Analyze a three-element compound term with deeper tree structure that is not ground",true([Name,Arity] == [p,3])) :-
   compound_name_arity(p(q(1),_,q(3)),Name,Arity).
   
test("Analyze SWI-Prolog specific compound term of arity 0",true([Name,Arity] == [p,0])) :-
   compound_name_arity(p(),Name,Arity).

test("Analyze SWI-Prolog specific (named) empty dict", true([Name,Arity] == [F,1])) :-
   compound_name_arity(alpha{},Name,Arity),
   dict_functor(F).

test("Analyze SWI-Prolog specific (named) dict with two pairs", true([Name,Arity] == [F,5])) :-
   compound_name_arity(alpha{x:1,y:2},Name,Arity),
   dict_functor(F).

test("Assemble SWI-Prolog specific compound term of arity 0",true(C == p())) :-
   compound_name_arity(C,p,0).

test("Assemble compound term of arity 3") :-
   compound_name_arity(C,p,3),
   % check structure; it's a compound term of arity 3 with freshvars on the three places
   p(X,Y,Z) = C,
   var(X),var(Y),var(Z).
   
test("Assemble compound term with name the dict functor",blocked("is_dict/1 says this is a dict")) :-
   dict_functor(F),
   compound_name_arity(C,F,5),
   % check structure; it's not a dict; just a compond term of arity 5, but is_dict/1 says otherwise.   
   \+ is_dict(C).   
   
:- end_tests(compound_name_arity).
