% ==========
% Testing SWI-Prolog dicts as described at 
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========

% ===
% This plunit block tests/demonstrates "equality" between dicts using "==". 
% ===

:- begin_tests(dict_equality).

% Check for "equality" using "==" between keys and values, but disregarding the tag.

dict_equality_sans_tag(D1,D2) :-
   ((var(D1);is_dict(D1)) -> true ; type_error("dict or var",D1)),
   ((var(D2);is_dict(D2)) -> true ; type_error("dict or var",D2)),   
   (nonvar(D1),nonvar(D2)) 
   -> 
   (assertion((is_dict(D1),is_dict(D2))),
    dict_pairs(D1,_Tag1,Pairs1), % Pairs1 will be ordered by natural order of keys
    dict_pairs(D2,_Tag2,Pairs2), % Pairs2 will be ordered by natural order of keys
    Pairs1 == Pairs2).

test("empty dict") :- 
   foo{} == foo{}.
   
test("nonempty dict") :-
   foo{a:x,b:y} == foo{a:x,b:y}.
   
test("nonempty dict arbitrarily ordered") :-
   foo{a:x,b:y,c:z} == foo{c:z,b:y,a:x}.

test("same dict pairs, both tags being the same unbound variable") :-
   X{a:x,b:y,c:z} == X{a:x,b:y,c:z}.

test("dict tag the same unbound variables, pairs with values the same unbound variables") :-
   A{a:X,b:Y,c:Z} == A{a:X,b:Y,c:Z}.
   
test("same dict pairs, different tags") :-
   foo{a:x,b:y,c:z} \== bar{a:x,b:y,c:z}.

test("same dict, one tag an unbound variable") :-
   foo{a:x,b:y,c:z} \== _{a:x,b:y,c:z}.
   
test("different key sets") :-
   foo{a:x,b:y,c:z} \== foo{b:y,c:z,d:q}.

test("different values") :-
   foo{a:x,b:y,c:z} \== foo{a:x,b:y,c:zz}.
   
test("dict equality disregarding the tag (same tag)") :-
   dict_equality_sans_tag(foo{a:x,b:y,c:z},foo{a:x,b:y,c:z}).
   
test("dict equality disregarding the tag (different tag)") :-
   dict_equality_sans_tag(foo{a:x,b:y,c:z},bar{a:x,b:y,c:z}).
      
test("dict equality disregarding the tag (different content #1)", fail) :-
   dict_equality_sans_tag(foo{a:x,b:y,c:z},bar{a:x,b:y,c:zz}).

test("dict equality disregarding the tag (different content #2)", fail) :-
   dict_equality_sans_tag(foo{a:x,b:y,c:z},bar{a:x,c:y,d:z}).
   
:- end_tests(dict_equality).
