:- begin_tests(dict_unification).

test("empty dict") :- 
   foo{} = foo{}.
   
test("nonempty dict") :-
   foo{a:x,b:y} = foo{a:x,b:y}.
   
test("nonempty dict arbitrarily ordered") :-
   foo{a:x,b:y,c:z} = foo{c:z,b:y,a:x}.

test("same dict pairs, tags can unify #1") :-
   _{a:x,b:y,c:z} = _{a:x,b:y,c:z}.

test("same dict pairs, tags can unify #2") :-
   _{a:x,b:y,c:z} = foo{a:x,b:y,c:z}.

test("unifiable, tags and dict values can unify #2") :-
   _{a:_X,b:y,c:_Z,k:K} = foo{a:x,b:y,c:z,k:K}.
      
:- end_tests(dict_unification).
