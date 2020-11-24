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
      
test("unification success if both keysets are equal and values unify",true([A,B] == [alpha,bravo])) :-
   DictLeft  = foo{x:A,     y:bravo},
   DictRight = foo{x:alpha, y:B    },
   DictLeft = DictRight.

test("unification failure if both keysets are equal but values don't unify",[fail]) :-
   DictLeft  = foo{x:alpha, y:bravo},
   DictRight = foo{x:alpha, y:xxxxx},
   DictLeft = DictRight.
      
test("unification properly works if subterms are dicts (as expected)",true([A,B,C] == [alpha,bravo,charlie])) :-
   DictLeft  = foo{x:alpha, y:sub{ x:B,     y:charlie }},
   DictRight = foo{x:A    , y:sub{ x:bravo, y:C       }},
   DictLeft = DictRight.
      
test("unification failure if there is a keyset difference",[fail]) :-
   DictLeft  = foo{x:alpha, y:bravo, z:charlie},
   DictRight = foo{x:alpha, y:bravo},
   DictLeft = DictRight.
      
:- end_tests(dict_unification).
