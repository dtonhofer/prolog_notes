% ==========
% Demonstrating/Testing SWI-Prolog dicts as described at 
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========

% ===
% Here, we test unification of two dicts
% ===

:- begin_tests(dict_unification).

test("unify two empty dicts") :- 
   foo{} = foo{}.
   
test("unify two nonempty dict which built from the same literal") :-
   foo{a:x,b:y} = foo{a:x,b:y}.
   
test("unify two nonempty dict arbitrarily ordered, but otherwise built from the same literal") :-
   foo{a:x,b:y,c:z} = foo{c:z,b:y,a:x}.

test("unify two anonymous (and otherwise identical) dicts; the tags can unify") :-
   _{a:x,b:y,c:z} = _{a:x,b:y,c:z}.

test("unify two dicts with same key-value pairs; the tags can unify #2") :-
   _{a:x,b:y,c:z} = foo{a:x,b:y,c:z}.

test("unifiy two dicts with unifiable key-value pairs and unifiable tags") :-
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
