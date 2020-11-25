% ==========
% Demonstrating/Testing SWI-Prolog dicts as described at 
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========

% ===
% Here, we demonstrate the selection operator ':<'
% ===

:- begin_tests(dict_selection).

test("After selection, unbound-variable tags are 'the same'",true(A == B)) :-
   DictLeft  = A{},
   DictRight = B{},
   DictLeft :< DictRight.

test("After selection, unbound-variable tag on the left has been instantiated",true(A == foo)) :-
   DictLeft  = A{},
   DictRight = foo{},
   DictLeft :< DictRight.

test("After selection, unbound-variable tag on the right has been instantiated",true(B == foo)) :-
   DictLeft  = foo{},
   DictRight = B{},
   DictLeft :< DictRight.

test("Selection: Unify common values",true([A,C,K1] == [alpha,charlie,K2])) :-
   DictLeft  = foo{x:alpha, y:bravo, z:C,       k:K1},
   DictRight = foo{x:A,     y:bravo, z:charlie, k:K2},
   DictLeft :< DictRight.

test("Selection: Properly works if values are dicts and they unify (as expected)",true([A,B,C] == [alpha,bravo,charlie])) :-
   DictLeft  = foo{x:alpha, y:subdict{ xx:B,     yy:charlie }            },
   DictRight = foo{x:A    , y:subdict{ xx:bravo, yy:C       }, z:foxtrott},
   DictLeft :< DictRight.

% The following is normal because values are unified, not "selected" (selection
% only makes sense for dicts, whereas unification makes sense for any value)
% If projection should be applied to subdicts (and subsubdicts etc.) a special
% predicate needs to be written for that.

test("Selection: Failure if subterms are dicts and they don't unify but would 'select'",fail) :-
   DictLeft  = foo{x:alpha, y:subdict{ xx:_,     yy:charlie               }            },
   DictRight = foo{x:_    , y:subdict{ xx:bravo, yy:_       , zz:foxtrott }, z:foxtrott},
   DictLeft :< DictRight.

test("Selection: Success if left-dict keyset is smaller or equal than right-dict keyset",true([A,B] == [alpha,bravo])) :-
   DictLeft  = foo{x:A,     y:B },
   DictRight = foo{x:alpha, y:bravo, z:charlie },
   DictLeft :< DictRight.

test("Selection: Failure if left-dict keyset is larger than right-dict keyset",fail) :-
   DictLeft  = foo{x:_A,    y:bravo, z:charlie, k:foxtrott},
   DictRight = foo{x:alpha, y:bravo, z:_C},
   DictLeft :< DictRight.

:- end_tests(dict_selection).
