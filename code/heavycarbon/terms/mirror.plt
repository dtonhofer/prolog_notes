:- use_module(library('heavycarbon/terms/mirror.pl')).

:- debug(mirror).

:- begin_tests(mirror).

backforth(NodeIn,NodeOut) :- 
   mirror(NodeIn,MirrorDict,Id),
   debug(mirror,"~q has been mirrored to ~q : ~q",[NodeIn,Id,MirrorDict]),
   unmirror(Id,MirrorDict,NodeOut).

test("variable", true(O == X))            :- backforth(X,O).
test("atomic",   true(O == foo))          :- backforth(foo,O).
test("compound", true(O == foo(a,X,c)))   :- backforth(foo(a,X,c),O).
test("dict",     true(O == tag{x:1,y:2})) :- backforth(tag{x:1,y:2},O).
test("list",     true(O == [1,2,3,4,5]))  :- backforth([1,2,3,4,5],O).

:- end_tests(mirror).
