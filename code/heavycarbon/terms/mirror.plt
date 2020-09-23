:- use_module(library('heavycarbon/terms/mirror.pl')).

:- debug(mirror).

:- begin_tests(mirror).

test("mirror a variable X", true(Mirror = hole(X,Id))) :- 
   mirror(X,Id,acc(md{},MD)),
   debug(mirror,"Mirrored variable ~q; entry in dict has key ~q, lookup dict is ~q",[X,Id,MD]),
   get_dict(Id,MD,Mirror),
   assertion(nonvar(Mirror)).

test("mirror an atomic", true(Mirror = atomic(Id,V))) :-
   V=some_atom,
   mirror(V,Id,acc(md{},MD)),
   debug(mirror,"Mirrored atomic ~q; entry in dict has key ~q, lookup dict is ~q",[V,Id,MD]),
   get_dict(Id,MD,Mirror),
   assertion(nonvar(Mirror)).

test("mirror an empty dict", true(Mirror = dict(Id,muh_dict,[],[],_Meta))) :-
   V=muh_dict{},
   mirror(V,Id,acc(md{},MD)),
   debug(mirror,"Mirrored dict ~q; entry in dict has key ~q, lookup dict is ~q",[V,Id,MD]),
   get_dict(Id,MD,Mirror),
   assertion(nonvar(Mirror)).

test("mirror an nonempty dict", true(Mirror = dict(Id,muh_dict,[_,_,_],[_,_,_],_Meta))) :-
   V=muh_dict{a:1,b:2,c:3},
   mirror(V,Id,acc(md{},MD)),
   debug(mirror,"Mirrored dict ~q; entry in dict has key ~q, lookup dict is ~q",[V,Id,MD]),
   get_dict(Id,MD,Mirror),
   assertion(nonvar(Mirror)).

test("mirror a compound with arity 0", true(Mirror = compound(Id,foo_compound,[]))) :-
   V=foo_compound(),
   mirror(V,Id,acc(md{},MD)),
   debug(mirror,"Mirrored compound ~q; entry in dict has key ~q, lookup dict is ~q",[V,Id,MD]),
   get_dict(Id,MD,Mirror),
   assertion(nonvar(Mirror)).

test("mirror a compound with arity 2", true(Mirror = compound(Id,foo_compound,[_,_]))) :-
   V=foo_compound(a,b),
   mirror(V,Id,acc(md{},MD)),
   debug(mirror,"Mirrored compound ~q; entry in dict has key ~q, lookup dict is ~q",[V,Id,MD]),
   get_dict(Id,MD,Mirror),
   assertion(nonvar(Mirror)).




:- end_tests(mirror).

/*

backforth(NodeIn,NodeOut) :- 
   mirror(NodeIn,MirrorDict,Id),
   debug(mirror,"~q has been mirrored to ~q : ~q",[NodeIn,Id,MirrorDict]),
   unmirror(Id,MirrorDict,NodeOut).

test("variable", true(O == X))            :- backforth(X,O).
test("atomic",   true(O == foo))          :- backforth(foo,O).
test("compound", true(O == foo(a,X,c)))   :- backforth(foo(a,X,c),O).
test("dict",     true(O == tag{x:1,y:2})) :- backforth(tag{x:1,y:2},O).
test("list",     true(O == [1,2,3,4,5]))  :- backforth([1,2,3,4,5],O).

*/
