*** 
*** At https://eu.swi-prolog.org/pldoc/doc_for?object=(%3A%3C)/2
*** 

## Naming

IMHO, "select" is not the appropriate name here, the more so because "select" is already used to describe the "successfully pick exactly 1 element out of a list" predicate select/3, which should be called "pick/3"...

The name "dict projection" sounds appropriate for this operation 

Then the left dict could be called "small dict" and the right dict "big dict" (ok, where's my coat...)

## Unit tests for dict unification

==
:- begin_tests(dict_unification).

   test("Unification: Success if both keysets are equal and values unify",[true(T)]) :-
      DictLeft  = foo{x:A,     y:bravo},
      DictRight = foo{x:alpha, y:B    },
      DictLeft = DictRight,
      T = ([A,B] == [alpha,bravo]).

   test("Unification: Failure if both keysets are equal but values don't unify",[fail]) :-
      DictLeft  = foo{x:alpha, y:bravo},
      DictRight = foo{x:alpha, y:xxxxx},
      DictLeft = DictRight.
      
   test("Unification: Properly works if subterms are dicts (as expected)",[true(T)]) :-
      DictLeft  = foo{x:alpha, y:sub{ x:B,     y:charlie }},
      DictRight = foo{x:A    , y:sub{ x:bravo, y:C       }},
      DictLeft = DictRight,
      T = ([A,B,C] == [alpha,bravo,charlie]).
      
   test("Unification: Failure if there is a keyset difference",[fail]) :-
      DictLeft  = foo{x:alpha, y:bravo, z:charlie},
      DictRight = foo{x:alpha, y:bravo},
      DictLeft = DictRight.

:- end_tests(dict_unification).

rt(dict_unification) :- run_tests(dict_unification).
==

## Unit tests for "dict projection" :< 

==
:- begin_tests(dict_projection).

   test("After projection, freshvar tags are 'equal terms'",[true(T)]) :-
      DictLeft  = A{},
      DictRight = B{},
      DictLeft :< DictRight,
      T = (A == B).

   test("After projection, freshvar tag has been instantiated (left)",[true(T)]) :-
      DictLeft  = A{},
      DictRight = foo{},
      DictLeft :< DictRight,
      T = (A == foo).

   test("After projection, freshvar tag has been instantiated (right)",[true(T)]) :-
      DictLeft  = foo{},
      DictRight = B{},
      DictLeft :< DictRight,
      T = (B == foo).

   test("Projection: Unify common values",[true(T)]) :-
      DictLeft  = foo{x:alpha, y:bravo, z:C,       k:K1},
      DictRight = foo{x:A,     y:bravo, z:charlie, k:K2},
      DictLeft :< DictRight,
      T = ([A,C,K1] == [alpha,charlie,K2]).

   test("Projection: Properly works if values are dicts and they unify (as expected)",[true(T)]) :-
      DictLeft  = foo{x:alpha, y:subdict{ xx:B,     yy:charlie }            },
      DictRight = foo{x:A    , y:subdict{ xx:bravo, yy:C       }, z:foxtrott},
      DictLeft :< DictRight,
      T = ([A,B,C] == [alpha,bravo,charlie]).

   % The following is normal because values are unified, not projected (projection
   % only makes sense for dicts, whereas unification makes sense for any value)
   % If projection should be applied to subdicts (and subsubdicts etc.) a special
   % predicate needs to be written for that.
   
   test("Projection: Fails if subterms are dicts and they don't unify but would project",[fail]) :-
      DictLeft  = foo{x:alpha, y:subdict{ xx:_,     yy:charlie               }            },
      DictRight = foo{x:_    , y:subdict{ xx:bravo, yy:_       , zz:foxtrott }, z:foxtrott},
      DictLeft :< DictRight.
      
   test("Projection: Success if left-dict keyset is smaller or equal than right-dict keyset",[true(T)]) :-
      DictLeft  = foo{x:A,     y:B },
      DictRight = foo{x:alpha, y:bravo, z:charlie },
      DictLeft :< DictRight,
      T = ([A,B] == [alpha,bravo]).
      
   test("Projection: Failure if left-dict keyset is larger than right-dict keyset",[fail]) :-
      DictLeft  = foo{x:_A,    y:bravo, z:charlie, k:foxtrott},
      DictRight = foo{x:alpha, y:bravo, z:_C},
      DictLeft :< DictRight.
     
:- end_tests(dict_projection).

rt(dict_projection) :- run_tests(dict_projection).
==

## A simple application

Default parameters which may be overriden with a dict.

Picking a value from a possibly large dict full of various parameters, and falling back to a default if it doesn't exist:

==
pick_encoding(Dict,Value)        :- _{encoding:Value} :< Dict,!.
pick_encoding(Dict,'iso-8859-1') :- is_dict(Dict).

:- begin_tests(default_values).
   
   test("Use default encoding",      [true(Enc == 'iso-8859-1')] ) :- pick_encoding(_{}                         , Enc).
   test("Override default encoding", [true(Enc == 'utf-8')] )      :- pick_encoding(_{foo:bar, encoding:'utf-8'}, Enc).
        
:- end_tests(default_values).

rt(default_values) :- run_tests(default_values).
==
