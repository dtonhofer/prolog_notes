% Used on this page:
% https://eu.swi-prolog.org/pldoc/man?predicate=put_dict/3

:- begin_tests(put_dict).
 
   empty_dict(Tag,Dict)    :- dict_pairs(Dict,Tag,[]).
   nonempty_dict(Tag,Dict) :- dict_pairs(Dict,Tag,[a-x,b-y,c-z]).

   test("Add nothing, using empty dict (predicate call notation)",[true(T)]) :- 
      empty_dict(foo,DictCur),
      put_dict(_{},DictCur,DictNext),
      T = (DictNext == foo{}).

   test("Add nothing, using empty dict (OO function call notation)",[true(T)]) :- 
      empty_dict(foo,DictCur),
      DictNext = DictCur.put(_{}),
      T = (DictNext == foo{}).
      
   test("Add nothing, using empty list (predicate call notation)",[true(T)]) :- 
      empty_dict(foo,DictCur),
      put_dict([],DictCur,DictNext),
      T = (DictNext == foo{}).

   test("Add nothing, using empty list (OO function call notation)",[true(T)]) :- 
      empty_dict(foo,DictCur),
      DictNext = DictCur.put([]),
      T = (DictNext == foo{}).
            
   test("Add/replace something using list of pairs (predicate call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      put_dict([a-2,d-1],DictCur,DictNext),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).

   test("Add/replace something using list of pairs (OO function call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put([a-2,d-1]),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
      
   test("Add/replace something using list of colon-separated pairs (predicate call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      put_dict([a:2,d:1],DictCur,DictNext),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).

   test("Add/replace something using list of colon-separated pairs (OO function call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put([a:2,d:1]),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
           
   test("Add/replace something using list of '='-separated_pairs (predicate call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      put_dict([a=2,d=1],DictCur,DictNext),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).

   test("Add/replace something using list of '='-separated_pairs (OO function call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put([a=2,d=1]),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
      
   test("Add/replace something using list of tagged values (predicate call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      put_dict([a(2),d(1)],DictCur,DictNext),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).

   test("Add/replace something using list of tagged values (OO function call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put([a(2),d(1)]),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
            
   test("Add/replace something using dict (predicate call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      put_dict(_{a:2,d:1},DictCur,DictNext),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
      
   test("Add/replace something using dict (OO function call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put(_{a:2,d:1}),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
      
   test("Add/replace something using dict with different tag (predicate call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      put_dict(bar{a:2,d:1},DictCur,DictNext),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}). % tag value doesn't change anything

   test("Add/replace something using dict with different tag (OO function call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put(bar{a:2,d:1}),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}). % tag value doesn't change anything

:- end_tests(put_dict).

rt(put_dict) :- run_tests(put_dict).
