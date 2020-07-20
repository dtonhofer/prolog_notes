% Used on this page:
% https://eu.swi-prolog.org/pldoc/man?predicate=put_dict/3

:- begin_tests(put_dotcall).
 
   empty_dict(Tag,Dict)    :- dict_pairs(Dict,Tag,[]).
   nonempty_dict(Tag,Dict) :- dict_pairs(Dict,Tag,[a-x,b-y,c-z]).

   test("Add nothing, using empty dict (OO method call notation)",[true(T)]) :- 
      empty_dict(foo,DictCur),
      DictNext = DictCur.put(_{}),
      T = (DictNext == foo{}).
      
   test("Add nothing, using empty list (OO method call notation)",[true(T)]) :- 
      empty_dict(foo,DictCur),
      DictNext = DictCur.put([]),
      T = (DictNext == foo{}).
            
   test("Add/replace something using 'list of pairs' (OO method call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put([a-2,d-1]),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).

   test("Add/replace something using 'list of : separated pairs' (OO method call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put([a:2,d:1]),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
         
   test("Add/replace something using 'list of = separated_pairs' (OO method call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put([a=2,d=1]),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
      
   test("Add/replace something using 'list of tagged values' (OO method call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put([a(2),d(1)]),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
                  
   test("Add/replace something using 'dict' (OO method call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put(_{a:2,d:1}),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}).
      
   test("Add/replace something using 'dict with different tag' (OO method call notation)",[true(T)]) :- 
      nonempty_dict(foo,DictCur),
      DictNext = DictCur.put(bar{a:2,d:1}),
      T = (DictNext == foo{a:2,b:y,c:z,d:1}). % tag value doesnt change anything

:- end_tests(put_dotcall).

rt(put_dotcall) :- run_tests(put_dotcall).
