:- begin_tests(dict_put_dotcall).
 
% "Dict" is an empty dict with tag "Tag"

empty_dict(Tag,Dict) :-
   dict_pairs(Dict,Tag,[]).

% "Dict" is a dict with some content and with tag "Tag"

nonempty_dict(Tag,Dict) :-
   dict_pairs(Dict,Tag,[a-x,b-y,c-z]).
   
test("Add nothing, using empty dict (dotcall)",true(DictNext == foo{})) :- 
   empty_dict(foo,DictCur),
   DictNext = DictCur.put(_{}).
      
test("Add nothing, using empty list (dotcall)",true(DictNext == foo{})) :- 
   empty_dict(foo,DictCur),
   DictNext = DictCur.put([]).
            
test("Add/replace something using 'list of pairs' (dotcall)",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put([a-2,d-1]).

test("Add/replace something using 'list of : separated pairs' (dotcall)",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put([a:2,d:1]).
         
test("Add/replace something using 'list of = separated_pairs' (dotcall)",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put([a=2,d=1]).
      
test("Add/replace something using 'list of tagged values' (dotcall)",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put([a(2),d(1)]).
                  
test("Add/replace something using 'dict' (dotcall)",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put(_{a:2,d:1}).
      
test("Add/replace something using 'dict with different tag' (OO method call notation)",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put(bar{a:2,d:1}). % tag value doesnt change anything

:- end_tests(dict_put_dotcall).
