% ==========
% Demonstrating/Testing SWI-Prolog dicts as described at 
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========

% ===
% This plunit block tests/demonstrates the .put/1 dotcall.
% ===

:- begin_tests(dict_put_dotcall).
 
% Helper: "Dict" is an empty dict with tag "Tag"

empty_dict(Tag,Dict) :-
   dict_pairs(Dict,Tag,[]).

% Helper: "Dict" is a dict with some content and with tag "Tag"

nonempty_dict(Tag,Dict) :-
   dict_pairs(Dict,Tag,[a-x,b-y,c-z]).
   
test(".put/1 dotcall: add nothing, using empty dict",true(DictNext == foo{})) :- 
   empty_dict(foo,DictCur),
   DictNext = DictCur.put(_{}).
      
test(".put/1 dotcall: add nothing, using empty list",true(DictNext == foo{})) :- 
   empty_dict(foo,DictCur),
   DictNext = DictCur.put([]).
            
test(".put/1 dotcall: add/replace something using 'list of pairs'",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put([a-2,d-1]).

test(".put/1 dotcall: add/replace something using 'list of : separated pairs'",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put([a:2,d:1]).
         
test(".put/1 dotcall: add/replace something using 'list of = separated_pairs'",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put([a=2,d=1]).
      
test(".put/1 dotcall: add/replace something using 'list of tagged values'",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put([a(2),d(1)]).
                  
test(".put/1 dotcall: add/replace something using 'dict'",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put(_{a:2,d:1}).
      
test(".put/1 dotcall: add/replace something using 'dict with different tag'",true(DictNext == foo{a:2,b:y,c:z,d:1})) :- 
   nonempty_dict(foo,DictCur),
   DictNext = DictCur.put(bar{a:2,d:1}). % tag value doesn't change anything

:- end_tests(dict_put_dotcall).
