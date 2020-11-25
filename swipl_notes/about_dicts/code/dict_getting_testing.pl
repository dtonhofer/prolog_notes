% ==========
% Demonstrating/Testing SWI-Prolog dicts as described at 
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========

% ===
% Demonstrating/Testing getting keys and values from a dict using: 
% .Key direct access
% .get/1 dotcall
% get/3 predicate
% ===

:- begin_tests(get_dict).
  
% ---  
% Define a few dictionaries
% ---

empty_dict(_{}).
xyz_dict(_{x:alpha, y:bravo, z:charlie}).
idx_dict(_{id1:v(11), id2:w(22), id3:w(33), id4:v(44), id5:w(55), id6:w(66)}).
twice_200_dict(foo{x:100, y:200, z:300, t:200}).

% ---
% A special predicate for retrieval, it performs a .Key direct access
% All variables can be unbound!
% ---

bagof_call(Value,Dict,Key) :-
   Value = Dict.Key.

% ---
% Tests below here
% ---

test("get_dict/3 to get value for existing key", true(Val == alpha)) :-
   xyz_dict(Dc),
   get_dict(x,Dc,Val).

test(".Key direct access with hardcoded key", true(R == [alpha,bravo,charlie])) :-
   xyz_dict(Dict),
   R = [Dict.x,Dict.y,Dict.z].

test("get_dict/3 to get value for missing key - fails",[fail])  :-
   xyz_dict(Dc), 
   get_dict(bar,Dc,_).

test("get_dict/3 to verify value for existing key (dict value and verification value unify)") :-
   xyz_dict(Dc), 
   get_dict(x,Dc,alpha).

test("get_dict/3 to verify value for existing key (dict value and verification value do not unify)",[fail]) :-
   xyz_dict(Dc),
   get_dict(x,Dc,something_or_other).

test("get_dict/3 inside bagof/3 to get all key-value pairs",true(SortedBag == [x-alpha,y-bravo,z-charlie])) :-
   xyz_dict(Dc),
   bagof((Key-Val),get_dict(Key,Dc,Val),Bag),
   keysort(Bag,SortedBag).

test("get_dict/3 inside of bagof/3 to get selected keys for which the value unifies with a term",true(SortedBag == [id2-22,id3-33,id5-55,id6-66])) :-
   idx_dict(Dc),
   bagof((Key-Val),get_dict(Key,Dc,w(Val)),Bag), % select ALSO on the form of the value
   keysort(Bag,SortedBag).

test(".Key direct access inside of findall/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   findall(Key-Value, (Value = Dict.Key), Bag),
   keysort(Bag,SortedBag).

test(".get/1 dotcall inside of findall/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   findall(Key-Value, (Value = Dict.get(Key)), Bag),
   keysort(Bag,SortedBag).
      
test("dedicated predicate bagof_call/3 inside findall/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   findall(Key-Value, bagof_call(Value,Dict,Key), Bag),
   keysort(Bag,SortedBag).
      
test("get_dict/3 inside findall/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   findall(Key-Value, get_dict(Key,Dict,Value), Bag),
   keysort(Bag,SortedBag).
      
% Using bagof/3 to get all key-value pairs out of a dict
%%% bagof/3 had some problems when an expansion of a dict call is done. Fixed now!
%%% See https://github.com/SWI-Prolog/swipl-devel/issues/637
   
test(".Key direct access inside bagof/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   bagof(Key-Value, (Value = Dict.Key), Bag),
   keysort(Bag,SortedBag).

test(".get/1 dotcall inside bagof/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   bagof(Key-Value, Value = Dict.get(Key), Bag),
   keysort(Bag,SortedBag).
      
test("dedicated predicate inside bagof/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   bagof(Key-Value, bagof_call(Value,Dict,Key), Bag),
   keysort(Bag,SortedBag).
      
test("get_dict/3 inside bagof/3  to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   bagof(Key-Value, get_dict(Key,Dict,Value), Bag),
   keysort(Bag,SortedBag).
   
test(".Key direct access to get keys via a matching value inside findall/3", true(SortedBag == [t,y])) :-
   twice_200_dict(Dict),
   findall(Key, Dict.Key == 200, Bag), % do not unify =, just check ==
   sort(Bag,SortedBag).

test(".get/1 dotcall to get keys via a matching value inside findall/3", true(SortedBag == [t,y])) :-
   twice_200_dict(Dict),
   findall(Key, Dict.get(Key) == 200, Bag), % do not unify =, just check ==
   sort(Bag,SortedBag).

test("get_dict/3 to get keys via a matching value inside findall/3", true(SortedBag == [t,y])) :-
   twice_200_dict(Dict),
   findall(Key, (get_dict(Key,Dict,Value), Value == 200), Bag), % do not unify =, just check ==
   sort(Bag,SortedBag).
      
test(".Key direct access for missing key entry access means exception",[error(existence_error(_,_,_),_)]) :-
   empty_dict(Dict),
   (_ = Dict.x).

test(".get/1 dotcall for missing key entry on empty dict access means failure",[fail]) :-
   empty_dict(Dict),
   (_ = Dict.get(x)).

test(".Key direct access for unbound key entry on empty dict access means failure (instead of exception)",[fail]) :-
   empty_dict(Dict),
   (_ = Dict._Key).

test(".get/1 dotcall for unbound key entry on empty dict access means failure",[fail]) :-
   empty_dict(Dict),
   (_ = Dict.get(_Key)).

:- end_tests(get_dict).
