% ==========
% Demonstrating/Testing SWI-Prolog dicts as described at
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========

% ===
% Demonstrating/Testing getting keys and values from a dict using:
% .Key   "direct access" syntax
% .get/1 "dotcall" (not the offical name for this but it sounds good)
% get/3   predicate
% ===

:- begin_tests(get_dict).

% ---
% Define a few dictionaries
% ---

empty_dict(_{}).
one_entry_dict(_{x:one}).
xyz_dict(_{x:alpha, y:bravo, z:charlie}).
idx_dict(_{id1:v(11), id2:w(22), id3:w(33), id4:v(44), id5:w(55), id6:w(66)}).
twice_200_dict(foo{x:100, y:200, z:300, t:200}).

% ---
% A special predicate for retrieval, it performs a .Key direct access.
% All variables can be unbound!
% ---

bagof_call(Value,Dict,Key) :-
   Value = Dict.Key.

% ---
% get_dict/3 basics
% ---

test("get_dict/3 to get value for existing key", true(Val == alpha)) :-
   xyz_dict(Dict),
   get_dict(x,Dict,Val).

test("get_dict/3 to get value for missing key - fails", [fail])  :-
   xyz_dict(Dict),
   get_dict(bar,Dict,_).

test("get_dict/3 to verify value for existing key (dict value and verification value unify)") :-
   xyz_dict(Dict),
   get_dict(x,Dict,alpha).

test("get_dict/3 to verify value for existing key (dict value and verification value do not unify)",[fail]) :-
   xyz_dict(Dict),
   get_dict(x,Dict,something_or_other).

% ---
% 'direct access' basics
% ---

test(".Key 'direct access' with hardcoded key where the entry exists", true(R == alpha)) :-
   xyz_dict(Dict),
   R = Dict.x.

% the same as above, really:

test(".Key 'direct access' with key as variable, but bound at call time, where the entry exists", true(R == one)) :-
   one_entry_dict(Dict),
   Key = x,
   R = Dict.Key.

test(".Key 'direct access' with unbound key binds the key to a en existing key value", true([R,Key] == [one,x])) :-
   one_entry_dict(Dict),
   R = Dict.Key.

test(".Key 'direct access' with hardcoded key with that entry missing raises exception (case of nonempty dict)", [error(existence_error(_,_,_),_)]) :-
   one_entry_dict(Dict),
   _ = Dict.v.

test(".Key 'direct access' with hardcoded key with that entry missing raises exception (case of empty dict)", [error(existence_error(_,_,_),_)]) :-
   empty_dict(Dict),
   _ = Dict.v.

% this is a bit astonishing:

test(".Key 'direct access' with unbound key and with an empty dict means failure instead of exception",[fail]) :-
   empty_dict(Dict),
   (_ = Dict._Key).

% ---
% .get/1 special cases
% ---

test(".get/1 dotcall with hardcoded key with entry missing means failure (case of empty dict)",[fail]) :-
   empty_dict(Dict),
   (_ = Dict.get(x)).

test(".get/1 dotcall with unbound key with empty dict means failure",[fail]) :-
   empty_dict(Dict),
   (_ = Dict.get(_Key)).

% ---
% getting all key-value pairs using findall
% ---

test(".Key 'direct access' inside of findall/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   findall(Key-Value, (Value = Dict.Key), Bag),
   keysort(Bag,SortedBag).

test(".get/1 'dotcall' inside of findall/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
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

% ---
% Getting key-value pairs with an additional constraint on the values
% ---

test(".Key 'direct access' to get keys via a matching value inside findall/3", true(SortedBag == [t,y])) :-
   twice_200_dict(Dict),
   findall(Key, Dict.Key == 200, Bag), % do not unify =, just check ==
   sort(Bag,SortedBag).

test(".get/1 'dotcall' to get keys via a matching value inside findall/3", true(SortedBag == [t,y])) :-
   twice_200_dict(Dict),
   findall(Key, Dict.get(Key) == 200, Bag), % do not unify =, just check ==
   sort(Bag,SortedBag).

test("get_dict/3 to get keys via a matching value inside findall/3", true(SortedBag == [t,y])) :-
   twice_200_dict(Dict),
   findall(Key, (get_dict(Key,Dict,Value), Value == 200), Bag), % do not unify =, just check ==
   sort(Bag,SortedBag).

% ---
% Getting all key-value pairs using bagof
% ---

%%% bagof/3 had some problems when an expansion of a dict call is done. Fixed now!
%%% See https://github.com/SWI-Prolog/swipl-devel/issues/637

test(".Key 'direct access' inside bagof/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   bagof(Key-Value, (Value = Dict.Key), Bag),
   keysort(Bag,SortedBag).

test(".get/1 'dotcall' inside bagof/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   bagof(Key-Value, Value = Dict.get(Key), Bag),
   keysort(Bag,SortedBag).

test("dedicated predicate inside bagof/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   bagof(Key-Value, bagof_call(Value,Dict,Key), Bag),
   keysort(Bag,SortedBag).

test("get_dict/3 inside bagof/3 to get all key-value pairs", true(SortedBag == [x-alpha, y-bravo, z-charlie])) :-
   xyz_dict(Dict),
   bagof(Key-Value, get_dict(Key,Dict,Value), Bag),
   keysort(Bag,SortedBag).

test("get_dict/3 inside of bagof/3 to get selected keys for which the value unifies with a term",true(SortedBag == [id2-22,id3-33,id5-55,id6-66])) :-
   idx_dict(Dict),
   bagof((Key-Val), get_dict(Key,Dict,w(Val)),Bag), % select ALSO on the form of the value: we want those tagged with w/1
   keysort(Bag,SortedBag).

:- end_tests(get_dict).

