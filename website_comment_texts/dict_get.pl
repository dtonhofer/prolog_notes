:- begin_tests(dict_get).

   empty_dict(foo{}).
   nonempty_dict(foo{x:alpha, y:bravo, z:charlie}).
   multival_dict(foo{x:100, y:200, z:300, t:200}).
   bagof_call(Value,Dict,Key) :- (Value = Dict.Key).

   result_check(Bag,T) :-
      keysort(Bag,SortedBag),
      T = (SortedBag == [x-alpha, y-bravo, z-charlie]).

   result_check_keys(Bag,T) :-
      sort(Bag,SortedBag),
      T = (SortedBag == [t,y]).

   test(".Key direct access with hardcoded key", [true(T)]) :-
      nonempty_dict(Dict),
      T = ([Dict.x,Dict.y,Dict.z] == [alpha,bravo,charlie]).

   % Using bagof/3 to get all key-value pairs out of a dict
   %%% bagof/3 has currently some problems when an expansion of a dict call
   %%% is done. See https://github.com/SWI-Prolog/swipl-devel/issues/637
   %%% (This should be fixed in latest release)

   % FAILS for 8.1.32-10, should be fixed in latest release
   test("bagof/3 using .Key direct access", [true(T)]) :-
      nonempty_dict(Dict),
      bagof(Key-Value, (Value = Dict.Key), Bag),
      result_check(Bag,T).

   % FAILS for 8.1.32-10, should be fixed in latest release
   test("bagof/3 using .get/1", [true(T)]) :-
      nonempty_dict(Dict),
      bagof(Key-Value, Value = Dict.get(Key), Bag),
      result_check(Bag,T).

   test("bagof/3 using dedicated predicate", [true(T)]) :-
      nonempty_dict(Dict),
      bagof(Key-Value, bagof_call(Value,Dict,Key), Bag),
      result_check(Bag,T).

   test("bagof/3 using get_dict/3", [true(T)]) :-
      nonempty_dict(Dict),
      bagof(Key-Value, get_dict(Key,Dict,Value), Bag),
      result_check(Bag,T).

   test("findall/3 using .Key direct access", [true(T)]) :-
      nonempty_dict(Dict),
      findall(Key-Value, (Value = Dict.Key), Bag),
      result_check(Bag,T).

   test("findall/3 using .get/1", [true(T)]) :-
      nonempty_dict(Dict),
      findall(Key-Value, Value = Dict.get(Key), Bag),
      result_check(Bag,T).

   test("findall/3 using dedicated predicate", [true(T)]) :-
      nonempty_dict(Dict),
      findall(Key-Value, bagof_call(Value,Dict,Key), Bag),
      result_check(Bag,T).

   test("findall/3 using get_dict/3", [true(T)]) :-
      nonempty_dict(Dict),
      findall(Key-Value, get_dict(Key,Dict,Value), Bag),
      result_check(Bag,T).

   test("getting keys by their associated value, direct access", [true(T)]) :-
      multival_dict(Dict),
      findall(Key, Dict.Key == 200, Bag), % do not unify =, just check ==
      result_check_keys(Bag,T).

   test("getting keys by their associated value using .get/1", [true(T)]) :-
      multival_dict(Dict),
      findall(Key, Dict.get(Key) == 200, Bag), % do not unify =, just check ==
      result_check_keys(Bag,T).

   test("getting keys by their associated value using get_dict/3", [true(T)]) :-
      multival_dict(Dict),
      findall(Key, (get_dict(Key,Dict,Value), Value == 200), Bag), % do not unify =, just check ==
      result_check_keys(Bag,T).

   test("missing key using direct access means exception",[error(existence_error(_,_,_))]) :-
      empty_dict(Dict),
      (_ = Dict.x).

   test("missing key using .get/1 access means failure",fail) :-
      empty_dict(Dict),
      Dict.get(x).

:- end_tests(dict_get).

rt(dict_get) :- run_tests(dict_get).
