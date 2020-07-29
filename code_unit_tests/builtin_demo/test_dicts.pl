% Simple testing of SWI-Prolog dicts

:- begin_tests(dicts).

   % obtaining dicts

   const_dict(xyz, _{x:alpha, y:beta, z:gamma}).
   const_dict(idx, _{id1:v(11), id2:w(22), id3:w(33), id4:v(44), id5:w(55), id6:w(66)}).

   test("get value using valid key",true(Val == alpha)) :-
       const_dict(xyz,Dc),
       get_dict(x,Dc,Val).

   test("get value using nonexistent key",fail)  :- 
      const_dict(xyz,Dc), 
      get_dict(bar,Dc,_).

   test("check value using valid key, against the correct value") :-
      const_dict(xyz,Dc), 
      get_dict(x,Dc,alpha).

   test("check value using valid key, against a bad value", fail) :-
      const_dict(xyz,Dc),
      get_dict(x,Dc,xxxxx).

   test("get all keys",true(SortedBag == [x-alpha,y-beta,z-gamma])) :- 
      const_dict(xyz,Dc), 
      bagof((Key-Val),get_dict(Key,Dc,Val),Bag),
      keysort(Bag,SortedBag).

   test("get selected keys by unifiying with a value",true(SortedBag == [id2-22,id3-33,id5-55,id6-66])) :-
      const_dict(idx,Dc), 
      bagof((Key-Val),get_dict(Key,Dc,w(Val)),Bag), % select ALSO on the form of the value
      keysort(Bag,SortedBag).

:- end_tests(dict).


