:- begin_tests(dict).

   xyz_dict(_{x:alpha, y:beta, z:gamma}).

   idx_dict(_{id1:v(11), id2:w(22), id3:w(33), id4:v(44), id5:w(55), id6:w(66)}).

   test(get_valid_key,[true(Val == alpha)]) :-
       xyz_dict(Dc), get_dict(x,Dc,Val).

   test(get_missing_key,[fail])  :- 
      xyz_dict(Dc), get_dict(bar,Dc,_).

   test(check_key_ok, [true]) :-
      xyz_dict(Dc), get_dict(x,Dc,alpha).

   test(check_key_notok, [fail]) :-
      xyz_dict(Dc), get_dict(x,Dc,ahpla).

   test(get_all_keys,[true(SortedBag == [x-alpha,y-beta,z-gamma])]) :- 
      xyz_dict(Dc), 
      bagof((Key-Val),get_dict(Key,Dc,Val),Bag),
      keysort(Bag,SortedBag).

   test(get_selected_keys,[true(SortedBag == [id2-22,id3-33,id5-55,id6-66])]) :-
      idx_dict(Dc), 
      bagof((Key-Val),get_dict(Key,Dc,w(Val)),Bag), % select ALSO on the form of the value
      keysort(Bag,SortedBag).

:- end_tests(dict).

rt(dict) :- run_tests(dict).

