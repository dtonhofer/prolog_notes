:- begin_tests(put_dict).

   % Ancillary "==" tests

   test(term_equality_empty_dict)             :- foo{}            == foo{}.
   test(term_equality_nonempty_dict)          :- foo{a:x,b:y}     == foo{a:x,b:y}.
   test(term_equality_nonempty_dict_reorderd) :- foo{a:x,b:y,c:z} == foo{c:z,b:y,a:x}.
 
   empty_dict(Tag,Dict)    :- dict_pairs(Dict,Tag,[]).
   nonempty_dict(Tag,Dict) :- dict_pairs(Dict,Tag,[a-x,b-y,c-z]).

   test(add_nothing_using_dict,[true(DcOut == foo{})]) :- 
      empty_dict(foo,Dc),
      put_dict(_{},Dc,DcOut).

   test(add_nothing_using_list,[true(DcOut == foo{})]) :- 
      empty_dict(foo,Dc),
      put_dict([],Dc,DcOut).

   test(addreplace_something_using_list_of_pairs,[true(DcOut == foo{a:2,b:y,c:z,d:1})]) :- 
      nonempty_dict(foo,Dc),
      put_dict([a-2,d-1],Dc,DcOut).

   test(addreplace_something_using_list_of_colon_separated_pairs,[true(DcOut == foo{a:2,b:y,c:z,d:1})]) :- 
      nonempty_dict(foo,Dc),
      put_dict([a:2,d:1],Dc,DcOut).

   test(addreplace_something_using_list_of_equal_separated_pairs,[true(DcOut == foo{a:2,b:y,c:z,d:1})]) :- 
      nonempty_dict(foo,Dc),
      put_dict([a=2,d=1],Dc,DcOut).

   test(addreplace_something_using_list_of_tagged_values,[true(DcOut == foo{a:2,b:y,c:z,d:1})]) :- 
      nonempty_dict(foo,Dc),
      put_dict([a(2),d(1)],Dc,DcOut).

   test(addreplace_something_using_dict,[true(DcOut == foo{a:2,b:y,c:z,d:1})]) :- 
      nonempty_dict(foo,Dc),
      put_dict(_{a:2,d:1},Dc,DcOut).

   test(addreplace_something_using_differently_named_dict,[true(DcOut == foo{a:2,b:y,c:z,d:1})]) :- 
      nonempty_dict(foo,Dc),
      put_dict(bar{a:2,d:1},Dc,DcOut).

:- end_tests(put_dict).

rt(put_dict) :- run_tests(put_dict).
