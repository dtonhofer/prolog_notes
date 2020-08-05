:- use_module(library('heavycarbon/utils/clashfree_id_selection.pl')).

:- begin_tests(clashfree_id_selection).

test("no id found",error(too_many_attempts,context(_,_))) :-
   Dict0=_{},MaxId=1,
   clashfree_id_selection(Dict0,Id0,MaxId),
   put_dict(Id0,Dict0,x,Dict1),
   clashfree_id_selection(Dict1,Id1,MaxId),
   put_dict(Id1,Dict1,x,Dict2),
   clashfree_id_selection(Dict2,_,MaxId). % will not find another id

test("no id found 2",error(too_many_attempts,context(_,_))) :-
   Dict0=_{},MaxId=1,
   insert_with_clashfree_id(Dict0,x,Dict1,_Id0,MaxId),
   insert_with_clashfree_id(Dict1,y,Dict2,_Id1,MaxId),
   insert_with_clashfree_id(Dict2,z,_,_,MaxId).  % will not find another id 
   
test("id found", true(Dict2 = _{0:x,1:x})) :-
   Dict0=_{},MaxId=1,
   clashfree_id_selection(Dict0,Id0,MaxId),   % get a random id from [0..MaxId] not yet in "Dict0"
   put_dict(Id0,Dict0,x,Dict1),               % store Id in Dict0, giving "Dict1"
   clashfree_id_selection(Dict1,Id1,MaxId),   % get another random Id not yet in "Dict1"
   put_dict(Id1,Dict1,x,Dict2).               % store Id in Dict1, giving "Dict2"

test("id found 2", true(Dict2 = _{0:x,1:x})) :-
   Dict0=_{},MaxId=1,
   insert_with_clashfree_id(Dict0,x,Dict1,_Id0,MaxId),
   insert_with_clashfree_id(Dict1,x,Dict2,_Id1,MaxId).
    
:- end_tests(clashfree_id_selection).
