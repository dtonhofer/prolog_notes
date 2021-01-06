minlist(List,Value) :- minormaxlist(List,Value,min).
maxlist(List,Value) :- minormaxlist(List,Value,max).
   
minormaxlist(List,Value,What) :-
   ((nonvar(Value),\+number(Value)) -> type_error("Value: var or number",Value) ; true),   
   ((List == []) -> domain_error("List: nonempty list",List) ; true),
   ((List = [X|Xs]) -> true ; type_error("List: nonempty list",List)),
   ((What == min) 
    ->
    foldl(
       ([Element,FromLeft,ToRight]>>(ToRight is min(FromLeft,Element))),
       Xs,X,Value)
    ;      
    foldl(
       ([Element,FromLeft,ToRight]>>(ToRight is max(FromLeft,Element))),
       Xs,X,Value)).
          
:- begin_tests(maxlist).

test("not a list on list position throws",error(type_error(_,_))) :- maxlist(foo,_).
test("empty list on list position throws",error(domain_error(_,_))) :- maxlist([],_).
test("good list 1",true(R==3)) :- maxlist([1,2,3,2,1],R).
test("good list 2",true(R==3.5)) :- maxlist([1,2,3.5,2,1],R).
test("good list with pi",true(R==PI)) :- maxlist([1,2,pi,2,1],R),PI is pi.
test("good list with pi",true(R==PI)) :- maxlist([1,2,pi(),2,1],R),PI is pi().
test("list with atom",error(type_error(evaluable,x/0))) :- maxlist([1,2,x,2,1],_).
test("list with variable",error(instantiation_error)) :- maxlist([1,2,_,2,1],_).
test("called with unacceptable value",error(type_error(_,_))) :- maxlist([1,2,3],foo).
test("called with good value") :- maxlist([1,2,3],3).
test("called with bad value",fail) :- maxlist([1,2,3],5).

:- end_tests(maxlist).

:- begin_tests(minlist).

test("not a list on list position throws",error(type_error(_,_))) :- minlist(foo,_).
test("empty list on list position throws",error(domain_error(_,_))) :- minlist([],_).
test("good list 1",true(R==1)) :- minlist([1,2,3,2,1],R).
test("good list 2",true(R==0)) :- minlist([1,2,0,2,1],R).
test("good list with pi",true(R==PI)) :- minlist([1,2,-pi,2,1],R),PI is -pi.
test("good list with pi",true(R==PI)) :- minlist([1,2,-pi(),2,1],R),PI is -pi().
test("list with atom",error(type_error(evaluable,x/0))) :- minlist([1,2,x,2,1],_).
test("list with variable",error(instantiation_error)) :- minlist([1,2,_,2,1],_).
test("called with unacceptable value",error(type_error(_,_))) :- minlist([1,2,3],foo).
test("called with good value") :- minlist([1,2,3],1).
test("called with bad value",fail) :- minlist([1,2,3],0).

:- end_tests(minlist).
