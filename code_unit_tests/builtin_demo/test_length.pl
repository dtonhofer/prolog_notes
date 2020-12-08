% A bunch of unit tests which explains the behaviour of length/2
% https://eu.swi-prolog.org/pldoc/doc_for?object=length/2

:- begin_tests(test_length).

test("Getting 'length of an unbound variable' succeeds with list generation",true([X,Len] == [[],0])) :-
   once(length(X,Len)).

test("Getting 'length of an unbound variable' generates successively larger lists") :- 
   bagof([X,Len],limit(5,length(X,Len)),Bag),
   Bag = [[[],0],
          [[_A0],1],
          [[_B0,_B1],2],
          [[_C0,_C1,_C2],3],
          [[_D0,_D1,_D2,_D3],4]].

test("Getting 'length of an open list' generates successively larger lists",true([L,T,Len]=[[0,1,2,3],[],4])) :- 
   bagof([L,T,Len],limit(5,(L=[0,1,2,3|T],length(L,Len))),Bag),
   Bag = [[[0,1,2,3],[],4],
          [[0,1,2,3,A0],[A0],5],
          [[0,1,2,3,B0,B1],[B0,B1],6],
          [[0,1,2,3,C0,C1,C2],[C0,C1,C2],7],
          [[0,1,2,3,D0,D1,D2,D3],[D0,D1,D2,D3],8]].

test("Getting 'length of an open list' gets you the length of the prefix on first call",true(L == 3)) :-
   once(length([1,2,3|_],L)).

test("Stating the 'length of an unbound variable' as 0 succeeds with generation of the empty list",true(L == [])) :- 
   length(L,0).

test("Stating the 'length of an unbound variable' succeeds with 'template list' generation") :-
   length(L,10),
   % after that:
   assertion(nonvar(L)),      % it's a nonvar
   assertion(is_list(L)),     % it's a list
   assertion(maplist(var,L)), % yup, they are all var! 
   assertion(length(L,10)).   % it has length 10

test("Unsurprising: The length of empty list is 0",true(Len==0)) :-
   length([],Len).

test("Unsurprising: The length of list of length 10 is 10",true(Len==10)) :-
   length([0,1,2,3,4,5,6,7,8,9],Len).

test("Getting length of an atom causes a 'type error' exception",[error(type_error(list,x))]) :-
   length(x,_).

test("Getting length of a dict causes a 'type error' exception",[error(type_error(list,_{}))]) :-
   length(_{},_).

test("Getting the length of a cyclic list causes a 'type error' exception",[error(type_error(list,_))]) :-
   T=[1,2,3|T],
   length(T,_).

test("Getting the length of a non-list compound term causes a 'type error' exception",[error(type_error(list,_))]) :-   
   length(foo(a,b),_).

test("That weird case unifying the list and its length, take 1",fail) :-
   length(L,L).

test("That weird case unifying the list and its length, take 2",fail) :-
   length([1,2,3|L],L).

test("Case where the length appears itself in the list",true(L == 4)) :-
   length([1,2,3,L],L).

test("Stating negative length causes a 'domain error' exception",[error(domain_error(not_less_than_zero, -1))]) :-
   length(_,-1).

test("Stating an atom as length causes a 'type error' exception",[error(type_error(integer,foo))]) :-
   length(_,foo).

test("Stating a float as length (even if correct) causes a 'type error' exception",[error(type_error(integer,3.0))]) :-
   length([1,2,3],3.0).

:- end_tests(test_length).
