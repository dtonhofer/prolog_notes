:- use_module(library('heavycarbon/utils/openlist_append.pl')).

% :- debug(openlist_append).

% ---
% Appending another element
% ---

:- begin_tests(openlist_append).

test("append to open list of length 0") :-
   openlist_append(L,payload,NewFin),
   debug(openlist_append,"List is now ~q",[L]),
   L == [payload|NewFin],
   var(NewFin).

test("append to open list of length 3") :-
   L = [1,2,3|Fin],
   openlist_append(L,payload,NewFin),
   debug(openlist_append,"List is now ~q",[L]),
   L == [1,2,3,payload|NewFin],
   var(NewFin),
   Fin == [payload|NewFin].

test("Not reaching any fixpoint with this") :-   
   openlist_append(Olist,foo,_),
   openlist_append(Olist,foo,_),
   openlist_append(Olist,foo,_),
   openlist_append(Olist,foo,NewFin3),
   debug(openlist_append,"List is now ~q",[Olist]),
   Olist == [foo,foo,foo,foo|NewFin3],
   var(NewFin3).

test("fail appending to a borked openlist: atom",error(type_error(openlist,_))) :-
   openlist_append(foo,elem,_).

test("fail appending to a borked openlist: closed list",error(type_error(openlist,_))) :-
   openlist_append([1,2,3,4],elem,_).

test("fail appending to a borked openlist: whatever",error(type_error(openlist,_))) :-
   openlist_append([1,2,3|4],elem,_).

test("fail appending to a borked openlist: cyclic",error(type_error(openlist,_))) :-
   X=[1,2,3|X],openlist_append(X,elem,_).

:- end_tests(openlist_append).

% ---
% Getting last element
% ---

:- begin_tests(openlist_last).
  
test("fail getting an element from open list of length 0",fail) :- 
   openlist_last(_Olist,_Last).
 
test("get the last element from open list of length 1",true(Last == foo)) :-
   openlist_last([foo|_],Last).

test("get the last element from open list of length 3",true(Last == foo)) :-
   openlist_last([1,2,foo|_],Last).

test("get the last element from open list of length 3, and it's a variable",true(Last == Z)) :-
   openlist_last([_,_,Z|_],Last).

test("fail getting the last element from a borked openlist: atom",error(type_error(openlist,_))) :-
   openlist_last(foo,_).

test("fail getting the last element from a borked openlist: closed list",error(type_error(openlist,_))) :-
   openlist_last([1,2,3,4],_).

test("fail getting the last element from a borked openlist: whatever",error(type_error(openlist,_))) :-
   openlist_last([1,2,3|4],_).

test("fail getting the last element from a borked openlist: cyclic",error(type_error(openlist,_))) :-
   X=[1,2,3|X],openlist_last(X,_).

:- end_tests(openlist_last).

% ---
% Recognizing an openlist
% ---

:- begin_tests(is_openlist).

test("a freshvar is an openlist") :- 
   is_openlist(_).
 
test("check an openlist of length 1") :-
   is_openlist([foo|_]).

test("check an openlist of length 1 with freshvar as first element") :-
   is_openlist([_|_]).

test("check an openlist of length 3") :-
   is_openlist([1,2,foo|_]).

test("check openlist of length 3 containing only freshvar elements") :-
   is_openlist([_,_,_|_]).

test("not an openlist: atom", fail) :-
   is_openlist(foo).

test("not an openlist: proper list", fail) :-
   is_openlist([1,2,3,4]).

test("not an openlist: empty proper list", fail) :-
   is_openlist([]).

test("not an openlist: badly terminated list", fail) :-
   is_openlist([1,2,3|4]).

test("not an openlist: cyclic structure", fail) :-
   X=[1,2,3|X],is_openlist(X).

:- end_tests(is_openlist).
 
 
