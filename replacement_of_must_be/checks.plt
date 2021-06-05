/*  MIT License Follows (https://opensource.org/licenses/MIT)

    Copyright 2021 David Tonhofer <ronerycoder@gluino.name>

    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files
    (the "Software"), to deal in the Software without restriction,
    including without limitation the rights to use, copy, modify, merge,
    publish, distribute, sublicense, and/or sell copies of the Software,
    and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be 
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY 
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/*
 * plunit tests for checks.pl, which implements a replacement for
 * the must_be/2 predicate.
 */


:- use_module(library('onepointfour_text/checks.pl')).


:- begin_tests(checks).

% --- no conditions 

test("no conditions always succeed #1") :-
   check_that(_,[]).

test("no conditions always succeed #2") :-
   check_that(foo,[]).

% --- var

test("var, success") :-
   check_that(_,[lenient(var)]).

test("var, failure", fail) :-
   check_that(foo,[lenient(var)]).

test("var, failure, throw", error(check(too_much_instantiation,_,_,_))) :-
   check_that(foo,[strict(var)]).

% --- nonvar

test("nonvar, success") :-
   check_that(foo,[lenient(nonvar)]).

test("nonvar, failure", fail) :-
   check_that(_,[lenient(nonvar)]).

test("nonvar, failure, throw", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(nonvar)]).

% --- nonground

test("nonground, success") :-
   forall(
      member(X,[_,f(_)]),
      check_that(X,[lenient(nonground)])
   ).

test("nonground, failure", fail) :-
   check_that(foo,[lenient(nonground)]).

test("nonground, failure, throw", error(check(domain,_,_,_))) :-
   check_that(foo,[strict(nonground)]).

% --- ground

test("ground, success") :-
   check_that(foo,[lenient(ground)]).

test("ground, failure") :-
   forall(
      member(X,[_,f(_)]),
      \+ check_that(X,[lenient(ground)])
   ).

test("ground, failure on var, throw", error(check(domain,_,_,_))) :-
   check_that(_,[strict(ground)]).

test("ground, failure on nonvar, throw", error(check(domain,_,_,_))) :-
   check_that(f(_),[strict(ground)]).

% --- atom

test("atom, success") :-
   check_that(foo,[lenient(atom)]).

test("atom, failure") :-
   forall(
      member(X,[1,f(_),f(g),"lol"]),
      \+ check_that(X,[lenient(atom)])
   ).

test("atom, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(atom)]).
 
test("atom, failure, throw", error(check(type,_,_,_))) :-
   check_that(444,[strict(atom)]).

% --- atomic

test("atomic, success") :-
   forall(
      member(X,[1,foo,"lol"]),
      check_that(X,[lenient(atomic)])
   ).

test("atomic, failure") :-
   forall(
      member(X,[f(_),f(g)]),
      \+ check_that(X,[lenient(atomic)])
   ).

test("atomic, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(atomic)]).
 
test("atomic, failure, throw", error(check(type,_,_,_))) :-
   check_that(f(g),[strict(atomic)]).

% --- compound

test("compound, success") :-
   forall(
      member(X,[f(_),f(g)]),
      check_that(X,[lenient(compound)])
   ).

test("compound, failure") :-
   forall(
      member(X,[1,foo,"lol"]),
      \+check_that(X,[lenient(compound)])
   ).

test("compound, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(compound)]).
 
test("compound, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(compound)]).

% --- boolean

test("boolean, success") :-
   forall(
      member(X,[false,true]),
      check_that(X,[lenient(boolean)])
   ).

test("boolean, failure") :-
   forall(
      member(X,[1,0,y,n,yes,no,f(x),alpha,[],'',"","false","true"]),
      \+check_that(X,[lenient(boolean)])
   ).

test("boolean, strict, type exception") :-
   forall(
      member(X,[1,0,f(x),"false","true"]),
      catch(check_that(X,[strict(boolean)]),error(check(type,_,_,_),_),true)
   ).

test("boolean, strict, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[strict(boolean)]),error(check(domain,_,_,_),_),true)
   ).

test("boolean, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(boolean)]).

% --- stringy_typeid

test("stringy_typeid, success") :-
   forall(
      member(X,[atom,string]),
      check_that(X,[lenient(stringy_typeid)])
   ).

test("stringy_typeid, failure") :-
   forall(
      member(X,[foo,"","atom","string",1,0]),
      \+check_that(X,[lenient(stringy_typeid)])
   ).

test("stringy_typeid, strict, type exception") :-
   forall(
      member(X,[1,0,f(x),"atom","string"]),
      catch(check_that(X,[strict(stringy_typeid)]),error(check(type,_,_,_),_),true)
   ).

test("stringy_typeid, strict, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[strict(stringy_typeid)]),error(check(domain,_,_,_),_),true)
   ).

test("stringy_typeid, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(stringy_typeid)]).

% --- pair

test("pair, success") :-
   forall(
      member(X,[a-b,1-2,_-_]),
      check_that(X,[lenient(pair)])
   ).

test("pair, failure") :-
   forall(
      member(X,[f(x),-(1,2,3),pair,'-']),
      \+check_that(X,[lenient(pair)])
   ).

test("pair, strict, type exception") :-
   forall(
      member(X,[1,0,hello]),
      catch(check_that(X,[strict(pair)]),error(check(type,_,_,_),_),true)
   ).

test("pair, strict, domain exception") :-
   forall(
      member(X,[f(x),-(_,_,_),-(1,2,3)]),
      catch(check_that(X,[strict(pair)]),error(check(domain,_,_,_),_),true)
   ).
 
% --- string

test("string, success") :-
   forall(
      member(X,["","foo"]),
      check_that(X,[lenient(string)])
   ).

test("string, failure") :-
   forall(
      member(X,[1,foo,f(x)]),
      \+check_that(X,[lenient(string)])
   ).

test("string, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(string)]).
 
test("string, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(string)]).

% --- stringy

test("stringy, success") :-
   forall(
      member(X,["","foo",'','foo']),
      check_that(X,[lenient(stringy)])
   ).

test("stringy, failure") :-
   forall(
      member(X,[1,f(x)]),
      \+check_that(X,[lenient(stringy)])
   ).

test("stringy, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(stringy)]).
 
test("stringy, failure, throw", error(check(type,_,_,_))) :-
   check_that(444,[strict(stringy)]).

% --- stringy, nonempty

test("nonempty stringy, success") :-
   forall(
      member(X,["foo",foo]),
      check_that(X,[lenient(nonempty_stringy)])
   ).

test("nonempty stringy, failure") :-
   forall(
      member(X,[1,"",'',f(x)]),
      \+check_that(X,[lenient(nonempty_stringy)])
   ).

test("nonempty stringy, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(nonempty_stringy)]).
 
test("nonempty stringy, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(444,[strict(nonempty_stringy)]).

test("nonempty stringy, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that("",[strict(nonempty_stringy)]).

% --- char

test("char, success") :-
   forall(
      member(X,[a,b,c,d]),
      check_that(X,[lenient(char)])
   ).

test("char, failure") :-
   forall(
      member(X,[1,f(x),ab,"a","b","",'',[]]),
      \+check_that(X,[lenient(char)])
   ).

test("char, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(char)]).
 
test("char, failure, throw", error(check(type,_,_,_))) :-
   check_that(444,[strict(char)]).

% --- number

test("number, success") :-
   NaN is nan,
   Inf is -1.0Inf,
   forall(
      member(X,[0,1,-1,1.0,-1.0,1r12,-1r12,NaN,Inf,-0.0]),
      check_that(X,[lenient(number)])
   ).

test("number, failure") :-
   forall(
      member(X,[a,"a",'0']),
      \+check_that(X,[lenient(number)])
   ).

test("number, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(number)]).
 
test("number, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(number)]).

% --- float

test("float, success") :-
   NaN is nan,
   Inf is -1.0Inf,
   forall(
      member(X,[1.0,-1.0,NaN,Inf,-0.0,3.1415]),
      check_that(X,[lenient(float)])
   ).

test("float, failure") :-
   forall(
      member(X,[1,1r12,foo]),
      \+check_that(X,[lenient(float)])
   ).

test("float, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float)]).
 
test("float, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float)]).

% --- integer

test("integer, success") :-
   forall(
      member(X,[1,0,-1]),
      check_that(X,[lenient(integer)])
   ).

test("integer, failure") :-
   forall(
      member(X,[0.0,1r12,-1.0]),
      \+check_that(X,[lenient(integer)])
   ).

test("integer, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(integer)]).
 
test("integer, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(integer)]).

% --- rational

test("rational, success") :-
   forall(
      member(X,[1,0,-1,1r12,-1r12]),
      check_that(X,[lenient(rational)])
   ).

test("rational, failure") :-
   NaN is nan,
   Inf is -1.0Inf,
   forall(
      member(X,[0.0,3.1415,foo,NaN,Inf]),
      \+check_that(X,[lenient(rational)])
   ).

test("rational, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(rational)]).
 
test("rational, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(rational)]).

% --- non-integer rational

test("nonint_rational, success") :-
   forall(
      member(X,[1r12,-1r12]),
      check_that(X,[lenient(nonint_rational)])
   ).

test("nonint_rational, failure") :-
   forall(
      member(X,[0.0,3.1415,foo,1,2,3,4,5]),
      \+check_that(X,[lenient(nonint_rational)])
   ).

test("nonint_rational, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(nonint_rational)]).
 
test("nonint_rational, failure, throw", error(check(domain,_,_,_))) :-
   check_that(777,[strict(nonint_rational)]).

% --- strictly negative number

test("negnumber, success") :-
   MinusInf is -1.0Inf,
   forall(
      member(X,[-1,-1.0,-1r12,MinusInf]),
      check_that(X,[lenient(negnumber)])
   ).

test("negnumber, failure") :-
   PlusInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,0,-0.0,0.0,1,1.0,1r12,NaN,PlusInf]),
      \+check_that(X,[lenient(negnumber)])
   ).

test("negnumber, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(negnumber)]).
 
test("negnumber, failure, throw", error(check(domain,_,_,_))) :-
   check_that(+1,[strict(negnumber)]).

% --- strictly positive number

test("posnumber, success") :-
   PlusInf is +1.0Inf,
   forall(
      member(X,[1,1.0,1r12,PlusInf]),
      check_that(X,[lenient(posnumber)])
   ).

test("posnumber, failure") :-
   MinusInf is -1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,0,+0.0,-1,-1.0,-1r12,NaN,MinusInf]),
      \+check_that(X,[lenient(posnumber)])
   ).

test("posnumber, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(posnumber)]).
 
test("posnumber, failure, throw", error(check(domain,_,_,_))) :-
   check_that(-1,[strict(posnumber)]).

% --- nonzero number

test("non0number, success") :-
   forall(
      member(X,[1,1.0,-1.0,-1,1r12]),
      check_that(X,[lenient(non0number)])
   ).

test("non0number, failure") :-
   forall(
      member(X,[foo,"foo",0,0.0,-0.0,0r1]),
      \+check_that(X,[lenient(non0number)])
   ).

test("non0number, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(non0number)]).
 
test("non0number, failure, throw", error(check(domain,_,_,_))) :-
   check_that(0.0,[strict(non0number)]).

% --- float, not NaN

test("float_not_nan, success") :-
   NegInf is -1.0Inf, 
   PosInf is +1.0Inf,
   forall(
      member(X,[NegInf,-1.0,0.0,-1.0,PosInf]),
      check_that(X,[lenient(float_not_nan)])
   ).

test("float_not_nan, failure") :-
   NaN is nan,
   forall(
      member(X,[foo,"foo",1,NaN]),
      \+check_that(X,[lenient(float_not_nan)])
   ).

test("float_not_nan, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float_not_nan)]).
 
test("float_not_nan, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float_not_nan)]).

% --- float, not infinity

test("float_not_inf, success") :-
   NaN is nan,
   forall(
      member(X,[-1.0,0.0,-1.0,NaN]),
      check_that(X,[lenient(float_not_inf)])
   ).

test("float_not_inf, failure") :-
   NegInf is -1.0Inf, 
   PosInf is +1.0Inf,
   forall(
      member(X,[foo,"foo",1,NegInf,PosInf]),
      \+check_that(X,[lenient(float_not_inf)])
   ).

test("float_not_inf, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float_not_inf)]).
 
test("float_not_inf, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float_not_inf)]).

% --- float, not negative infinity

test("float_not_neginf, success") :-
   NaN is nan,
   PosInf is +1.0Inf,
   forall(
      member(X,[-1.0,0.0,-1.0,NaN,PosInf]),
      check_that(X,[lenient(float_not_neginf)])
   ).

test("float_not_neginf, failure") :-
   NegInf is -1.0Inf, 
   forall(
      member(X,[foo,"foo",1,NegInf]),
      \+check_that(X,[lenient(float_not_neginf)])
   ).

test("float_not_neginf, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float_not_neginf)]).
 
test("float_not_neginf, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float_not_neginf)]).

% --- float, not positive infinity

test("float_not_posinf, success") :-
   NaN is nan,
   NegInf is -1.0Inf,
   forall(
      member(X,[-1.0,0.0,-1.0,NaN,NegInf]),
      check_that(X,[lenient(float_not_posinf)])
   ).

test("float_not_posinf, failure") :-
   PosInf is +1.0Inf, 
   forall(
      member(X,[foo,"foo",1,PosInf]),
      \+check_that(X,[lenient(float_not_posinf)])
   ).

test("float_not_posinf, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float_not_posinf)]).
 
test("float_not_posinf, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float_not_posinf)]).

% --- (strictly) negative integer

test("negint, success") :-
   forall(
      member(X,[-2,-1,-6r3]),
      check_that(X,[lenient(negint)])
   ).

test("negint, failure") :-
   forall(
      member(X,[foo,"foo",1,0,-1.0,-1r12]),
      \+check_that(X,[lenient(negint)])
   ).

test("negint, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(negint)]).

test("negint, failure, throw #1", error(check(type,_,_,_))) :-
   check_that(foo,[strict(negint)]).

test("negint, failure, throw #2", error(check(domain,_,_,_))) :-
   check_that(0,[strict(negint)]).

% --- (strictly) positive integer

test("posint, success") :-
   forall(
      member(X,[2,1,6r3]),
      check_that(X,[lenient(posint)])
   ).

test("posint, failure") :-
   forall(
      member(X,[foo,"foo",-1,0,1.0,1r12]),
      \+check_that(X,[lenient(posint)])
   ).

test("posint, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(posint)]).

test("posint, failure, throw #1", error(check(type,_,_,_))) :-
   check_that(foo,[strict(posint)]).

test("posint, failure, throw #2", error(check(domain,_,_,_))) :-
   check_that(0,[strict(posint)]).

% --- negative-or-zero integer

test("neg0int, success") :-
   forall(
      member(X,[-2,-1,-6r3,0]),
      check_that(X,[lenient(neg0int)])
   ).

test("neg0int, failure") :-
   forall(
      member(X,[foo,"foo",1,-1.0,-1r12,0.0]),
      \+check_that(X,[lenient(neg0int)])
   ).

test("neg0int, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(neg0int)]).
 
test("neg0int, failure, throw", error(check(type,_,_,_))) :-
   check_that(0.0,[strict(neg0int)]).

% --- positive-or-zero integer

test("pos0int, success") :-
   forall(
      member(X,[2,1,6r3,0]),
      check_that(X,[lenient(pos0int)])
   ).

test("pos0int, failure") :-
   forall(
      member(X,[foo,"foo",-1,1.0,1r12,0.0]),
      \+check_that(X,[lenient(pos0int)])
   ).

test("pos0int, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(pos0int)]).
 
test("posint, failure, throw", error(check(type,_,_,_))) :-
   check_that(0.0,[strict(pos0int)]).

% --- inty

test("inty, success") :-
   forall(
      member(X,[1, 0, -1, 1.0, 0.0, -0.0, 7777777, 7777777.0]),
      check_that(X,[lenient(inty)])
   ).

test("inty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,   
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001]),
      \+check_that(X,[lenient(inty)])
   ).

test("inty, strict, type exception") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,   
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, NegInf, PosInf, NaN, -1.5, +1.5, 0.00000001]),
      catch(check_that(X,[strict(inty)]),error(check(type,_,_,_),_),true)
   ).
      
test("inty, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(inty)]).

% --- posinty

test("posinty, success") :-
   forall(
      member(X,[1, 1.0, 7777777, 7777777.0]),
      check_that(X,[lenient(posinty)])
   ).

test("posinty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,   
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, 0, 0.0, -0.0, NegInf, PosInf, NaN, 0.00000001, -1000, -1000.0]),
      \+check_that(X,[lenient(posinty)])
   ).

test("posinty, strict, type exception") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,   
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001]),
      catch(check_that(X,[strict(posinty)]),error(check(type,_,_,_),_),true)
   ).

test("posinty, strict, domain exception") :-
   forall(
      member(X,[-1000, -1000.0]),
      catch(check_that(X,[strict(posinty)]),error(check(domain,_,_,_),_),true)
   ).
   
test("posinty, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(posinty)]).
 
% --- neginty

test("neginty, success") :-
   forall(
      member(X,[-1, -1.0, -7777777, -7777777.0]),
      check_that(X,[lenient(neginty)])
   ).

test("neginty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,   
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, 0, 0.0, -0.0, NegInf, PosInf, NaN, 0.00000001, 1000, 1000.0]),
      \+check_that(X,[lenient(neginty)])
   ).

test("neginty, strict, type exception") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,   
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001 ]),
      catch(check_that(X,[strict(neginty)]),error(check(type,_,_,_),_),true)
   ).

test("neginty, strict, domain exception") :-
   forall(
      member(X,[0, 0.0, -0.0, 1000, 1000.0]),
      catch(check_that(X,[strict(neginty)]),error(check(domain,_,_,_),_),true)
   ).

test("neginty, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(neginty)]).
 
% --- pos0inty

test("pos0inty, success") :-
   forall(
      member(X,[1, 1.0, 7777777, 7777777.0, 0.0, 0, -0.0]),
      check_that(X,[lenient(pos0inty)])
   ).

test("pos0inty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,   
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001, -1000, -1000.0]),
      \+check_that(X,[lenient(pos0inty)])
   ).

test("pos0inty, strict, type exception") :-
   NaN is nan,
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,   
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001]),
      catch(check_that(X,[strict(pos0inty)]),error(check(type,_,_,_),_),true)
   ).

test("pos0inty, strict, domain exception") :-
   forall(
      member(X,[ -1000.0 ]),
      catch(check_that(X,[strict(pos0inty)]),error(check(domain,_,_,_),_),true)
   ).

test("pos0inty, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(pos0inty)]).
 
% --- neg0inty

test("neg0inty, success") :-
   forall(
      member(X,[-1, -1.0, -7777777, -7777777.0, 0, 0.0, -0.0]),
      check_that(X,[lenient(neg0inty)])
   ).

test("neg0inty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,   
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001, 1000, 1000.0]),
      \+check_that(X,[lenient(neg0inty)])
   ).

test("neg0inty, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(neg0inty)]).
 
test("neg0inty, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(neg0inty)]).

% --- (strictly) negative float

test("negfloat, success") :-
   NegInf is -1.0Inf,
   forall(
      member(X,[NegInf, -2.0, -1.0]),
      check_that(X,[lenient(negfloat)])
   ).

test("negfloat, failure") :-
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1.0, -1,0, -0.0, 0.0, PosInf, NaN]),
      \+check_that(X,[lenient(negfloat)])
   ).

test("negfloat, strict, type exception") :-
   forall(
      member(X,[foo, "foo", 1r12, 0]),
      catch(check_that(X,[strict(negfloat)]),error(check(type,_,_,_),_),true)
   ).

test("negfloat, strict, domain exception") :-
   NaN is nan,
   PosInf is +1.0Inf,
   forall(
      member(X,[0.0, -0.0, 1000.0, PosInf, NaN]),
      catch(check_that(X,[strict(negfloat)]),error(check(domain,_,_,_),_),true)
   ).

test("negfloat, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(negfloat)]).

% --- (strictly) positive float

test("posfloat, success") :-
   PosInf is +1.0Inf,
   forall(
      member(X,[PosInf,2.0,1.0]),
      check_that(X,[lenient(posfloat)])
   ).

test("posfloat, failure") :-
   NegInf is -1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,"foo",1,-1.0,0,0.0,1r12,NegInf,NaN]),
      \+check_that(X,[lenient(posfloat)])
   ).

test("posfloat, strict, type exception") :-
   forall(
      member(X,[foo, "foo", 1r12, 0]),
      catch(check_that(X,[strict(posfloat)]),error(check(type,_,_,_),_),true)
   ).

test("posfloat, strict, domain exception") :-
   NaN is nan,
   NegInf is -1.0Inf,
   forall(
      member(X,[0.0, -0.0, -1000.0, NegInf, NaN]),
      catch(check_that(X,[strict(posfloat)]),error(check(domain,_,_,_),_),true)
   ).

test("posfloat, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(posfloat)]).
 
% --- negative-or-zero float

test("neg0float, success") :-
   NegInf is -1.0Inf,
   forall(
      member(X,[NegInf, -2.0, -1.0, 0.0]),
      check_that(X,[lenient(neg0float)])
   ).

test("neg0float, failure") :-
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, 0, 1000, 1000.0, 1.0, PosInf, NaN]),
      \+check_that(X,[lenient(neg0float)])
   ).

test("neg0float, strict, type exception") :-
   forall(
      member(X,[foo, "foo", 1r12, 0, 1000]),
      catch(check_that(X,[strict(neg0float)]),error(check(type,_,_,_),_),true)
   ).

test("neg0float, strict, domain exception") :-
   NaN is nan,
   PosInf is +1.0Inf,
   forall(
      member(X,[0.0, -0.0, 1000.0, 1.0, PosInf,NaN]),
      catch(check_that(X,[strict(neg0float)]),error(check(domain,_,_,_),_),true)
   ).

test("neg0float, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(neg0float)]).
 
% --- positive-or-zero float

test("pos0float, success") :-
   PosInf is +1.0Inf,
   forall(
      member(X,[PosInf,2.0,1.0,0.0]),
      check_that(X,[lenient(pos0float)])
   ).

test("pos0float, failure") :-
   NegInf is -1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,"foo",1,-1.0,0,1r12,NegInf,NaN]),
      \+check_that(X,[lenient(pos0float)])
   ).

test("pos0float, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(pos0float)]).
 
test("pos0float, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(pos0float)]).

% --- list

test("list, success") :-
   forall(
      member(X,[[],[_],[1,2,3]]),
      check_that(X,[lenient(list)])
   ).

test("list, failure") :-
   forall(
      member(X,["",[1|2],[1|_]]),
      \+check_that(X,[lenient(list)])
   ).

test("list, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(list)]).
 
test("list, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(list)]).

% --- dict

test("dict, success") :-
   forall(
      member(X,[_{},foo{},bar{a:1,b:2}]),
      check_that(X,[lenient(dict)])
   ).

test("dict, failure") :-
   forall(
      member(X,["",foo]),
      \+check_that(X,[lenient(dict)])
   ).

test("dict, lenient, uninstantiated exception",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(dict)]).
 
test("dict, failure, throw", error(check(type,_,_,_))) :-
   check_that(foo,[strict(dict)]).

% --- member

test("member, success #1") :-
   check_that(a,[lenient(member([a,b,c]))]).

test("member, success #2") :-
   check_that(a,[lenient(member([a,X,c]))]),
   assertion(var(X)).

test("member, success #3") :-
   check_that(v,[lenient(member([a,_,c]))]).

test("member, failure #1",fail) :-
   check_that(v,[lenient(member([a,b,c]))]).

test("member, failure #2",fail) :-
   check_that(v,[lenient(member([]))]).

test("member, element nonground",error(check(not_ground,_,_,_))) :-
   check_that(_,[lenient(member([a,b,c]))]).

test("member, not a proper list",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[lenient(member(_))]). % already caught in well-formedness check

test("member, not a proper list",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[lenient(member(foo))]).  % already caught in well-formedness check

test("member, not a proper list",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[lenient(member([a,b,c|_]))]).  % already caught in well-formedness check

% --- random

test("random until failure",error(check(random,_,_,_))) :-
   repeat,
   check_that(a,[strict(random(0.5))]),
   fail.

% --- forall

test("forall, success") :-
   check_that(12.0,[lenient(forall([float,inty,posnum]))]).

test("forall, empty list of checks, success") :-
   check_that(12.0,[]).

test("forall, failure",fail) :-
   check_that(-12.0,[lenient(forall([float,inty,posnum]))]).

test("forall, strict, failure",error(check(domain,_,_,_))) :-
   check_that(-12.0,[strict(forall([float,inty,posnum]))]). % fails on posnum

% --- forany

test("forany, success") :-
   check_that(-12,[lenient(forany([float,inty,posnum]))]).

test("forany, empty list of checks, failure",fail) :-
   check_that(-12,[lenient(forany([]))]).

test("forany, failure",fail) :-
   check_that(-12,[lenient(forany([float,proper_rational,posnum]))]).

test("forany, strict, failure",error(check(type,_,_,_))) :-
   check_that(-12,[strict(forany([float,proper_rational,posnum]))]).

% --- fornone

test("fornone, success") :-
   check_that(foo,[lenient(fornone([float,inty,posnum]))]).

test("fornone, empty list of checks, failure", fail) :-
   check_that(foo,[lenient(fornone([]))]).

test("fornone, failure",fail) :-
   check_that(12.0,[lenient(fornone([integer,negnum,inty]))]).

test("fornone, strict, failure",error(check(type,_,_,_))) :-
   check_that(12.0,[strict(fornone([integer,negnum,inty]))]).

% --- passall

test("passall, not a list as X", error(check(type,_,_,_),_)) :-
   check_that(foo,[lenient(passall(integer))]).

test("passall, empty list as X, success") :-
   check_that([],[lenient(passall(integer))]).

test("passall, success") :-
   check_that([1, 0, 3.0],[lenient(passall(inty))]).

test("passall, failure",fail) :-
   check_that([1, 0, 3.1],[lenient(passall(inty))]).

test("passall, strict, failure, #1", error(check(type,_,_,_),_)) :-
   check_that([1, 0, 3.1],[lenient(passall(inty))],throw).

test("passall, strict, failure, #2", error(check(type,_,_,_),_)) :-
   check_that([1, 0, 3.1],[strict(passall(inty))]).

test("passall, strict, failure, #3", error(check(type,_,_,_),_)) :-
   check_that([-1, foo, -2],[strict(passall(negint))]).

test("passall, strict, failure, #4", error(check(domain,_,_,_),_)) :-
   check_that([-1, 0, -2],[strict(passall(negint))]).


% --- passany

test("passany, not a list as X", error(check(type,_,_,_),_)) :-
   check_that(foo,[lenient(passany(integer))]).

test("passany, empty list as X, failure", fail) :-
   check_that([],[lenient(passany(integer))]).

test("passany, success") :-
   check_that([foo, g(x), 3.0],[lenient(passany(inty))]).

test("passany, failure",fail) :-
   check_that([foo, g(x), 3.1],[lenient(passany(inty))]).

test("passany, strict, failure, #1", error(check(passany,_,_,_),_)) :-
   check_that([foo, g(x), 3.1],[lenient(passany(inty))],throw).

test("passany, strict, failure, #2", error(check(passany,_,_,_),_)) :-
   check_that([foo, g(x), 3.1],[strict(passany(inty))]).

test("passany, strict, failure, #3", error(check(passany,_,_,_),_)) :-
   check_that([1, 2, foo],[strict(passany(negint))]). % fails on 1

test("passany, strict, failure, #4", error(check(passany,_,_,_),_)) :-
   check_that([1, 2, 3],[strict(passany(negint))]).

% --- passnone

test("passnone, not a list as X", error(check(type,_,_,_),_)) :-
   check_that(foo,[lenient(passnone(integer))]).

test("passnone, empty list as X, failure", fail) :-
   check_that([],[lenient(passnone(integer))]).

test("passnone, success: there is no inty") :-
   check_that([foo, g(x), 3.1],[lenient(passnone(inty))]).

test("passnone, failure: there is an inty",fail) :-
   check_that([foo, 2, 3.1],[lenient(passnone(inty))]).

test("passnone, strict, failure: there is an inty, #1", error(check(passnone,_,_,_),_)) :-
   check_that([foo, 2, 3.1],[lenient(passnone(inty))],throw).

test("passnone, strict, failure: there is an inty, #2", error(check(passnone,_,_,_),_)) :-
   check_that([foo, 2, 3.1],[strict(passnone(inty))]).






% --- list of several conditions

test("12 is nonvar and an integer, strict, succeeds") :-
   check_that(12,[strict(nonvar),strict(int)]).

test("foo is nonvar and an integer, strict, throws",error(check(type,_,_,_))) :-
   check_that(foo,[strict(nonvar),strict(int)]).

test("foo is nonvar and an integer, lenient, fails", fail) :-
   check_that(foo,[lenient(nonvar),lenient(int)]).

test("foo is nonvar and an integer, marked lenient, but is strict by switching on strictness, throws", error(check(type,_,_,_))) :-
   check_that(foo,[lenient(nonvar),lenient(int)],throw).

test("a var is nonvar and an integer, lenient, fails", fail) :-
   check_that(_,[lenient(nonvar),lenient(int)]).

:- end_tests(checks).
