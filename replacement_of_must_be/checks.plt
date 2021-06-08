/*  Zero-Clause BSD (0BSD) follows (https://opensource.org/licenses/0BSD)

    Permission to use, copy, modify, and/or distribute this software for
    any purpose with or without fee is hereby granted.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
    WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
    AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
    DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
    TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
    PERFORMANCE OF THIS SOFTWARE.
*/

/*
 * plunit tests for checks.pl, which implements a replacement for
 * the must_be/2 predicate.
 */

:- use_module(library('onepointfour_basics/checks.pl')).


:- begin_tests(check_that_using_no_conditions).

test("no conditions always succeed #1") :-
   check_that(_,[]).

test("no conditions always succeed #2") :-
   check_that(foo,[]).

:- end_tests(check_that_using_no_conditions).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_var).

test("var, success") :-
   check_that(_,[lenient(var)]).

test("var, failure", fail) :-
   check_that(foo,[lenient(var)]).

test("var, failure, throw", error(check(too_much_instantiation,_,_,_))) :-
   check_that(foo,[strict(var)]).

:- end_tests(check_that_using_var).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonvar).

test("nonvar, success") :-
   check_that(foo,[lenient(nonvar)]).

test("nonvar, failure", fail) :-
   check_that(_,[lenient(nonvar)]).

test("nonvar, failure, throw", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(nonvar)]).

:- end_tests(check_that_using_nonvar).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonground).

test("nonground, success") :-
   forall(
      member(X,[_,f(_)]),
      check_that(X,[lenient(nonground)])
   ).

test("nonground, failure", fail) :-
   check_that(foo,[lenient(nonground)]).

test("nonground, failure, throw", error(check(domain,_,_,_))) :-
   check_that(foo,[strict(nonground)]).

:- end_tests(check_that_using_nonground).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_ground).

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

:- end_tests(check_that_using_ground).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_atom).

test("atom, success") :-
   check_that(foo,[lenient(atom)]).

test("atom, lenient, failure") :-
   forall(
      member(X,[1,f(g),"lol",f(_)]),
      \+ check_that(X,[lenient(atom)])
   ).

test("atom, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(444,[strict(atom)]).

test("atom, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(atom)]).

test("atom, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(atom)]).

test("atom, fully lenient, fail on nonground X", fail) :-
   check_that(_,[fullylenient(atom)]).

:- end_tests(check_that_using_atom).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_atomic).

test("atomic, success") :-
   forall(
      member(X,[1,foo,"lol"]),
      check_that(X,[lenient(atomic)])
   ).

test("atomic, failure",fail) :-
   check_that(f(g(x)),[lenient(atomic)]).

test("atomic, strict failure, throw type error", error(check(type,_,_,_))) :-
   check_that(f(g),[strict(atomic)]).

test("atomic, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(atomic)]).

test("atomic, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(atomic)]).

test("atomic, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(atomic)]).

:- end_tests(check_that_using_atomic).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_compound).

test("compound, success") :-
   forall(
      member(X,[f(g),f(_)]),
      check_that(X,[lenient(compound)])
   ).

test("compound, failure") :-
   forall(
      member(X,[1,foo,"lol"]),
      \+check_that(X,[lenient(compound)])
   ).

test("compound, strict failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(compound)]).

test("compound, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(compound)]).

test("compound, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(compound)]).

test("compound, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(compound)]).

:- end_tests(check_that_using_compound).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_boolean).

test("boolean, success") :-
   forall(
      member(X,[false,true]),
      check_that(X,[lenient(boolean)])
   ).

test("boolean, failure") :-
   forall(
      member(X,[1,0,y,n,yes,no,f(x),f(_),alpha,[],'',"","false","true"]),
      \+check_that(X,[lenient(boolean)])
   ).

test("boolean, strict, type exception") :-
   forall(
      member(X,[1,0,f(x),f(_),"false","true"]),
      catch(check_that(X,[strict(boolean)]),error(check(type,_,_,_),_),true)
   ).

test("boolean, strict, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[strict(boolean)]),error(check(domain,_,_,_),_),true)
   ).

test("boolean, strict failure, throw type error", error(check(type,_,_,_))) :-
   check_that(g(g),[strict(boolean)]).

test("boolean, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(boolean)]).

test("boolean, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(boolean)]).

test("boolean, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(boolean)]).

:- end_tests(check_that_using_boolean).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_stringy_typeid).

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

test("stringy_typeid, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(stringy_typeid)]).

test("stringy_typeid, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(stringy_typeid)]).

test("stringy_typeid, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(stringy_typeid)]).


:- end_tests(check_that_using_stringy_typeid).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_chary_typeid).

test("chary_typeid, success") :-
   forall(
      member(X,[char,code]),
      check_that(X,[lenient(chary_typeid)])
   ).

test("chary_typeid, failure") :-
   forall(
      member(X,[foo,"","char","code",1,0]),
      \+check_that(X,[lenient(chary_typeid)])
   ).

test("chary_typeid, strict, type exception") :-
   forall(
      member(X,[1,0,f(x),"char","code"]),
      catch(check_that(X,[strict(chary_typeid)]),error(check(type,_,_,_),_),true)
   ).

test("chary_typeid, strict, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[strict(chary_typeid)]),error(check(domain,_,_,_),_),true)
   ).

test("chary_typeid, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(chary_typeid)]).

test("chary_typeid, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(chary_typeid)]).

test("chary_typeid, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(chary_typeid)]).

:- end_tests(check_that_using_chary_typeid).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pair).

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

test("pair, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(pair)]).

test("pair, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(pair)]).

test("pair, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(pair)]).

:- end_tests(check_that_using_pair).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_string).

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

test("string, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(string)]).

test("string, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(string)]).

test("string, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(string)]).

test("string, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(string)]).

:- end_tests(check_that_using_string).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_stringy).

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

test("stringy, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(444,[strict(stringy)]).

test("stringy, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(stringy)]).

test("stringy, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(stringy)]).

test("stringy, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(stringy)]).

:- end_tests(check_that_using_stringy).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonempty_stringy).

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

test("nonempty stringy, stringy, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(444,[strict(nonempty_stringy)]).

test("nonempty stringy, stringy, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that("",[strict(nonempty_stringy)]).

test("nonempty stringy, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(nonempty_stringy)]).

test("nonempty stringy, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(nonempty_stringy)]).

test("nonempty stringy, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(nonempty_stringy)]).

:- end_tests(check_that_using_nonempty_stringy).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_char).

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

test("char, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(444,[strict(char)]).

test("char, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(foo,[strict(char)]).

test("char, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(char)]).

test("char, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(char)]).

test("char, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(char)]).

:- end_tests(check_that_using_char).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_code).

test("code, success") :-
   forall(
      member(X,[0,1,2,3,0x10FFFF]),
      check_that(X,[lenient(code)])
   ).

test("code, failure") :-
   forall(
      member(X,[f(x),ab,a,"a",-1,1.0,0xFFFFFFF]),
      \+check_that(X,[lenient(code)])
   ).

test("code, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[strict(code)]).

test("code, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(-1,[strict(code)]).

test("code, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(code)]).

test("code, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(code)]).

test("code, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(code)]).

:- end_tests(check_that_using_code).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_chary).

test("chary, success") :-
   forall(
      member(X,[0,1,2,3,a,b,c]),
      check_that(X,[lenient(chary)])
   ).

test("chary, failure") :-
   forall(
      member(X,[f(x),ab,"a",'',"",-1,[]]),
      \+check_that(X,[lenient(chary)])
   ).

test("chary, strict, failure, type exception", error(check(type,_,_,_))) :-
   check_that("foo",[strict(chary)]).

test("chary, strict, failure, domain exception") :-
   forall(
      member(X,[foo,-1]),
      catch(check_that(X,[strict(chary)]),error(check(domain,_,_,_),_),true)
   ).

test("chary, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(chary)]).

test("chary, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(chary)]).

test("chary, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(chary)]).

:- end_tests(check_that_using_chary).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_number).

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

test("number, istrict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(number)]).

test("number, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(number)]).

test("number, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(number)]).

test("number, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(number)]).

:- end_tests(check_that_using_number).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float).

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

test("float, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float)]).

test("float, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(float)]).

test("float, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float)]).

test("float, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(float)]).

:- end_tests(check_that_using_float).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_integer).

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

test("integer, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(integer)]).

test("integer, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(integer)]).

test("integer, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(integer)]).

test("integer, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(integer)]).

:- end_tests(check_that_using_integer).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_rational).

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

test("rational, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(rational)]).

test("rational, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(rational)]).

test("rational, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(rational)]).

test("rational, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(rational)]).

:- end_tests(check_that_using_rational).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonint_rational).

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

test("nonint_rational, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(nonint_rational)]).

test("nonint_rational, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(777,[strict(nonint_rational)]).

test("nonint_rational, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(nonint_rational)]).

test("nonint_rational, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(nonint_rational)]).

test("nonint_rational, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(nonint_rational)]).

:- end_tests(check_that_using_nonint_rational).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_negnumber).

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

test("negnumber, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(negnumber)]).

test("negnumber, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(+1,[strict(negnumber)]).

test("negnumber, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(negnumber)]).

test("negnumber, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(negnumber)]).

test("negnumber, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(negnumber)]).

:- end_tests(check_that_using_negnumber).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posnumber).

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

test("posnumber, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(posnumber)]).

test("posnumber, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(-1,[strict(posnumber)]).

test("posnumber, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(posnumber)]).

test("posnumber, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(posnumber)]).

test("posnumber, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(posnumber)]).

:- end_tests(check_that_using_posnumber).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_non0number).

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

test("non0number, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(non0number)]).

test("non0number, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(0.0,[strict(non0number)]).

test("non0number, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(non0number)]).

test("non0number, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(non0number)]).

test("non0number, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(non0number)]).

:- end_tests(check_that_using_non0number).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_nan).

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

test("float_not_nan, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float_not_nan)]).

test("float_not_nan, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   NaN is nan,
   check_that(NaN,[strict(float_not_nan)]).

test("float_not_nan, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(float_not_nan)]).

test("float_not_nan, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float_not_nan)]).

test("float_not_nan, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(float_not_nan)]).

:- end_tests(check_that_using_float_not_nan).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_inf).

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

test("float_not_inf, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float_not_inf)]).

test("float_not_inf, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   NegInf is -1.0Inf,
   check_that(NegInf,[strict(float_not_inf)]).

test("float_not_inf, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(float_not_inf)]).

test("float_not_inf, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float_not_inf)]).

test("float_not_inf, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(float_not_inf)]).

:- end_tests(check_that_using_float_not_inf).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_neginf).

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

test("float_not_neginf, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float_not_neginf)]).

test("float_not_neginf, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   NegInf is -1.0Inf,
   check_that(NegInf,[strict(float_not_neginf)]).

test("float_not_neginf, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(float_not_neginf)]).

test("float_not_neginf, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float_not_neginf)]).

test("float_not_neginf, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(float_not_neginf)]).

:- end_tests(check_that_using_float_not_neginf).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_posinf).

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

test("float_not_posinf, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(float_not_posinf)]).

test("float_not_posinf, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   PosInf is +1.0Inf,
   check_that(PosInf,[strict(float_not_posinf)]).

test("float_not_posinf, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(float_not_posinf)]).

test("float_not_posinf, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(float_not_posinf)]).

test("float_not_posinf, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(float_not_posinf)]).

:- end_tests(check_that_using_float_not_posinf).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_negint).

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

test("negint, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(negint)]).

test("negint, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(1,[strict(negint)]).

test("negint, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(negint)]).

test("negint, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(negint)]).

test("negint, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(negint)]).

:- end_tests(check_that_using_negint).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posint).

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

test("posint, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(posint)]).

test("posint, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(-1,[strict(posint)]).

test("posint, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(posint)]).

test("posint, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(posint)]).

test("posint, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(posint)]).

:- end_tests(check_that_using_posint).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neg0int).

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

test("neg0int, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(neg0int)]).

test("neg0int, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(1,[strict(neg0int)]).

test("neg0int, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(neg0int)]).

test("neg0int, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(neg0int)]).

test("neg0int, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(neg0int)]).

:- end_tests(check_that_using_neg0int).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pos0int).

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

test("pos0int, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[strict(pos0int)]).

test("pos0int, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(-1,[strict(pos0int)]).

test("pos0int, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(pos0int)]).

test("pos0int, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(pos0int)]).

test("pos0int, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(pos0int)]).

:- end_tests(check_that_using_pos0int).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_inty).

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

test("inty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(inty)]).

test("inty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(inty)]).

test("inty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(inty)]).

:- end_tests(check_that_using_inty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posinty).

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

test("posinty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(posinty)]).

test("posinty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(posinty)]).

test("posinty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(posinty)]).

:- end_tests(check_that_using_posinty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neginty).

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

test("neginty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(neginty)]).

test("neginty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(neginty)]).

test("neginty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(neginty)]).

:- end_tests(check_that_using_neginty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pos0inty).

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

test("pos0inty, strict, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[strict(pos0inty)]).

test("pos0inty, strict, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(-1.0,[strict(pos0inty)]).

test("pos0inty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(pos0inty)]).

test("pos0inty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(pos0inty)]).

test("pos0inty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(pos0inty)]).

:- end_tests(check_that_using_pos0inty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neg0inty).

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

test("neg0inty, strict, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[strict(neg0inty)]).

test("neg0inty, strict, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(1.0,[strict(neg0inty)]).

test("neg0inty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(neg0inty)]).

test("neg0inty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(neg0inty)]).

test("neg0inty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(neg0inty)]).

:- end_tests(check_that_using_neg0inty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_negfloat).

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

test("negfloat, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(negfloat)]).

test("negfloat, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(negfloat)]).

test("negfloat, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(negfloat)]).

:- end_tests(check_that_using_negfloat).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posfloat).

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

test("posfloat, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(posfloat)]).

test("posfloat, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(posfloat)]).

test("posfloat, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(posfloat)]).

:- end_tests(check_that_using_posfloat).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neg0float).

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

test("neg0float, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(neg0float)]).

test("neg0float, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(neg0float)]).

test("neg0float, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(neg0float)]).

:- end_tests(check_that_using_neg0float).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pos0float).

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

test("pos0float, strict, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[strict(pos0float)]).

test("pos0float, strict, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(-1.0,[strict(pos0float)]).

test("pos0float, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(pos0float)]).

test("pos0float, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(pos0float)]).

test("pos0float, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(pos0float)]).

:- end_tests(check_that_using_pos0float).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_list).

test("list, success") :-
   forall(
      member(X,[ [], [_], [1,2,3], [_,_,_] ]),
      check_that(X,[lenient(list)])
   ).

test("list, failure") :-
   forall(
      member(X,["",[1|2],[1|_]]),
      \+check_that(X,[lenient(list)])
   ).

test("list, failure, strict, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[strict(list)]).

test("list, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(list)]).

test("list, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(list)]).

test("list, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(list)]).

:- end_tests(check_that_using_list).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonempty_list).

test("nonempty list, success") :-
   forall(
      member(X,[[1,2,3],[_,_,_]]),
      check_that(X,[lenient(nonempty_list)])
   ).

test("nonempty list, failure") :-
   forall(
      member(X,["",[1|2],'',foo,123,"",[]]),
      \+check_that(X,[lenient(nonempty_list)])
   ).

test("nonempty list, strict, throw type exception") :-
   forall(
      member(X,[foo, "foo", [1|2]]),
      catch(check_that(X,[strict(nonempty_list)]),error(check(type,_,_,_),_),true)
   ).

test("nonempty list, strict, throw domain exception") :-
   catch(check_that([],[strict(nonempty_list)]),error(check(domain,_,_,_),_),true).

test("nonempty_list, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(nonempty_list)]).

test("nonempty_list, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(nonempty_list)]).

test("nonempty_list, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(nonempty_list)]).

:- end_tests(check_that_using_nonempty_list).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_member).

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

test("member, X nonground",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(member([a,b,c]))]).

test("member, argument to member/1 term not a proper list #1",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[lenient(member(_))]). % already caught in well-formedness check

test("member, argument to member/1 term not a proper list #2",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[lenient(member(foo))]).  % already caught in well-formedness check

test("member, argument to member/1 term not a proper list #3",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[lenient(member([a,b,c|_]))]).  % already caught in well-formedness check

:- end_tests(check_that_using_member).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_random).

test("random until failure",error(check(random,_,_,_))) :-
   repeat,
   check_that(a,[strict(random(0.5))]),
   fail.

:- end_tests(check_that_using_random).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_dict).

test("dict, success") :-
   forall(
      member(X,[_{},foo{},bar{a:1,b:2}]),
      check_that(X,[lenient(dict)])
   ).

test("dict, failure") :-
   forall(
      member(X,["",foo,foo(a,b)]),
      \+check_that(X,[lenient(dict)])
   ).

test("dict, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(dict)]).

test("dict, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(dict)]).

test("dict, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(dict)]).

:- end_tests(check_that_using_dict).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_cyclic).

test("cyclic, success") :-
   A=h(a,b,c,A),
   B=[a,b,c|B],
   C=[1,2|B],
   forall(
      member(X,[A,B,C]),
      check_that(X,[lenient(cyclic)])
   ).

test("cyclic, failure") :-
   forall(
      member(X,['',[1,2,3],foo]),
      \+check_that(X,[lenient(cyclic)])
   ).

test("unbound variable is not cyclic",fail) :-
   check_that(_,[lenient(cyclic)]).

:- end_tests(check_that_using_cyclic).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_acyclic_now).

test("tentatively acyclic, success") :-
   forall(
      member(X,['',[1,2,3],foo,[1,2,3|_]]),
      check_that(X,[lenient(acyclic_now)])
   ).

test("tentatively acyclic, failure") :-
   A=h(a,b,c,A),
   B=[a,b,c|B],
   C=[1,2|B],
   forall(
      member(X,[A,B,C]),
      \+check_that(X,[lenient(acyclic_now)])
   ).

test("unbound variable is tentatively acyclic") :-
   check_that(_,[lenient(acyclic_now)]).

:- end_tests(check_that_using_acyclic_now).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_acyclic_forever).

test("acyclic_forever, success") :-
   forall(
      member(X,['',[1,2,3],foo]),
      check_that(X,[lenient(acyclic_forever)])
   ).

test("acyclic_forever, failure") :-
   A=h(a,b,c,A), % definitely cyclic
   B=[a,b,c|_],  % may become cyclic
   forall(
      member(X,[A,B]),
      \+check_that(X,[lenient(acyclic_forever)])
   ).

test("unbound variable is not 'acyclic forever'",fail) :-
   check_that(_,[lenient(acyclic_forever)]).

:- end_tests(check_that_using_acyclic_forever).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_stream).

test("stream atoms (aliases), success") :-
   forall(
      member(X,[user_input, user_output, user_error, current_input, current_output]),
      check_that(X,[lenient(stream)])
   ).

test("stream atoms (aliases), failure") :-
   forall(
      member(X,[foo,bar,'']),
      catch(check_that(X,[strict(stream)]),error(check(domain,_,_,_),_),true)
   ).

test("stream, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(stream)]).

test("stream, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(stream)]).

test("stream, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(stream)]).

:- end_tests(check_that_using_stream).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_chars).

test("chars, success") :-
   forall(
      member(X,[[],[a,b,c]]),
      check_that(X,[lenient(chars)])
   ).

test("chars, failure (includes things that are not a list)") :-
   forall(
      member(X,[[1,2,3], ["a","b"], [ab,cd], [1|2], foo ]),
      \+check_that(X,[lenient(chars)])
   ).

test("chars, strict, type exception (not a list, not a list of atoms)") :-
   forall(
      member(X,[foo, "foo", [1|2], ["a","b"], [1, 2]]),
      catch(check_that(X,[strict(chars)]),error(check(type,_,_,_),_),true)
   ).

test("chars, strict, domain exception (list of atoms, but one atom is not of length 1)") :-
   catch(check_that([a,bb,c],[strict(chars)]),error(check(domain,_,_,_),_),true).

test("chars, fullylenient, fails because of uninstantiated member",fail) :-
   check_that([a,b,c,_,d,e,f],[fullylenient(chars)]).

test("chars, strict, uninstantiated exception because of uninstantiated member",error(check(too_little_instantiation,_,_,_))) :-
   check_that([a,b,c,_,d,e,f],[strict(chars)]).

test("chars, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(chars)]).

test("chars, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(chars)]).

test("chars, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(chars)]).

:- end_tests(check_that_using_chars).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_codes).

test("codes, success") :-
   forall(
      member(X,[[],[1,2,3]]),
      check_that(X,[lenient(codes)])
   ).

test("codes, failure (includes things that are not a list)") :-
   forall(
      member(X,["",[1|2],'',foo,123,"",[a,b,c],["a","b"],[ab,cd],[-1,0],[0xFFFFFFFF,0]]),
      \+check_that(X,[lenient(codes)])
   ).

test("codes, strict, type exception (not a list, not a list of codes)") :-
   forall(
      member(X,[foo, "foo", [1|2], ["a","b"], [a, b]]),
      catch(check_that(X,[strict(codes)]),error(check(type,_,_,_),_),true)
   ).

test("codes, strict, domain exception (list of codes, but one of the codes is not in range)") :-
   catch(check_that([1,-1,2],[strict(codes)]),error(check(domain,_,_,_),_),true).

test("codes, fullylenient, fails because of uninstantiated member",fail) :-
   check_that([1,2,3,_,4,5,6],[fullylenient(codes)]).

test("codes, strict, uninstantiated exception because of uninstantiated member",error(check(too_little_instantiation,_,_,_))) :-
   check_that([1,2,3,_,4,5,6],[strict(codes)]).

test("codes, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(codes)]).

test("codes, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(codes)]).

test("codes, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(codes)]).

:- end_tests(check_that_using_codes).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_charys).

test("charys, success") :-
   forall(
      member(X,[[],[1,2,3]]),
      check_that(X,[lenient(charys)])
   ).

test("charys, failure") :-
   forall(
      member(X,["",[1|2],'',foo,123,"",["a","b"],[ab,cd],[-1,0],[1,2,a],[0xFFFFFFFF,0]]),
      \+check_that(X,[lenient(charys)])
   ).

test("charys, strict, type exception") :-
   forall(
      member(X,[foo, "foo", [1|2]]),
      catch(check_that(X,[strict(charys)]),error(check(type,_,_,_),_),true)
   ).

test("charys, lenient, failure in case of char/code mix",fail) :-
   check_that([1,2,a,3],[lenient(charys)]).

test("charys, strict, forany exception in case of char/code mix",error(check(forany,_,_,_),_)) :-
   check_that([1,2,a,3],[strict(charys)]).

test("charys, strict, forany exception (list of codes, but one code is not in range)") :-
   catch(check_that([1,-1,2],[strict(charys)]),error(check(forany,_,_,_),_),true).

test("charys, strict, forany exception (list of 1-character strings, which is not chary)") :-
   catch(check_that(["a","b"],[strict(charys)]),error(check(forany,_,_,_),_),true).

test("charys, fullylenient, fails because of uninstantiated member",fail) :-
   check_that([1,2,3,_,4,5,6],[fullylenient(charys)]).

test("charys, strict, uninstantiated exception because of uninstantiated member",error(check(too_little_instantiation,_,_,_))) :-
   check_that([1,2,3,_,4,5,6],[strict(charys)]).

test("charys, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[strict(charys)]).

test("charys, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[lenient(charys)]).

test("charys, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[fullylenient(charys)]).

:- end_tests(check_that_using_charys).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_forX).

% --- forall: the single element X must pass all of the checks in the list that is "Check"

test("forall, success") :-
   check_that(12.0,[lenient(forall([float,inty,posnum]))]).

test("forall, empty list of checks, success") :-
   check_that(12.0,[]).

test("forall, failure",fail) :-
   check_that(-12.0,[lenient(forall([float,inty,posnum]))]).

test("forall, strict, failure",error(check(domain,_,_,_))) :-
   check_that(-12.0,[strict(forall([float,inty,posnum]))]). % fails on posnum

% --- forany: the single element X must pass at least one of the checks in the list that is "Check"

test("forany, success") :-
   check_that(-12,[lenient(forany([float,inty,posnum]))]).

test("forany, empty list of checks, failure",fail) :-
   check_that(-12,[lenient(forany([]))]).

test("forany, failure",fail) :-
   check_that(-12,[lenient(forany([float,proper_rational,posnum]))]).

test("forany, strict, failure",error(check(forany,_,_,_))) :-
   check_that(-12,[strict(forany([float,proper_rational,posnum]))]).

% --- fornone: the single element X must not pass any of the checks in the list that is "Check"

test("fornone, success") :-
   check_that(foo,[lenient(fornone([float,inty,posnum]))]).

test("fornone, empty list of checks, failure", fail) :-
   check_that(foo,[lenient(fornone([]))]).

test("fornone, failure",fail) :-
   check_that(12.0,[lenient(fornone([integer,negnum,inty]))]).

test("fornone, strict, failure",error(check(fornone,_,_,_))) :-
   check_that(12.0,[strict(fornone([integer,negnum,inty]))]).

:- end_tests(check_that_using_forX).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_passX).

% --- passall: all of the elements in the list that is X must pass the single "check"

test("passall, not a list as X", error(check(type,_,_,_),_)) :-
   check_that(foo,[lenient(passall(integer))]).

test("passall, empty list as X, success") :-
   check_that([],[lenient(passall(integer))]).

test("passall, success (all elements are inty)") :-
   check_that([1, 0, 3.0],[lenient(passall(inty))]).

test("passall, failure (some elements are not inty)",fail) :-
   check_that([1, 0, 3.1],[lenient(passall(inty))]).

test("passall, strict, failure, throws (type error on encountering 3.1)", error(check(type,_,_,_),_)) :-
   check_that([1, 0, 3.1],[strict(passall(pos0inty))]).

test("passall, strict, failure, throws (domain error on encountering -1)", error(check(domain,_,_,_),_)) :-
   check_that([1, -1, 3.1],[strict(passall(pos0inty))]).

% --- passany: at least one of the elements in the list that is X must pass the single "check"

test("passany, not a list as X", error(check(type,_,_,_),_)) :-
   check_that(foo,[lenient(passany(integer))]).

test("passany, empty list as X, failure", fail) :-
   check_that([],[lenient(passany(integer))]).

test("passany, success (at least one element is inty)") :-
   check_that([foo, g(x), 3.0],[lenient(passany(inty))]).

test("passany, failure (none of the elements is inty)",fail) :-
   check_that([foo, g(x), 3.1],[lenient(passany(inty))]).

test("passany, strict, failure (none of the elements is posinty, they are all out-of-type)", error(check(passany,_,_,_),_)) :-
   check_that([foo, g(x), 3.1],[strict(passany(inty))]).

test("passany, strict, failure (one of the elements is nonground)") :-
   check_that([1, g(_), 1],[strict(passany(inty))]).

test("passany, strict, failure (none of the elements is posinty, they are all out-of-domain)", error(check(passany,_,_,_),_)) :-
   check_that([-1, -2, 0],[strict(passany(posinty))]).

test("passany, strict, failure (none of the elements is posinty, and some are uninstantiated)", error(check(too_little_instantiation,_,_,_),_)) :-
   check_that([-1, _, _],[strict(passany(posinty))]).

test("passany, strict, failure (none of the elements is posinty, and some are uninstantiated)", error(check(too_little_instantiation,_,_,_),_)) :-
   check_that([-1, _, _],[strict(passany(posinty))]).

% --- passnone: none of the elements in the list that is X may pass the single "check"

test("passnone, not a-list-as-X", error(check(type,_,_,_),_)) :-
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

test("passnone, strict, failure: there is a nonground") :-
   check_that([foo, g(_), 3.1],[strict(passnone(inty))]).

test("passnone, strict, failure: there is an uninstantiated", error(check(too_little_instantiation,_,_,_),_)) :-
   check_that([foo, _, 3.1],[strict(passnone(inty))]).

:- end_tests(check_that_using_passX).

% -------------------------------------------------------------------

:- begin_tests(check_that_multicondition).

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

:- end_tests(check_that_multicondition).


