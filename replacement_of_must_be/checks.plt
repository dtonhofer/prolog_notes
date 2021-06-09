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
   check_that(_,[tuned(var)]).

test("var, failure", fail) :-
   check_that(foo,[tuned(var)]).

test("var, failure, throw", error(check(too_much_instantiation,_,_,_))) :-
   check_that(foo,[hard(var)]).

:- end_tests(check_that_using_var).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonvar).

test("nonvar, success") :-
   check_that(foo,[tuned(nonvar)]).

test("nonvar, failure", fail) :-
   check_that(_,[tuned(nonvar)]).

test("nonvar, failure, throw", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(nonvar)]).

:- end_tests(check_that_using_nonvar).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonground).

test("nonground, success") :-
   forall(
      member(X,[_,f(_)]),
      check_that(X,[tuned(nonground)])
   ).

test("nonground, failure", fail) :-
   check_that(foo,[tuned(nonground)]).

test("nonground, failure, throw", error(check(domain,_,_,_))) :-
   check_that(foo,[hard(nonground)]).

:- end_tests(check_that_using_nonground).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_ground).

test("ground, success") :-
   check_that(foo,[tuned(ground)]).

test("ground, failure") :-
   forall(
      member(X,[_,f(_)]),
      \+ check_that(X,[tuned(ground)])
   ).

test("ground, failure on var, throw", error(check(domain,_,_,_))) :-
   check_that(_,[hard(ground)]).

test("ground, failure on nonvar, throw", error(check(domain,_,_,_))) :-
   check_that(f(_),[hard(ground)]).

:- end_tests(check_that_using_ground).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_atom).

test("atom, success") :-
   check_that(foo,[tuned(atom)]).

test("atom, lenient, failure") :-
   forall(
      member(X,[1,f(g),"lol",f(_)]),
      \+ check_that(X,[tuned(atom)])
   ).

test("atom, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(444,[hard(atom)]).

test("atom, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(atom)]).

test("atom, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(atom)]).

test("atom, fully lenient, fail on nonground X", fail) :-
   check_that(_,[smooth(atom)]).

:- end_tests(check_that_using_atom).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_atomic).

test("atomic, success") :-
   forall(
      member(X,[1,foo,"lol"]),
      check_that(X,[tuned(atomic)])
   ).

test("atomic, failure",fail) :-
   check_that(f(g(x)),[tuned(atomic)]).

test("atomic, strict failure, throw type error", error(check(type,_,_,_))) :-
   check_that(f(g),[hard(atomic)]).

test("atomic, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(atomic)]).

test("atomic, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(atomic)]).

test("atomic, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(atomic)]).

:- end_tests(check_that_using_atomic).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_compound).

test("compound, success") :-
   forall(
      member(X,[f(g),f(_)]),
      check_that(X,[tuned(compound)])
   ).

test("compound, failure") :-
   forall(
      member(X,[1,foo,"lol"]),
      \+check_that(X,[tuned(compound)])
   ).

test("compound, strict failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(compound)]).

test("compound, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(compound)]).

test("compound, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(compound)]).

test("compound, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(compound)]).

:- end_tests(check_that_using_compound).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_boolean).

test("boolean, success") :-
   forall(
      member(X,[false,true]),
      check_that(X,[tuned(boolean)])
   ).

test("boolean, failure") :-
   forall(
      member(X,[1,0,y,n,yes,no,f(x),f(_),alpha,[],'',"","false","true"]),
      \+check_that(X,[tuned(boolean)])
   ).

test("boolean, strict, type exception") :-
   forall(
      member(X,[1,0,f(x),f(_),"false","true"]),
      catch(check_that(X,[hard(boolean)]),error(check(type,_,_,_),_),true)
   ).

test("boolean, strict, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[hard(boolean)]),error(check(domain,_,_,_),_),true)
   ).

test("boolean, strict failure, throw type error", error(check(type,_,_,_))) :-
   check_that(g(g),[hard(boolean)]).

test("boolean, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(boolean)]).

test("boolean, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(boolean)]).

test("boolean, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(boolean)]).

:- end_tests(check_that_using_boolean).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_stringy_typeid).

test("stringy_typeid, success") :-
   forall(
      member(X,[atom,string]),
      check_that(X,[tuned(stringy_typeid)])
   ).

test("stringy_typeid, failure") :-
   forall(
      member(X,[foo,"","atom","string",1,0]),
      \+check_that(X,[tuned(stringy_typeid)])
   ).

test("stringy_typeid, strict, type exception") :-
   forall(
      member(X,[1,0,f(x),"atom","string"]),
      catch(check_that(X,[hard(stringy_typeid)]),error(check(type,_,_,_),_),true)
   ).

test("stringy_typeid, strict, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[hard(stringy_typeid)]),error(check(domain,_,_,_),_),true)
   ).

test("stringy_typeid, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(stringy_typeid)]).

test("stringy_typeid, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(stringy_typeid)]).

test("stringy_typeid, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(stringy_typeid)]).


:- end_tests(check_that_using_stringy_typeid).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_chary_typeid).

test("chary_typeid, success") :-
   forall(
      member(X,[char,code]),
      check_that(X,[tuned(chary_typeid)])
   ).

test("chary_typeid, failure") :-
   forall(
      member(X,[foo,"","char","code",1,0]),
      \+check_that(X,[tuned(chary_typeid)])
   ).

test("chary_typeid, strict, type exception") :-
   forall(
      member(X,[1,0,f(x),"char","code"]),
      catch(check_that(X,[hard(chary_typeid)]),error(check(type,_,_,_),_),true)
   ).

test("chary_typeid, strict, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[hard(chary_typeid)]),error(check(domain,_,_,_),_),true)
   ).

test("chary_typeid, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(chary_typeid)]).

test("chary_typeid, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(chary_typeid)]).

test("chary_typeid, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(chary_typeid)]).

:- end_tests(check_that_using_chary_typeid).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pair).

test("pair, success") :-
   forall(
      member(X,[a-b,1-2,_-_]),
      check_that(X,[tuned(pair)])
   ).

test("pair, failure") :-
   forall(
      member(X,[f(x),-(1,2,3),pair,'-']),
      \+check_that(X,[tuned(pair)])
   ).

test("pair, strict, type exception") :-
   forall(
      member(X,[1,0,hello]),
      catch(check_that(X,[hard(pair)]),error(check(type,_,_,_),_),true)
   ).

test("pair, strict, domain exception") :-
   forall(
      member(X,[f(x),-(_,_,_),-(1,2,3)]),
      catch(check_that(X,[hard(pair)]),error(check(domain,_,_,_),_),true)
   ).

test("pair, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(pair)]).

test("pair, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(pair)]).

test("pair, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(pair)]).

:- end_tests(check_that_using_pair).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_string).

test("string, success") :-
   forall(
      member(X,["","foo"]),
      check_that(X,[tuned(string)])
   ).

test("string, failure") :-
   forall(
      member(X,[1,foo,f(x)]),
      \+check_that(X,[tuned(string)])
   ).

test("string, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(string)]).

test("string, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(string)]).

test("string, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(string)]).

test("string, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(string)]).

:- end_tests(check_that_using_string).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_stringy).

test("stringy, success") :-
   forall(
      member(X,["","foo",'','foo']),
      check_that(X,[tuned(stringy)])
   ).

test("stringy, failure") :-
   forall(
      member(X,[1,f(x)]),
      \+check_that(X,[tuned(stringy)])
   ).

test("stringy, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(444,[hard(stringy)]).

test("stringy, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(stringy)]).

test("stringy, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(stringy)]).

test("stringy, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(stringy)]).

:- end_tests(check_that_using_stringy).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonempty_stringy).

test("nonempty stringy, success") :-
   forall(
      member(X,["foo",foo]),
      check_that(X,[tuned(nonempty_stringy)])
   ).

test("nonempty stringy, failure") :-
   forall(
      member(X,[1,"",'',f(x)]),
      \+check_that(X,[tuned(nonempty_stringy)])
   ).

test("nonempty stringy, stringy, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(444,[hard(nonempty_stringy)]).

test("nonempty stringy, stringy, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that("",[hard(nonempty_stringy)]).

test("nonempty stringy, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(nonempty_stringy)]).

test("nonempty stringy, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(nonempty_stringy)]).

test("nonempty stringy, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(nonempty_stringy)]).

:- end_tests(check_that_using_nonempty_stringy).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_char).

test("char, success") :-
   forall(
      member(X,[a,b,c,d]),
      check_that(X,[tuned(char)])
   ).

test("char, failure") :-
   forall(
      member(X,[1,f(x),ab,"a","b","",'',[]]),
      \+check_that(X,[tuned(char)])
   ).

test("char, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(444,[hard(char)]).

test("char, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(foo,[hard(char)]).

test("char, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(char)]).

test("char, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(char)]).

test("char, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(char)]).

:- end_tests(check_that_using_char).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_code).

test("code, success") :-
   forall(
      member(X,[0,1,2,3,0x10FFFF]),
      check_that(X,[tuned(code)])
   ).

test("code, failure") :-
   forall(
      member(X,[f(x),ab,a,"a",-1,1.0,0xFFFFFFF]),
      \+check_that(X,[tuned(code)])
   ).

test("code, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(code)]).

test("code, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(-1,[hard(code)]).

test("code, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(code)]).

test("code, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(code)]).

test("code, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(code)]).

:- end_tests(check_that_using_code).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_chary).

test("chary, success") :-
   forall(
      member(X,[0,1,2,3,a,b,c]),
      check_that(X,[tuned(chary)])
   ).

test("chary, failure") :-
   forall(
      member(X,[f(x),ab,"a",'',"",-1,[]]),
      \+check_that(X,[tuned(chary)])
   ).

test("chary, strict, failure, type exception", error(check(type,_,_,_))) :-
   check_that("foo",[hard(chary)]).

test("chary, strict, failure, domain exception") :-
   forall(
      member(X,[foo,-1]),
      catch(check_that(X,[hard(chary)]),error(check(domain,_,_,_),_),true)
   ).

test("chary, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(chary)]).

test("chary, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(chary)]).

test("chary, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(chary)]).

:- end_tests(check_that_using_chary).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_number).

test("number, success") :-
   NaN is nan,
   Inf is -1.0Inf,
   forall(
      member(X,[0,1,-1,1.0,-1.0,1r12,-1r12,NaN,Inf,-0.0]),
      check_that(X,[tuned(number)])
   ).

test("number, failure") :-
   forall(
      member(X,[a,"a",'0']),
      \+check_that(X,[tuned(number)])
   ).

test("number, istrict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(number)]).

test("number, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(number)]).

test("number, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(number)]).

test("number, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(number)]).

:- end_tests(check_that_using_number).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float).

test("float, success") :-
   NaN is nan,
   Inf is -1.0Inf,
   forall(
      member(X,[1.0,-1.0,NaN,Inf,-0.0,3.1415]),
      check_that(X,[tuned(float)])
   ).

test("float, failure") :-
   forall(
      member(X,[1,1r12,foo]),
      \+check_that(X,[tuned(float)])
   ).

test("float, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float)]).

test("float, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(float)]).

test("float, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(float)]).

test("float, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(float)]).

:- end_tests(check_that_using_float).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_integer).

test("integer, success") :-
   forall(
      member(X,[1,0,-1]),
      check_that(X,[tuned(integer)])
   ).

test("integer, failure") :-
   forall(
      member(X,[0.0,1r12,-1.0]),
      \+check_that(X,[tuned(integer)])
   ).

test("integer, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(integer)]).

test("integer, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(integer)]).

test("integer, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(integer)]).

test("integer, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(integer)]).

:- end_tests(check_that_using_integer).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_rational).

test("rational, success") :-
   forall(
      member(X,[1,0,-1,1r12,-1r12]),
      check_that(X,[tuned(rational)])
   ).

test("rational, failure") :-
   NaN is nan,
   Inf is -1.0Inf,
   forall(
      member(X,[0.0,3.1415,foo,NaN,Inf]),
      \+check_that(X,[tuned(rational)])
   ).

test("rational, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(rational)]).

test("rational, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(rational)]).

test("rational, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(rational)]).

test("rational, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(rational)]).

:- end_tests(check_that_using_rational).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonint_rational).

test("nonint_rational, success") :-
   forall(
      member(X,[1r12,-1r12]),
      check_that(X,[tuned(nonint_rational)])
   ).

test("nonint_rational, failure") :-
   forall(
      member(X,[0.0,3.1415,foo,1,2,3,4,5]),
      \+check_that(X,[tuned(nonint_rational)])
   ).

test("nonint_rational, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(nonint_rational)]).

test("nonint_rational, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(777,[hard(nonint_rational)]).

test("nonint_rational, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(nonint_rational)]).

test("nonint_rational, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(nonint_rational)]).

test("nonint_rational, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(nonint_rational)]).

:- end_tests(check_that_using_nonint_rational).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_negnumber).

test("negnumber, success") :-
   MinusInf is -1.0Inf,
   forall(
      member(X,[-1,-1.0,-1r12,MinusInf]),
      check_that(X,[tuned(negnumber)])
   ).

test("negnumber, failure") :-
   PlusInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,0,-0.0,0.0,1,1.0,1r12,NaN,PlusInf]),
      \+check_that(X,[tuned(negnumber)])
   ).

test("negnumber, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(negnumber)]).

test("negnumber, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(+1,[hard(negnumber)]).

test("negnumber, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(negnumber)]).

test("negnumber, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(negnumber)]).

test("negnumber, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(negnumber)]).

:- end_tests(check_that_using_negnumber).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posnumber).

test("posnumber, success") :-
   PlusInf is +1.0Inf,
   forall(
      member(X,[1,1.0,1r12,PlusInf]),
      check_that(X,[tuned(posnumber)])
   ).

test("posnumber, failure") :-
   MinusInf is -1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,0,+0.0,-1,-1.0,-1r12,NaN,MinusInf]),
      \+check_that(X,[tuned(posnumber)])
   ).

test("posnumber, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(posnumber)]).

test("posnumber, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(-1,[hard(posnumber)]).

test("posnumber, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(posnumber)]).

test("posnumber, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(posnumber)]).

test("posnumber, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(posnumber)]).

:- end_tests(check_that_using_posnumber).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_non0number).

test("non0number, success") :-
   forall(
      member(X,[1,1.0,-1.0,-1,1r12]),
      check_that(X,[tuned(non0number)])
   ).

test("non0number, failure") :-
   forall(
      member(X,[foo,"foo",0,0.0,-0.0,0r1]),
      \+check_that(X,[tuned(non0number)])
   ).

test("non0number, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(non0number)]).

test("non0number, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(0.0,[hard(non0number)]).

test("non0number, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(non0number)]).

test("non0number, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(non0number)]).

test("non0number, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(non0number)]).

:- end_tests(check_that_using_non0number).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_nan).

test("float_not_nan, success") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   forall(
      member(X,[NegInf,-1.0,0.0,-1.0,PosInf]),
      check_that(X,[tuned(float_not_nan)])
   ).

test("float_not_nan, failure") :-
   NaN is nan,
   forall(
      member(X,[foo,"foo",1,NaN]),
      \+check_that(X,[tuned(float_not_nan)])
   ).

test("float_not_nan, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float_not_nan)]).

test("float_not_nan, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   NaN is nan,
   check_that(NaN,[hard(float_not_nan)]).

test("float_not_nan, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(float_not_nan)]).

test("float_not_nan, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(float_not_nan)]).

test("float_not_nan, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(float_not_nan)]).

:- end_tests(check_that_using_float_not_nan).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_inf).

test("float_not_inf, success") :-
   NaN is nan,
   forall(
      member(X,[-1.0,0.0,-1.0,NaN]),
      check_that(X,[tuned(float_not_inf)])
   ).

test("float_not_inf, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   forall(
      member(X,[foo,"foo",1,NegInf,PosInf]),
      \+check_that(X,[tuned(float_not_inf)])
   ).

test("float_not_inf, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float_not_inf)]).

test("float_not_inf, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   NegInf is -1.0Inf,
   check_that(NegInf,[hard(float_not_inf)]).

test("float_not_inf, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(float_not_inf)]).

test("float_not_inf, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(float_not_inf)]).

test("float_not_inf, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(float_not_inf)]).

:- end_tests(check_that_using_float_not_inf).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_neginf).

test("float_not_neginf, success") :-
   NaN is nan,
   PosInf is +1.0Inf,
   forall(
      member(X,[-1.0,0.0,-1.0,NaN,PosInf]),
      check_that(X,[tuned(float_not_neginf)])
   ).

test("float_not_neginf, failure") :-
   NegInf is -1.0Inf,
   forall(
      member(X,[foo,"foo",1,NegInf]),
      \+check_that(X,[tuned(float_not_neginf)])
   ).

test("float_not_neginf, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float_not_neginf)]).

test("float_not_neginf, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   NegInf is -1.0Inf,
   check_that(NegInf,[hard(float_not_neginf)]).

test("float_not_neginf, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(float_not_neginf)]).

test("float_not_neginf, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(float_not_neginf)]).

test("float_not_neginf, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(float_not_neginf)]).

:- end_tests(check_that_using_float_not_neginf).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_posinf).

test("float_not_posinf, success") :-
   NaN is nan,
   NegInf is -1.0Inf,
   forall(
      member(X,[-1.0,0.0,-1.0,NaN,NegInf]),
      check_that(X,[tuned(float_not_posinf)])
   ).

test("float_not_posinf, failure") :-
   PosInf is +1.0Inf,
   forall(
      member(X,[foo,"foo",1,PosInf]),
      \+check_that(X,[tuned(float_not_posinf)])
   ).

test("float_not_posinf, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float_not_posinf)]).

test("float_not_posinf, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   PosInf is +1.0Inf,
   check_that(PosInf,[hard(float_not_posinf)]).

test("float_not_posinf, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(float_not_posinf)]).

test("float_not_posinf, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(float_not_posinf)]).

test("float_not_posinf, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(float_not_posinf)]).

:- end_tests(check_that_using_float_not_posinf).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_negint).

test("negint, success") :-
   forall(
      member(X,[-2,-1,-6r3]),
      check_that(X,[tuned(negint)])
   ).

test("negint, failure") :-
   forall(
      member(X,[foo,"foo",1,0,-1.0,-1r12]),
      \+check_that(X,[tuned(negint)])
  ).

test("negint, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(negint)]).

test("negint, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(1,[hard(negint)]).

test("negint, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(negint)]).

test("negint, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(negint)]).

test("negint, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(negint)]).

:- end_tests(check_that_using_negint).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posint).

test("posint, success") :-
   forall(
      member(X,[2,1,6r3]),
      check_that(X,[tuned(posint)])
   ).

test("posint, failure") :-
   forall(
      member(X,[foo,"foo",-1,0,1.0,1r12]),
      \+check_that(X,[tuned(posint)])
   ).

test("posint, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(posint)]).

test("posint, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(-1,[hard(posint)]).

test("posint, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(posint)]).

test("posint, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(posint)]).

test("posint, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(posint)]).

:- end_tests(check_that_using_posint).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neg0int).

test("neg0int, success") :-
   forall(
      member(X,[-2,-1,-6r3,0]),
      check_that(X,[tuned(neg0int)])
   ).

test("neg0int, failure") :-
   forall(
      member(X,[foo,"foo",1,-1.0,-1r12,0.0]),
      \+check_that(X,[tuned(neg0int)])
   ).

test("neg0int, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(neg0int)]).

test("neg0int, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(1,[hard(neg0int)]).

test("neg0int, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(neg0int)]).

test("neg0int, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(neg0int)]).

test("neg0int, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(neg0int)]).

:- end_tests(check_that_using_neg0int).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pos0int).

% --- positive-or-zero integer

test("pos0int, success") :-
   forall(
      member(X,[2,1,6r3,0]),
      check_that(X,[tuned(pos0int)])
   ).

test("pos0int, failure") :-
   forall(
      member(X,[foo,"foo",-1,1.0,1r12,0.0]),
      \+check_that(X,[tuned(pos0int)])
   ).

test("pos0int, strict, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(pos0int)]).

test("pos0int, strict, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(-1,[hard(pos0int)]).

test("pos0int, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(pos0int)]).

test("pos0int, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(pos0int)]).

test("pos0int, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(pos0int)]).

:- end_tests(check_that_using_pos0int).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_inty).

test("inty, success") :-
   forall(
      member(X,[1, 0, -1, 1.0, 0.0, -0.0, 7777777, 7777777.0]),
      check_that(X,[tuned(inty)])
   ).

test("inty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001]),
      \+check_that(X,[tuned(inty)])
   ).

test("inty, strict, type exception") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, NegInf, PosInf, NaN, -1.5, +1.5, 0.00000001]),
      catch(check_that(X,[hard(inty)]),error(check(type,_,_,_),_),true)
   ).

test("inty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(inty)]).

test("inty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(inty)]).

test("inty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(inty)]).

:- end_tests(check_that_using_inty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posinty).

test("posinty, success") :-
   forall(
      member(X,[1, 1.0, 7777777, 7777777.0]),
      check_that(X,[tuned(posinty)])
   ).

test("posinty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, 0, 0.0, -0.0, NegInf, PosInf, NaN, 0.00000001, -1000, -1000.0]),
      \+check_that(X,[tuned(posinty)])
   ).

test("posinty, strict, type exception") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001]),
      catch(check_that(X,[hard(posinty)]),error(check(type,_,_,_),_),true)
   ).

test("posinty, strict, domain exception") :-
   forall(
      member(X,[-1000, -1000.0]),
      catch(check_that(X,[hard(posinty)]),error(check(domain,_,_,_),_),true)
   ).

test("posinty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(posinty)]).

test("posinty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(posinty)]).

test("posinty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(posinty)]).

:- end_tests(check_that_using_posinty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neginty).

test("neginty, success") :-
   forall(
      member(X,[-1, -1.0, -7777777, -7777777.0]),
      check_that(X,[tuned(neginty)])
   ).

test("neginty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, 0, 0.0, -0.0, NegInf, PosInf, NaN, 0.00000001, 1000, 1000.0]),
      \+check_that(X,[tuned(neginty)])
   ).

test("neginty, strict, type exception") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001 ]),
      catch(check_that(X,[hard(neginty)]),error(check(type,_,_,_),_),true)
   ).

test("neginty, strict, domain exception") :-
   forall(
      member(X,[0, 0.0, -0.0, 1000, 1000.0]),
      catch(check_that(X,[hard(neginty)]),error(check(domain,_,_,_),_),true)
   ).

test("neginty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(neginty)]).

test("neginty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(neginty)]).

test("neginty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(neginty)]).

:- end_tests(check_that_using_neginty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pos0inty).

test("pos0inty, success") :-
   forall(
      member(X,[1, 1.0, 7777777, 7777777.0, 0.0, 0, -0.0]),
      check_that(X,[tuned(pos0inty)])
   ).

test("pos0inty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001, -1000, -1000.0]),
      \+check_that(X,[tuned(pos0inty)])
   ).

test("pos0inty, strict, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(pos0inty)]).

test("pos0inty, strict, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(-1.0,[hard(pos0inty)]).

test("pos0inty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(pos0inty)]).

test("pos0inty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(pos0inty)]).

test("pos0inty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(pos0inty)]).

:- end_tests(check_that_using_pos0inty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neg0inty).

test("neg0inty, success") :-
   forall(
      member(X,[-1, -1.0, -7777777, -7777777.0, 0, 0.0, -0.0]),
      check_that(X,[tuned(neg0inty)])
   ).

test("neg0inty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001, 1000, 1000.0]),
      \+check_that(X,[tuned(neg0inty)])
   ).

test("neg0inty, strict, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(neg0inty)]).

test("neg0inty, strict, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(1.0,[hard(neg0inty)]).

test("neg0inty, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(neg0inty)]).

test("neg0inty, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(neg0inty)]).

test("neg0inty, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(neg0inty)]).

:- end_tests(check_that_using_neg0inty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_negfloat).

test("negfloat, success") :-
   NegInf is -1.0Inf,
   forall(
      member(X,[NegInf, -2.0, -1.0]),
      check_that(X,[tuned(negfloat)])
   ).

test("negfloat, failure") :-
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1.0, -1,0, -0.0, 0.0, PosInf, NaN]),
      \+check_that(X,[tuned(negfloat)])
   ).

test("negfloat, strict, type exception") :-
   forall(
      member(X,[foo, "foo", 1r12, 0]),
      catch(check_that(X,[hard(negfloat)]),error(check(type,_,_,_),_),true)
   ).

test("negfloat, strict, domain exception") :-
   NaN is nan,
   PosInf is +1.0Inf,
   forall(
      member(X,[0.0, -0.0, 1000.0, PosInf, NaN]),
      catch(check_that(X,[hard(negfloat)]),error(check(domain,_,_,_),_),true)
   ).

test("negfloat, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(negfloat)]).

test("negfloat, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(negfloat)]).

test("negfloat, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(negfloat)]).

:- end_tests(check_that_using_negfloat).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posfloat).

test("posfloat, success") :-
   PosInf is +1.0Inf,
   forall(
      member(X,[PosInf,2.0,1.0]),
      check_that(X,[tuned(posfloat)])
   ).

test("posfloat, failure") :-
   NegInf is -1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,"foo",1,-1.0,0,0.0,1r12,NegInf,NaN]),
      \+check_that(X,[tuned(posfloat)])
   ).

test("posfloat, strict, type exception") :-
   forall(
      member(X,[foo, "foo", 1r12, 0]),
      catch(check_that(X,[hard(posfloat)]),error(check(type,_,_,_),_),true)
   ).

test("posfloat, strict, domain exception") :-
   NaN is nan,
   NegInf is -1.0Inf,
   forall(
      member(X,[0.0, -0.0, -1000.0, NegInf, NaN]),
      catch(check_that(X,[hard(posfloat)]),error(check(domain,_,_,_),_),true)
   ).

test("posfloat, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(posfloat)]).

test("posfloat, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(posfloat)]).

test("posfloat, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(posfloat)]).

:- end_tests(check_that_using_posfloat).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neg0float).

test("neg0float, success") :-
   NegInf is -1.0Inf,
   forall(
      member(X,[NegInf, -2.0, -1.0, 0.0]),
      check_that(X,[tuned(neg0float)])
   ).

test("neg0float, failure") :-
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, 0, 1000, 1000.0, 1.0, PosInf, NaN]),
      \+check_that(X,[tuned(neg0float)])
   ).

test("neg0float, strict, type exception") :-
   forall(
      member(X,[foo, "foo", 1r12, 0, 1000]),
      catch(check_that(X,[hard(neg0float)]),error(check(type,_,_,_),_),true)
   ).

test("neg0float, strict, domain exception") :-
   NaN is nan,
   PosInf is +1.0Inf,
   forall(
      member(X,[0.0, -0.0, 1000.0, 1.0, PosInf,NaN]),
      catch(check_that(X,[hard(neg0float)]),error(check(domain,_,_,_),_),true)
   ).

test("neg0float, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(neg0float)]).

test("neg0float, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(neg0float)]).

test("neg0float, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(neg0float)]).

:- end_tests(check_that_using_neg0float).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pos0float).

test("pos0float, success") :-
   PosInf is +1.0Inf,
   forall(
      member(X,[PosInf,2.0,1.0,0.0]),
      check_that(X,[tuned(pos0float)])
   ).

test("pos0float, failure") :-
   NegInf is -1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,"foo",1,-1.0,0,1r12,NegInf,NaN]),
      \+check_that(X,[tuned(pos0float)])
   ).

test("pos0float, strict, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(pos0float)]).

test("pos0float, strict, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(-1.0,[hard(pos0float)]).

test("pos0float, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(pos0float)]).

test("pos0float, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(pos0float)]).

test("pos0float, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(pos0float)]).

:- end_tests(check_that_using_pos0float).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_list).

test("list, success") :-
   forall(
      member(X,[ [], [_], [1,2,3], [_,_,_] ]),
      check_that(X,[tuned(list)])
   ).

test("list, failure") :-
   forall(
      member(X,["",[1|2],[1|_]]),
      \+check_that(X,[tuned(list)])
   ).

test("list, failure, strict, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(list)]).

test("list, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(list)]).

test("list, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(list)]).

test("list, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(list)]).

:- end_tests(check_that_using_list).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonempty_list).

test("nonempty list, success") :-
   forall(
      member(X,[[1,2,3],[_,_,_]]),
      check_that(X,[tuned(nonempty_list)])
   ).

test("nonempty list, failure") :-
   forall(
      member(X,["",[1|2],'',foo,123,"",[]]),
      \+check_that(X,[tuned(nonempty_list)])
   ).

test("nonempty list, strict, throw type exception") :-
   forall(
      member(X,[foo, "foo", [1|2]]),
      catch(check_that(X,[hard(nonempty_list)]),error(check(type,_,_,_),_),true)
   ).

test("nonempty list, strict, throw domain exception") :-
   catch(check_that([],[hard(nonempty_list)]),error(check(domain,_,_,_),_),true).

test("nonempty_list, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(nonempty_list)]).

test("nonempty_list, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(nonempty_list)]).

test("nonempty_list, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(nonempty_list)]).

:- end_tests(check_that_using_nonempty_list).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_member).

test("member, success #1") :-
   check_that(a,[tuned(member([a,b,c]))]).

test("member, success #2") :-
   check_that(a,[tuned(member([a,X,c]))]),
   assertion(var(X)).

test("member, success #3") :-
   check_that(v,[tuned(member([a,_,c]))]).

test("member, failure #1",fail) :-
   check_that(v,[tuned(member([a,b,c]))]).

test("member, failure #2",fail) :-
   check_that(v,[tuned(member([]))]).

test("member, X nonground",error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(member([a,b,c]))]).

test("member, argument to member/1 term not a proper list #1",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[tuned(member(_))]). % already caught in well-formedness check

test("member, argument to member/1 term not a proper list #2",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[tuned(member(foo))]).  % already caught in well-formedness check

test("member, argument to member/1 term not a proper list #3",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[tuned(member([a,b,c|_]))]).  % already caught in well-formedness check

:- end_tests(check_that_using_member).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_random).

test("random until failure",error(check(random,_,_,_))) :-
   repeat,
   check_that(a,[hard(random(0.5))]),
   fail.

:- end_tests(check_that_using_random).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_dict).

test("dict, success") :-
   forall(
      member(X,[_{},foo{},bar{a:1,b:2}]),
      check_that(X,[tuned(dict)])
   ).

test("dict, failure") :-
   forall(
      member(X,["",foo,foo(a,b)]),
      \+check_that(X,[tuned(dict)])
   ).

test("dict, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(dict)]).

test("dict, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(dict)]).

test("dict, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(dict)]).

:- end_tests(check_that_using_dict).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_cyclic).

test("cyclic, success") :-
   A=h(a,b,c,A),
   B=[a,b,c|B],
   C=[1,2|B],
   forall(
      member(X,[A,B,C]),
      check_that(X,[tuned(cyclic)])
   ).

test("cyclic, failure") :-
   forall(
      member(X,['',[1,2,3],foo]),
      \+check_that(X,[tuned(cyclic)])
   ).

test("unbound variable is not cyclic",fail) :-
   check_that(_,[tuned(cyclic)]).

:- end_tests(check_that_using_cyclic).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_acyclic_now).

test("tentatively acyclic, success") :-
   forall(
      member(X,['',[1,2,3],foo,[1,2,3|_]]),
      check_that(X,[tuned(acyclic_now)])
   ).

test("tentatively acyclic, failure") :-
   A=h(a,b,c,A),
   B=[a,b,c|B],
   C=[1,2|B],
   forall(
      member(X,[A,B,C]),
      \+check_that(X,[tuned(acyclic_now)])
   ).

test("unbound variable is tentatively acyclic") :-
   check_that(_,[tuned(acyclic_now)]).

:- end_tests(check_that_using_acyclic_now).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_acyclic_forever).

test("acyclic_forever, success") :-
   forall(
      member(X,['',[1,2,3],foo]),
      check_that(X,[tuned(acyclic_forever)])
   ).

test("acyclic_forever, failure") :-
   A=h(a,b,c,A), % definitely cyclic
   B=[a,b,c|_],  % may become cyclic
   forall(
      member(X,[A,B]),
      \+check_that(X,[tuned(acyclic_forever)])
   ).

test("unbound variable is not 'acyclic forever'",fail) :-
   check_that(_,[tuned(acyclic_forever)]).

:- end_tests(check_that_using_acyclic_forever).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_stream).

test("stream atoms (aliases), success") :-
   forall(
      member(X,[user_input, user_output, user_error, current_input, current_output]),
      check_that(X,[tuned(stream)])
   ).

test("stream atoms (aliases), failure") :-
   forall(
      member(X,[foo,bar,'']),
      catch(check_that(X,[hard(stream)]),error(check(domain,_,_,_),_),true)
   ).

test("stream, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(stream)]).

test("stream, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(stream)]).

test("stream, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(stream)]).

:- end_tests(check_that_using_stream).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_chars).

test("chars, success") :-
   forall(
      member(X,[[],[a,b,c]]),
      check_that(X,[tuned(chars)])
   ).

test("chars, failure (includes things that are not a list)") :-
   forall(
      member(X,[[1,2,3], ["a","b"], [ab,cd], [1|2], foo ]),
      \+check_that(X,[tuned(chars)])
   ).

test("chars, strict, type exception (not a list, not a list of atoms)") :-
   forall(
      member(X,[foo, "foo", [1|2], ["a","b"], [1, 2]]),
      catch(check_that(X,[hard(chars)]),error(check(type,_,_,_),_),true)
   ).

test("chars, strict, domain exception (list of atoms, but one atom is not of length 1)") :-
   catch(check_that([a,bb,c],[hard(chars)]),error(check(domain,_,_,_),_),true).

test("chars, fullylenient, fails because of uninstantiated member",fail) :-
   check_that([a,b,c,_,d,e,f],[smooth(chars)]).

test("chars, strict, uninstantiated exception because of uninstantiated member",error(check(too_little_instantiation,_,_,_))) :-
   check_that([a,b,c,_,d,e,f],[hard(chars)]).

test("chars, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(chars)]).

test("chars, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(chars)]).

test("chars, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(chars)]).

:- end_tests(check_that_using_chars).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_codes).

test("codes, success") :-
   forall(
      member(X,[[],[1,2,3]]),
      check_that(X,[tuned(codes)])
   ).

test("codes, failure (includes things that are not a list)") :-
   forall(
      member(X,["",[1|2],'',foo,123,"",[a,b,c],["a","b"],[ab,cd],[-1,0],[0xFFFFFFFF,0]]),
      \+check_that(X,[tuned(codes)])
   ).

test("codes, strict, type exception (not a list, not a list of codes)") :-
   forall(
      member(X,[foo, "foo", [1|2], ["a","b"], [a, b]]),
      catch(check_that(X,[hard(codes)]),error(check(type,_,_,_),_),true)
   ).

test("codes, strict, domain exception (list of codes, but one of the codes is not in range)") :-
   catch(check_that([1,-1,2],[hard(codes)]),error(check(domain,_,_,_),_),true).

test("codes, fullylenient, fails because of uninstantiated member",fail) :-
   check_that([1,2,3,_,4,5,6],[smooth(codes)]).

test("codes, strict, uninstantiated exception because of uninstantiated member",error(check(too_little_instantiation,_,_,_))) :-
   check_that([1,2,3,_,4,5,6],[hard(codes)]).

test("codes, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(codes)]).

test("codes, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(codes)]).

test("codes, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(codes)]).

:- end_tests(check_that_using_codes).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_charys).

test("charys, success") :-
   forall(
      member(X,[[],[1,2,3]]),
      check_that(X,[tuned(charys)])
   ).

test("charys, failure") :-
   forall(
      member(X,["",[1|2],'',foo,123,"",["a","b"],[ab,cd],[-1,0],[1,2,a],[0xFFFFFFFF,0]]),
      \+check_that(X,[tuned(charys)])
   ).

test("charys, strict, type exception") :-
   forall(
      member(X,[foo, "foo", [1|2]]),
      catch(check_that(X,[hard(charys)]),error(check(type,_,_,_),_),true)
   ).

test("charys, lenient, failure in case of char/code mix",fail) :-
   check_that([1,2,a,3],[tuned(charys)]).

test("charys, strict, forany exception in case of char/code mix",error(check(forany,_,_,_),_)) :-
   check_that([1,2,a,3],[hard(charys)]).

test("charys, strict, forany exception (list of codes, but one code is not in range)") :-
   catch(check_that([1,-1,2],[hard(charys)]),error(check(forany,_,_,_),_),true).

test("charys, strict, forany exception (list of 1-character strings, which is not chary)") :-
   catch(check_that(["a","b"],[hard(charys)]),error(check(forany,_,_,_),_),true).

test("charys, fullylenient, fails because of uninstantiated member",fail) :-
   check_that([1,2,3,_,4,5,6],[smooth(charys)]).

test("charys, strict, uninstantiated exception because of uninstantiated member",error(check(too_little_instantiation,_,_,_))) :-
   check_that([1,2,3,_,4,5,6],[hard(charys)]).

test("charys, strict, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[hard(charys)]).

test("charys, lenient, throw instantiation error on uninstantiated X", error(check(too_little_instantiation,_,_,_))) :-
   check_that(_,[tuned(charys)]).

test("charys, fully lenient, fail on uninstantiated X", fail) :-
   check_that(_,[smooth(charys)]).

:- end_tests(check_that_using_charys).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_forX).

% --- forall: the single element X must pass all of the checks in the list that is "Check"

test("forall, success") :-
   check_that(12.0,[tuned(forall([float,inty,posnum]))]).

test("forall, empty list of checks, success") :-
   check_that(12.0,[]).

test("forall, failure",fail) :-
   check_that(-12.0,[tuned(forall([float,inty,posnum]))]).

test("forall, strict, failure",error(check(domain,_,_,_))) :-
   check_that(-12.0,[hard(forall([float,inty,posnum]))]). % fails on posnum

% --- forany: the single element X must pass at least one of the checks in the list that is "Check"

test("forany, success") :-
   check_that(-12,[tuned(forany([float,inty,posnum]))]).

test("forany, empty list of checks, failure",fail) :-
   check_that(-12,[tuned(forany([]))]).

test("forany, failure",fail) :-
   check_that(-12,[tuned(forany([float,proper_rational,posnum]))]).

test("forany, strict, failure",error(check(forany,_,_,_))) :-
   check_that(-12,[hard(forany([float,proper_rational,posnum]))]).

% --- fornone: the single element X must not pass any of the checks in the list that is "Check"

test("fornone, success") :-
   check_that(foo,[tuned(fornone([float,inty,posnum]))]).

test("fornone, empty list of checks, failure", fail) :-
   check_that(foo,[tuned(fornone([]))]).

test("fornone, failure",fail) :-
   check_that(12.0,[tuned(fornone([integer,negnum,inty]))]).

test("fornone, strict, failure",error(check(fornone,_,_,_))) :-
   check_that(12.0,[hard(fornone([integer,negnum,inty]))]).

:- end_tests(check_that_using_forX).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_passX).

% --- passall: all of the elements in the list that is X must pass the single "check"

test("passall, not a list as X", error(check(type,_,_,_),_)) :-
   check_that(foo,[tuned(passall(integer))]).

test("passall, empty list as X, success") :-
   check_that([],[tuned(passall(integer))]).

test("passall, success (all elements are inty)") :-
   check_that([1, 0, 3.0],[tuned(passall(inty))]).

test("passall, failure (some elements are not inty)",fail) :-
   check_that([1, 0, 3.1],[tuned(passall(inty))]).

test("passall, strict, failure, throws (type error on encountering 3.1)", error(check(type,_,_,_),_)) :-
   check_that([1, 0, 3.1],[hard(passall(pos0inty))]).

test("passall, strict, failure, throws (domain error on encountering -1)", error(check(domain,_,_,_),_)) :-
   check_that([1, -1, 3.1],[hard(passall(pos0inty))]).

% --- passany: at least one of the elements in the list that is X must pass the single "check"

test("passany, not a list as X", error(check(type,_,_,_),_)) :-
   check_that(foo,[tuned(passany(integer))]).

test("passany, empty list as X, failure", fail) :-
   check_that([],[tuned(passany(integer))]).

test("passany, success (at least one element is inty)") :-
   check_that([foo, g(x), 3.0],[tuned(passany(inty))]).

test("passany, failure (none of the elements is inty)",fail) :-
   check_that([foo, g(x), 3.1],[tuned(passany(inty))]).

test("passany, strict, failure (none of the elements is posinty, they are all out-of-type)", error(check(passany,_,_,_),_)) :-
   check_that([foo, g(x), 3.1],[hard(passany(inty))]).

test("passany, strict, failure (one of the elements is nonground)") :-
   check_that([1, g(_), 1],[hard(passany(inty))]).

test("passany, strict, failure (none of the elements is posinty, they are all out-of-domain)", error(check(passany,_,_,_),_)) :-
   check_that([-1, -2, 0],[hard(passany(posinty))]).

test("passany, strict, failure (none of the elements is posinty, and some are uninstantiated)", error(check(too_little_instantiation,_,_,_),_)) :-
   check_that([-1, _, _],[hard(passany(posinty))]).

test("passany, strict, failure (none of the elements is posinty, and some are uninstantiated)", error(check(too_little_instantiation,_,_,_),_)) :-
   check_that([-1, _, _],[hard(passany(posinty))]).

% --- passnone: none of the elements in the list that is X may pass the single "check"

test("passnone, not a-list-as-X", error(check(type,_,_,_),_)) :-
   check_that(foo,[tuned(passnone(integer))]).

test("passnone, empty list as X, failure", fail) :-
   check_that([],[tuned(passnone(integer))]).

test("passnone, success: there is no inty") :-
   check_that([foo, g(x), 3.1],[tuned(passnone(inty))]).

test("passnone, failure: there is an inty",fail) :-
   check_that([foo, 2, 3.1],[tuned(passnone(inty))]).

test("passnone, strict, failure: there is an inty, #1", error(check(passnone,_,_,_),_)) :-
   check_that([foo, 2, 3.1],[tuned(passnone(inty))],throw).

test("passnone, strict, failure: there is an inty, #2", error(check(passnone,_,_,_),_)) :-
   check_that([foo, 2, 3.1],[hard(passnone(inty))]).

test("passnone, strict, failure: there is a nonground") :-
   check_that([foo, g(_), 3.1],[hard(passnone(inty))]).

test("passnone, strict, failure: there is an uninstantiated", error(check(too_little_instantiation,_,_,_),_)) :-
   check_that([foo, _, 3.1],[hard(passnone(inty))]).

:- end_tests(check_that_using_passX).

% -------------------------------------------------------------------

:- begin_tests(check_that_multicondition).

test("12 is nonvar and an integer, strict, succeeds") :-
   check_that(12,[hard(nonvar),hard(int)]).

test("foo is nonvar and an integer, strict, throws",error(check(type,_,_,_))) :-
   check_that(foo,[hard(nonvar),hard(int)]).

test("foo is nonvar and an integer, lenient, fails", fail) :-
   check_that(foo,[tuned(nonvar),tuned(int)]).

test("foo is nonvar and an integer, marked lenient, but is strict by switching on strictness, throws", error(check(type,_,_,_))) :-
   check_that(foo,[tuned(nonvar),tuned(int)],throw).

test("a var is nonvar and an integer, lenient, fails", fail) :-
   check_that(_,[tuned(nonvar),tuned(int)]).

:- end_tests(check_that_multicondition).


