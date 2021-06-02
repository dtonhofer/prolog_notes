:- module(onepointfour_text_checks,
          [
           check_that/2
          ,check_that/3
          ,check_that_named/3
          ,check_that_named/4
          ]).

/*

How to load:

assertz(file_search_path(library,'/home/rost/Development/PACKING/prolog_code/unpacked')).
use_module(library(onepointfour_text/checks)).

*/

/*

Checks currently implemented (aliases are indicated with "/")

  var nonvar
  nonground ground
  atom atomic compound
  string stringy char                ("stringy" means it's an atom or a string)
  number 
  float 
  float_not_nan
  float_not_inf 
  float_not_neginf float_not_posinf
  int/integer                        (an integer)
  rational                           (a rational, incldues integers)
  nonint_rational/proper_rational    (a rational that is not an integer)
  negnum/negnumber                   (strictly negative number)
  posnum/posnumber                   (strictly positive number)
  neg0num/neg0number                 (negative-or-zero number)
  pos0num/pos0number                 (positive-or-zero number)  
  non0num/non0number                 (non-zero number)
  negint/negative_integer            (strictly negative integer)
  posint/positive_integer            (strictly positive integer)
  neg0int                            (negative-or-zero integer)
  pos0int/nonneg                     (positive-or-zero integer)
  negfloat posfloat                  (strictly negative/positive float)
  neg0float pos0float                (negative-or-zero/positive-or-zero float)
  inty                               (an integer or a float that represents an integer)
  neginty posinty                    (strictly negative/positive inty)
  neg0inty pos0inty                  (negative-or-zero/positive-or-zero inty)
  list/proper_list                   (a proper list, including the empty list)
  member(List)                       (member of a list of values)
  random(V)                          (randomly fails with probability V)
  forany(List)                       (recursive: pass any type test in list) 
  forall(List)                       (recursive: pass all type tests in list)
  fornone(List)                      (recursive: pass no type test in list

*/

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

% ---
% 1) 'lenient' checks will fail instead of throw (unless there the check cannot be executed)
% 2) No specific name for the value is given, so messages in exceptions will be relatively generic.
% If Conditions is not a list, it is transformed into a list first.
% ---

check_that(X,Conditions) :-
   check_that(X,Conditions,false).
 
% ---
% 1) You can select whether 'lenient' checks should fail or throw. If throwing is preferred,
%    set Throw to one of the atoms 'throw' or 'true'. Anything else means failing is preferred.
% 2) No specific name for the value is given, so messages in exceptions will be relatively generic.
% If Conditions is not a list, it is transformed into a list first.
% ---

check_that(X,Conditions,Throw) :-
   is_proper_list(Conditions)
   ->
   check_that_1(Conditions,X,"",Throw)
   ;
   check_that_1([Conditions],X,"",Throw).

% ---
% As above, but additionally pass a name to be be used in messages used in exceptions
% ---

check_that_named(X,Conditions,Name) :-
   check_that_named(X,Conditions,Name,false).

% ---
% As above, but additionally pass a name to be be used in messages used in exceptions
% ---

check_that_named(X,Conditions,Name,Throw) :-
   is_proper_list(Conditions)
   ->
   check_that_1(Conditions,X,Name,Throw)
   ;
   check_that_1([Conditions],X,Name,Throw).

% ---
% Pre-check: Verify the "Conditions" list (this may be commented out in
% well-working programs)
% ---

check_that_1(Conditions,X,Name,Throw) :-
   wellformed(Conditions,X),
   check_that_2(Conditions,X,Name,Throw).
 
% ---
% Check the "Conditions" list for well-formed-ness.
% No need to cut, the caller cuts on success.
%
% Allowed:
%
% true           : Succeed unconditionally, do not verify conditions "further to the right".
% false,fail     : Fail unconditionally.
% break(Check)   : Succeed (and break off further tests) if Check succeeds, do not verifiy conditions "further to the right".
% fail(Check)    : Fail if Check succeeds.
% lenient(Check) : If Check succeeds, proceed, otherwise fail if "not throw" and throw if "throw" (Throw is one of =|true|= or =|throw|=).
% strict(Check)  : If Check succeeds, proceed, otherwise throw irrespective of the value of Throw.
% --- 

wellformed([true|_],_)  :- !.
wellformed([false|_],_) :- !.
wellformed([fail|_],_)  :- !.
wellformed([break(Check)|More],X) :-
   wellformed_check(Check,X),
   wellformed(More,X),
   !.
wellformed([fail(Check)|More],X) :-
   wellformed_check(Check,X),
   wellformed(More,X),
   !.
wellformed([lenient(Check)|More],X) :-
   wellformed_check(Check,X),
   wellformed(More,X),
   !.
wellformed([strict(Check)|More],X) :-
   wellformed_check(Check,X),
   wellformed(More,X),
   !.
wellformed([UnknownOrCausingFailure|_],_) :-
   domain_error("...a known entry in list-of-checks (wellformed/2)",UnknownOrCausingFailure). % ISO!
wellformed([],_).

wellformed_check(Check,_) :-
   atom(Check),
   !,
   elementary_checks(ECs),
   memberchk(Check,ECs).
wellformed_check(member(List),_) :-
   !,   
   is_proper_list(List).
wellformed_check(type(List),_) :-
   !,
   is_proper_list(List),
   elementary_checks(ETs),
   maplist({ETs}/[X]>>memberchk(X,ETs),List).
wellformed_check(random(V),_) :-
   !,
   number(V),V>=0,V=<1.
wellformed_check(forany(List),X) :-
   !,
   wellformed_sublist_check(List,X).
wellformed_check(forall(List),X) :-
   !,
   wellformed_sublist_check(List,X).
wellformed_check(fornone(List),X) :-
   !,
   wellformed_sublist_check(List,X).

wellformed_sublist_check(List,X) :-
   is_proper_list(List),
   forall(
      member(Check,List),
      wellformed_check(Check,X)).

elementary_checks([
   var,nonvar,
   nonground,ground,
   atom,atomic,compound,
   string,stringy,char,
   number,float,integer,int,rational,nonint_rational,proper_rational,
   negnum,negnumber,
   posnum,posnumber,
   neg0num,neg0number,
   pos0num,pos0number,
   non0num,non0number,   
   float_not_nan,
   float_not_inf,
   float_not_neginf,
   float_not_posinf,
   negint,negative_integer,
   posint,positive_integer,
   neg0int,pos0int,nonneg,
   negfloat,posfloat,
   neg0float,pos0float,
   inty,
   neginty,posinty,
   neg0inty,pos0inty,
   list,proper_list,
   forall,forany,fornone]).

is_proper_list(L) :- is_list(L).

check_that_2([true|_],_,_,_) :- !.
check_that_2([fail|_],_,_,_) :- !,fail.
check_that_2([false|_],_,_,_) :- !,fail.
check_that_2([break(Check)|More],X,Name,Throw) :-
   !,
   (eval(Check,X,Name,false) 
    ->
    true
    ;
    check_that_2(More,X,Name,Throw)).
check_that_2([fail(Check)|More],X,Name,Throw) :-
   !,
   (eval(Check,X,Name,false)
    ->
    fail
    ;
    check_that_2(More,X,Name,Throw)).
check_that_2([lenient(Check)|More],X,Name,Throw) :-
   !,
   (eval(Check,X,Name,Throw)
    ->
    check_that_2(More,X,Name,Throw) 
    ;
    fail).
check_that_2([strict(Check)|More],X,Name,Throw) :-
   !,
   (eval(Check,X,Name,true)
    -> 
    check_that_2(More,X,Name,Throw)
    ;
    fail).
check_that_2([Unknown|_],_,_,_) :-
   type_error("... a known entry in check list",Unknown).
check_that_2([],_,_,_).

% ---
% Evaluation
% ---

% Generate a string from an uncertain "Name", which may be unset

select_name(Name,"the value") :-
   (var(Name);Name=='';Name==""),
   !.
select_name(NameIn,NameOut) :-
   format(string(NameOut),"~q",[NameIn]).

throw_is_set(Throw) :- Throw==true.
throw_is_set(Throw) :- Throw==throw.

throw_if_X_uninstantiated(X,Name,Ness) :-
   var(X)
   -> 
   (select_name(Name,Name2),
    format(string(Msg),"... ~s to be instantiated. Can't check for ~s-ness",[Name2,Ness]),
    throw_2(instantiation,Msg,X))
   ;
   true.

throw_if_X_nonground(X,Name,Ness) :-
   \+ground(X)
   ->
   (select_name(Name,Name2),
    format(string(Msg),"... ~s to be ground. Can't check for ~s-ness",[Name2,Ness]),
    throw_2(groundedness,Msg,X))
   ;
   true.

throw_if_X_nonlist(X,Name,Ness) :-
   \+is_proper_list(X)
   ->
   (select_name(Name,Name2),
    format(string(Msg),"... ~s to be a proper list. Can't check for ~s-ness",[Name2,Ness]),
    throw_2(type,Msg,X))
   ;
   true.
 
throw_or_fail_for_case_random(Throw) :-
   throw_is_set(Throw), % if this fails, the call fails (which is what we want)
   throw_2(random,"... random failure after calling maybe/1").

throw_or_fail(Error,X,Name,Throw,Ness) :-
   throw_if_X_uninstantiated(X,Name,Ness),
   throw_is_set(Throw), % if this fails, the call fails (which is what we want)
   select_name(Name,Name2),
   format(string(Msg),"... ~s to fulfill ~s-ness",[Name2,Ness]),
   throw_2(Error,Msg,X).

throw_or_fail_missing_ok(Error,X,Name,Throw,Ness) :-
   throw_is_set(Throw), % if this fails, the call fails (which is what we want)
   select_name(Name,Name2),
   format(string(Msg),"... ~s to fulfill ~s-ness",[Name2,Ness]),
   throw_2(Error,Msg,X).

throw_2(domain(Expected),Msg,Culprit) :- throw(error(check(domain                 ,Expected,Msg,Culprit),_)).
throw_2(type(Expected),Msg,Culprit)   :- throw(error(check(type                   ,Expected,Msg,Culprit),_)).
throw_2(domain,Msg,Culprit)           :- throw(error(check(domain                 ,'?'     ,Msg,Culprit),_)).
throw_2(type,Msg,Culprit)             :- throw(error(check(type                   ,'?'     ,Msg,Culprit),_)).
throw_2(uninstantiation,Msg,Culprit)  :- throw(error(check(too_instantiated       ,'?'     ,Msg,Culprit),_)).
throw_2(instantiation,Msg,Culprit)    :- throw(error(check(not_instantiated_enough,'?'     ,Msg,Culprit),_)).
throw_2(groundedness,Msg,Culprit)     :- throw(error(check(not_ground             ,'?'     ,Msg,Culprit),_)).
throw_2(random,Msg)                   :- throw(error(check(random                 ,'?'     ,Msg,'?'    ),_)).

% TODO: the combined type-and-domain tests should probably throw "type error" on type
% violation and "domain error" on domain violation; for now it's always just "type error".

% TODO: these should not throw the ISO error but a dedicated error:
%
% check_error([domain|type],Msg,Culprit)
%
% For this, a proper printer for the toplevel needs to be defined.

eval(var,X,Name,Throw) :-
   !,
   (var(X)
    -> 
    true
    ;
    throw_or_fail_missing_ok(domain,X,Name,Throw,"var")).
eval(nonvar,X,Name,Throw) :-
   !,
   (nonvar(X)
    -> 
    true
    ;
    throw_or_fail_missing_ok(domain,X,Name,Throw,"nonvar")).
eval(ground,X,Name,Throw) :-
   !,
   (ground(X)
    -> 
    true
    ;
    throw_or_fail_missing_ok(domain,X,Name,Throw,"ground")).
eval(nonground,X,Name,Throw) :-
   !,
   (\+ground(X)
    -> 
    true
    ;
    throw_or_fail_missing_ok(domain,X,Name,Throw,"nonground")).
eval(atom,X,Name,Throw) :-
   !,
   (atom(X)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"atom")).
eval(atomic,X,Name,Throw) :-
   !,
   (atomic(X)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"atomic")).
eval(compound,X,Name,Throw) :-
   !,
   (compound(X)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"compound")).
eval(string,X,Name,Throw) :-
   !,
   (string(X)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"string")).
eval(stringy,X,Name,Throw) :-
   !,
   ((atom(X);string(X))
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"stringy")).
eval(char,X,Name,Throw) :-
   !,
   ((atom(X),atom_length(X,1)) % is this fast?? Note that we need to test first because atom_length/2 is lenient ("atomizes")
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"char")).
eval(number,X,Name,Throw) :-
   !,
   (number(X)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"number")).
eval(float,X,Name,Throw) :-
   !,
   (float(X)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"float")).
eval(int,X,Name,Throw) :-
   !,
   (integer(X)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"integer")).
eval(integer,X,Name,Throw) :-
   eval(int,X,Name,Throw).
eval(rational,X,Name,Throw) :-
   !,
   (rational(X)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"rational")).
eval(nonint_rational,X,Name,Throw) :-
   !,
   ((rational(X),\+integer(X))
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"nonint_rational")).
eval(proper_rational,X,Name,Throw) :-
   eval(nonint_rational,X,Name,Throw).
eval(negnum,X,Name,Throw) :-
   !,
   ((number(X),X<0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"strictly negative number")).
eval(negnumber,X,Name,Throw) :-
   eval(negnum,X,Name,Throw).
eval(posnum,X,Name,Throw) :-
   !,
   ((number(X),X>0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"strictly positive number")).
eval(posnumber,X,Name,Throw) :-
   eval(posnum,X,Name,Throw).
eval(neg0num,X,Name,Throw) :-
   !,
   ((number(X),X =< 0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"number that is =< 0")).
eval(neg0number,X,Name,Throw) :-
   eval(neg0num,X,Name,Throw).
eval(pos0num,X,Name,Throw) :-
   !,
   ((number(X),X >= 0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"number that is >= 0")).
eval(pos0number,X,Name,Throw) :-
   eval(pos0num,X,Name,Throw).
eval(non0num,X,Name,Throw) :-
   !,
   ((number(X),X =\= 0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"number that is not 0")).
eval(non0number,X,Name,Throw) :-
   eval(non0num,X,Name,Throw).
eval(float_not_nan,X,Name,Throw) :-
   !,
   ((float(X),NaN is nan,X \== NaN) % arithmetic comparison would fail
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"float that is not NaN")).
eval(float_not_inf,X,Name,Throw) :-
   !,
   ((float(X),X =\= -1.0Inf,X =\= +1.0Inf)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"float that is not positive or negative infinity")).
eval(float_not_neginf,X,Name,Throw) :-
   !,
   ((float(X),X =\= -1.0Inf)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"float that is not negative infinity")).
eval(float_not_posinf,X,Name,Throw) :-
   !,
   ((float(X),X =\= +1.0Inf)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"float that is not positive infinity")).
eval(negint,X,Name,Throw) :-
   !,
   ((integer(X),X<0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"strictly negative integer")).
eval(negative_integer,X,Name,Throw) :-
   eval(negint,X,Name,Throw).
eval(posint,X,Name,Throw) :-
   !,
   ((integer(X),X>0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"strictly positive integer")).
eval(positive_integer,X,Name,Throw) :-
   eval(posint,X,Name,Throw).
eval(neg0int,X,Name,Throw) :-
   !,
   ((integer(X),X =< 0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"integer that is =< 0")).
eval(pos0int,X,Name,Throw) :-
   !,
   ((integer(X),X >= 0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"integer that is >= 0")).
eval(nonneg,X,Name,Throw) :-
   eval(pos0int,X,Name,Throw).
eval(inty,X,Name,Throw) :-
   !,
   inty_typeness(X,Name,Throw).
eval(neginty,X,Name,Throw) :-
   !,
   inty_typeness(X,Name,Throw),
   ((integer(X),X<0;float(X),X<0.0)
    -> 
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly negative inty")).
eval(posinty,X,Name,Throw) :-
   !,
   inty_typeness(X,Name,Throw),
   ((integer(X),X>0;float(X),X>0.0)
    -> 
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"strictly positive inty")).
eval(neg0inty,X,Name,Throw) :-
   !,
   inty_typeness(X,Name,Throw),
   ((integer(X),X=<0;float(X),X=<0.0)
    -> 
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"inty that is =< 0")).
eval(pos0inty,X,Name,Throw) :-
   !,
   inty_typeness(X,Name,Throw),
   ((integer(X),X>=0;float(X),X>=0.0)
    -> 
    true
    ;
    throw_or_fail(domain,X,Name,Throw,"inty that is >= 0")).
eval(negfloat,X,Name,Throw) :-
   !,
   ((float(X),X<0.0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"strictly negative float")).
eval(posfloat,X,Name,Throw) :-
   !,
   ((float(X),X>0.0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"strictly positive float")).
eval(neg0float,X,Name,Throw) :-
   !,
   ((float(X),X =< 0.0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"float that is =< 0")).
eval(pos0float,X,Name,Throw) :-
   !,
   ((float(X),X >= 0.0)
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"float that is >= 0")).
eval(list,X,Name,Throw) :-
   !,
   (is_proper_list(X) 
    -> 
    true
    ;
    throw_or_fail(type,X,Name,Throw,"proper list")).
eval(proper_list,X,Name,Throw) :-
   eval(list,X,Name,Throw).
eval(member(List),X,Name,Throw) :-  % TODO this can probably be optimized
   !,
   throw_if_X_nonground(X,Name,"listmembership"), 
   throw_if_X_nonlist(List,Name,member), % must be a proper list
   ((\+ \+ member(X,List)) % \+ \+ to unroll any bindings
    -> 
    true
    ;  
    throw_or_fail(domain,X,Name,Throw,"listmembership")).
eval(random(V),_X,_Name,Throw) :-
   !,
   (maybe(V) 
    -> 
    true
    ;  
    throw_or_fail_for_case_random(Throw)).
eval(forall(List),X,Name,Throw) :-
   !,
   forall(
      member(Check,List),
      eval(Check,X,Name,Throw)).
eval(forany(List),X,Name,Throw) :-
   !,
   \+forall(
      member(Check,List),
      \+eval(Check,X,Name,Throw)).
eval(fornone(List),X,Name,Throw) :-
   !,
   forall(
      member(Check,List),
      \+eval(Check,X,Name,Throw)).

inty_typeness(X,Name,Throw) :-
   (integer(X);inty_float(X))
   ->
   true
   ;
   throw_or_fail(type(int_or_float),X,Name,Throw,"inty"). 

inty_float(X) :-
   float(X),
   X =\= -1.0Inf,
   X =\= +1.0Inf,
   round(X)=:=X.
 
