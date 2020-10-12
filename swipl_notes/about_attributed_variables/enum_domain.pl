% ===
% Rewritten demo code for "attributed variables" from the page
% https://eu.swi-prolog.org/pldoc/man?section=attvar
% ===

% ---
% Unlike in the original code, I have separated enum_domain/2 into
%
% 1) enum_domain_get/2 - get the value of the variable attribute
% 2) enum_domain_set/3 - set the value of the variable attribute
%
% which is clearer than merging both functionalities into a single enum_domain/2.
% All of this code essentially works imperatively, not logically in any case.
% There is no gain in pretending otherwise.
% ---

:- module(enum_domain,
          [
              enum_domain_get/2       % ?Var, ?ListOfAtoms
             ,enum_domain_set/3       % ?Var, ?ListOfAtoms, ++Op
             ,enum_domain_set/2       % ?Var, ?ListOfAtoms (only intersect)
             ,get_hook_call_info/1    % -Info
             ,reset_hook_call_info/0 
          ]).

:- use_module(library(ordsets)).

% ---
% Perform some debug printing
% ---

:- debug(enum_domain).

% ---
% For readability, I will not use ->/2 all that much. Instead I will use
% dedicated meta-predicates (these are slow though, so maybe unusable in
% production). Should really be rewrite instructions.
% ---

if_then_else(Condition,Then,Else) :-
   call(Condition)
   -> call(Then)
   ;  call(Else).

switch(If1,Then1,If2,Then2,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(Else).

switch(If1,Then1,If2,Then2,If3,Then3,If4,Then4,Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(If3)
   ->  call(Then3)
   ;   call(If4)
   ->  call(Then4)
   ;   call(Else).

% ---
% The name/key of our variable attribute.
%
% This is really a "hole attribute" rather than a "variable attribute":
% Variables are just temporary clause-local names which don't carry
% attributes at all. Instead the attributes are carried by the hole (on the
% global term store) which are designated by variables. This abuse of language
% is standard though.
%
% The name MUST be the same name as the module so that the correct unification
% hook can be found at run-time.
%
% (This line corresponds to a "factored-out constant" in other programming
% languages; maybe Prolog should be given a specific syntax for this?)
% ---

attr_key(enum_domain).

% ---
% Client code calls enum_domain_get/2 to get the value of the attribute
% with key "enum_domain" carried by (the hole designated by) the unbound
% variable X.
%
% In this application, the attribute is a non-empty ordset of atoms listing
% allowed values of the attributed variable.
% ---

enum_domain_get(X, ATTV) :-
   var(ATTV),!,
   attr_key(Key),
   get_attr(X,Key,ATTV).    % This fails if X is not unbound or doesn't carry the attribute "Key"

enum_domain_get(X, ATTV) :-
   nonvar(ATTV),!,
   catch(
      list_to_ord_set(ATTV,ATTVos), % This may throw with at least a "type exception"
      _,
      fail),               % ...but, don't throw, just fail
   attr_key(Key),
   get_attr(X,Key,ATTVX),  % This fails if X is not unbound or doesn't carry the attribute "Key"
   ATTVX = ATTVos.         % This may fail or else perform element-wise unification

% ---
% Client code calls enum_domain_set/3 to modify/set the value of attribute
% with key "enum_domain" carried by (the hole designated by) the unbound
% variable "X", based on the values in "ATTV". Modification is done according
% to argument "Op".
%
% In this application, the attribute is a non-empty ordset of atoms listing
% allowed values of the attributed variable.
%
% - Op must be one of: "union","intersect","subtract","set";
% - We allow standard lists as arguments, too, instead of only ordsets;
% - We don't allow setting the attribute to an unbound variable, although that
%   might make sense for other applications.
% ---

enum_domain_set(X, ATTV, Op) :-
   must_be(oneof([union,intersect,subtract,set]),Op), % Throwy parameter check
   must_be(var,X),                                    % A nonvar X would eventually lead to an exception; check it here
   must_be(list(atom),ATTV),
   list_to_ord_set(ATTV,ATTVos),
   attr_key(Key),
   if_then_else(
      get_attr(X,Key,ATTVold),                        % This fails if X is not unbound or doesn't carry the attribute "Key"
      true,
      ATTVold = []),
   switch(
      (Op == union),
      ord_union(ATTVold,ATTVos,ATTVnew),
      (Op == intersect),
      ord_intersect(ATTVold,ATTVos,ATTVnew),
      (Op == subtract),
      ord_subtract(ATTVold,ATTVos,ATTVnew),
      (Op == set),
      ATTVnew=ATTVos,
      fail),
   deluxe_put_attr(ATTVnew,Key,X).                    % Branch by special case

deluxe_put_attr([],_,_) :-                            % Empty attribute value means "constraint will never be fulfilled"; fail the setter!
   !,fail.

deluxe_put_attr([A],Key,X) :-                         % Unique attribute value possibility means "X must be that value "
   !,
   del_attr(X,Key),                                   % ... make sure to delete any (possibly present) attributed variable to avoid triggering the hook
   X=A.

deluxe_put_attr([A1,A2|As],Key,X) :-                  % At least two attribute value possibilities; retain ordset as attribute value
   put_attr(X,Key,[A1,A2|As]).                        % Would throw is X were uninstantiated

% ---
% Alternative, written according to the original example.
% Client code calls enum_domain_set/2 to modify/set the value of attribute
% "enum_domain" of unbound variable "X", based on the values in "ATTV".
%
% In this application, the attribute is a non-empty ordset of atoms listing
% allowed values for variable X.
%
% - We allow standard lists as arguments, too.
% - We don't allow setting the attribute to an unbound variable, although that
%   might indeed make sense.
%
% If "X" already has an an attribute "enum_domain", the new and old ordsets
% are intersected.
%
% Otherwise the attribute "enum_domain" of "X" is set to the new ordset.
%
% The idea here is that we punt all the work (failing if there is no solution,
% setting the value of the unbound variable to the single solution, or
% generally intersecting the ordsets) to the "attr_unify_hook" by performing
% unification.
%
% For the hook to be triggered, both variables involved in the unification
% must carry the attribute, so we set up that situation (it's a bit
% artificial).
% ---

enum_domain_set(X, ATTV) :-
   must_be(var,X),                          % A nonvar X would eventually lead to an exception; check it here
   must_be(list(atom),ATTV),
   list_to_ord_set(ATTV,ATTVos),
   attr_key(Key),
   put_attr(Y,Key,ATTVos),                  % Setting the new attribute value on not-yet-attributed freshvar Y
   if_then_else(
      get_attr(X, Key, _),                  % This fails if X does not designate a hole or does not carry the attribute "Key"
      true,
      put_attr(X,Key,ATTV)),                % Make X carry the same attribute value as Y
   debug_before_unification(X,Y),
   X = Y.                                   % This triggers the attr_unify_hook

% ---
% Define the "hook predicate" to call whenever a hole (unbound variable) with
% "our" attribute is involved in unification.
%
% From the SWI-Prolog reference manual:
%
% - - - - - - -
% attr_unify_hook(AttValue,VarValue)
%
%  A hook that must be defined in the module to which an attributed variable
%  refers. It is called after the attributed variable has been unified with a
%  non-var term, possibly another attributed variable. AttValue is the
%  attribute that was associated to the variable in this module and VarValue
%  is the new value of the variable. If this predicate fails, the unification
%  fails. If VarValue is another attributed variable the hook often combines
%  the two attributes and associates the combined attribute with VarValue
%  using put_attr/3.
% - - - - - - -
%
% ("AttValue" could, in fact, itself be unbound if one of the arguments of
%  the unification that triggers the hook has an attribute with an unbound
%  value)
%
% - ATTV : attribute value on (one of) the attributed variables involved
%          in the unification, in this application a nonempty list of atoms
% - PUV  : the Result of Unification (Post-Unification-Value);
%          if the unification involves two unbound variables,
%          then this is an unbound variable with the attribute value
%          the counterpart of ATTV
% ---

attr_unify_hook(ATTV,PUV) :-
  debug_on_entry(ATTV,PUV),
  assert_on_entry(ATTV,PUV),
  update_hook_call_info(ATTV,PUV),           % update global variable to enable test checks.
  if_then_else(
      attr_unify_hook_inner(ATTV,PUV),
      debug_on_success(PUV),
      (debug_on_failure,fail)).

assert_on_entry(ATTV,PUV) :-
   attr_key(Key),
   assertion(
      nonvar(PUV);                           % PUV is nonvar or a 'hole' with attribute 'key'...
      get_attr(PUV,Key,_)                    % ... or a 'hole' with attribute 'key' (otherwise we wouldn't even be in the hook!)
   ),                                           
   assertion(nonvar(ATTV)),                  % nonvar(ATTV) is NOT necessarily the case, but it *is* in this application.
   assertion(is_ordset(ATTV)),               % And this is also true in this application.
   assertion(maplist(atom,ATTV)),            % ...and so is this.
   assertion(\+ord_empty(ATTV)).             % ...and so is this.

attr_unify_hook_inner(ATTV,PUV) :-
   attr_key(Key),
   switch(
      get_attr(PUV,Key,ATTVPUV),             % This succeeds iff PUV denotes a 'hole' with attribute 'Key'...
      intersection(ATTV,PUV,ATTVPUV),        % ...so intersect the attribute values.
      var(PUV),                              % Otherwise if PUV is a 'hole'...
      fail,                                  % ...then something is very wrong (that case should have been caught by assert_on_entry/2)
      ord_memberchk(PUV,ATTV)).              % PUV is not a hole: check whether PUV one of the allowed atoms, possibly vetoing unification.

intersection(ATTV,PUV,ATTVPUV) :-
   ord_intersection(ATTV,ATTVPUV,AX),        % Fails only if there is some typing problem with ATTV,ATTVPUV (should not happen).
   ax_decision(AX,PUV).

ax_decision([],_) :-                         % Intersection of allowed atoms is empty.
   !,fail.                                   % So ther is no solution fulfilling constraint: veto the unification!

ax_decision([A],PUV) :-                      % Intersection contains only a single allowed value A...
   !,                                        % ... and so PUV, an unbound variable, can be unified with A ... from within the hook itself!
   attr_key(Key),                            % To avoid triggering attr_unify_hook **again** on the upcoming "=" ...
   del_attr(PUV,Key),                        % ... make sure to delete the (certainly present) attributed variable.
   PUV = A.                                  % Now we can unify. Done! PUV is now certainly no "attributed variable" anymore.
                                             % Note that unification may cause a (second) call to attr_unify_hook/2 clauses of *OTHER* modules.

ax_decision([A1,A2|As],PUV) :-               % Default case: PUV is attributed with an intersection of cardinality >= 2...
   attr_key(Key),                            % ...in which case the intersection is just assigned as new attribute value.
   put_attr(PUV,Key,[A1,A2|As]).

% ---
% This is for testing: Mark that the hook has been called by setting
% a global variable named "enum_domain_hook_calls".
% ---

update_hook_call_info(ATTV,PUV) :-
   attr_key(Key),
   atom_concat(Key,'_hook_calls',GlobVarName),
   if_then_else(
      get_attr(PUV,Key,ATTVPUV),
      true,
      ATTVPUV = 'none'),
   NOW=hooky{attv:ATTV,puv:PUV,attv_of_puv:ATTVPUV}, % SWI-Prolog Dict is excellent here
   if_then_else(
      nb_current(GlobVarName,OLD),
      append([NOW],OLD,NEW),
      NEW=[NOW]),
   nb_setval(GlobVarName,NEW).

% ---
% Translate attributes from this module to residual goals for printout
% ---

attribute_goals(X) -->
   { attr_key(Key),
     get_attr(X,Key,ATTV),
     compound_name_arguments(C,Key,[X|ATTV]) },
   [C].

% ---
% Various helpers
% ---

debug_on_entry(ATTV,PUV) :-
   attr_key(Key),
   (get_attrs(PUV,ATTVsPUV) -> true ; ATTVsPUV = 'none'),
   debug(enum_domain,"~q:attr_unify_hook called with ATTV = ~q, PUV = ~q, attributes(PUV) = ~q",[Key,ATTV,PUV,ATTVsPUV]).

debug_on_success(PUV) :-
   attr_key(Key),
   if_then_else(
      get_attr(PUV,Key,ATTVPUV),   % This succeeds iff PUV denotes a 'hole' with attribute 'Key'
      debug(enum_domain,"~q:attr_unify_hook succeeds. PUV is an unbound variable with attribute = ~q",[Key,ATTVPUV]),
      debug(enum_domain,"~q:attr_unify_hook succeeds. PUV is now ~q (without attributes)",[Key,PUV])).

debug_on_failure :-
   attr_key(Key),
   debug(enum_domain,"~q:attr_unify_hook fails",[Key]).

debug_before_unification(X,Y) :-
   if_then_else(
      get_attrs(X,ATTVsX),
      true,
      ATTVsX = 'none'),
   if_then_else(
      get_attrs(Y,ATTVsY),
      true,
      ATTVsY = 'none'),
   debug(enum_domain,"enum_domain_set/2: going to unify: X attributes: ~q, Y attributes: ~q",[ATTVsX,ATTVsY]).

% ---
% For tests: Access the global variable holding hook call info
% ---

get_hook_call_info(I) :-
   must_be(var,I),
   nb_current(enum_domain_hook_calls,I)
   -> true
   ;  I=[].

reset_hook_call_info :-
   nb_setval(enum_domain_hook_calls,[]).

