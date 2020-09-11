% ===
% Rewritten example of 
% https://eu.swi-prolog.org/pldoc/man?section=attvar
% ===

:- module(enum_domain,
          [ 
          enum_domain/2      % Var, ?Domain
          ]).

:- use_module(library(ordsets)).

% ---
% Perform some debug printing
% ---

:- debug(enum_domain).

% ---
% For readability, I will not use ->/2 all that much. Instead I will use dedicated
% meta-predicates (these are slow though, so maybe unusable in production). Should
% really be rewrite instructions. 
% ---

if_then_else(Condition,Then,Else) :- 
   call(Condition) 
   -> call(Then)
   ;  call(Else).

switch([If1,Then1],[If2,Then2],Else) :-
   call(If1)
   ->  call(Then1)
   ;   call(If2)
   ->  call(Then2)
   ;   call(Else).

% ---
% "Our" variable attribute (really, "'hole' attribute" more than "variable attribute")
% key. This corresponds to a factored-out constant in other programming languages.
% It MUST be the same name as the module.
% ---

attr_key(enum_domain).

% ---
% Define the hook predicate to call whenever a variable with "our" attribute is 
% involved in unification.
%
% From the SWI-Prolog reference manual:
%
% - - - - - - -
% "A hook that must be defined in the module to which an attributed variable 
%  refers. It is called after the attributed variable has been unified with a 
%  non-var term, possibly another attributed variable. AttValue is the 
%  attribute that was associated to the variable in this module and VarValue
%  is the new value of the variable. If this predicate fails, the unification
%  fails. If VarValue is another attributed variable the hook often combines
%  the two attributes and associates the combined attribute with VarValue
%  using put_attr/3." 
% - - - - - - - 
%
% "AttValue" could, in fact, be unbound on call. 
%
% This is the predicate that gets called on unfication.
% ATTV (attribute value on the old variable) is a list of allowed values for
% PUV (the Post-Unification Value).
%
% In this clause we do nothing except surround a worker predicate with logging.
% ---

attr_unify_hook(ATTV,PUV) :-
   debug(enum_domain,"enum_domain:attr_unify_hook called with ATTV = ~q, PUV = ~q",[ATTV, PUV]),
   assertion(nonvar(ATTV)),       % this is not generally the case, but it is in this application
   assertion(is_ordset(ATTV)),    % additionally it's an ordset
   assertion(maplist(atom,ATTV)), % and additionally, elements are all atoms
   if_then_else(
      auh_worker(ATTV,PUV), 
      (debug(enum_domain,"enum_domain:attr_unify_hook succeeds",[]),true),
      (debug(enum_domain,"enum_domain:attr_unify_hook fails",[]),fail)),
   % some additional logging before returning (it's fun!)
   attr_key(Key),
   if_then_else(
      get_attr(PUV,Key,PUVATTV),  % This succeeds iff PUV denotes a 'hole' with our attribute
      debug(enum_domain,"enum_domain:attr_unify_hook on successful return, PUV is an unbound variable with attribute = ~q",[PUVATTV]),
      debug(enum_domain,"enum_domain:attr_unify_hook on successful return, PUV is no longer a variable but ~q",[PUV])).
 
% ---
% ATTV (attribute value on the old variable) is a list of allowed values for
% PUV (the Post-Unification Value).
%
% In this application ATTV is always a list of atoms.
%
% The variable PUV may denote:
%
% - A 'hole' with our attribute ("PUV is an unbound variable with our attribute")
%   ... then (for this application) intersect ATTV with our attribute-value on that hole.
%   ... because what happened was that two 'holes', both with our attribute, were unified.
%   ... and PUV must fulfill the constraint of both attribute values.
%   In this application, if only one element remains in the intersection of allowed values,
%   then PUV must be equal to it, so we can unify PUV and that value and don't need to
%   make PUV an attributed variable.
% - A 'hole' without our attribute ("PUV is an unbound variable without our attribute")
%   ... then set our attribute on PUV to ATTV.
%   ... because what happened was that two 'holes' were unified, but only one had our attribute
%       and we want to propagate the constraint.
% - Not a hole (PUV is a bound variable)
%   ... then verify that PUV indeed has a value allowed by ATTV.
%   ... because what happened was that a hole with our attribute was unified with a value
%       and we have to veto the unification as the constraint is not fulfuilled. 
% ---

auh_worker(ATTV,PUV) :-
   attr_key(Key), 
   switch(
      [get_attr(PUV,Key,PUVATTV),            % This succeeds iff PUV denotes a 'hole' with our attribute
       auh_intersection(ATTV,PUVATTV,PUV)], 
      [var(PUV),put_attr(PUV,Key,ATTV)],     % PUV could still denote a 'hole' without our attribute
      ord_memberchk(PUV,ATTV)).              % Not a 'hole', so check whether value allowed (incl. whether it is an atom)

auh_intersection(ATTV,PUVATTV,PUV) :-
   ord_intersection(ATTV,PUVATTV,AX),        % Fails only if there is some typing problem with ATTV,PUVATTV (should not happen)
   auh_ax_decision(AX,PUV).

auh_ax_decision([],_) :-                     % Intersection of allowed values is empty.
   !,fail.                                   % No solution, veto unification.

auh_ax_decision([A],PUV) :-                  % Intersection contains only one allowed value A
   !,                                        % ... and so PUV can unified with A.
   attr_key(Key),                            % To avoid triggering attr_unify_hook again on the upcoming "=" ...
   del_attr(PUV,Key),                        % ... make sure to delete the (certainly present) attributed variable.
   PUV = A.                                  % Now we can unify. Done! PUV is now certainly no "attributed variable" anymore.
                                             % Note that unification may cause a call to OTHER attr_unify_hook/2 clauses.

auh_ax_decision(AX,PUV) :-                   % Default case.
   attr_key(Key),put_attr(PUV,Key,AX).       % PUV is attributed with the intersection AX.

% ---
% Translate attributes from this module to residual goals
% ---

attribute_goals(X) -->
   { attr_key(Key), 
     get_attr(X, Key, ATTV),
     compound_name_arguments(C,Key,[X|ATTV]) },
   [C].

% ---
% Either 
% - get the value of attribute "enum_domain" (a list of atoms enumerating the
%   values allowed) associated to variable X (really, "... associated to the 
%   'hole' denoted at call time by variable X", but let's keep the english 
%   description conveniently fluent)
% or 
% - merge a list of additional atoms with the list of atoms already associated 
%   to variable X using attribute "enum_domain". This reduces to setting the list
%   to the passed list-of-atoms if there is no such attribute yet.
% ---

enum_domain(X, ED) :-
   var(ED),                      % ED unbound? Then get the attribute value of the hole denoted by X
   !,                            % If the guard succeeds, commit and ...
   attr_key(Key),                % Retrieve Key value
   get_attr(X, Key, ED).         % This fails if X does not designate a hole or has no attribute Key

enum_domain(X, ED) :-
   nonvar(ED),                   % ED bound? Then set the attribute value of the hole denoted by X   
   list_to_ord_set(ED, EDOS),    % Sort/uniquify ED to form EDOS
   assertion(maplist(atom,ED)),  % All should be atom
   attr_key(Key),                % Retrieve Key value
   put_attr(Y, Key, EDOS),       % Instead of replacing an existing attribute on X, set it on freshvar Y...
   X = Y.                        % ...then merge the attributes. *** THIS WON'T TRIGGER THE HOOK ***


