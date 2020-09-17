:- module(heavycarbon_difflist_length,
          [
             difflist_length/2   % difflist_length(Tip-Fin,Length)
            ,difflist_length/4   % difflist_length(Tip-Fin,Length,ListType,Intention)
            ,openlist_length/2   % openlist_length(Tip,Length)
          ]).

:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).
:- include(library('heavycarbon/support/throwme_nonmodular.pl')).

% ============================================================================
% Notes on Vocabulary
% ============================================================================
% There are two views of the "list":
%
% - The abstract one where a list is a sort of datastructure containing a
%   sequence of elements, where you can add or remove an element at the
%   front.
%
% - The more detailed one where the list is a specific form of graph, with
%   chained listboxes each referencing
%   - a single list element and
%   - the remainder of the chain of listboxes.
%
% ... And one can generally use the first view if appropriate, but may have
% to switch to the second view if necessary.
%
% "Tip" and "Fin" is vocabulary I use, but no-one else does:
%
% - A list's "Tip" is the first listbox.
% - A list's "Fin" is the last listbox.
%
% A "closed" or "proper" list is a standard list:
%
% Detailed view:       Element0<--[.|.]    "Tip"
%                                    |
%                       Element1<--[.|.]
%                                     |
%                        Element2<--[.|.]  "Fin"
%                                      |
%                                      []  "the empty list"
%
% An "open" list is a graph with list structure but with a "hole" at the 
% end, which can be designated by a variable, which in that case appears
% as an "unbound variable".
%
% Detailed view:       Element0<--[.|.]    "Tip"
%                                    |
%                       Element1<--[.|.]
%                                     |
%                        Element2<--[.|.]  "Fin"
%                                      |
%                                      ~  "a hole"
%
% A list's "head" is the first element of the list, i.e. list content.
%
% A list's "tail" is the smaller list created by shaving off the Tip or
% the head, depending on the view.
%
% A list's "last" is the last element of the list, i.e. list content.
%
% A list's termination is the empty list in case of a proper list, and
% a "hole" (named by an "unbound variable") in case of an open list.
%
% A listbox's "first" argument ("car" in the LISP world) is a reference to 
% an element.
%
% A listbox's "second" argument ("cdr" in the LISP world) is a reference
% to the next listbox.
%
% A "prefix" of a list is a sublist encompassing N>=0 consecutive elements,
% including the head. (The empty prefix encompasses no elements).
%
% A "suffix" of a list is a sublist encompassing N>=0 consecutive elements
% including the "last". (The empty suffix encompasses no elements).
% ============================================================================

% ============================================================================
% Relate a "difference list" and its length.
%
% Easy:     difflist_length(Difflist,Length)
% Complex:  difflist_length(Difflist,Length,Listtype,Intention)
%
% The following args are accepted:
%  - - - - - - - - - - - - - - - -
%
% Difflist   : Either an unbound variable to generate difflist templates
%              or a difflist structured as the term "Tip-Fin", to be analyzed.
% Length     : Either a unbound variable or an integer >= 0
%              - to verify (if Difflist is bound)
%              - or to set as template length (if Difflist is unbound)
% Listtype   : One of "open" or "closed". Generally unbound on call,
%              indicates whether the difflist is still "open" or already
%              "closed " (i.e. whether the Tip indicates a "proper list")
% Intention  : What is actually being done in the call, depending on which
%              parameters are unbound variables or not:
%              "templatize" - A Difflist of unbound variables is constructed as template
%              "verify"     - A Difflist's length and listtype is verified
%              "determine"  - A Difflist's length and listtype is determined
%              *verify_determine" - Part "verify", part "determine"
%              ** This argument should always be left an unbound variable and only be used
%              ** after success to see what happened. An inappropriate value
%              ** will cause a exception during consistency checking
%
% ============================================================================
% BUGS:
%
% - Cyclic list handling is not supported.
%   While an "open list" cannot be cyclic, a cyclic structure may
%   be passed, which would be bad.
%
% - A fat chunk of the code is actually about parameter checking.
%   There should be a "raw" version just going ahead w/o checks,
%   which would be easier to explain and digest. OTOH, the
%   parameter checking and error-handling parts are of some interest
%   by themselves.
% ============================================================================

difflist_length(Difflist,Length) :-
   difflist_length(Difflist,Length,_,_).

difflist_length(Difflist,Length,Listtype,Intention) :-
   contract_checking(Difflist,Length,Listtype,Intention),
   argument_tagging(Difflist,Length,Listtype,TggDifflist,TggLength,TggListtype),
   consistency_checking(TggDifflist,TggLength,TggListtype,Intention),
   % Branch depending on what we want to do: templatize (if Difflist is unbound),
   % or analyze (verify/determine/verify-determine) (if Difflist is bound)
   if_then_else(
      unbound(Difflist),
      templatize(Difflist,Length),
      (Difflist=Tip-Fin,analyze(Tip,Fin,Length,Listtype))).

openlist_length(Tip,Length) :-
   switch(
      (unbound(Tip),unbound(Length)),
         (Length=0),                                    % more or less reasonable
      (unbound(Tip),bound(Length)),
         (difflist_length(DL,Length),DL=Tip-_Fin),      % generate an open list of the given length
      bound(Tip),
         (contract_checking(length(Length)),
          walk_openlist_backbone_finless(Tip,1,Length))). % determine or verify length

% ---
% Define enums, Prolog-style: Values that parameters can take on
% ---

const_listtypes([open,closed]).
const_intentions([templatize,verify,determine,verify_determine]).

% ---
% Contract verification. Always call the same predicate contract_checking/2
% to avoid too many names but "tag" any value first to allow distinction.
% This performs generic verification to see whether the received arguments are
% indeed "inside the allowed domain". Exceptions are thrown if not.
% ---

contract_checking(Difflist,Length,Listtype,Intention) :-
   contract_checking(difflist(Difflist)),    % "Difflist" must be unbound variable or a term "Tip-Fin" with additional restrictions.
   contract_checking(length(Length)),        % "Length" must be unbound variable or integer >= 0.
   contract_checking(listtype(Listtype)),    % "Listtype" must be unbound variable or one of known atoms for that parameter.
   contract_checking(intention(Intention)).  % "Intention" must be unbound variable or one of known atoms for that parameter.

% "Difflist" arg must be unbound variable or structured like a pair "Tip-Fin"

contract_checking(difflist(DL)) :-
   switch((unbound(DL)),true,
          (DL=_Tip-_Fin),true,
          throwme(contract_checking,bad_difflist(DL))).

% "Length" arg must be an unbound variable or an integer >= 0

contract_checking(length(L)) :-
   switch((unbound(L)),true,
          (integer(L),L>=0),true,
          throwme(contract_checking,bad_length(L))).

% "Listtype" arg must be unbound variable or one of the known listtypes

contract_checking(listtype(LT)) :-
   switch((unbound(LT)),true,
          (const_listtypes(LTs),member(LT,LTs)),true,
          throwme(contract_checking,bad_listtype(LT))).

% "Intention" arg must be unbound var or one of the known intentions

contract_checking(intention(I)) :-
   switch((unbound(I)),true,
          (const_intentions(Is),member(I,Is)),true,
          throwme(contract_checking,bad_intention(I))).

% ---
% To make further processing somewhat agreeable, tag args to
% unbound(X) and bound(X) to indicate whether X is unbound or not.
% ---

argument_tagging(Difflist,Length,Listtype,TggDifflist,TggLength,TggListtype) :-
   bound_unbound_tag(Difflist , TggDifflist),
   bound_unbound_tag(Length   , TggLength),
   bound_unbound_tag(Listtype , TggListtype).

% ---
% consistency_checking(TggDifflist,TggLength,TggListtype,Intention)
% Ensure consistency of received parameters.
% This also sets values for unbound variables parameters.
% An inconsistency causes an Exception (it could arguably lead to failure instead)
% ---

% If difflist is an unbound variable:
% Then we want to "templatize", i.e. emit one of many difflist templates
% consisting only of unbound variables (possibly a single difflist if
% parameter "length" is bound).
% - The "listtype" must be "open" (this is either verified or set)
% - The "intention" must be "templatize" (this is either verified or set)

consistency_checking(unbound(_DL), _TggLen, unbound(open), templatize) :- !.  % Good case
consistency_checking(unbound(_DL), _TggLen, bound(open), templatize)   :- !.  % Good case
consistency_checking(unbound(_DL), _TggLen, TggType, Intention)        :-     % Inconsistent case
   bound_unbound_tag(Type,TggType),
   throwme(consistency_checking,difflist_unbound_but(Type,Intention)).

% If difflist is not an unbound variable (but is structured as Tip-Fin, which we already
% checked), then:
% - The intention may be to "determine", i.e. find the unknown length and
%   listtype ("open" or "closed"?).
% - The intention may be to "verify", i.e. compare the actual length and listtype
%   against the passed parameters.
% - The intention may be to "verify_determine", part "verify", part "determine".

consistency_checking(bound(_DL), unbound(_Len), unbound(_Type), determine)        :- !. % Good case
consistency_checking(bound(_DL), unbound(_Len), bound(_Type), verify_determine) :- !. % Good case
consistency_checking(bound(_DL), bound(_Len), unbound(_Type), verify_determine) :- !. % Good case
consistency_checking(bound(_DL), bound(_Len), bound(_Type), verify)           :- !. % Good case
consistency_checking(bound(_DL), TggLength, TggType, Intention) :-                  % Inconistent case
   bound_unbound_tag(Type,TggType),
   bound_unbound_tag(Length,TggLength),
   throwme(consistency_checking,difflist_bound_but(Type,Length,Intention)).

% ============================================================================
% Templatize/Generate
% ============================================================================
% Generate longer and longer difflists of unbound variables if "length" is
% unbound, or a single difflist of unbound variables of the desired length if
% length is bound.

templatize(Difflist,Length) :-
   assertion(unbound(Difflist)),                            % remind the developer what's up
   assertion(unbound(Length);(integer(Length),Length>=0)),  % remind the developer what's up
   if_then(
      unbound(Length),between(0,inf,Length)                 % backtrackable generation of increasing lengths
   ),
   Tip=Fin,                                               % empty difflist
   templatize_construct(Length,Fin,FinOut),               % append Length unbound vars
   Difflist=Tip-FinOut.

templatize_construct(Length,Fin,FinOut) :-
   Length>0,!,
   Fin=[_|NewFin], % anti-append
   succ(NewLength,Length),
   templatize_construct(NewLength,NewFin,FinOut).

templatize_construct(0,Shunt,Shunt).

% ============================================================================
% Analyze or Verify
% ============================================================================
% Express the various allowed cases of Tip-Fin combinations, with a catchall
% at the end to throw in case of error.

% The empty difference list, the "listtype" is "open"

analyze(Tip,Fin,0,open) :-
   unbound(Fin),
   unbound(Tip),
   Tip == Fin,   % must be "equivalent" (i.e. be "sharing variables")
   !.            % commit

% A nonempty difference list, the "listtype" is "open"

analyze(Tip,Fin,Len,open) :-
   bound(Tip),
   unbound(Fin),
   Tip \== Fin,           % must not be "equivalent" (sharing variables are equivalent!!)
   !,
   walk_openlist_backbone(Tip,Fin,1,Len).

% An already-closed difference list (i.e. a "proper list"), the "listtype" is "closed"
% This is a bit expensive and could be rewritten to a single walk but probably called
% rarely. Doing the check for cycles oneself would also be difficult to say the least.

analyze(Tip,Fin,Len,closed) :-
   is_list(Tip),            % This will check that Tip is a closed list and not cyclic (by walking the list)
   is_list(Fin),            % This will check that Fin is a closed list and not cyclic (by walking the list)
   append(_Prefix,Fin,Tip), % You can obtain Tip = _Prefix + Fin (this creates a new list _Prefix)
   !,                       % commit
   length(Tip,Len).         % And the length matches (or is determined), too (this walks the list again)

% Neither a closed list nor a difference list. Could be anything!

analyze(_Tip,_Fin,_,_) :-
   throwme(analysis,bad_structure).

% ---
% Walk through an open list, accumulating length
% ---

walk_openlist_backbone([_|Xs],Fin,Len,Out) :-
   assertion(unbound(Fin)),    % remind the developer what's up
   bound(Xs),!,              % the list backbone continues!
   succ(Len,LenPlus),
   walk_openlist_backbone(Xs,Fin,LenPlus,Out).

walk_openlist_backbone([_|Xs],Fin,Shunt,Shunt) :-
   assertion(unbound(Fin)),    % remind the developer what's up
   unbound(Xs),!,              % the end of the backbone of the open list!
   unless(Xs == Fin,throwme(analysis,bad_structure_at_fin)).

walk_openlist_backbone([],_,_,_) :-
   throwme(analysis,closed_list_at_fin).

walk_openlist_backbone(X,_,_,_) :-
   throwme(analysis,not_a_listbox(X)).

% ---
% Special case when the caller just has the bound Tip
% ---

analyze_finless(Tip,Len) :-
   assertion(bound(Tip)),
   walk_openlist_backbone_finless(Tip,1,Len).

% ---
% Walk through an open list, accumulating length. The fin is unknown.
% ---

walk_openlist_backbone_finless([_|Xs],Len,Out) :-
   bound(Xs),!,
   succ(Len,LenPlus),
   walk_openlist_backbone_finless(Xs,LenPlus,Out).

walk_openlist_backbone_finless([_|Xs],Shunt,Shunt) :-
   unbound(Xs),!.

walk_openlist_backbone_finless([],_,_) :-
   throwme(analysis,closed_list).

walk_openlist_backbone_finless(X,_,_) :-
   throwme(analysis,not_a_listbox(X)).

% ---
% Aliases for var/1 and nonvar/1. Are the names of these less confusing?
% I hope so. var/1 and nonvar/1 are really badly chosen.
% TODO: export into another module
% ---

bound(X)   :- nonvar(X).
unbound(X) :- var(X).

% ---
% Tagging a term (enclosing it into a arity 1 compound term giving information
% about the term) depening on whether it is bound or unbound.
% ---

bound_unbound_tag(X,unbound(X)) :- var(X),!.
bound_unbound_tag(X,bound(X))   :- nonvar(X).

% ---
% Exception descriptors
% ---

exc_desc(analysis,bad_structure_at_fin,
         _,
         analysis(difflist_structure(nonmatching_fin)),
         "Bad 'difference list' -- The reached 'fin' and the actual 'fin' are not the same").

exc_desc(analysis,closed_list_at_fin,
         _,
         analysis(difflist_structure(closed_list)),
         "Bad 'difference list' -- The given 'tip' actually designates a closed list").

exc_desc(analysis,closed_list,
         _,
         analysis(closed_list),
         "Bad 'open list' -- it's actually a closed list").

exc_desc(analysis,not_a_listbox(X),
         _,
         analysis(not_a_listbox(X)),
         "Bad 'open list' -- backbone contains something that is not a listbox").

exc_desc(contract_checking,bad_difflist(DL),
         _,
         contract(difflist,DL),
         "'difflist' arg must be unbound variable or structured like a pair 'Tip-Fin'").

exc_desc(contract_checking,bad_listtype(LT),
         _,
         contract(listtype,LT),
         "'listtype' arg must be unbound variable or one of the known listtypes").

exc_desc(contract_checking,bad_length(L),
         _,
         contract(length,L),
         "'length' arg must be unbound variable or an integer >= 0").

exc_desc(contract_checking,bad_intention(I),
         _,
         contract(intention,I),
         "'intention' arg must be unbound variable or one of the known intentions").

exc_desc(consistency_checking,difflist_unbound_but(Type,Intention),
         _,
         consistency(difflist_unbound,Type,Intention),
         Msg) :-
   textize("'difflist' is unbound, but 'listtype' = «~q», 'intention' = «~q»",[Type,Intention],Msg).

exc_desc(consistency_checking,difflist_bound_but(Type,Length,Intention),
         _,
         consistency(difflist_bound,type(Type),length(Length),intention(Intention)),
         Msg) :-
   textize("'difflist' is bound, but 'length' = «~q», 'listtype' = «~q», 'intention' = «~q»",[Length,Type,Intention],Msg).

exc_desc(analysis,bad_structure,
         _,
         analysis(difflist_structure),
         "Bad 'difference list' -- the structure is wrong").

% ---
% format/2 (https://eu.swi-prolog.org/pldoc/doc_for?object=format/2)
% is precise in what it expected and throws an exception if there is a mismatch
% in argument count or type. This is unfortunate in situations of dynamic code
% or code lacking coverage. Use this predicate to make format generate "Text"
% from "Msg" and "Args", catch any exceptions generated and generate some
% replacement message instead.
%
% textize(+Msg,+Args,-Text)
%
% TODO: Export into another Module
% ---

textize(Msg,Args,FinalText) :-
   (is_list(Args) -> ListyArgs = Args ; ListyArgs = [Args]),
   catch(
      % happy path
      with_output_to(string(FinalText),format(Msg,ListyArgs)),
      _Catchall_catcher,
      catch(
         % we end up here if format/2 doesn't like what it sees; finagle something!
         (maplist([X,Buf]>>with_output_to(string(Buf),format("~q",[X])),[Msg|ListyArgs],L),
          atomic_list_concat(["Replacement Msg!"|L]," & ",FinalText)),
         _Another_catchall_catcher,
         throw("Can't happen"))).


