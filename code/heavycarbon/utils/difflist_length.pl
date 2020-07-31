:- module(heavycarbon_difflist_length,
          [
             difflist_length/2   % difflist_length(Tip-Fin,Length)
            ,difflist_length/4   % difflist_length(Tip-Fin,Length,ListType,Intention)
            ,openlist_length/2   % openlist_length(Tip,Length)
          ]).

:- use_module(library('heavycarbon/support/utils.pl')).

:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).
:- include(library('heavycarbon/support/throwme_nonmodular.pl')).

% ============================================================================
% Relate a "difference list" and its length.
%
% Vocabulary:
%
% This is made a bit more complicated because there are two views of the list
% - The abstract one where a list is a sort of datastructure containing a
%   sequence of elements, where you can add or remove an element at the
%   front.
% - The more detailed one where the list is a specific form of graph, with
%   chained listboxes each referencing a single element and the rest of the
%   chain of listboxes.
% ... And one can generally use the first view if appropriate, but may have
% to switch to the second view if necessary.
%
% A "closed" or "proper" list is the standard list.
%
% Detailed view:       Element0<--[.|.]    "Tip"
%                                    |
%                       Element1<--[.|.]
%                                     |
%                        Element2<--[.|.]  "Fin"
%                                      |
%                                      []  "the empty list" (however implemented)
%
% An "open" list is a graph with list structure but with a "hole" at the end
%
% Detailed view:       Element0<--[.|.]    "Tip"
%                                    |
%                       Element1<--[.|.]
%                                     |
%                        Element2<--[.|.]  "Fin"
%                                      |
%                                      ~  "a hole"
%
% A list's "tip" is the first listbox.
% A list's "fin" is the last listbox.
%
% A list's "head" is the first element of the list, i.e. list content.
% A list's "tail" is the smaller list created by shaving off the head.
% A list's "last" is the last element of the list, i.e. list content.
% A list's termination is the empty list in case of a proper list, and
% a "hole" (named by a freshvar) in case of an open list.
%
% A listbox's "first" argument ("car") is a reference to an element.
% A listbox's "second" argument ("cdr") is a reference to the next listbox.
%
% A "prefix" of a list is a sublist encompassing N>=0 consecutive elements,
% including the head. (The empty prefix encompasses no elements).
%
% A "suffix" of a list is a sublist encompassing N>=0 consecutive elements
% including the "last". (The empty suffix encompasses no elements).
%
% The following args are accepted:
%  - - - - - - - - - - - - - - - -
% Difflist   : Either a fresh variable to generate difflist templates
%              or a difflist structured as the term "Tip-Fin", to be analyzed.
% Length     : Either a fresh variable or an integer >= 0
%              - to verify (if Difflist is nonfresh)
%              - or to set as template length (if Difflist is fresh)
% Listtype   : One of "open" or "closed". Generally fresh on call,
%              indicates whether the difflist is still "open" or already
%              "closed " (i.e. whether the Tip indicates a "proper list")
% Intention  : What is actually being done in the call, depending on what
%              parameters are freshvar or not:
%              "templatize" - A Difflist of freshvars is constructed as template
%              "verify"     - A Difflist's length and listtype is verified
%              "determine"  - A Difflist's length and listtype is determined
%              *verify_determine" - Part "verify", part "determine"
%              ** This argument should always be left a freshvar and only be used
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
   %
   % Branch depending on what we want to do: templatize (if Difflist is fresh),
   % or analyze (verify/determine/verify-determine) (if Difflist is nonfresh)
   %
   if_then_else(
      fresh(Difflist),
      templatize(Difflist,Length),
      (Difflist=Tip-Fin,analyze(Tip,Fin,Length,Listtype))).

openlist_length(Tip,Length) :-
   switch_throw_on_default(
      (fresh(Tip),fresh(Length)),
         (Length=0),                                    % more or less reasonable
      (fresh(Tip),stale(Length)),
         (difflist_length(DL,Length),DL=Tip-_Fin),      % generate an open list of the given length
      stale(Tip),
         (contract_checking(length(Length)),
          walk_openlist_backbone_finless(Tip,1,Length))). % determine or verify length

% ---
% Define enums, Prolog-style: Values that parameters can take on
% ---

const__listtypes([open,closed]).
const__intentions([templatize,verify,determine,verify_determine]).

% ---
% Contract verification. Always call the same predicate contract_checking/2
% to avoid too many names but "tag" any value first to allow distinction.
% This performs generic verification to see whether the received arguments are
% indeed "inside the allowed domain". Exceptions are thrown if not.
% ---

contract_checking(Difflist,Length,Listtype,Intention) :-
   contract_checking(difflist(Difflist)),    % "Difflist" must be freshvar or a term "Tip-Fin" with additional restrictions.
   contract_checking(length(Length)),        % "Length" must be freshvar or integer >= 0.
   contract_checking(listtype(Listtype)),    % "Listtype" must be freshvar or one of known atoms for that parameter.
   contract_checking(intention(Intention)).  % "Intention" must be freshvar or one of known atoms for that parameter.

% "Difflist" arg must be freshvar or structured like a pair "Tip-Fin"

contract_checking(difflist(DL)) :-
   switch((fresh(DL)),true,
          (DL=_Tip-_Fin),true,
          throwme(contract_checking,bad_difflist(DL))).

% "Length" arg must be freshvar or an integer >= 0

contract_checking(length(L)) :-
   switch((fresh(L)),true,
          (integer(L),L>=0),true,
          throwme(contract_checking,bad_length(L))).

% "Listtype" arg must be freshvar or one of the known listtypes

contract_checking(listtype(LT)) :-
   switch((fresh(LT)),true,
          (const__listtypes(LTs),member(LT,LTs)),true,
          throwme(contract_checking,bad_listtype(LT))).

% "Intention" arg must be freshvar or one of the known intentions

contract_checking(intention(I)) :-
   switch((fresh(I)),true,
          (const__intentions(Is),member(I,Is)),true,
          throwme(contract_checking,bad_intention(I))).

% ---
% To make further processing somewhat agreeable, tag args to
% fresh(X) and cured(X) to indicate whether X is fresh or not.
% ---

argument_tagging(Difflist,Length,Listtype,TggDifflist,TggLength,TggListtype) :-
   fresh_tag(Difflist , TggDifflist),
   fresh_tag(Length   , TggLength),
   fresh_tag(Listtype , TggListtype).

% ---
% consistency_checking(TggDifflist,TggLength,TggListtype,Intention)
% Ensure consistency of received parameters.
% This also sets values for freshvar parameters.
% An inconsistency causes an Exception (it could arguably lead to failure instead)
% ---

% If difflist is a freshvar:
% Then we want to "templatize", i.e. emit one of many difflist templates
% consisting only of fresh variables (possibly a single difflist if
% parameter "length" is nonfresh).
% - The "listtype" must be "open" (this is either verified or set)
% - The "intention" must be "templatize" (this is either verified or set)

consistency_checking(fresh(_DL), _TggLen, fresh(open), templatize) :- !.  % Good case
consistency_checking(fresh(_DL), _TggLen, stale(open), templatize) :- !.  % Good case
consistency_checking(fresh(_DL), _TggLen, TggType, Intention)  :-         % Inconsistent case
   fresh_tag(Type,TggType),
   throwme(consistency_checking,difflist_fresh_but(Type,Intention)).

% If difflist is not a freshvar (but is structured as Tip-Fin, which we already
% checked), then:
% - The intention may be to "determine", i.e. find the unknown length and
%   listtype ("open" or "closed"?).
% - The intention may be to "verify", i.e. compare the actual length and listtype
%   against the passed parameters.
% - The intention may be to "verify_determine", part "verify", part "determine".

consistency_checking(stale(_DL), fresh(_Len), fresh(_Type), determine)        :- !. % Good case
consistency_checking(stale(_DL), fresh(_Len), stale(_Type), verify_determine) :- !. % Good case
consistency_checking(stale(_DL), stale(_Len), fresh(_Type), verify_determine) :- !. % Good case
consistency_checking(stale(_DL), stale(_Len), stale(_Type), verify)           :- !. % Good case
consistency_checking(stale(_DL), TggLength, TggType, Intention) :-                  % Inconistent case
   fresh_tag(Type,TggType),
   fresh_tag(Length,TggLength),
   throwme(consistency_checking,difflist_stale_but(Type,Length,Intention)).

% ============================================================================
% Templatize/Generate
% ============================================================================
% Generate longer and longer difflists of fresh variables if "length" is
% fresh, or a single difflist of fresh variables of the desired length if
% length is nonfresh.

templatize(Difflist,Length) :-
   assertion(fresh(Difflist)),                            % remind the developer what's up
   assertion(fresh(Length);(integer(Length),Length>=0)),  % remind the developer what's up
   if_then(
      fresh(Length),between(0,inf,Length)                 % backtrackable generation of increasing lengths
   ),
   Tip=Fin,                                               % empty difflist
   templatize_construct(Length,Fin,FinOut),               % append Length freshvars
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
   fresh(Fin),
   fresh(Tip),
   Tip == Fin,   % must be "equivalent" (i.e. be "sharing variables")
   !.            % commit

% A nonempty difference list, the "listtype" is "open"

analyze(Tip,Fin,Len,open) :-
   stale(Tip),
   fresh(Fin),
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
   assertion(fresh(Fin)),    % remind the developer what's up
   stale(Xs),!,              % the list backbone continues!
   succ(Len,LenPlus),
   walk_openlist_backbone(Xs,Fin,LenPlus,Out).

walk_openlist_backbone([_|Xs],Fin,Shunt,Shunt) :-
   assertion(fresh(Fin)),    % remind the developer what's up
   fresh(Xs),!,              % the end of the backbone of the open list!
   unless(Xs == Fin,throwme(analysis,bad_structure_at_fin)).

walk_openlist_backbone([],_,_,_) :-
   throwme(analysis,closed_list_at_fin).

walk_openlist_backbone(X,_,_,_) :-
   throwme(analysis,not_a_listbox(X)).

% ---
% Special case when the caller just has the nonfresh Tip
% ---

analyze_finless(Tip,Len) :-
   assertion(stale(Tip)),
   walk_openlist_backbone_finless(Tip,1,Len).

% ---
% Walk through an open list, accumulating length. The fin is unknown.
% ---

walk_openlist_backbone_finless([_|Xs],Len,Out) :-
   stale(Xs),!,
   succ(Len,LenPlus),
   walk_openlist_backbone_finless(Xs,LenPlus,Out).

walk_openlist_backbone_finless([_|Xs],Shunt,Shunt) :-
   fresh(Xs),!.

walk_openlist_backbone_finless([],_,_) :-
   throwme(analysis,closed_list).

walk_openlist_backbone_finless(X,_,_) :-
   throwme(analysis,not_a_listbox(X)).

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
         "'difflist' arg must be freshvar or structured like a pair 'Tip-Fin'").

exc_desc(contract_checking,bad_listtype(LT),
         _,
         contract(listtype,LT),
         "'listtype' arg must be freshvar or one of the known listtypes").

exc_desc(contract_checking,bad_length(L),
         _,
         contract(length,L),
         "'length' arg must be freshvar or an integer >= 0").

exc_desc(contract_checking,bad_intention(I),
         _,
         contract(intention,I),
         "'intention' arg must be freshvar or one of the known intentions").

exc_desc(consistency_checking,difflist_fresh_but(Type,Intention),
         _,
         consistency(difflist_fresh,Type,Intention),
         Msg) :-
   textize("'difflist' is fresh, but 'listtype' = «~q», 'intention' = «~q»",[Type,Intention],Msg).

exc_desc(consistency_checking,difflist_stale_but(Type,Length,Intention),
         _,
         consistency(difflist_nonfresh,type(Type),length(Length),intention(Intention)),
         Msg) :-
   textize("'difflist' is stale, but 'length' = «~q», 'listtype' = «~q», 'intention' = «~q»",[Length,Type,Intention],Msg).

exc_desc(analysis,bad_structure,
         _,
         analysis(difflist_structure),
         "Bad 'difference list' -- the structure is wrong").


