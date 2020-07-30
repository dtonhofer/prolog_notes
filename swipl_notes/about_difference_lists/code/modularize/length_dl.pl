:- module(heavycarbon_list_difflist_length,
          [
             length_dl/2   % length_dl(Difflist,Length)
            ,length_dl/4   % length_dl(Difflist,Length,Listtype,Intention)
          ]).

:- use_module(library('heavycarbon/utils/utils.pl')).

% ============================================================================
% Determine length of an "open list" (a list which has a "hole" at its "Fin"
% position instead of a "[]")
%
% or 
%
% Determine the length of a "difference list" if both "Tip" and "Fin" are 
% given. In that case, additional checks are performed.
% ============================================================================
% BUGS:  
%
% - Cyclic list handling is not supported. Infinity beckons!!
%   While an "open list" cannot be cyclic, a cyclic structure may
%   be passed to the length_dl/N predicates, which would be bad.
%          
% - A fat chunk of the code is actually about parameter checking.
%   There should be a "raw" version just going ahead w/o checks,
%   which would be easier to explain and digest. OTOH, the
%   parameter checking and error-handling parts are of some interest
%   by themselves.
% ============================================================================

% ============================================================================
% Entry point: Relate a difference list and its "Length".
% ============================================================================
% Difflist   : Either a fresh variable to generate difflist templates
%              or a difflist structured as the term "Tip-Fin", to be analyzed.
% Length     : Either a fresh variable or an integer >= 0
%              - to verify (if Difflist is nonfresh/cured)
%              - to set as template length (if Difflist is fresh)
% Listtype   : One of "open" or "closed". Generally fresh on call,
%              indicates whether the difflist is still "open" or already
%              "closed " (i.e. whether the Tip indicates a "proper list")
% Intention  : What is actually being done in the call, depending on what
%              parameters are freshvar or not:
%              "templatize" - A Difflist of freshvars is constructed as template
%              "verify"     - A Difflist's length and listtype is verified
%              "determine"  - A Difflist's length and listtype is determined
%              *verify_determine" - Part "verify", part "determine"
%              !! This argument should always be left a freshvar and only be used
%              !! after success to see what happened. An inappropriate value
%              !! will cause a exception (consistency error)

% -- Simple call ---

length_dl(Difflist,Length) :- length_dl(Difflist,Length,_,_).

% -- Call exposing all ---

length_dl(Difflist,Length,Listtype,Intention) :-
   %
   % Contract verification. Always call the same predicate (to avoid too many names),
   % but "tag" any value first to allow distinction.
   % This performs generic verification to see whether the received parameters
   % have been set to be "inside the allowed domain".
   % Exceptions are thrown if parameters are found to be outside the domain.
   %
   contract_check(difflist(Difflist)),    % "Difflist" must be freshvar or a term "Tip-Fin" with additional restrictions.
   contract_check(length(Length)),        % "Length" must be freshvar or integer >= 0.
   contract_check(listtype(Listtype)),    % "Listtype" must be freshvar or one of known atoms for that parameter.
   contract_check(intention(Intention)),  % "Intention" must be freshvar or one of known atoms for that parameter.
   %
   % To make further processing somewhat agreeable, tag args to
   % fresh(X) and cured(X) to indicate whether X is fresh or not.
   %
   fresh_tag(Difflist , TgDifflist),
   fresh_tag(Length   , TgLength),
   fresh_tag(Listtype , TgListtype),
   %
   % Finally ensure consistency of received parameters.
   % This also sets values for freshvar parameters.
   % An inconsistency causes an Exception (it could arguably lead to failure instead)
   %
   consistency_check(TgDifflist,TgLength,TgListtype,Intention),
   %
   % Branch depending on what we want to do: templatize (if Difflist is fresh),
   % or analyze (verify/determine/verify-determine) (if Difflist is nonfresh/cured)
   %
   (fresh(Difflist)
    ->
    templatize(Difflist,Length)
    ;
    (Difflist=Tip-Fin,length_dl_analyze(Tip,Fin,Length,Listtype))).










% ============================================================================
% Templatize
% ============================================================================
% Generate longer and longer difflists of fresh variables if "length" is
% fresh, or a single difflist of fresh variables of the desired length if
% length is nonfresh/cured.

templatize(Difflist,Length) :-
   assertion(fresh(Difflist)),                            % remind the developer what's up
   assertion(fresh(Length);(integer(Length),Length>=0)),  % remind the developer what's up
   (fresh(Length)
    ->
    between(0,inf,Length)                                 % backtrackable generation of increasing lengths
    ;
    true),
   templatize_construct(Length,X-X,Difflist).   % Recursion: Starter=X-X, Final=Difflist

templatize_construct(Length,Acc,Fish) :-        % Acc is the Accumulator
   Length>0,
   !,
   % Lengthen the accumulator/difflist by one freshvar
   % We can lengthen it at the tip, no need to "append"!
   Acc=Tip-Fin,
   NewAcc=[_|Tip]-Fin,
   succ(NewLength,Length),
   templatize_construct(NewLength,NewAcc,Fish).

templatize_construct(0,AccToFish,AccToFish).

% ===
% Verify Tip-Fin structure
% ===
% Express the various allowed cases of Tip-Fin combinations, with a catchall
% at the end to throw in case of error.

% ---
% The empty difference list, the "listtype" is "open"
% ---

analyze(Tip,Fin,0,open) :-
   fresh(Fin),
   fresh(Tip),
   Tip == Fin,   % must be "equivalent" (i.e. be "sharing variables")
   !.            % commit

% ---
% A nonempty difference list, the "listtype" is "open"
% ---

analyze(Tip,Fin,Len,open) :-
   cured(Tip),
   fresh(Fin),
   Tip \== Fin,  % must not be "equivalent" (sharing variables are equivalent!!)
   !,            % commit
   % Walk through open list with an accumulator value starting at 1.
   % Verifies that the backbone of the difflist is valid and fails if it is not.
   (length_dl_walk(Tip,Fin,1,LenDetermined)
    ->
    true
    ;
    (length_dl_exception_structure_error(Code),
     mythrow(Code,Tip-Fin,"Bad 'difference list' -- the structure is wrong"))),
   % If Len is nonfresh/cured, this becomes a verification:
   Len=LenDetermined.

% ---
% An already-closed difference list (i.e. a "proper list"), the "listtype" is "closed"
% ---

analyze(Tip,Fin,Len,closed) :-
   is_list(Tip),            % This will check that Tip is a closed list and not cyclic (by walking the list)
   is_list(Fin),            % This will check that Fin is a closed list and not cyclic (by walking the list)
   append(_Prefix,Fin,Tip), % You can obtain Tip = _Prefix + Fin (this creates a new list _Prefix)
   !,                       % commit
   length(Tip,Len).         % And the length matches, too (this walks the list again)

% ---
% Neither a closed list nor a difference list. Could be anything!
% ---

length_dl_analyze(Tip,Fin,_,_) :-
   length_dl_exception_structure_error(Code),
   mythrow(Code,Tip-Fin,"Bad 'difference list' -- the structure is wrong").

% ===
% Walk through an open list, accumulating length.
% ===
% Note that in call cases, we know that var(Fin)!

walk([_|Xs],Fin,Len,Out) :-
   assertion(fresh(Fin)),    % remind the developer what's up
   stale(Xs),                % the list backbone continues!
   !,                        % commit
   succ(Len,LenPlus),
   walk(Xs,Fin,LenPlus,Out).

walk([_|Xs],Fin,Shunt,Shunt) :-
   assertion(fresh(Fin)),    % remind the developer what's up
   fresh(Xs),                % the end of the backbone of the open list!
   Xs == Fin.                % double-check the structure using "equivalence" (could also be an assertion)

% ===
% Contract verification of passed arguments
% ===

% Difflist must be fresh or structured like a pair Tip-Fin

contract_check(difflist(DL)) :-
   fresh(DL) -> true
   ;
   (DL=_Tip-_Fin) -> true
   ;
   throwme(contract_check,bad_difflist(DL)).

% Length must be fresh or an integer >= 0

contract_check(length(L)) :-
   fresh(L) -> true
   ;
   (integer(L),L>=0) -> true
   ;
   throwme(contract_check,bad_length(L)).

% Listtype must be fresh or one of the known listtypes

contract_check(listtype(LT)) :-
   const__listtypes(LTs),
   (fresh(LT) -> true
    ;
    member(LT,LTs) -> true
    ;
    throwme(contract_check,bad_listtype(L)).

% Intention must be fresh or one of the known intentions

contract_check(intention(I)) :-
   const__intentions(Is),
   (fresh(I) -> true
    ;
    member(I,Is) -> true
    ;
    throwme(contract_check,intention(I)).

% ===
% Consistency check for passed (and already tagged) parameters:
% consistency_check(TgDifflist,TgLength,TgListtype,Intention)
% ===

% If difflist is a freshvar ...
% Then we want to "templatize", i.e. emit one of many difflist templates
% consisting only of fresh variables (possibly a single difflist if
% parameter "length" is nonfresh).
% - The "listtype" must be "open" (this is either verified or set)
% - The "intention" must be "templatize" (this is either verified or set)
% - Any inconsistent values of the pair "(listtype,intention)" leads to an
%   exception through the catchall clause beyond the red cut.

consistency_check(fresh(_DL), _TL, fresh(open), templatize) :- !.
consistency_check(fresh(_DL), _TL, stale(open), templatize) :- !.

consistency_check(fresh(_DL), _TL, TgLT, Intention)  :-
   TgLT =.. [_Tag,ListType],
   const__consistency_error(Code),
   mythrow(Code,_,"'difflist' is fresh, but 'listtype' = «~q», 'intention' = «~q»",ListType,Intention).

% If difflist is not a freshvar (but is structured as Tip-Fin, which we already
% checked), then:
% - The intention may be to "determine", i.e. find the unknown length and
%   listtype ("open" or "closed"?).
% - The intention may be to "verify", i.e. compare the actual length and listtype
%   against the passed parameters.
% - The intention may be to "verify_determine", part "verify", part "determine".
% - Any inconsistent values of "(listtype,intention)" leads to an exception
%   through the catchall clause beyond the red cut.

consistency_check(stale(_DL), fresh(_L), fresh(_LT), determine)        :- !.
consistency_check(stale(_DL), fresh(_L), stale(_LT), verify_determine) :- !.
consistency_check(stale(_DL), stale(_L), fresh(_LT), verify_determine) :- !.
consistency_check(stale(_DL), stale(_L), stael(_LT), verify)           :- !.

consistency_check(stale(_DL), TgL, TgLT, Intention) :-
   TgL  =.. [_,Length],
   TgLT =.. [_,ListType],
   const__consistency_error(Code),
   mythrow(Code,_,"'difflist' is stale, but 'length' = «~q», 'listtype' = «~q», 'intention' = «~q»",Length,ListType,Intention).

% ===
% Values that exceptions can take on. Non-ISO standard.
% ===

const__contract_error(contract_error).        % raised when parameter checks fail (actually a "domain error")
const__consistency_error(consistency_error).  % raised when combinations-of-parameter checks fail (actually a "contract error")
const__structure_error(structure_error).      % raised when deep checks fail (actually a "contract error" too costly to check at once)

% ===
% Define enums, Prolog-style: Values that parameters can take on
% ===

const__listtypes([open,closed]).
const__intentions([templatize,verify,determine,verify_determine]).

% ===
% Throwing Exception with body "length_dl(Code,Culprit,Freetext)"
% ===
% Code    : An atom indicating the actual problem.
% Culprit : The datum that gave rise to the problem
% Msg     : A cleartext message, possibly with format "~" placeholders
%           "Msg" will be combined with "Args" to form the "Text" of the
%           exception.
% Args    : Additional arguments to fill placeholders in Msg. Unlike
%           format/2, not in a list, so several cases exist.

% Non-ISO ...

mythrow(Code,Culprit,Msg) :-
   mythrow_args(Code,Culprit,Msg,[]). 

mythrow(Code,Culprit,Msg,Arg0) :-
   mythrow_args(Code,Culprit,Msg,[Arg0]).

mythrow(Code,Culprit,Msg,Arg0,Arg1) :-
   mythrow_args(Code,Culprit,Msg,[Arg0,Arg1]).

mythrow(Code,Culprit,Msg,Arg0,Arg1,Arg2) :-
   mythrow_args(Code,Culprit,Msg,[Arg0,Arg1,Arg2]).

mythrow_args(Code,Culprit,Msg,Args) :-
   textize(Msg,Args,Text),
   throw(error(length_dl(Code,Culprit),Text)).

desc(contract_check,bad_difflist(DL)) 
   (const__contract_error(Code),
    mythrow(tyCode,difflist(DL),"Bad 'difference list' «~q» -- should be freshvar or a term structured as Tip-Fin",DL)).

exe_desc(contract_check,bad_length(L))
   (const__contract_error(Code),
    mythrow(Code,length(L),"Bad 'length' «~q» -- should be freshvar or an integer >= 0",L)).

exe_desc(bad_listtype(L))
    (const__contract_error(Code),
     mythrow(Code,listtype(LT),"Bad 'listtype' «~q» -- should be freshvar or one of ~q",LT,LTs))).

exe_desc(contract_check,intention(I))
de,intention(I),"Bad 'intention' «~q» -- should be freshvar or one of ~q",I,Is)).


