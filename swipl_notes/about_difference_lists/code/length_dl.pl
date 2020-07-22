% 2345678901234567890123456789012345678901234567890123456789012345678901234567
% ============================================================================
% 2020-05-XX
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% This is free and unencumbered software released into the public domain.
%
% Anyone is free to copy, modify, publish, use, compile, sell, or
% distribute this software, either in source code form or as a compiled
% binary, for any purpose, commercial or non-commercial, and by any
% means.
%
% For more information, please refer to <http://unlicense.org/>
% ============================================================================
% VERSION:   Sun 10 May 20:19:44 CEST 2020
% BUGS:      - Cyclic list handling is not supported. Infinity beckons!
%            - A fat chunk of the code is actually about parameter checking.
%              There should be a "raw" version just forging ahead w/o checks,
%              which would be easier to explain and digest. OTOH, the
%              parameter checking and error-handling parts are of some interest
%              by themselves.
% RUNS ON:   SWI Prolog 8.1.24
% PREDICATE: length_dl/3, length_dl/5
% DOCS:      The full difflist explainer:
%            https://bit.ly/2STX769_prolog
%            https://bit.ly/2WmphYy_prolog
% ============================================================================

:- include(misc).

% ============================================================================
% Define enums, Prolog-style
% ============================================================================

% Values that parameters can take on

length_dl_allowed_listtypes([open,closed]).
length_dl_allowed_intentions([templatize,verify,determine,verify_determine]).

% Values that exceptions can take on

length_dl_exception_contract_error(contract_error).        % raised when parameter checks fail (actually a "domain error")
length_dl_exception_consistency_error(consistency_error).  % raised when combinations-of-parameter checks fail (actually a "contract error")
length_dl_exception_structure_error(structure_error).      % raised when deep checks fail (actually a "contract error" too costly to check at once)

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
   length_dl_contract_check(difflist(Difflist)),    % "Difflist" must be freshvar or a term "Tip-Fin" with additional restrictions.
   length_dl_contract_check(length(Length)),        % "Length" must be freshvar or integer >= 0.
   length_dl_contract_check(listtype(Listtype)),    % "Listtype" must be freshvar or one of known atoms for that parameter.
   length_dl_contract_check(intention(Intention)),  % "Intention" must be freshvar or one of known atoms for that parameter.
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
   length_dl_consistency_check(TgDifflist,TgLength,TgListtype,Intention),
   %
   % Branch depending on what we want to do: templatize (if Difflist is fresh),
   % or analyze (verify/determine/verify-determine) (if Difflist is nonfresh/cured)
   %
   (fresh(Difflist)
    ->
    length_dl_templatize(Difflist,Length)
    ;
    (Difflist=Tip-Fin,length_dl_analyze(Tip,Fin,Length,Listtype))).

% ============================================================================
% Templatize
% ============================================================================
% Generate longer and longer difflists of fresh variables if "length" is
% fresh, or a single difflist of fresh variables of the desired length if
% length is nonfresh/cured.

length_dl_templatize(Difflist,Length) :-
   assertion(fresh(Difflist)),                            % remind the developer what's up
   assertion(fresh(Length);(integer(Length),Length>=0)),  % remind the developer what's up
   (fresh(Length)
    ->
    between(0,inf,Length)                                 % backtrackable generation of increasing lengths
    ;
    true),
   length_dl_templatize_construct(Length,X-X,Difflist).   % Recursion: Starter=X-X, Final=Difflist

length_dl_templatize_construct(Length,Acc,Fish) :-        % Acc is the Accumulator
   Length>0,
   !,
   % Lengthen the accumulator/difflist by one freshvar
   % We can lengthen it at the tip, no need to "append"!
   Acc=Tip-Fin,
   NewAcc=[_|Tip]-Fin,
   succ(NewLength,Length),
   length_dl_templatize_construct(NewLength,NewAcc,Fish).

length_dl_templatize_construct(0,AccToFish,AccToFish).

% ============================================================================
% Analyze
% ============================================================================
% Express the various allowed cases of Tip-Fin combinations, with a catchall
% at the end to throw in case of error.

% ---
% The empty difference list, the "listtype" is "open"
% ---

length_dl_analyze(Tip,Fin,0,open) :-
   fresh(Fin),
   fresh(Tip),
   Tip == Fin,   % must be "equivalent" (i.e. be "sharing variables")
   !.            % commit

% ---
% A nonempty difference list, the "listtype" is "open"
% ---

length_dl_analyze(Tip,Fin,Len,open) :-
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
     length_dl_throw(Code,Tip-Fin,"Bad 'difference list' -- the structure is wrong"))),
   % If Len is nonfresh/cured, this becomes a verification:
   Len=LenDetermined.

% ---
% An already-closed difference list (i.e. a "proper list"), the "listtype" is "closed"
% ---

length_dl_analyze(Tip,Fin,Len,closed) :-
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
   length_dl_throw(Code,Tip-Fin,"Bad 'difference list' -- the structure is wrong").

% ============================================================================
% Walk through an open list, accumulating length.
% ============================================================================
% Note that in call cases, we know that var(Fin)!

length_dl_walk([_|Xs],Fin,Len,LenFish) :-
   assertion(fresh(Fin)),    % remind the developer what's up
   cured(Xs),                % the backbone continues!
   !,                        % commit
   succ(Len,LenPlus),
   length_dl_walk(Xs,Fin,LenPlus,LenFish).

length_dl_walk([_|Xs],Fin,LenAccToFish,LenAccToFish) :-
   assertion(fresh(Fin)),    % remind the developer what's up
   fresh(Xs),                % the end of the backbone of the open list!
   Xs == Fin.                % double-check the structure using "equivalence"

% ============================================================================
% Throwing Exception with body "length_dl(Code,Culprit,Freetext)"
% ============================================================================
% Code    : An atom indicating the actual problem.
% Culprit : The datum that gave rise to the problem
% Msg     : A cleartext message, possibly with format "~" placeholders
%           "Msg" will be combined with "Args" to form the "Text" of the
%           exception.
% Args    : Additional arguments to fill placeholders in Msg. Unlike
%           format/2, not in a list, so several cases exist.

length_dl_throw(Code,Culprit,Msg) :-
   length_dl_throw_listargs(Code,Culprit,Msg,[]).

length_dl_throw(Code,Culprit,Msg,Arg0) :-
   length_dl_throw_listargs(Code,Culprit,Msg,[Arg0]).

length_dl_throw(Code,Culprit,Msg,Arg0,Arg1) :-
   length_dl_throw_listargs(Code,Culprit,Msg,[Arg0,Arg1]).

length_dl_throw(Code,Culprit,Msg,Arg0,Arg1,Arg2) :-
   length_dl_throw_listargs(Code,Culprit,Msg,[Arg0,Arg1,Arg2]).

length_dl_throw_listargs(Code,Culprit,Msg,Args) :-
   textize(Msg,Args,Freetext),
   throw(length_dl(Code,Culprit,Freetext)).

% ============================================================================
% Contract verification
% ============================================================================

length_dl_contract_check(difflist(DL)) :-
   fresh(DL) -> true
   ;
   (DL=_Tip-_Fin) -> true
   ;
   (length_dl_exception_contract_error(Code),
    length_dl_throw(Code,difflist(DL),"Bad 'difference list' «~q» -- should be freshvar or a term structured as Tip-Fin",DL)).

length_dl_contract_check(length(L)) :-
   fresh(L) -> true
   ;
   (integer(L),L>=0) -> true
   ;
   (length_dl_exception_contract_error(Code),
    length_dl_throw(Code,length(L),"Bad 'length' «~q» -- should be freshvar or an integer >= 0",L)).

length_dl_contract_check(listtype(LT)) :-
   length_dl_allowed_listtypes(LTs),
   (fresh(LT) -> true
    ;
    member(LT,LTs) -> true
    ;
    (length_dl_exception_contract_error(Code),
     length_dl_throw(Code,listtype(LT),"Bad 'listtype' «~q» -- should be freshvar or one of ~q",LT,LTs))).

length_dl_contract_check(intention(I)) :-
   length_dl_exception_contract_error(Code),
   length_dl_allowed_intentions(Is),
   (fresh(I) -> true
    ;
    member(I,Is) -> true
    ;
    length_dl_throw(Code,intention(I),"Bad 'intention' «~q» -- should be freshvar or one of ~q",I,Is)).

% ============================================================================
% Consistency check for passed parameters
% ============================================================================

% If difflist is actually a freshvar, then we want to "templatize", i.e. emit
% one of many difflist templates consisting only of fresh variables
% (possibly a single difflist if parameter "length" is nonfresh/cured).
% The "listtype" must be "open" (this is either verified or set)
% The "intention" must be "templatize" (this is either verified or set)
% Any inconsistent values of "(listtype,intention)" leads to an exception
% through the catchall clause beyond the red cut.

length_dl_consistency_check(fresh(_DL), _TgL, fresh(open), templatize) :- !.
length_dl_consistency_check(fresh(_DL), _TgL, cured(open), templatize) :- !.
length_dl_consistency_check(fresh(_DL), _TgL,  TgLT, Intention)  :-
   TgLT =.. [_Tag,Listtype],
   length_dl_exception_consistency_error(Code),
   length_dl_throw(Code,_,"'difflist' is fresh, but 'listtype' = «~q», 'intention' = «~q»",Listtype,Intention).

% If difflist is not a freshvar (but is structured as Tip-Fin, which we already
% checked), then:
% - We may want to "determine", i.e. find the unknown length and listtype
%   ("open" or "closed"?).
% - We may want to "verify", i.e. compare the actual length and listtype
%   against the passed parameters.
% - We may want to "verify_determine", part "verify", part "determine".
% Any inconsistent values of "(listtype,intention)" leads to an exception
% through the catchall clause beyond the red cut.

length_dl_consistency_check(cured(_DL), fresh(_L), fresh(_LT), determine)        :- !.
length_dl_consistency_check(cured(_DL), fresh(_L), cured(_LT), verify_determine) :- !.
length_dl_consistency_check(cured(_DL), cured(_L), fresh(_LT), verify_determine) :- !.
length_dl_consistency_check(cured(_DL), cured(_L), cured(_LT), verify)           :- !.

length_dl_consistency_check(cured(_DL), TgL, TgLT, Intention) :-
   TgL  =.. [_,Length],
   TgLT =.. [_,Listtype],
   length_dl_exception_consistency_error(Code),
   length_dl_throw(Code,_,"'difflist' is nonfresh, but 'length' = «~q», 'listtype' = «~q», 'intention' = «~q»",Length,Listtype,Intention).
