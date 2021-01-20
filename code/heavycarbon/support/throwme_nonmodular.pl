% =============================================================================
% Predicates to throw exceptions
% =============================================================================
% This is not a module! This file is "consulted" in any module that needs
% it, thus creating "local copies" of the predicates in that module.
%
% Add this directive invoking include/1
%
% :- include(library('heavycarbon/support/throwme_nonmodular.pl')).
%
% Code using this will declare "exception descriptors" as described below,
% and invoke throwme/2 with the approprite "LookupPred" and "LookupTerm".
% One then just needs to maintain or tune the "exception descriptors" to
% generate the desired exceptions.
%
% TODO: Write a little program that extract the throwme/2 calls from
%       code so that a plunit block can be created to call all the throwme/2
%       to see whether they are well-formed
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under: 
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% Latest review: Tue 19 January 2021
% =============================================================================

% ===
% throwme(+LookupPred,+LookupTerm)
%
% Predicate called to construct an exception term and throw it. Information 
% about how to construct the actual exception is found by performing a lookup
% based on the key formed by the pair (LookupPred,LookupTerm).
%
% LookupPred :
%    What predicate is throwing; this is an atom (a keyword) generally shaped
%    after the actual predicate name of the throwing predicate. It is not a
%    predicate indicator.
%
% LookupTerm :
%    A term, possibly compound, that describes the problem somehow. It is both
%    programmer-interpretable (but still abstract) as well as a way of passing
%    values that can be inserted into the "Formal" part.
%
% Example: throwme(setter_atomic,nonzero(A))
% ===

throwme(LookupPred,LookupTerm) :-
   findall([Location,Formal,Msg],exc_desc(LookupPred,LookupTerm,Location,Formal,Msg),Bag),
   length(Bag,BagLength),
   throwme_help(BagLength,Bag,LookupPred,LookupTerm).

% ---
% Helper invoked if exactly 1 applicable "exception descriptor" could be found.
% Throw the corresponding exception!
% This is the first clause in line. If there is no match on arg1, the catchall
% fallback is used instead.
% The constructed error term is "quasi ISO-standard" because its structure is
% "error(Formal,Context)" -- but there is not guarantee that the "Formal" term
% is any of the ISO-listed allowed "Formal" term (in fact, it generally is not).
% The "Context" (about which the ISO standard says nothing, leaving it to be
% "implementation-defined") is structured according to SWI-Prolog conventions:
% "context(Location,Msg)" where "Location", if left fresh, can be filled with
% a stack trace on the toplevel or by a catching catch_with_backtrace/3. It
% is, however, often filled with the predicate indicator of the throwing
% predicate. The "Msg" should be a stringy thing to printed out, i.e. a 
% human-readable explainer that is either an atom or a string. 
% - Is there a requirement that "Msg" be forced to an atom? 
% ---

throwme_help(1,[[Location,Formal,Msg]],_,_) :-
   throw(error(Formal,context(Location,Msg))).

% ---
% Helper invoked if not exactly 1 applicable "exception descriptor" could be found.
% That means the set of exception descriptors is incomplete/ambiguous or the lookup
% query is wrong. Throws a quasi-ISO-standard exception following the format 
% error(_,_) but with the formal term the non-ISO atom 'programming_error'.
% - Note that "Msg" is an atom, not a string (is that ok? it should probably be
%   a String, at least in SWI-Prolog)
% - Note that the second argument for error(_,_) follows SWI-Prolog conventions
%   and with its first position fresh, may be filled with a backtrace.
% ---

throwme_help(Count,_,LookupPred,LookupTerm) :-
   Count \== 1,
   with_output_to(
      atom(Msg),
      format("Instead of 1, found ~d exception descriptors for LookupPred = ~q, LookupTerm = ~q", 
         [Count,LookupPred,LookupTerm])),
   throw(error(programming_error,context(_,Msg))).

% ===
% exc_desc(+LookupPred,+LookupTerm,?Location,?Formal,?Msg)
% ===
% Descriptors for exceptions.
%
% The first two arguments are used for lookup. See throwme/2 for an explainer.
%
% The three last arguments are output values which are use to construct
% the exception term that is suppoed to be thrown by the caller.
%
% If "Location" is left a freshvar, it can be instantiated to a backtrack if
% the exception reaches the Prolog Toplevel or is caught by 
% catch_with_backtrace/3. 
% Otherwise, "Location" should be a predicate indicator or something similar.
% 
% Example:
%
% exc_desc(jpl_call_static,no_such_method(M),
%          jpl_call/4,
%          existence_error(method,M),
%          'some text')
%
% exc_desc(jpl_call_static,no_such_method(M),
%          _,
%          existence_error(method,M),
%          'some text')
%
% The "Msg" is a user-readable message. For now, it is not dynamically
% constructed (i.e. using format/3 calls) inside of exc_desc/5, nor is 
% internationalization supported for that matter. In some cases, the "Msg"
% has been created by caller and is passed in inside "LookupTerm", from where
% it is unification-picked-out-of-there into arg 5.
%
% The "Formal" is exactly the "formal term" that will used in the "exception
% term", and it is built by unification doing pick/put against "LookupTerm".
% It may or may not be ISO-Standard.
% ---

% exc_desc/5 clauses are Module-specifc!

