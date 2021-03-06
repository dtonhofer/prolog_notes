% =============================================================================
% Like length/2 but takes an additional option to select whether it should
% behave like SWI-Prolog's length/2, additionally throw if it receives a 
% negative integer as Length, or never throw and just fail on bad arguments.
%
% Actually, it turns out that length_strict/2 is the same as length/2:
%
%                   SWI-Prolog length/2    length_lenient/2     length_strict/2
% length not int       type error               fail              type error   
% length int < 0      domain error              fail             domain error
% length(L,L)            fail                   fail                 fail
% length([1|L],L)        fail                   fail                 fail
% length([1|X],L)      generates              generates            generates
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% Latest revision: 2020-12
% =============================================================================

:- module(lenient_length,
          [
           length/3           % length(List,Length,Option:[swi,lenient,strict])
          ,length_lenient/2   % never throws, just fails
          ,length_strict/2    % throws on negative Length argument
          ]).

% ===
% length/3 takes an atom option (or a list of atom options where
% the first recognized entry wins:
%
% 'strict'  - throws if "Length" is nonvar and not an integer or < 0 (ISO like)
% 'lenient' - never throws; instead fails if SWI-Prolog's length/2 throws
% 'swi'     - just call SWI-Prolog's length/2
%
% the default being 'swi' if the option is not recognized.
% ===

length(List,Length,Options) :-
   which(Options,Selected),
   length_selector(Selected,List,Length).

% ---
% which(Options,Selected).
% ---

which(strict,strict)       :- !.
which(lenient,lenient)     :- !.
which(swi,swi)             :- !.
which([strict |_],strict)  :- !. % first option recognized wins
which([lenient|_],lenient) :- !. % first option recognized wins
which([swi    |_],swi)     :- !. % first option recognized wins
which([_|Opts],Selected)   :- !,which(Opts,Selected).
which([],swi)              :- !. % default on no options
which(_,swi)               :- !. % default on incomprehensible options

% ---
% length_selector(Selected,List,Length)
%
% Call the correct "length" predicate depending on "Selected".
% No need to cut as the clauses are indexed on argument 1.
% ---

length_selector(strict,List,Length)  :- length_strict(List,Length).
length_selector(lenient,List,Length) :- length_lenient(List,Length).
length_selector(swi,List,Length)     :- length(List,Length).

% ---
% Test length and throw if bad: that's according to ISO
% ---

length_strict(List,Length) :-
   (nonvar(Length) ->
      ((\+ integer(Length) -> type_error(integer,Length) ; true),
       (Length < 0         -> domain_error(not_less_than_zero,Length) ; true))
      ; true),
   % then test list; leave that to the builtin as testing for listness is expensive
   length(List,Length).

% ---
% Never throws but instead fails on nonsense (wrong type, wrong
% domain) input. We do not do anything complex, just catch all
% exceptions throw by length/2 and transform them into failure.
% ---

length_lenient(List,Length) :- catch(length(List,Length),_,fail).


