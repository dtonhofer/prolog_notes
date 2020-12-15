:- module(lenient_length,
          [
               length/3
          ]).

% ============================================================================
% 2020-12
% https://github.com/dtonhofer/prolog_notes
% ----------------------------------------------------------------------------
% ronerycoder@gluino.name (me) says this is licensed under
% https://opensource.org/licenses/0BSD
% ============================================================================

% a length/2 which is stricter than the SWI-Prolog length
% it is "near" the ISO length in behaviour

length_strict(List,Length) :-
   % first test on length: that's according to ISO
   (nonvar(Length) ->
      ((\+ integer(Length) -> type_error(integer,Length) ; true),
       (Length < 0         -> domain_error(not_less_than_zero,Length) ; true))
      ; true),
   % then test list; leave that to the builtin as testing for listness is expensive
   length(List,Length).
   
% a length/2 which never throws but instead fails on nonsense (wrong type, wrong
% domain) input. We do not do anything complex, just catch all exceptions and 
% transform them into failure.

length_lenient(List,Length) :-
   catch(length(List,Length),_,fail).

% pack the above into one predicate, which takes option 'strict' or 'lenient' or 'swi'
% with the default being 'swi'

length(List,Length,Options) :-
   which(Options,Selected),
   length_selector(Selected,List,Length). 

% length_selector(Selected,List,Length)   
% call the correct "length" predicate depending non Selected

length_selector(strict,List,Length)  :- length_strict(List,Length).
length_selector(lenient,List,Length) :- length_lenient(List,Length).
length_selector(swi,List,Length)     :- length(List,Length).   

% which(Options,Selected).

which(strict,strict)       :- !.
which(lenient,lenient)     :- !.
which(swi,swi)             :- !.
which([strict|_],strict)   :- !. % first option recognized wins
which([lenient|_],lenient) :- !. % first option recognized wins
which([swi|_],swi)         :- !. % first option recognized wins
which([_|Opts],Selected)   :- !,which(Opts,Selected).
which([],swi)              :- !. % default on no options
which(_,swi)               :- !. % default on incomprehensible options

 
