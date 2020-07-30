:- module(heavycarbon_utils_utils,
          [
            textize/3      % textize(Msg,Args,FinalText): leniently generate text, like format/2
           ,fresh/1,       % fresh(Term): an alias for var(Term) 
           ,stale/1,       % stale(Term): an alias for nonvar(Term)
           ,fresh_tag/2    % fresh_tag(X,Tagged) tags X with fresh/1 or stale/1 for further processing
          ]).

% ============================================================================
% Various pieces of code that may be useful
% ============================================================================

% ===
% format/2 (https://eu.swi-prolog.org/pldoc/doc_for?object=format/2)
% is precise in what it expected and throws an exception if there is a mismatch
% in argument count or type. This is unfortunate in situations of dynamic code
% or code lacking coverage. Use this predicate to make format generate "Text"
% from "Msg" and "Args", catch any exceptions generated and generate some
% replacement message instead.
%
% textize(+Msg,+Args,-Text)
% ===

textize(Msg,Args,FinalText) :-
   (is_list(Args) -> ListyArgs = Args ; ListyArgs = [Args]),
   catch(
      % happy path
      with_output_to(string(FinalText),format(Msg,ListyArgs)),
      _catchall_catcher,
      catch(
         % we end up here if format/2 doesn't like what it sees; finagle something!
         (maplist([X,Buf]>>with_output_to(string(Buf),format("~q",[X])),[Msg|ListyArgs],L),
          atomic_list_concat(["Replacement Msg!"|L]," & ",FinalText)),
         _another_catchall_catcher,
         throw("Can't happen"))).

% ===
% This fixes one of my pet peeves of Prolog: var(X) is badly named. It should
% fresh(X) or freshvar(X), because we are not testing whether X is a variable (we
% know *that*), but whether it references a fresh (as yet unconstrained/uninstantiated)
% term, i.e. a "hole". 
% ===

fresh(X) :- var(X).     % whatever is between the parentheses is a variable (it cannot
                        % be anything else!) and that variable designates a "hole":
                        % the variable is "fresh", "unbound", "uninstantiated", "unrefined"

stale(X) :- nonvar(X).  % the complement of the fresh/1

% ===
% Tagging, used in pre-processing arguments before they are used so that
% the correct clause can be easily matched.
% ===

fresh_tag(X,fresh(X)) :- var(X),!.
fresh_tag(X,stale(X)) :- nonvar(X).


