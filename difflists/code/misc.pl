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
% VERSION: Sun 10 May 12:43:19 CEST 2020
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
      _Catch_all_catcher,
      catch(
         % we end up here if format/2 doesn't like what it sees; finagle something!
         (maplist([X,Buf]>>with_output_to(string(Buf),format("~q",[X])),[Msg|ListyArgs],L),
          atomic_list_concat(["Replacement Msg!"|L]," & ",FinalText)),
         _Another_catch_all_catcher,
         throw("Can't happen"))).

% ===
% This fixes one of my pet peeves of Prolog: var(X) is badly named. It should
% fresh(X) or freshvar(X), because we are not testing whether X is a variable (we
% know *that*), but whether it references a fresh (as yet unconstrained) term!
% fresh/1 and cured/1 sound better to me, they also have same number of characters,
% so align nicely in source code.
% ===

fresh(X) :- var(X).
cured(X) :- nonvar(X).

% ===
% Tagging, used in pre-processing arguments before they are used so that
% the correct clause can be easily matched.
% ===

fresh_tag(X,fresh(X)) :- var(X),!.
fresh_tag(X,cured(X)) :- nonvar(X),!.
