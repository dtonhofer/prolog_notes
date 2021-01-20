% ============================================================================
% format/2 (https://eu.swi-prolog.org/pldoc/doc_for?object=format/2)
% is **precise** in what it expects.
% It throws an exception if there is a mismatch in argument count or type.
% This is unfortunate in situations of dynamic code or code lacking coverage.
% Use this predicate to make format/2 generate "FinalText" (always a string) 
% from "Msg" and "Args", catch any exceptions generated and generate some 
% replacement message instead.
%
% safe_format(+Msg,+Args,-FinalText)
%
% Usage:
%
% ?- use_module(library('heavycarbon/support/safe_format.pl')).
%
% ?- safe_format("Hello ~d",[7889],TXT).
% TXT = "Hello 7889".
%
% ?- safe_format("Hello ~d",[hello],TXT).
% TXT = "Exception in format/2. Args: <Hello ~d> & <hello>".
%
% Another example:
% 
% This works nicely:
%
% ?- safe_format("Everything is going extremely ~s. We have ~s complaints",["well","no"],Text).
% Text = "Everything is going extremely well. We have no complaints".
%
% safe_format/3 saves your bacon, you forgot a placeholder in the template:
%
% ?- safe_format("Open the ~s.",["pod bay doors","HAL"],Text).
% Text = "Exception in format/2. Args: <Open the ~s.> & <pod bay doors> & <HAL>".
% ============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under: 
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================
% Latest review: Tue 20 January 2021
% =============================================================================

:- module(heavycarbon_support_safe_format,
          [
          safe_format/3   % safe_format(+Msg,+Args,-FinalText)
          ]).

safe_format(Msg,Args,FinalText) :-
   (is_list(Args) -> ListyArgs = Args ; ListyArgs = [Args]),
   catch(
      format(string(FinalText),Msg,ListyArgs), % this "should" work
      _Catcher1,
      safe_format_exception_handler(Msg,ListyArgs,FinalText)).

% we end up here if format/3 doesn't like what it sees; finagle something!
% while still catching in case of further problems

safe_format_exception_handler(Msg,ListyArgs,FinalText) :-
   catch(
      (foldl(format_whatever,[Msg|ListyArgs],TheList,[]), % this generates a new closed list
       TheList = [_|OutList], % shave off the first separator
       atomics_to_string(["Exception in format/2. Args: "|OutList],FinalText)),
      _Catcher2,
      FinalText = "Exception inside of exception handler of safe_format/3").

% add a separator & the datum, ToRight is the new FIN of open list

format_whatever(Element,FromLeft,ToRight) :-
   format(string(Out),"<~s>",[Element]),
   FromLeft = [" & ",Out|ToRight].
   
   
   
 
