:- module(heavycarbon_support_safe_format,
          [
          safe_format/3   % safe_format(+Msg,+Args,-ResultString)
          ]).

%! safe_format(+Msg,+Args,-ResultString) is det
%
% Call format/3 to format Msg with Args resulting in ResultString, but catch
% and defuse any exceptions raised.
%
% format/3 and format/2 are demanding in what they require as arguments. 
% They throw an exception if there is a mismatch between the placeholders
% count and the count of elements in Args, or if an element in Args is
% a mismatch for a placeholder in Msg. This can lead to exceptions at inopportune
% times, especially if the Msg or Args arguments are built dynamically and code
% lacks sufficient coverage.
%
% Use this predicate to make format/3 generate 
% ResultString (always an SWI-Prolog string) from Msg and Args. If an 
% exception is thrown from the call to format/3, it is caught and a 
% replacement message is generated in ResultString.
%
% ### Examples
%
% ```
% ?- use_module(library('heavycarbon/support/safe_format.pl')).
%
% ?- safe_format("Hello ~d",[7889],Result).
% Result = "Hello 7889".
%
% ?- safe_format("Hello ~d",[hello],Result).
% Result = "Exception in format/3 with format string <Hello ~d> and args <hello>".
%
% ?- safe_format("Open the ~s.",["pod bay doors","HAL"],Result).
% Result = "Exception in format/3 with format string <Open the ~s.> and args <\"pod bay doors\">,<\"HAL\">".
% ```
%
% ### History
% 
%    1. 2021-01-20 - Code review.
%    1. 2021-02-04 - Documentation rewritten to pldoc.
%
% ### More
%
%    @arg Msg The placeholder-adorned message to print
%    @see format/2, format/3
%    @arg Args The list of parameters that will be inserted into Msg. If not a list,
%         Args is transformed into a list with a single argument.
%    @arg ResultString The result of formatting. Always an SWI-Prolog string.
%         Contains a replacement message in case format/3 threw an exception.
%    @license [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)
%    @author David Tonhofer (ronerycoder@gluino.name)
%    @tbd Escape non-printable characters to their hex respresentation.

safe_format(Msg,Args,Result) :-
   (is_list(Args) -> ListyArgs = Args ; ListyArgs = [Args]),
   catch(
      format(string(Result),Msg,ListyArgs), % this "should" work
      _,
      safe_format_exception_handler(Msg,ListyArgs,Result)).

% ---
% We end up here if format/3 doesn't like its arguments; finagle something!
% While still catching any exception thrown by the building of the replacement
% string.
% ---

safe_format_exception_handler(Msg,ListyArgs,Result) :-
   catch(
      build_replacement_string(Msg,ListyArgs,Result),
      _,
      (Result = "Exception inside of exception handler of safe_format/3")).

% ---

build_replacement_string(Msg,ListyArgs,Result) :-
   maplist(
       format_whatever,
       ListyArgs,           % input
       FormattedArgs),      % ouput is a list of string
   atomics_to_string(FormattedArgs,",",ArgsString),
   atomics_to_string([
       "Exception in format/3 with format string <",
       Msg,
       "> and args ",
       ArgsString],Result).

% ---

format_whatever(Element,Formatted) :-
   format(string(Formatted),"<~p>",[Element]). % "~p" == "print term for debugging purposes"

