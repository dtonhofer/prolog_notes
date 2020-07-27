:- module(clashfree_id_selection,[clashfree_id_selection/3]).

% ============================================================================
% Sun 26 Jul 12:46:46 CEST 2020
%
% Generate a new integer Id that doesn't exist in a Dict yet.
% This will mostly succeed on first try!
%
% Switch on debugging output with
% ?- debug(clashfree).
%
% Example usage:
% 
% DictIn=_{},MaxKey=200,
% clashfree_id_selection(DictIn,Id,MaxKey),
% put_dict(Id,DictIn,x,DictOut),
% clashfree_id_selection(DictOut,Id2,MaxKey).
%
% Cause impossibility to find another Id:
%
% MaxId=1,DictIn=_{},
% clashfree_id_selection(DictIn,Id,MaxId),
% put_dict(Id,DictIn,x,DictOut),
% clashfree_id_selection(DictOut,Id2,MaxId),
% put_dict(Id2,DictOut,x,DictOut3),
% clashfree_id_selection(DictOut3,Id3,MaxId).
% ============================================================================

clashfree_id_selection(Dict,Id,MaxId) :-
   assertion((nonvar(MaxId),integer(MaxId),MaxId>0)),
   endlessly_propose_id(Id,MaxId,0),
   print_clash_on_redo(Id),
   \+ get_dict(Id,Dict,_),
   debug(clashfree,"FOUND ~q",[Id]),
   !. % make deterministic
      
endlessly_propose_id(Id,MaxId,_) :- 
   random_between(0,MaxId,Id).

endlessly_propose_id(Id,MaxId,Attempts) :- 
   succ(Attempts,AttemptsPlus),
   % throw non-ISO exception if too many attempts reached
   ((AttemptsPlus < 100) -> true ; throw_too_many_attempts(MaxId,AttemptsPlus)),
   endlessly_propose_id(Id,MaxId,AttemptsPlus).

% Print only when being traversed on redo

print_clash_on_redo(_)  :- true.
print_clash_on_redo(Id) :- debug(clashfree,"CLASH for ~q",[Id]),fail.

% throw non-ISO exception if too many attempts reached (there is no good ISO exception for this)

throw_too_many_attempts(MaxId,Attempts) :-
   with_output_to(string(Txt),format("Out of Ids? Too many attempts with MaxId = ~q, Attempts = ~q",[MaxId,Attempts])),
   throw(error(too_many_attempts,context(_Trace,Txt))).


