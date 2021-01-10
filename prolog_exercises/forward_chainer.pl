% ===
% Call like this:
%
% ?- questions(Dict),chain(X,Dict,Out).
%
% You will asked about patient data, then the chainer will do one step
% to find the value for slot "admission".
% ===

% ===================
% Slot typing
%
% slot(+Key,+AllowedValues)
% ===================

slot(name,           []).
slot(patient_risk,   [high,medium,low]).
slot(condition_time, [months,weeks,days]).
slot(clinical_trial, [yes,no]).
slot(antibody_count, [high,medium,low]).
slot(life_style,     [sedentary,active]).
slot(anaemia_state,  [yes,no]).
slot(blood_pressure, [yes,no]).
slot(admission,      [inpatient,outpatient,daypatient,waitlist]).

allowed(Key,Value) :-
   must_be(atom,Key),
   allowed_2(Key,Value).
 
allowed_2(Key,Value) :-
   slot(Key,Allowed),
   !,
   allowed_3(Key,Value,Allowed).

allowed_2(Key,_) :-
   format(string(Msg),"There is no slot typing entry for key ~q",[Key]),
   throw(Msg).

allowed_3(_,Value,Allowed) :- 
   memberchk(Value,Allowed),!.

allowed_3(Key,Value,Allowed) :-
   format(string(Msg),"The value ~q is not allowed for slot ~q (allowed are ~q)",[Value,Key,Allowed]),
   throw(Msg).

all_allowed(_Key,[]) :- !.
all_allowed(Key,[Value|Values]) :- 
   allowed(Key,Value),
   all_allowed(Key,Values).

% ====================
% Questions to ask
% 
% q(+Key,+Question,+Expecteds,+Default)
%
% with:
% Key:       The Key under with the collected datum will appear in a SWI-Prolog dict
% Question:  A string printed to stdout, the question asked to the user (includes the question mark)
% Expecteds: The empty list if anything is accepted or a list of expected values,
%            one of which will be found in Found after the call. These need not be
%            strings, they are transformed to strings when comparison against user
%            input is needed.
% Default:   A default value to be used when the user just hits ENTER.
%            Maybe a string, an atom, even an arbitrary term.
%            When set to the empty string or the empty atom or the empty list
%            (or more generally, any X for which atom_string(X,"") succeeds)
%            it is assumed to be missing.
% ====================

q(name           , "what is subject name?"                     , Vals , [])        :- slot(name,Vals).
q(patient_risk   , "what is patient risk?"                     , Vals , medium)    :- slot(patient_risk,Vals).
q(condition_time , "how long have they had the condition for?" , Vals , weeks)     :- slot(condition_time,Vals).
q(clinical_trial , "is the trial justifiable?"                 , Vals , yes)       :- slot(clinical_trial,Vals).
q(antibody_count , "what is their antibody count?"             , Vals , medium)    :- slot(antibody_count,Vals).
q(life_style     , "how do they live?"                         , Vals , sedentary) :- slot(life_style,Vals).
q(anaemia_state  , "do they have anaemia?"                     , Vals , no)        :- slot(anaemia_state,Vals).
q(blood_pressure , "is their blood pressure raised?"           , Vals , no)        :- slot(blood_pressure,Vals).

% ====================
% "Question asker".
%
% questions/2: Fill an SWI-Prolog dict with data collected from the user according to 
%              predicate q/4.
% questions/1: The initial SWIProlog dict is a given and is patient{}
% ====================

questions(Dict) :- 
   questions(patient{},Dict).

% Collect questions via bagof/3 and then ask via foldl/3. This seems elegant enough.

questions(DictIn,DictOut) :-
   bagof(
      q(Key,Question,Expecteds,Default),
      q(Key,Question,Expecteds,Default),
      Questions),
   foldl(ask_user_single_question,Questions,DictIn,DictOut).   


ask_user_single_question(q(Key,Question,Expecteds,Default),DictIn,DictOut) :-
   slot(Key,Allowed),
   (Expecteds == []
    -> (ReallyExpecteds = Allowed) 
    ;  (all_allowed(Key,Expecteds),ReallyExpecteds = Expecteds)),
   (nonempty_stringy(Default) 
    -> allowed(Key,Default) 
    ; true),
   obtain_from_user(Question,ReallyExpecteds,Default,Obtained,Quit),
   ((Quit == true) 
    -> fail                                    % If Quit is the atom 'true' the user wants to quit
    ;  put_dict(Key,DictIn,Obtained,DictOut)). % otherwise augment DictOut

% ====================
% Find a string match in a list of stringy things, but also return it (not
% the LookFor that was input, but the element in the list, which may be of different type!)
% stringy_memberchk(+LookFor,+ListOfStringyThings,-Found)
% ====================

stringy_memberchk(X,[S|_],S) :-
   atom_string(X,S),!.

stringy_memberchk(X,[_|Ss],F) :-
   stringy_memberchk(X,Ss,F).

% ====================
% Test for nonempty list
% ====================
   
is_nonempty_list(L) :- is_list(L),L\==[].
   
% ====================
% Test for nonempty stringy thing
% ====================

nonempty_stringy(S) :- var(S),!,fail.
nonempty_stringy(S) :- atom_string(S,""),!,fail.
nonempty_stringy(_).

% ====================
% Test for empty stringy thing
% ====================

empty_stringy(S) :- var(S),!,fail.
empty_stringy(S) :- atom_string(S,""),!.
 
% ====================
% Ask the user until a good answer has been obtained
% obtain_from_user(+Message,+Expecteds:List,+Default,-Obtained,-Quit)
% ====================

obtain_from_user(Message,Expecteds,Default,Obtained,Quit) :-
   format("~s",[Message]),
   (is_nonempty_list(Expecteds) -> format(" (one of: ~q)",[Expecteds]) ; true),
   (nonempty_stringy(Default)   -> format(" (default: ~q)",[Default]) ; true),
   format("~n",[]),
   (
      (read_string(user_input, "\n", "\t ", _, RawInput),   % read input up to \n (anything after that stays in the input buffer)
       expect(Expecteds,Default,RawInput,Obtained,Quit))
      -> 
      true 
      ; 
      obtain_from_user(Message,Expecteds,Default,Obtained,Quit)  % ask again because that input was bad
   ).

% ====================
% Process user input
% expect(+Expecteds,+Default,+Input,-Obtained,-Quit)
% ====================

% ---
% User entered empty string. 
% Obtained becomes Default if Default is nonempty.
% ---

expect(_,Default,"",Default,false) :-
   nonempty_stringy(Default),!.
   
% ---
% User entered empty string. 
% Failure obtaining something if Default is empty.
% ---

expect(_,Default,"",_,_) :-
   empty_stringy(Default),!,fail.

% ---
% User wants to quit
% ---

expect(_,_,Input,'',true) :-            % The Obtained is arbitrarily set to ''
   string_lower(Input,InputLower),
   stringy_memberchk(InputLower,["q","quit","exit"],_),!.

% ---
% There are no expected strings -> freestyle entry!
% ---

expect([],_,Input,Input,false) :- !.

% ---
% User entered an expected string (possibly cased badly but we fix that)
% ---

expect(Expecteds,_,Input,Obtained,false) :-   
   string_lower(Input,InputLower),
   stringy_memberchk(InputLower,Expecteds,Obtained),!.

% ---
% User entered an unexpected string; failure after helpful output
% ---

expect(Expecteds,_,Input,_,_) :-
   string_lower(Input, InputLower),
   \+ stringy_memberchk(InputLower,Expecteds,_),
   format("Expecting one of: ~q, but got: ~s~n",[Expecteds,InputLower]),
   fail.

% ---
% Analyzing and updating a Dict representing a Patient
% ---

look(Key,Values,Dict) :-
   must_be(atom,Key),
   must_be(list,Values),
   all_allowed(Key,Values),
   get_dict(Key,Dict,Actual),  % fails if entry does not exist
   must_be(atom,Actual),       % relax this later
   memberchk(Actual,Values).   % may extract value via unification

punch(Key,NewValue,DictIn,OldValue,DictOut) :-
   must_be(atom,Key),
   must_be(atom,NewValue), 
   allowed(Key,NewValue),
   (get_dict(Key,DictIn,OldValue) -> true ; OldValue = ''),
   put_dict(Key,DictIn,NewValue,DictOut).

% ---
% Rules
% ---

chain(clinical_trial_not_ok,DictIn,DictOut) :-
   look(patient_risk,[high],DictIn),
   punch(clinical_trial, no         , DictIn, _, Dict2),
   punch(admission,      daypatient , Dict2 , _, DictOut),
   format("The recommended admission is ~s~n",[DictOut.admission]).

chain(condition_months,DictIn,DictOut) :-
   look(patient_risk,[low,medium],DictIn),
   look(condition_time,[months],DictIn),
   punch(antibody_count, high       , DictIn , _, Dict2),
   punch(admission,      daypatient , Dict2  , _, DictOut),
   format("The recommended admission is ~s~n",[DictOut.admission]).

chain(condition_days,DictIn,DictOut) :-
   look(condition_time,[days],DictIn),
   punch(antibody_count, low        , DictIn , _, Dict2),
   punch(admission,      outpatient , Dict2  , _, DictOut),
   format("The recommended admission is ~s~n",[DictOut.admission]).

chain(rule_active_not_anaemic,DictIn,DictOut) :-
   look(patient_risk,[low,medium],DictIn),
   look(condition_time,[weeks],DictIn),
   look(life_style,[active],DictIn),
   look(anaemia_state,[no],DictIn),
   punch(admission, outpatient , DictIn ,_, DictOut),
   format("The recommended admission is ~s~n",[DictOut.admission]).

chain(sedentary_highblood_anaemic,DictIn,DictOut) :-
   look(patient_risk,[low,medium],DictIn),
   look(life_style,[sedentary],DictIn),
   look(anaemia_state,[yes],DictIn),
   look(blood_pressure,[yes],DictIn),
   punch(admission, inpatient ,DictIn ,_,DictOut),
   format("The recommended admission is ~s~n",[DictOut.admission]).

chain(active_and_anaemic,DictIn,DictOut) :-
   look(patient_risk,[low,medium],DictIn),
   look(condition_time,[weeks],DictIn),
   look(life_style,[active],DictIn),
   look(anaemia_state,[yes],DictIn),
   punch(admission, waitlist ,DictIn ,_,DictOut),
   format("The recommended admission is ~s~n",[DictOut.admission]).

chain(sedentary_fineblood_anaemic,DictIn,DictOut) :-
   look(patient_risk,[low,medium],DictIn),
   look(life_style,[sedentary],DictIn),
   look(anaemia_state,[yes],DictIn),
   look(blood_pressure,[no],DictIn),
   punch(admission, waitlist ,DictIn ,_,DictOut),
   format("The recommended admission is ~s~n",[DictOut.admission]).

chain(sedentary_not_anaemic,DictIn,DictOut) :-
   look(patient_risk,[low,medium],DictIn),
   look(life_style,[sedentary],DictIn),
   look(anaemia_state,[no],DictIn),
   punch(admission, waitlist ,DictIn ,_,DictOut),
   format("The recommended admission is ~s~n",[DictOut.admission]).

