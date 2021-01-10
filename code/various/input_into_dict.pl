% =============================================================================
% Code to ask the user a series of question (in this particular example, 
% patient information; this code was inspired by a Stack Overflow question).
% Every questions collects a single value from the user, where the value must 
% be found in the list of allowed values for the respective "slot" of the 
% "frame" (Expert System jargon), in other words, for the respective "attribute"
% of the "object". A default, whereby the user may just hit enter instead of
% typing the value, can be provided.
%
% In this example, the "object" is simply implemented by an SWI-Prolog dict
% (i.e. a map), so it doesn't have the trappings of a "real" object.
%
% Once everything has been read and a dict has been filled with the key-value
% pairs one can prettyprint the dict.
% 
% This code is SWI-Prolog specific as it uses "dicts" and "strings".
%
% Here is an example run
% ----------------------
% N.B. The casing of the input string is unimportant
%
% ?- questions(Dict),prettyprint_dict(Dict).
% what is subject name?
% |: J.F. Sebastian
% what is patient risk? (one of: [high,medium,low]) (default: medium)
% |: don't know
% Expecting one of: [high,medium,low], but got: don't know
% what is patient risk? (one of: [high,medium,low]) (default: medium)
% |: LOW
% how long have they had the condition for? (one of: [months,weeks,days]) (default: weeks)
% |: months
% is the trial justifiable? (one of: [yes,no]) (default: yes)
% |: 
% what is their antibody count? (one of: [high,medium,low]) (default: medium)
% |: low
% how do they live? (one of: [sedentary,active]) (default: sedentary)
% |: sedentary
% do they have anaemia? (one of: [yes,no]) (default: no)
% |: 
% is their blood pressure raised? (one of: [yes,no]) (default: no)
% |: 
%   anaemia_state   no   
%   antibody_count  low  
%   blood_pressure  no   
%   clinical_trial  yes  
%   condition_time  months
%   life_style      sedentary
%   name            J.F. Sebastian
%   patient_risk    low  
%
% Dict = patient{anaemia_state:no,antibody_count:low,blood_pressure:no,
%                clinical_trial:yes,condition_time:months,
%                life_style:sedentary,name:"J.F. Sebastian",patient_risk:low}.
% 
% =============================================================================
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% =============================================================================

% ===================
% Slot domains: For each slot, list the allowed values through a list of atoms.
% If the list of atoms is empty, the meaning is "anything is allowed".
%
% slot_domain(?SlotName,?AllowedValues)
% ===================

slot_domain(name,           []).
slot_domain(patient_risk,   [high,medium,low]).
slot_domain(condition_time, [months,weeks,days]).
slot_domain(clinical_trial, [yes,no]).
slot_domain(antibody_count, [high,medium,low]).
slot_domain(life_style,     [sedentary,active]).
slot_domain(anaemia_state,  [yes,no]).
slot_domain(blood_pressure, [yes,no]).
slot_domain(admission,      [inpatient,outpatient,daypatient,waitlist]).

% ==================
% Is the given value allowed for the given slot domain?
% Throws if the slot is unknown or the value is not allowed, otherwise
% succeeds (basically, it behaves like must_be/2)
%
% allowed(+SlotName,+Value)
% ==================

% first level performs argument checks

allowed(SlotName,Value) :-
   must_be(atom,SlotName),
   allowed_2(SlotName,Value).

% helper, which throws if there is no slot_domain/2 fact for SlotName
 
allowed_2(SlotName,Value) :-
   slot_domain(SlotName,_),
   !,
   allowed_3(SlotName,Value).

allowed_2(SlotName,_) :-
   format(string(Msg),"There is no slot_domain/2 fact for slot ~q",[SlotName]),
   throw(Msg). % not a valid ISO exception

% helper, which throws if the value is not allowed in the slot's domain
% note that if the domain is given by the empty list, anything is allowed

allowed_3(SlotName,Value) :- 
   slot_domain(SlotName,Allowed),
   (Allowed == []; memberchk(Value,Allowed)),
   !.

allowed_3(SlotName,Value) :-
   slot_domain(SlotName,Allowed),
   format(string(Msg),"The value ~q is not allowed for slot ~q (allowed are ~q)",[Value,SlotName,Allowed]),
   throw(Msg). % not a valid ISO exception

% ====================
% Check whether all the values in the ValueList are allowed.
% Succeeds if ValueList is empty.
%
% all_allowed(+SlotName,+Values)
% ====================

all_allowed(_SlotName,[]) :- !.

all_allowed(SlotName,[Value|Values]) :- 
   allowed(SlotName,Value),
   all_allowed(SlotName,Values).

% ====================
% Questions to ask user
% 
% q(+SlotName,+Question,+Expecteds,+Default)
%
% with:
%
% SlotName:  Slot name and key under with the collected datum will appear in
%            a SWI-Prolog dict ;
% Question:  A string printed to stdout, the question asked to the user 
%            (includes the question mark) ;
% Expecteds: The empty list if "anything" is accepted or a list of expected 
%            values, one of which will be the answer to the question.
%            Thes need not be strings, they are transformed to strings when
%            comparison against user input is needed.
% Default:   A default value to be used when the user just hits ENTER.
%            Maybe a string, an atom, even an arbitrary term.
%            Must be one of the Expecteds.
%            When set to the empty string or the empty atom or the empty list
%            (or more generally, any X for which atom_string(X,"") succeeds)
%            it is assumed to be missing.
% ====================

% We don't bother to list the Expecteds separately and just get them
% directly from slot_domain/2. But you can use a subset instead for example.

q(name           , "what is subject name?"                     , E , [])        :- slot_domain(name,E).
q(patient_risk   , "what is patient risk?"                     , E , medium)    :- slot_domain(patient_risk,E).
q(condition_time , "how long have they had the condition for?" , E , weeks)     :- slot_domain(condition_time,E).
q(clinical_trial , "is the trial justifiable?"                 , E , yes)       :- slot_domain(clinical_trial,E).
q(antibody_count , "what is their antibody count?"             , E , medium)    :- slot_domain(antibody_count,E).
q(life_style     , "how do they live?"                         , E , sedentary) :- slot_domain(life_style,E).
q(anaemia_state  , "do they have anaemia?"                     , E , no)        :- slot_domain(anaemia_state,E).
q(blood_pressure , "is their blood pressure raised?"           , E , no)        :- slot_domain(blood_pressure,E).

% ====================
% "Question asker".
%
% questions/1: The initial SWIProlog dict is a given and is patient{}
% questions/2: Fill an SWI-Prolog dict with data collected from the user according to 
%              predicate q/4.
% ====================

questions(Dict) :- 
   questions(patient{},Dict).

% Collect questions via bagof/3 and then ask via foldl/3. This seems elegant enough.

questions(DictIn,DictOut) :-
   bagof(
      q(SlotName,Question,Expecteds,Default),
      q(SlotName,Question,Expecteds,Default),
      Questions),
   foldl(ask_user_single_question,Questions,DictIn,DictOut).   


ask_user_single_question(q(SlotName,Question,Expecteds,Default),DictIn,DictOut) :-
   slot_domain(SlotName,Allowed),
   (Expecteds == []
    -> (ReallyExpecteds = Allowed) 
    ;  (all_allowed(SlotName,Expecteds),ReallyExpecteds = Expecteds)),
   (nonempty_stringy(Default) 
    -> allowed(SlotName,Default) 
    ; true),
   obtain_from_user(Question,ReallyExpecteds,Default,Obtained,Quit),
   ((Quit == true) 
    -> fail                                         % If Quit is the atom 'true' the user wants to quit
    ;  put_dict(SlotName,DictIn,Obtained,DictOut)). % otherwise augment dict 

% ====================
% Find an object that "stringy matches" argument "Select" in a list of 
% "stringy things" and also return the *corresponding element of the list*. 
% This may well be something different from "Select", for example an atom,
% whereas "Select" might be a string.
%
% stringy_memberchk(+Select,+ListOfStringyThings,-Found)
% ====================

stringy_memberchk(Select,[Stringy|_],Stringy) :-
   atom_string(Select,Stringy),
   !.

stringy_memberchk(Select,[_|Stringies],Found) :-
   stringy_memberchk(Select,Stringies,Found).

% ====================
% Test for "nonempty list": both a list and nonempty.
% ====================
   
% unify with nonempty start-of-list first for fats failure,
% then verify the list structure extensively

is_nonempty_list(L) :- L=[_|_],is_list(L). 
   
% ====================
% Test for empty or nonempty "stringy thing"
% ====================

empty_stringy(S) :- var(S),!,fail.
empty_stringy(S) :- atom_string(S,""),!.
 
nonempty_stringy(S) :- \+ empty_stringy(S).

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

% ====================
% These are used from rules
% ====================

% ---
% Analyzing and updating a Dict representing a Patient
% ---

look(SlotName,Values,Dict) :-
   must_be(atom,SlotName),
   must_be(list,Values),
   all_allowed(SlotName,Values),
   get_dict(SlotName,Dict,Actual),  % fails if entry does not exist
   must_be(atom,Actual),            % relax this later
   memberchk(Actual,Values).        % may extract value via unification

punch(SlotName,NewValue,DictIn,OldValue,DictOut) :-
   must_be(atom,SlotName),
   must_be(atom,NewValue), 
   allowed(SlotName,NewValue),
   (get_dict(SlotName,DictIn,OldValue) -> true ; OldValue = ''),
   put_dict(SlotName,DictIn,NewValue,DictOut).

% ====================
% Dict prettyprinting
% ====================

prettyprint_dict(Dict) :-
   bagof(
      Key-Value,
      get_dict(Key,Dict,Value),
      Flat),
   keysort(Flat,FlatSorted),
   max_key_length(FlatSorted,MaxKeyLength),
   build_format_string(MaxKeyLength,FormatString),
   format_dict(FlatSorted,FormatString).

max_key_length(Flat,MaxKeyLength) :-
   foldl(
      ([Key-_,FromLeft,ToRight]>>(atom_length(Key,KeyLength),ToRight is max(FromLeft,KeyLength))),
      Flat,
      0,
      MaxKeyLength).
   
build_format_string(MaxKeyLength,FormatString) :-
   FieldWidth is MaxKeyLength+2,
   % set tab ~|, print datum ~a, add tab expanding-filler (will thus left justify) ~t,
   % set next tab stop FieldWidth positions after the previous tab stop ~FieldWidth+,
   % print datum ~a, add tab expanding-filler (will thus left justify) ~t,    
   % set next tab stop 5 positions after the previous tab stop, add newline ~n
   atomics_to_string(['  ~|~a~t~',FieldWidth,'+~a~t~5+~n'],FormatString).
   % format("Format string is: ~q\n",[FormatString]).

format_dict([Key-Value|More],FormatString) :-
   format(FormatString,[Key,Value]),   
   format_dict(More,FormatString).
   
format_dict([],_).

