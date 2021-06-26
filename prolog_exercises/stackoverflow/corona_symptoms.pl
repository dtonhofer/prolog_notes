% ---
% Symptoms are listed naturally in a list.
% There are 11 symptoms

symptoms( [conjunctivitis,
           sore_throat,
           fatigue,
           shake,
           diarrhea,        
           runny_nose,      
           cold,
           muscle_pain,
           chest_pressure,
           loss_of_taste,          
           fever] ).

% ---
% Go through the symptoms list, but don't use read/1
% instead us read_line_to_string/2
% https://eu.swi-prolog.org/pldoc/man?predicate=read_line_to_string/2

ask_about_symptoms([],[]).
ask_about_symptoms([Symptom|MoreSymptoms],[Answer-Symptom|MorePairs]) :-
   repeat,
   format("Do you have: ~s?\n",[Symptom]),
   read_line_to_string(user_input,S1),
   string_lower(S1,S2),
   (
      member(S2,["yes","y","ja","oui"])
      ->
      Answer = yes
      ;
      member(S2,["no","n","nein","non"])
      ->
      Answer = no
      ;
      format("Please try again\n",[]),fail % Back to "repeat"
   ),   
   ask_about_symptoms(MoreSymptoms,MorePairs).
    
is_yes_pair(yes-_).

ask_candidate :-
   symptoms(Symptoms),
   ask_about_symptoms(Symptoms,AnswerPairs),
   format("So you say you have the following symptoms: ~q~n",[AnswerPairs]),
   !, % don't go back to asking
   include(is_yes_pair,AnswerPairs,Included),
   format("So you say you have the following symptoms: ~q~n",[Included]),
   length(Included,Hits),
   ((Hits>=3) -> format("Looks bad.~n",[]) ; format("You probably don't have it.~n",[])).
   

