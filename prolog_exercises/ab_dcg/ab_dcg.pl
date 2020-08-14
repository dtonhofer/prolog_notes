:- module(ab_dcg,
   [
    phrase_acceptable/2
   ,acceptable//1
   ]).

:- use_module(library(clpfd)).

acceptable(0) --> [].
acceptable(N1) -->  `ab`, { N #= N1-1, N #>= 0 }, acceptable(N).

phrase_acceptable(Text,N) :-
   nonvar(Text),!,                   % guard to select "recognize" case
   atom_codes(Text,Codes),           % text-to-recognize can be string or atom or numbner
   phrase(acceptable(N),Codes,[]).
   
phrase_acceptable(Text,N) :- 
   var(Text),!,                      % guard to select "generate" case
   phrase(acceptable(N),Codes,[]),
   atom_codes(Text,Codes).           % generated text is atom
      
