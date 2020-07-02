% ===
% Generate or recognize (ab)*
% ===

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
      
:- begin_tests(dcg).

test(generate,[true(Truly)])  :- 
   bagof(T,phrase_acceptable(T,6),Bag),
   Truly = (Bag == ['abababababab']).
   
test(recognize,[true(Truly)]) :- 
   bagof(N,phrase_acceptable("ababab",N),Bag),
   Truly = (Bag == [3]).
   
test(enumerate5,[true(Truly)]) :- 
   bagof([T,N],limit(5,phrase_acceptable(T,N)),Bag), 
   Truly = (Bag == [['',0],['ab',1],['abab',2],['ababab',3],['abababab',4]]).
   
:- end_tests(dcg).

rt(dcg) :- run_tests(dcg).
