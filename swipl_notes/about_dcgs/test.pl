% ==
% Generate or test strings obeying the Perl Regex `(ab)*`
% ==

ff(X) :- var(X).    % "freshvar(X)" using 2 letters
bb(X) :- nonvar(X). % "notfreshvar(X)" using 2 letters

acceptable(0) --> [].
acceptable(N) --> { bb(N), N>0, succ(Nm,N) }, `ab`, acceptable(Nm).
acceptable(N) --> { ff(N) }, `ab`, acceptable(Nm), { succ(Nm,N) }.

phrase_acceptable(Text,N) :-
   bb(Text),!,
   % erroneously putting atom_chars/2 here causes phrase/3 to fail
   % atom_codes/2 throws if Text is not of the correct type
   % but an integer or float is stringified first
   atom_codes(Text,Codes),
   writeln(Codes),
   phrase(acceptable(N),Codes,[]).
   
phrase_acceptable(Text,N) :-
   ff(Text),!,
   phrase(acceptable(N),Codes,[]),
   atom_codes(Text,Codes).

