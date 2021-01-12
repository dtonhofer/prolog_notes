% ===
% Recognize or generate a language of bitstrings
% https://stackoverflow.com/questions/65676002/defining-a-dcg-in-prolog-for-bit-strings
% ===
% 
% For each integer n > 0,
% 
% let Ln := {s ∈ {0, 1}+ | s ends in a string from 1(0 + 1)n−1}
% 
% be the set of bit-strings whose n-th to the last bit is 1. That is, Ln is described by the regular expression
% 
% (0 + 1)∗1(0 + 1)n−1.
% 
% Define a DCG for the 3-ary predicate s/3 such that s(n,s,[]) is true exactly if s encodes a string in Ln.

% ---
% It turns out we need these
% ---

% "any(N,gen)" generates/accepts any bitstring of KNOWN length N
% The order of clauses is unimportant!

any(N,gen) --> { N > 0 }, ( `0` ; `1` ), { Nm is N-1 }, any(Nm,gen).
any(0,gen) --> [].

% "any(N,genfree)" generates/accepts any bitstring of ARBITRARY length N
% The clause for [] must be first and the 01 arbitrary choice must come
% after the recursion.

any(0,genfree) --> [].
any(N,genfree) --> any(Nm,genfree), ( `0` ; `1` ), { N is Nm+1 }.

% ---
% Run the DCG grammer in various modes
% ---

% When generating, use 'x' instead of '1' to make the special '1' visible
% When generating with N unstated we need to make sure we "go to infinity"
% in an equitable manner. Going to infinity for the overall length and 
% dividing that length up between "any" suffix and "any" prefix sounds fair!

my_language(N,gen)     --> any(_,genfree), `1`, any(N,gen).
my_language(N,genfree) --> { between(0,inf,L), between(0,L,N), Prefix is L-N }, any(Prefix,gen), `1`, any(N,gen).

% Pretending the overall length of the bitstring is unknown is quixotic.
% First it's inefficient and then it's awkward to avoid guessing longer
% and longer prefixes that are longer than the string overall and sink
% into the backtracking tarpit. Much better to use the overall bitstring 
% length!

my_language(N,accept,L)     --> { M is L-N-1 }, any(M,gen), `1`, any(N,gen).
my_language(N,acceptfree,L) --> { Lm is L-1, between(0,Lm,N), M is L-N-1 }, any(M,gen), `1`, any(N,gen).

% ===
% Printing
% ===

explain(Text,Bits,N) :-
  atom_chars(Bits,Chars),
  length(Suffix,N),
  append([Prefix,[X],Suffix],Chars),
  format("~s [~s][~s][~s]~n",[Text,Prefix,[X],Suffix]).

% ===
% Call these. In all cases, the remainder must be the empty string because
% we use phrase/2, which is the same as phrase/3 with emtpy remainder
% ===

% accept Bits, with N unstated, and using the overall length of Bits

lang(Bits,N) :-
  var(N),nonvar(Bits),
  !,
  atom_codes(Bits,Codes),
  length(Codes,L),  
  format("Accepting ~q of length ~q, suffix length unstated~n",[Bits,L]),
  phrase(my_language(N,acceptfree,L),Codes),
  explain("Found",Bits,N).

% accept Bits, with N stated, and using the overall length of Bits

lang(Bits,N) :-
  nonvar(N),nonvar(Bits),
  !,
  atom_codes(Bits,Codes),
  length(Codes,L),
  format("Accepting ~q of length ~q, need suffix of length ~q~n",[Bits,L,N]),
  phrase(my_language(N,accept,L),Codes),
  explain("Found",Bits,N).

% generate Bits, with N unstated (and thus proposable by Prolog)
   
lang(Bits,N) :- 
   var(N),var(Bits),
   !,
   format("Generating phrases, nothing stated~n",[]),
   phrase(my_language(N,genfree),Codes), 
   atom_codes(Bits,Codes),
   explain("Generated",Bits,N).

% generate Bits, with N stated

lang(Bits,N) :- 
   integer(N),var(Bits),
   !,
   format("Generating phrases, need suffix of length ~q~n",[N]),
   phrase(my_language(N,gen),Codes),
   atom_codes(Bits,Codes),
   explain("Generated",Bits,N).



 
