% ===
% Some simple code to transform non-negative integers 
% into lists of bits and the converse.
%
% See https://eu.swi-prolog.org/pldoc/doc_for?object=f((%3E%3E)/2)
%
% TODO: Extend for negative integers for a known number of bits to
% be able to do 2-complement
% ===
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% ===


% ---
% "bits to atom"
%
% ?- bits_atom(X,'101011').
% X = [1, 0, 1, 0, 1, 1].
% 
% ?- bits_atom([1, 0, 1, 0, 1, 1],A).
% A = '101011'.
% 
% ?- bits_atom(X,'foo').
% false.
% ---

bits_atom(Bits,Atom) :-
   nonvar(Bits), % atom can be unbound or not
   !,
   maplist([B,ASCII]>>(memberchk(B,[0,1]),ASCII is 0'0+B),Bits,Chars),
   atom_chars(Atom,Chars).

bits_atom(Bits,Atom) :-
   var(Bits),nonvar(Atom),
   !,
   atom_codes(Atom,Codes),
   maplist([B,ASCII]>>(B is ASCII - 0'0,memberchk(B,[0,1])),Bits,Codes).

% ---
% Remove leading 0s. Also returns the number of 0s trimmed.
% 
% ?- bits_trim([0,0,0,1,0],X,N).
% X = [1, 0].
% 
% ?- bits_trim([0,0,0,2,0],X,N).
% false.
% ---
   
bits_trim(Orig,Trimmed,N) :-
   must_be(nonvar,Orig),
   bits_trim_2(Orig,Trimmed,0,N).

bits_trim_2([0],[0],N,N) :- !.
bits_trim_2([1|Bits],[1|Bits],N,N) :- !.

bits_trim([0|Bits],BitsTrimmed,K,N) :-
   Kp is K+1,
   bits_trim(Bits,BitsTrimmed,Kp,N).
   
% ---
% Prepend 0s to yield a bits list of length L. 
% Just succeeds if there is nothing to prepend, even for a negative number of 0s.
% Also returns the number of 0s prepended (may be negative, in which
% case nothing has been prepended).
% ---

bits_pad(Orig,Padded,PadTo,N) :-
   must_be(nonvar,Orig),
   must_be(integer,PadTo),
   length(Orig,OrigL),
   N is PadTo - OrigL,
   bits_pad_2(N,Orig,Padded).
   
bits_pad_2(0,Bits,Bits) :- !.

bits_pad_2(N,Bits,[0|BitsPadded]) :-
   N > 0,!,
   Nm is N-1,
   bits_pad_2(Nm,Bits,BitsPadded).
   
% ---
% Transform an integer value V to a list Bits of 0,1
% or
% Transform a list Bits of 0,1 to an integer value V
%
% Bits may be a list, possibly with elements already bound to 0,1.
% If that list is longer than needed, 0s are filled into the
% prefix of surplus bits.
%
% Also sets the "Prefix Length of 0" in Plen.
% ---
   
val_bits(V,Bits,Plen) :-
   nonvar(V),
   must_be(nonneg,V),
   !,   
   val_bits_2(V,[],BitsList2),
   (
      (var(Bits),var(Plen)) 
      -> (Bits = BitsList2, Plen=0)
      ; 
      (var(Bits),nonvar(Plen))     
      -> (length(BitsList2,Len),NewLen is Len+Plen,bits_pad(BitsList2,Bits,NewLen,_))
      ;
      (nonvar(Bits)) 
      -> (length(Bits,BitsLen),bits_pad(BitsList2,Bits,BitsLen,Plen))
   ).

val_bits_2(0,[],[0]) :- !.

val_bits_2(0,[B|Bs],[B|Bs]) :- !.
     
val_bits_2(V,BitsAcc,BitsFin) :-
   V > 0,
   !,
   V2 is V div 2,
   Bit is V mod 2,
   val_bits_2(V2,[Bit|BitsAcc],BitsFin). % may not be tail-recursive   
   
   
% ---
% Testing
% ---

:- begin_tests(bits).

test("v to bits: 0",true(Bits == [0])) :-
   val_bits(0,Bits,_).

test("v to bits: 1",true(Bits == [1])) :-
   val_bits(1,Bits,_).

test("v to bits: 2",true(Bits == [1,0])) :-
   val_bits(2,Bits,_).

test("v to bits: 3",true(Bits == [1,1])) :-
   val_bits(3,Bits,_).

test("v to bits: 7",true(Bits == [1,1,1])) :-
   val_bits(7,Bits,_).

test("v to bits: 7, with padding by 0 0s",true(Bits == [1,1,1])) :-
   val_bits(7,Bits,0).
   
test("v to bits: 7, with padding by 2 0s",true(Bits == [0,0,1,1,1])) :-
   val_bits(7,Bits,2).

test("v to bits: 7, with padding by -1 0s",fail) :-
   val_bits(7,_,-1).
      
test("v to bits: 459549",true(A == '1110000001100011101')) :-
   val_bits(459549,Bits,0),
   bits_atom(Bits,A).

test("v to bits: 7, with size-fitting partial solution provided",true(X == 1)) :-
   val_bits(7,[1,X,1],0).

test("v to bits: 7, with too-small partial solution provided",fail) :-
   val_bits(7,[_,_],_).

test("v to bits: 0 with leading 0s as partial solution of 15 bits provided",true([A,Plen] == ['000000000000000',14])) :-
   length(Bits,15),
   val_bits(0,Bits,Plen),
   bits_atom(Bits,A).
   
test("v to bits: 1221 with leading 0s as partial solution of 15 bits provided",true([A,Plen] == ['000010011000101',4])) :-
   length(Bits,15),
   val_bits(1221,Bits,Plen),
   bits_atom(Bits,A).
      
test("bits_atom, bad bits",fail) :-
   bits_atom([0,1,2,0,1],_).

test("bits_atom, bad atom",fail) :-
   bits_atom(_,'foo').

test("bits_atom, both directions",true) :-
   maplist(
      [V]>>(val_bits(V,Bits,_),
            bits_atom(Bits,Atom),   % Bits -> Atom
            bits_atom(Bits2,Atom),  % Atom -> Bits2
            Bits == Bits2),
      [0,1,2,3,4,31,454,34721,45276587,4897592489]).
         
:- end_tests(bits).
