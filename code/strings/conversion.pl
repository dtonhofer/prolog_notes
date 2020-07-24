% ===
% Perform certain type conversion in an easy-on-the-programmer's mind matter
% ===

% clause 1: just need to make sure X is nonvar (groundedness etc are checked by atom_string/2)
% clause 2: raise exception if X is a var

convert_to_string(X,Str) :- nonvar(X),!,atom_string(X,Str).  
convert_to_string(X,_)   :- var(X),!,instantiation_error(X). 
   
convert_to_atom(X,Atom)  :- nonvar(X),!,atom_string(Atom,X).
convert_to_atom(X,_ )    :- var(X),!,instantiation_error(X).


