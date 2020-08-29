:- module(heavycarbon_strings_stringy,
   [
       stringy/1         % stringy(X)                       X is stringy or can it be transformed into a string
      ,stringy_length/2  % string_length(S,L)               L is the length of stringy thing S
      ,stringy_chars/3   % stringy_chars(S,Cs,W)            S is the string thingy made from chars in Cs, W is the atom giving the type of S
      ,stringy_concat/4  % string_concat(S1,S2,SX,Want)     SX is the concatenation of stringy thing S1,S2. W is the type of SX (going S1,S2->SX)
      ,stringy_ensure/3  % string_ensure(S,SX,W)            SX is the stringy thing S with type W
   ]).

:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')). % Not a module, just (meta) predicates

% This is a nice exercise in thinking about how to approach writing
% Prolog predicates that give information on failure.

% TODO: Capture "wrong direction" problems right at entry and throw.
%       Throw early while the semantics of the problem are still visible is good,
%       even if it slows down the program. Hell, wrap things in assertions if you must,
%       so the code can be removed later.
% TODO: Better exceptions using throwme/2. The ISO throwers are inadequate.
% TODO: Replace -> by meta-predicates. Test slowdown.
% TODO: Add some test cases

% ==============================================================================
% A few very simple predicates that try to make "manipulation of strings"
% and "manipulation of atoms" a bit more uniform.
%
% In particular, in SWI Prolog there currently function-like predicates
% taking strings or atoms as input (they are converted as needed)
% and generating either strings or atoms as output depending on the name of
% the predicate. Here, the selection of whether the output should be a
% string or an atom is made using a third atom argument, which is either
% 'string' or 'atom'
% ==============================================================================

% ---
% Succeed if X is something "stringy", in particular an "atom" or a "string",
% or can be transformed into an atom or a string, as is the case for a "number".
%
% Succeeds or fails only.
% ---

stringy(X) :-
   ground(X),
   (atom(X);string(X);number(X)),!. % terminal ! to make this deterministic

% ---
% This is not needed a atom_length/2 and string_length/2 works for both strings
% and atoms, but it removes the specificity of using atom_length/2 or
% string_length/2 from the program text.
%
% This only works in direction +S -> -L. May throw.
% ---

stringy_length(S,L) :-
   switch(
      atom(S),    atom_length(S,L),    % arg checking is left to atom_length/2    
      string(S),  string_length(S,L),  % arg checking is left to to string_length/2   
      ground(S),  string_length(S,L),  % neither an atom nor a string; arbitrarily leave it to string_length/2, which may throw or not   
      domain_error(_,_)).              % placeholder throw; don't bother with getting this exception ISO-correct; TODO throw your own

% ---
% This is not needed as atom_length/2 and string_length/2 work for both strings
% and atoms, but it removes specificity of using atom_chars/2 or string_chars/2
% from the program text.
%
% This works both in directions:
%
% (+S->-Chars | -Want) (preferentially), where "Want" is unified with 'string' or
% 'atom' or% "other" depending on the type of S.
%
% (+Chars->-S | +Want), where "Want" determines whether you get a string or an
% atom depending on whether it is the atom 'string' or 'atom'.
%
% This can fail in many ways if the arguments are not as expected. It may throw.
% ---

stringy_chars(S,Chars,Want) :-
   switch(
      stringy(S)  , stringy_chars_when_S_is_stringy(S,Chars,Want),
      var(S)      , stringy_chars_when_S_is_var(S,Chars,Want),
      domain_error(_,_)).      % placeholder throw

stringy_chars_when_S_is_stringy(S,Chars,Want) :-
   switch(
      atom(S)     , (Want=atom,atom_chars(S,Chars)),
      string(S)   , (Want=string,string_chars(S,Chars)),
      (Want=other,string_chars(S,Chars))). % neither an atom nor a string (e.g. integer); try string_chars

stringy_chars_when_S_is_var(S,Chars,Want) :-
   switch(
      Want==atom   , atom_chars(S,Chars),
      Want==string , string_chars(S,Chars),
      var(Want)    , (Want=string,string_chars(S,Chars)), % force to string processing
      domain_error(_,_)).     % placeholder throw

% ---
% Concatenate mode: concatenate two stringy things into a third, which shall have the type given
% by "Want" (either 'string' or 'atom').
%
% However, if this predicate works in "generate mode", then the situation about "Want" is not
% so clear-cut. Does it express type of SX? Does it express the wanted type of S1 and S2? For
% now, assume that if it is set, it selects between atom_concat and string_concat.
% ---

stringy_concat(S1,S2,SX,Want) :-
   switch(
      stringy(SX) , stringy_concat_when_SX_is_stringy(S1,S2,SX,Want),
      var(SX)     , stringy_concat_when_SX_is_var(S1,S2,SX,Want),
      type_error(_,_)).  % placeholder throw

stringy_concat_when_SX_is_var(S1,S2,SX,Want) :-
   switch(
      Want==atom   , atom_concat(S1,S2,SX),
      Want==string , string_concat(S1,S2,SX),
      domain_error(_,_)).  % ISO levels of awkward, don't bother

stringy_concat_when_SX_is_stringy(S1,S2,SX,Want) :-
   switch(
      Want==atom           , atom_concat(S1,S2,SX),
      Want==string         , string_concat(S1,S2,SX),
      (var(Want),atom(SX)) , (Want=atom,atom_concat(S1,S2,SX)),
      var(Want)            , (Want=string,string_concat(S1,S2,SX)),   % don't even test the type of SX
      domain_error(_,_)).  % ISO levels of awkward, don't bother

% ---
% Transforming a string thing S into the stringy thing SX, which shall have
% the type given by "Want" (either 'string' or 'atom')
% ---

stringy_ensure(S,SX,Want) :-
   stringy_ensure_fast(S,SX,Want).
 
stringy_ensure_fast(S,SX,Want) :-
   switch(
      Want==atom   , atom_concat('',S,SX),  
      Want==string , string_concat("",S,SX), 
      domain_error(_,_)).  % ISO levels of awkward, don't bother

stringy_ensure_slow(S,SX,Want) :-
   switch(
      Want==atom   , atom_concat(S,'',SX),   % apparently a bit slower than the above (maybe)
      Want==string , string_concat(S,"",SX), % apparently a bit slower than the above (maybe)
      domain_error(_,_)).  % ISO levels of awkward, don't bother
 
