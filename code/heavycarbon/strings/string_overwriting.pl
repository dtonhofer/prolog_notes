:- module(heavycarbon_strings_overwriting,
          [
             string_overwriting/4
            ,ovw_helper_mid/6
            ,ovw_helper_top/6
          ]).
  
:- use_module(library('heavycarbon/strings/conversion.pl')).

:- load_files(['heavycarbon/support/meta_helpers_nonmodular.pl']).  % not a module, just predicates

% ===
% (Partially) ovwerwriting an original string "StrIn" with "OverwriteStr" giving a "StrOut".
% The overwriting starts at (0-indexed) "OverwritePos" (possibly negative) and may involve
% overwriting characters in the middle of "StrIn", appending to "StrIn", with or without
% partial overwriting, or appending to "StrIn" and filling any gap between the end of
% "StrIn" and the start of "OverwriteStr" at "OverwritePos" with spaces.
% ===

string_overwriting(StrIn,OvwStr,OvwPos,StrOut) :-
   must_be(integer,OvwPos),               % will throw of non-integer  
   convert_to_string(StrIn,StrInSure),    % will throw if conversion impossible
   convert_to_string(OvwStr,OvwStrSure),  % will throw if conversion impossible
   string_length(StrInSure,StrInLen),
   string_length(OvwStrSure,OvwStrLen),
   (((OvwPos + OvwStrLen =< 0) ; (OvwStrLen == 0))
      ->  % nothing to do; OvwStr is too far left or empty
      leveling_string(StrInSure,StrOut)
      ;   % proceed normally
      (ovw_helper_top(StrInSure,StrInLen,OvwStrSure,OvwStrLen,OvwPos,StrOutSure),
       leveling_string(StrOutSure,StrOut))).

% ---
% Helpers
% --- 

ovw_helper_top(StrIn,StrInLen,OvwStr,OvwStrLen,OvwPos,StrOut) :-
   if_then_else(
      % if
      (OvwPos < 0),
      % then cut off any characters of PatchStr that are not visible 
      ((Before is -OvwPos,
        OvwStrLenFixed is OvwStrLen-Before,
        sub_string(OvwStr,Before,OvwStrLenFixed,0,OvwStrFixed),
        ovw_helper_mid(StrIn,StrInLen,OvwStrFixed,OvwStrLenFixed,0,StrOut))),
      % else proceed normally
      (ovw_helper_mid(StrIn,StrInLen,OvwStr,OvwStrLen,OvwPos,StrOut))).
    
ovw_helper_mid(StrIn,StrInLen,OvwStr,OvwStrLen,OvwPos,StrOut) :-
   OvwPosEnd is OvwPos+OvwStrLen,
   switch(
      % a suffix of StrInLen (and maybe a prefix) must be kept
      (OvwPosEnd < StrInLen), 
      (ovw_string_with_suffix(StrIn,OvwStr,OvwPos,OvwPosEnd,StrOut)),
      % only (maybe) a prefix of StrInLen must be kept
      (OvwPos < StrInLen),
      (ovw_string_without_suffix(StrIn,OvwStr,OvwPos,StrOut)),
      % the OvwStr shall be appended to StrIn with possibly spaces in between
      (StrInLen =< OvwPos),
      (ovw_string_append(StrIn,StrInLen,OvwStr,OvwPos,StrOut)),
      % else never happens
      (cannot_happen_error("impossible case"))).

ovw_string_with_suffix(StrIn,OvwStr,OvwPos,OvwPosEnd,StrOut) :-
   sub_string(StrIn,0,OvwPos,_,Prefix),
   sub_string(StrIn,OvwPosEnd,_,0,Suffix),
   string_concat(Prefix,OvwStr,SX),
   string_concat(SX,Suffix,StrOut).
 
ovw_string_without_suffix(StrIn,OvwStr,OvwPos,StrOut) :-
   sub_string(StrIn,0,OvwPos,_,Prefix),
   string_concat(Prefix,OvwStr,StrOut).

ovw_string_append(StrIn,StrInLen,OvwStr,OvwPos,StrOut) :-
   SpaceCount is OvwPos-StrInLen,
   string_of_spaces(SpaceCount,Spaces),
   string_concat(StrIn,Spaces,SX),
   string_concat(SX,OvwStr,StrOut).


