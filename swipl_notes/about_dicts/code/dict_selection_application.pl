% ==========
% Testing SWI-Prolog dicts as described at 
% https://eu.swi-prolog.org/pldoc/man?section=bidicts
% ==========

% ===
% A simple application of dict selection: Override default parameters with a dict.
%
% We pick a value from a possibly large dict full of various parameter/value pairs,
% and fall back to a default value if the parameter doesn't exist.
% ===

pick(encoding,Dict,Value) :-
   (_{encoding:Value} :< Dict) -> true ; Value = 'iso-8859-1'.

% ---
% Testing the above
% ---

:- begin_tests(default_values).
   
test("use default encoding", true(Enc == 'iso-8859-1')) :-
   pick(encoding, _{} , Enc).
   
test("override default encoding", true(Enc == 'utf-8')) :-
   pick(encoding, _{foo:bar, encoding:'utf-8'}, Enc).
        
:- end_tests(default_values).
