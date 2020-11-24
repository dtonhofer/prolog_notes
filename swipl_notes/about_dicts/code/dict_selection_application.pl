% ===
% A simple application of dict selection
% 
% Override defalt parameters with a dict.
% We pick a value from a possibly large dict full of various parameters, 
% and fall back to a default if it doesn't exist.

% success of Dict contains a key 'encoding'; then Value contains the value

pick(encoding,Dict,Value) :-
   (_{encoding:Value} :< Dict) -> true ; Value = 'iso-8859-1'.

:- begin_tests(default_values).
   
test("Use default encoding", true(Enc == 'iso-8859-1')) :-
   pick(encoding, _{} , Enc).
   
test("Override default encoding", true(Enc == 'utf-8')) :-
   pick(encoding, _{foo:bar, encoding:'utf-8'}, Enc).
        
:- end_tests(default_values).
