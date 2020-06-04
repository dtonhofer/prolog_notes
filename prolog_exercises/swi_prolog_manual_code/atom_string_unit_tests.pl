
% https://eu.swi-prolog.org/pldoc/doc_for?object=atom_string/2

:- begin_tests(atom_string).

% ---
% Type-Transforming leniently 
% ---

% Takes string or atom on the left but generates string on the right

test(stringout_right_1,true(S == "atom")) :- atom_string('atom',S).
test(stringout_right_2,true(S == "atom")) :- atom_string("atom",S).

% Takes string or atom on the right but generates atom on the left

test(atomout_left_1   ,true(S == 'atom')) :- atom_string(S,'atom'). 
test(atomout_left_2   ,true(S == 'atom')) :- atom_string(S,"atom").

% ---
% Comparing leniently
% ---

% Atom on the left is good, whatever is on the right (agree)

test(same1) :- atom_string('atom',"atom").
test(same4) :- atom_string('atom','atom').

% ---
% Comparing strictly
% ---

% String on the left is bad, whatever is on the right 

test(same2,fail) :- atom_string("atom",'atom'). % Consistent but essentially surprising
test(same3,fail) :- atom_string("atom","atom"). % Inconsistent with test stringout_right_2

% ---
% Tests are not expected to be surprising
% ---

test(notsame1,fail)       :- atom_string('mota',"atom").
test(notsame2,fail)       :- atom_string("mota",'atom').
test(notsame3,fail)       :- atom_string("mota","atom").
test(notsame4,fail)       :- atom_string('mota','atom').

:- end_tests(atom_string).

rt(atom_string) :- version,run_tests(atom_string).
