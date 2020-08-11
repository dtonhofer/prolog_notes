:- module(trivials, [jpl_tt_en_primitive//1, jpl_tt_en_void//1, nonempty_digits//1 ]).


% ---
% jpl_tt_en_void//1
% No description found for this; empirical
% ---

jpl_tt_en_void(void) --> 'V'.

% ---
% jpl_tt_en_primitive//1
% Described informally in Javadoc for Class.getName()
% ---

jpl_tt_en_primitive(boolean) --> 'Z',!.
jpl_tt_en_primitive(byte)    --> 'B',!.
jpl_tt_en_primitive(char)    --> 'C',!.
jpl_tt_en_primitive(double)  --> 'D',!.
jpl_tt_en_primitive(float)   --> 'F',!.
jpl_tt_en_primitive(int)     --> 'I',!.
jpl_tt_en_primitive(long)    --> 'J',!.
jpl_tt_en_primitive(short)   --> 'S'.

% ---
% Grab the longest nonempty sequence of digits
% https://eu.swi-prolog.org/pldoc/man?section=basics
% ---

nonempty_digits([D|Ds]) --> digit(D),nonempty_digits(Ds), !.
nonempty_digits([D])    --> digit(D).

% I sure hope the compiler optimizes the decomposition of 0123456789
 
digit(D) --> [D], { memberchk(D,`0123456789`) }.

