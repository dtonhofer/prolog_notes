% ===
% Hierarchical sorting with 2-element commalists
% See https://stackoverflow.com/questions/65264756/sorting-a-list-in-prolog-based-on-2-keys
% ===

% ---
% Serious testing is serious
% ---

list_of_random_pairs(List,Length) :-
   length(List,Length),
   maplist(
      [(Char,Num)]>>(
         random_between(0,9,Num),
         random_between(97,122,Code),  % 16-bit Unicode codepoint of a character in the range a-z
         atom_codes(Char,[Code])       % transform code into "character" aka. "char", i.e. atom of length 1
      ),
      List).
      
% ---                        
% Sort ListIn of (Char,Num) pairs into ListOut (sorted by Num, then by Char,
% with duplicates eliminated).
%
% ?- list_of_random_pairs(List,1000000), time(my_sort_1(List,ListOut)).
% % 2,574,560 inferences, 0.658 CPU in 0.661 seconds (100% CPU, 3911720 Lips)
% ---

my_sort_1(ListIn,ListOut) :-
   maplist([(Char,Num),(Num,Char)]>>true,ListIn,List2),  % build list of Num-Char while keeping order
   predsort(compare,List2,List3),                        % sort Num-Char list according to the "standard order of terms"
   maplist([(Char,Num),(Num,Char)]>>true,ListOut,List3). % build list of Char-Num while keeping new order

% ---
% More compact than the above but actually slower, presumably because it
% needs to create two new pairs on every comparison instead of 2 times
% one pair on each list position.
% 
% ?- list_of_random_pairs(List,100000), time(my_sort_2(List,ListOut)).
% % 3,064,787 inferences, 0.780 CPU in 0.783 seconds (100% CPU, 3928340 Lips)
% ---

my_sort_2(ListIn,ListOut) :-
   predsort(([Order,(Char1,Num1),(Char2,Num2)]>>compare(Order,(Num1,Char1),(Num2,Char2))),ListIn,ListOut).

% ---
% Compact but tabled version. This one is extremely slow. Tabling doesn't pay for
% such simple constructions like building pairs! (there are 230^2 = 52900 different
% ways this predicate can be called).
% 
% ?- list_of_random_pairs(List,100000), time(my_sort_3(List,ListOut)).
% 6,941,128 inferences, 3.124 CPU in 3.143 seconds (99% CPU, 2221892 Lips)
% ---

my_sort_3(ListIn,ListOut) :-
   table(my_compare/3 as private),
   predsort(my_compare,ListIn,ListOut).
   untable(my_compare/3).
   
my_compare(Order,(Char1,Num1),(Char2,Num2)) :-
   compare(Order,(Num1,Char1),(Num2,Char2)).

% ---
% Testing
% Add 
% set_prolog_flag(answer_write_options,[max_depth(100)]).
% when printing
% ---

:- begin_tests(sorting).

length(1000).
      
test(my_sort_1) :-   
   length(Length),
   list_of_random_pairs(List,Length),
   my_sort_1(List,ListOut),
   debug(test_sorting,"List: ~q",[List]),
   debug(test_sorting,"ListOut as sorted by my_sort_1/2: ~q",[ListOut]),   
   assertion(my_sort_2(List,ListOut)),      % it gives same results as the other
   assertion(my_sort_1(ListOut,ListOut)).   % it is idempotent

test(my_sort_2) :-
   length(Length),
   list_of_random_pairs(List,Length),
   my_sort_2(List,ListOut),
   debug(test_sorting,"List: ~q",[List]),
   debug(test_sorting,"ListOut as sorted by my_sort_2/2: ~q",[ListOut]),   
   assertion(my_sort_1(List,ListOut)),      % it gives same results as the other
   assertion(my_sort_2(ListOut,ListOut)).   % it is idempotent

:- end_tests(sorting).
