% ===
% From https://stackoverflow.com/questions/63314500/how-to-join-rules-and-print-out-outputs-in-prolog
% ===

:- use_module(library('heavycarbon/strings/string_of_spaces.pl')).
:- use_module(library('heavycarbon/strings/string_overwriting.pl')).

items(itemId('P01'),prodName('Pots'),stockOty(50),price(8200)).
items(itemId('P02'),prodName('Pans'),stockOty(50),price(400)).
items(itemId('P03'),prodName('Spoons'),stockOty(50),price(200)).
items(itemId('P04'),prodName('Forks'),stockOty(50),price(120)).
items(itemId('P05'),prodName('Kettles'),stockOty(50),price(500)).
items(itemId('P06'),prodName('Plates'),stockOty(50),price(60)).

printthem :-
   % ideally these should be built by getting max(length) over a column - hardcode for now!
   string_of_spaces(5,SpacesId),
   string_of_spaces(10,SpacesName),
   string_of_spaces(4,SpacesQuant),
   string_of_spaces(6,SpacesPrice),
   % begin failure-driven loop!
   items(itemId(Id),prodName(Name),stockOty(Quant),price(Price)), % backtrack over this until no more solutions
   % transform data into string; see predicate format/2;
   % capture output instead of letting it escape to STDOUT
   with_output_to(string(TxtId),format("~q",[Id])),
   with_output_to(string(TxtName),format("~q",[Name])),
   with_output_to(string(TxtQuant),format("~d",[Quant])),
   with_output_to(string(TxtPrice),format("~d",[Price])),
   % formatting consist in overwriting the space string with the data-carrying string   
   string_overwriting(SpacesId,TxtId,       1,TxtIdFinal),
   string_overwriting(SpacesName,TxtName,   1,TxtNameFinal),
   string_overwriting(SpacesQuant,TxtQuant, 1,TxtQuantFinal),
   string_overwriting(SpacesPrice,TxtPrice, 1,TxtPriceFinal),
   % output the line
   format("~s~s~s~s\n",[TxtIdFinal,TxtNameFinal,TxtQuantFinal,TxtPriceFinal]),
   % close the loop
   fail. 

/* =======================
 * A run
 
?- [printthem].
true.

?- printthem.
 'P01' 'Pots'    50  8200 
 'P02' 'Pans'    50  400  
 'P03' 'Spoons'  50  200  
 'P04' 'Forks'   50  120  
 'P05' 'Kettles' 50  500  
 'P06' 'Plates'  50  60   
false.   

======================= */

