% ===
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% ===

% Explainer currently at https://eu.swi-prolog.org/pldoc/doc_for?object=is_list/1
%
%   - "oclist" stands for "open or proper list (but not unbound variable)"
%   - "voclist" stands for "open or proper list or unbound variable"
%   - "canlist" stands for "not currently a proper list but can become one as it's an open list or an unbound variable"
%   - "true" means "proper (also known as 'closed') list"
%   - "false" means "absolutely not a list and can't become one"

% ---
% is_list(@Term,?What)
% ---
%
%                         +----------------------------------------------+
%                         |              accepted as What                |
%             +-----------+--------+---+----+-----+------+-------+-------+
%             | generated | true   |var|open|false|oclist|voclist|canlist|
%             | as What   | closed |   |    |     |      |       |       |
%             |           | proper |   |    |     |      |       |       |
%   +---------+-----------+--------+---+----+-----+------+-------+-------+
%   | []      | true      |  x     ,   ,    ,     ,  x   ,  x    ,       |
%   | [a,b,c] | true      |  x     ,   ,    ,     ,  x   ,  x    ,       |
%   | _       | var       |        , x ,    ,     ,      ,  x    ,  x    |
%   | [a,b|_] | open      |        ,   , x  ,     ,  x   ,  x    ,  x    |
%   | [a,b|x] | false     |        ,   ,    ,  x  ,      ,       ,       |
%   | foo     | false     |        ,   ,    ,  x  ,      ,       ,       |
%   +---------+-----------+----------------------------------------------+


% memberchk/2 is used to unify the first entry in the memberchk-list with W
% when W is unbound; and to accept any entry in the memberchk-list otherwise

is_list(T,W) :-
   (nonvar(W) -> must_be(oneof([true,false,var,open,oclist,voclist,canlist,closed,proper]),W) ; true),
   var(T) 
   ->
   memberchk(W,[var,voclist,canlist]) 
   ;
   T=[_|_]
   ->
   is_listlike(T,W)
   ;
   T==[]
   ->
   memberchk(W,[true,oclist,voclist,closed,proper])
   ;
   W=false.

is_listlike([_|Xs],W) :-
   var(Xs) 
   ->
   memberchk(W,[open,oclist,voclist,canlist])
   ;
   Xs=[_|_]
   -> 
   is_listlike(Xs,W)
   ;
   Xs==[]
   ->
   memberchk(W,[true,oclist,voclist,closed,proper])
   ;
   W=false.

% ---
% Time for testing
% ---

:- begin_tests(is_list_two).

% A direct translation of the truth table

%             | generated |true|closed|proper|var |open |false|oclist|voclist|canlist|

cases(
  [[ []       , true     , yes , yes  , yes ,  n  ,  n  ,  n  , yes  , yes   ,  n   ],
   [ [a,b,c]  , true     , yes , yes  , yes ,  n  ,  n  ,  n  , yes  , yes   ,  n   ],
   [ _        , var      ,  n  ,  n   ,  n  , yes ,  n  ,  n  ,  n   , yes   , yes  ],
   [ [a,b|_]  , open     ,  n  ,  n   ,  n  ,  n  , yes ,  n  , yes  , yes   , yes  ],
   [ [a,b|x]  , false    ,  n  ,  n   ,  n  ,  n  ,  n  , yes ,  n   ,  n    ,  n   ],
   [ foo      , false    ,  n  ,  n   ,  n  ,  n  ,  n  , yes ,  n   ,  n    ,  n   ]]).
 
test("list/2 truth table") :-
   cases(Cases),
   maplist(checkcase,Cases).

checkcase([Example,W,OnTrue,OnClosed,OnProper,OnVar,OnOpen,OnFalse,OnOclist,OnVoclist,OnCanlist]) :-
   generates(Example,W),
   maplist({Example}/[OnW,W]>>accepts(OnW,Example,W),
           [OnTrue,OnClosed,OnProper,OnVar,OnOpen,OnFalse,OnOclist,OnVoclist,OnCanlist],
           [true  ,closed  ,proper  ,var  ,open  ,false  ,oclist  ,voclist  ,canlist]).

generates(Example,W) :-
   must_be(atom,W),
   is_list(Example,WW),
   assertion(WW == W).
 
accepts('yes', Example,W) :-
   debug(test_is_list_two,"Received ~q, ~q, ~q",['yes',Example,W]),
   must_be(atom,W),
   assertion(is_list(Example,W)).   % accepts W for Example

accepts('n'  , Example,W) :-
   debug(test_is_list_two,"Received ~q, ~q, ~q",['n',Example,W]),
   must_be(atom,W),
   assertion(\+is_list(Example,W)). % does not accept W for Example

:- end_tests(is_list_two).
