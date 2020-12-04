% ===
% ronerycoder@gluino.name (me) says this is licensed under 
% https://opensource.org/licenses/0BSD
% ===

% ---
% is_list(@Term,?What)
% ---
%
%                         +-------------------------------------------+
%                         |           accepted as What                |
%             +-----------+-----+---+----+-----+------+-------+-------+
%             | generated | true|var|open|false|oclist|voclist|canlist|
%             | as What   |     |   |    |     |      |       |       |
%   +---------+-----------+-----+---+----+-----+------+-------+-------+
%   | []      | true      |  x  ,   ,    ,     ,  x   ,  x    ,       |
%   | [a,b,c] | true      |  x  ,   ,    ,     ,  x   ,  x    ,       |
%   | _       | var       |     , x ,    ,     ,      ,  x    ,  x    |
%   | [a,b|_] | open      |     ,   , x  ,     ,  x   ,  x    ,  x    |
%   | [a,b|x] | false     |     ,   ,    ,  x  ,      ,       ,       |
%   | foo     | false     |     ,   ,    ,  x  ,      ,       ,       |
%   +---------+-----------+-------------------------------------------+


% memberchk/2 is used to unify the first entry in the memberchk-list with W
% when W is unbound; and to accept any entry in the memberchk-list otherwise

is_list(T,W) :-
   (nonvar(W) -> must_be(oneof([true,false,var,open,oclist,voclist,canlist]),W) ; true),
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
   memberchk(W,[true,oclist,voclist])
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
   memberchk(W,[true,oclist,voclist])
   ;
   W=false.

% ---
% Time for testing
% ---

:- begin_tests(is_list_two).

% A direct translation of the above

%             | generated | true|var|open|false|oclist|voclist|canlist|

cases(
  [[ []       , true     , YES , n , n  ,  n  , YES , YES   ,  n   ],
   [ [a,b,c]  , true     , YES , n , n  ,  n  , YES , YES   ,  n   ],
   [ _        , var      ,  n  ,YES, n  ,  n  ,  n  , YES   , YES  ],
   [ [a,b|_]  , open     ,  n  , n ,YES ,  n  , YES , YES   , YES  ],
   [ [a,b|x]  , false    ,  n  , n , n  , YES ,  n  ,  n    ,  n   ],
   [ foo      , false    ,  n  , n , n  , YES ,  n  ,  n    ,  n   ]]).
 
test("list/2 truth table") :-
   cases(Cases),
   maplist(checkcase,Cases).

checkcase([Example,W,OnTrue,OnVar,OnOpen,OnFalse,OnOclist,OnVoclist,OnCanlist]) :-
   generates(Example,W),
   maplist({Example}/[OnW,W]>>accepts(OnW,Example,W),
           [OnTrue,OnVar,OnOpen,OnFalse,OnOclist,OnVoclist,OnCanlist],
           [true  ,var  ,open  ,false  ,oclist  ,voclist  ,canlist]).

generates(Example,W) :-
   must_be(atom,W),
   is_list(Example,WW),
   assertion(WW == W).
 
accepts('YES', Example,W) :-
   !, 
   debug(test_is_list_two,"Received ~q, ~q, ~q",['YES',Example,W]),
   must_be(atom,W),
   assertion(is_list(Example,W)).   % accepts W for Example

accepts('n'  , Example,W) :-
   debug(test_is_list_two,"Received ~q, ~q, ~q",['n',Example,W]),
   must_be(atom,W),
   assertion(\+is_list(Example,W)). % does not accept W for Example

:- end_tests(is_list_two).
