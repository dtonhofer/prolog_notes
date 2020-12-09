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
%
% memberchk/2 is used to unify the first entry in the memberchk-list with What
% when What is unbound; and to accept any entry in the memberchk-list otherwise.

is_list(Term,What) :-
   (nonvar(What) -> must_be(oneof([true,false,var,open,oclist,voclist,canlist,closed,proper]),What) ; true),
   var(Term)  -> memberchk(What,[var,voclist,canlist]) ;
   Term=[_|_] -> is_listlike(Term,What) ;
   Term==[]   -> memberchk(What,[true,oclist,voclist,closed,proper]) ;
   What=false.

is_listlike([_|Xs],What) :-
   var(Xs)    -> memberchk(What,[open,oclist,voclist,canlist]) ;
   Xs=[_|_]   -> is_listlike(Xs,What) ;
   Xs==[]     -> memberchk(What,[true,oclist,voclist,closed,proper]) ;
   What=false.

% ---
% Time for testing
% ---

:- begin_tests(is_list_two).

% A direct translation of the truth table
%
%             | generated |true|closed|proper|var |open |false|oclist|voclist|canlist|

cases(
  [[ []       , true     , yes , yes  , yes ,  n  ,  n  ,  n  , yes  , yes   ,  n   ],
   [ [a,b,c]  , true     , yes , yes  , yes ,  n  ,  n  ,  n  , yes  , yes   ,  n   ],
   [ _        , var      ,  n  ,  n   ,  n  , yes ,  n  ,  n  ,  n   , yes   , yes  ],
   [ [a,b|_]  , open     ,  n  ,  n   ,  n  ,  n  , yes ,  n  , yes  , yes   , yes  ],
   [ [a,b|x]  , false    ,  n  ,  n   ,  n  ,  n  ,  n  , yes ,  n   ,  n    ,  n   ],
   [ foo      , false    ,  n  ,  n   ,  n  ,  n  ,  n  , yes ,  n   ,  n    ,  n   ]]).
 
test("is_list/2 truth table") :-
   cases(Cases),               % get the "cases" list of cases (where each case is a list)
   maplist(test_a_case,Cases). % test each case in Cases

test_a_case([Example,ShouldGenerate,OnTrue,OnClosed,OnProper,OnVar,OnOpen,OnFalse,OnOclist,OnVoclist,OnCanlist]) :-
   test_generation(Example,ShouldGenerate),
   test_acceptance(Example,[OnTrue,OnClosed,OnProper,OnVar,OnOpen,OnFalse,OnOclist,OnVoclist,OnCanlist]).

% does is_list/2 properly generate "ShouldGenerate" when presented with "Example"?

test_generation(Example,ShouldGenerate) :-
   is_list(Example,What),
   assertion(What == ShouldGenerate).

% does is_list/2 properly accept/reject (according to the OnX values) a keyword when presented with "Example"?
% This could be written in one line with maplist/3, but let's be clear!

test_acceptance(Example,[OnTrue,OnClosed,OnProper,OnVar,OnOpen,OnFalse,OnOclist,OnVoclist,OnCanlist]) :-
   accepts(Example, true   , OnTrue),
   accepts(Example, closed , OnClosed),
   accepts(Example, proper , OnProper),
   accepts(Example, var    , OnVar),
   accepts(Example, open   , OnOpen),
   accepts(Example, false  , OnFalse),
   accepts(Example, oclist , OnOclist),
   accepts(Example, voclist, OnVoclist),
   accepts(Example, canlist, OnCanlist).

accepts(Example,What,YesOrNo) :-
   debug(test_is_list_two,"Received ~q, ~q, ~q",[Example,What,YesOrNo]),
   must_be(oneof([yes,n]),YesOrNo),   
   (YesOrNo == yes) -> assertion(is_list(Example,What)) ; assertion(\+is_list(Example,What)).
 
:- end_tests(is_list_two).
