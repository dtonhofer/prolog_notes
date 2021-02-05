:- module(heavycarbon_utils_dict_prettyprint_checking,
          [
           is_list_of_positive_integers/1
          ,is_list_of_string/1
          ,is_boolean/1
          ,is_boolean_or_inherit/1
 	  ,is_positive_integer/1
 	  ,is_left_right_center/1
          ]).

% ----------

/** <module> dict prettyprinter helper predicates
 
Very simple "checking predicates" used in assertion/1 calls.

If you need to load this module manually, run:

```
?- use_module(library('heavycarbon/utils/dict_prettyprint/checking.pl')).
```

@license [Zero-Clause BSD / Free Public License 1.0.0 (0BSD)](https://opensource.org/licenses/0BSD)
@author David Tonhofer (ronerycoder@gluino.name)

*/

% ----------

%! is_list_of_positive_integers(+List)
% Check wether List is a proper list, and all the elements are integers >= 0.

is_list_of_positive_integers(L) :-
   maplist([X]>>is_positive_integer(X),L). % maplist fails if L is not a list: good!

% ----------

%! is_list_of_string(+List)
% Check wether List is a proper list, and all the elements are strings.

is_list_of_string(L) :- 
   maplist([X]>>(string(X)),L). % maplist fails if L is not a list: good!

% ----------

%! is_boolean(?X)
% Check whether X is =true= or =false=, or enumerate.

is_boolean(X) :- 
   member(X,[false,true]).

% ----------

%! is_boolean_or_inherit(X)
% Check whether X is =true= or =false= or =inherit=, or enumerate.

is_boolean_or_inherit(X) :- 
   member(X,[false,true,inherit]).

% ----------

%! is_positive_integer(X)
% Check whether X is an integer >= 0.

is_positive_integer(X) :- 
   integer(X),X>=0.

% ----------

%! is_left_right_center(X)
% Check whether X is =left=, =right= or =center=, or enumerate.

is_left_right_center(X) :- 
   member(X,[left,right,center]).

