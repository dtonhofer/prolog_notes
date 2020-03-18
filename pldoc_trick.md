In `${SWIPL_HOME}/lib/swipl/library/pldoc.pl`

````
:- module(pldoc,
          [ doc_collect/1,              % +Bool
            pldoc_loading/0             % True if we are loading
          ]).
          
:- dynamic
    pldoc_loading/0.

pldoc_loading.
````

... code, and at the end:

````
%!  pldoc_loading is semidet.
%
%   True if we are loading the  PlDoc libraries. Required internally
%   to avoid undefined predicates  while   re-loading  and  document
%   itself.


                 /*******************************
                 *           FINISH UP          *
                 *******************************/

:- retract(pldoc_loading).
````

_semidet_ means "succeed at most once"
