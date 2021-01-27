% ===
% Just snippets
% ===

:- module(snippets_increasing_integers,
          [
           list_of_integers_between/3    % list_of_integers_between(+Low,+High,?List)
          ,list_of_integers_between/4    % list_of_integers_between(+HighInclusive:['high_yes','high_no'],+Low,+High,?List)
          ,random_between/4              % random_between(+HighInclusive:['high_yes','high_no'], +Low:int, +High:int, -Random:int)
          ,integer_strictly_positive/1   % integer_strictly_positive(@X)
          ,integer_strictly_negative/1   % integer_strictly_negative(@X)
          ,integer_positive/1            % integer_positive(@X)
          ,integer_negative/1            % integer_negative(@X)
          ,dict_size/2                   % dict_size(+Dict,-Size)
          ,dict_functor_name/1           % dict_functor_name(+Blob)
          ]).

:- use_module(library('heavycarbon/support/meta_helpers.pl')).

% ============================================================================
% Create a list of integers, monotonically increasing by 1.
%
% TODO: Make it work in "reverse direction", too.
% TODO: A lazy list using freeze/2 that generates the next integer on need
%
% (The style of using a flag or option is actually Prolog-unlike and much
% more imperative-like... in Prolog one would create a slightly differently
% names predicate I think)
%
% list_of_integers_between(+Low,+High,?List)
%
% list_of_integers_between(+HighInclusive:['high_yes','high_no'],+Low,+High,?List) 
% ============================================================================

list_of_integers_between(Low,High,List) :-
   list_of_integers_between(high_yes,Low,High,List).

list_of_integers_between(high_yes,Low,High,List) :-
   !,
   bagof(X,between(Low,High,X),List). 
   
list_of_integers_between(high_no,Low,High,List) :-
   !,
   ActualHigh is High-1,
   bagof(X,between(Low,ActualHigh,X),List). 
   
list_of_integers_between(HighInclusive,_,_,_) :-
   domain_error([high_yes,high_no],HighInclusive).
   
% ============================================================================
% The same as random_between/3 but you can indicate where the upper limit
% should be included. 
%
% Helps you avoid adding spurious "ActualHigh is High-1" 
%
% random_between(+HighInclusive:['high_yes','high_no'], +Low:int, +High:int, -Random:int)
% ============================================================================

random_between(high_yes,Low,High,Random) :-
   !,
   random_between_check(Low,High,Random),
   random_between(Low,High,Random).
   
random_between(high_no,Low,High,Random) :-
   !,
   random_between_check(Low,High,Random),
   ActualHigh is High-1,   
   random_between(Low,ActualHigh,Random).
   
random_between(HighInclusive,_,_,_) :-
   domain_error([high_yes,high_no],HighInclusive).

random_between_check(Low,High,Random) :-
   assertion(integer(Low)),
   assertion(integer(High)),
   assertion(var(Random)).
   
% ============================================================================
% Useful in assertions
% ============================================================================
   
integer_strictly_positive(X) :- integer(X),X>0.
integer_strictly_negative(X) :- integer(X),X<0.
integer_positive(X)          :- integer(X),X>=0.
integer_negative(X)          :- integer(X),X=<0.

% ============================================================================
% Get the number of entries (the "size") of a dict
% ============================================================================
  
dict_size(Dict,Size) :-
   assertion(is_dict(Dict)),
   assertion(var(Size);integer(Size)),
   compound_name_arity(Dict,_,Arity),
   Size is (Arity-1)//2. 
   
% ============================================================================
% Obtain the special "SWI-Prolog dict functor name". 
% ============================================================================
% It is a dedicated blob that is used as functor symbol in the compound term 
% used in the implementation of the dict under the hood. 
% It is printed as C'dict' ... but one cannot type that directly.

dict_functor_name(DFN) :- compound_name_arity(_{},DFN,_Arity).
   
