:- module(heavycarbon_terms_openlist_append,
          [
               openlist_append/3   % openlist_append(Olist,Element,NewFin)
              ,openlist_append/2   % openlist_append(Olist,Element) no interest in NewFin
              ,openlist_last/2     % openlist_last(+Olist,?Last)
              ,is_openlist/1       % is_openlist(+Olist)
          ]).

:- include(library('heavycarbon/support/throwme_nonmodular.pl')).
:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).

% ===
% Appending "Element" to an openlist "Olist". The new "Fin" - the hole that
% may be filled in with "[]" to form a proper list - is denoted by "NewFin"
% on return.
% We always assume Olist is a valid openlist in sense that, if a freshvar
% is passed here, this is interpreted as the empty openlist.
% Throws if the list has some defect and is not an openlist.
% ===

% In code below, note that we are throwing "from the top", not "from inside
% the recursion": on problem, the recursion simply fails ("fail the
% computation") which is handled distincly from failing due to an empty list
% ("fail the query"). After failure of the recursion, which is detected at
% the "top", we have access to the whole Olist when building the exception
% term. Doing it like this also means that we don't need to add a "catchall
% clause" for the recursion to generate an exception from inside the
% recursion.

openlist_append(Olist,Element,NewFin) :-
   unless(
      (acyclic_term(Olist),openlist_append_2(Olist,Element,NewFin)),
      throwme(openlist,not_an_openlist(Olist))).

openlist_append(Olist,Element) :-
   openlist_append(Olist,Element,_).

% ---
% Case of: Olist is freshvar
% We either have an empty list with Olist indicating the hole that can be
% filled with a listbox or the Fin end of a list backone (which is really the
% same)
% ---

openlist_append_2(Olist,Element,NewFin) :-
   var(Olist),
   !,
   Olist=[Element|NewFin].

% ---
% Case of: Olist is not a freshvar and is actually a listbox
% Continue walking down the backbone of listboxes (note that More may not be a
% listbox or freshvar; we will find out soon enough)
% ---

openlist_append_2(Olist,Element,NewFin) :-
   nonvar(Olist),    % need to test before unification
   Olist=[_|More],   % unification is nonvar against nonvar
   !,
   openlist_append(More,Element,NewFin).

% ---
% Case of: Anything else.
% Just do nothing and let the predicate fail! Less code is good.
% ---

% ===
% Getting the "last valid element" out of an openlist.
% Fails if the openlist is empty (if we find a situation where throwing makes
% sense, appropriate code can be added).
% Throws if the list has some defect and is not an openlist.
% ===

% In code below, note that we are throwing "from the top", not "from inside
% the recursion": on problem, the recursion simply fails ("fail the computation")
% which is handled distincly from failing due to an empty list ("fail the query").
% After failure of the recursion, which is detected at the "top", we have access to
% the whole Olist when building the exception term. Doing it like this also
% means that we don't need to add a "catchall clause" for the recursion to generate
% an exception from inside the recursion.

openlist_last(Olist,Last) :-
   if_then_else(
      var(Olist), % if it's an empty openlist
      false,      % then fail
      unless(
         (acyclic_term(Olist),openlist_last_nonempty(Olist,Last)),
         throwme(openlist,not_an_openlist(Olist)))).

% ---
% Check whether Olist denotes the final listbox. This is essentially
% pattern matching, but there is not much Prolog support for that.
% ---

is_final_listbox(Olist) :-
   nonvar(Olist),
   Olist=[_|More],
   var(More).

% ---
% Check whether Olist denotes a nonfinal listbox. This is essentially
% pattern matching, but there is not much Prolog support for that.
% ---

is_nonfinal_listbox(Olist) :-
   nonvar(Olist),
   Olist=[_|More],
   nonvar(More). % one *could* unify More out

% ---
% Case of final listbox
% ---

openlist_last_nonempty(Olist,Last) :-
   is_final_listbox(Olist), % guard
   !,
   Olist=[Last|_]. % could be done in the guard, but that sounds wrong

% ---
% Case of nonfinal listbox
% ---

openlist_last_nonempty(Olist,Last) :-
   is_nonfinal_listbox(Olist), % guard
   !,
   Olist=[_|More], % could be done in the guard, but that sounds wrong
   openlist_last_nonempty(More,Last).

% ===
% Checking whether something is an openlist. A freshvar _is_ considered
% an openlist. Never throws; semidet.
% This is based on getting the "last" element out of an openlist.
% ===

is_openlist(Olist) :-
   if_then_else(
      var(Olist), % if it's an empty openlist
      true,       % then yes
      (acyclic_term(Olist),openlist_last_nonempty(Olist,_))).

% ===
% Exception descriptors
% ===
% As for the error ... domain error, type error? who knows! is it worth thinking
% about that and adding coding for distinguishing these cases? NOPE! It should be
% "bad list" and that's it. I loathe ISO exception specs.

exc_desc(openlist,not_an_openlist(Olist),
         _,
         type_error(openlist,Olist),
         "Not an openlist").

