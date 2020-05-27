:- module(plspec_logger, [log/3, log/2, set_loglevel/1, possible_loglevel/1]).

possible_loglevel(info).
possible_loglevel(warning).
possible_loglevel(error).
possible_loglevel(debug).
possible_loglevel(nothing).

:- dynamic loglevel/1.
set_loglevel(info) :-
  retractall(loglevel(_)),
  assert(loglevel(info)),
  assert(loglevel(error)),
  assert(loglevel(warning)).

set_loglevel(warning) :-
  retractall(loglevel(_)),
  assert(loglevel(error)),
  assert(loglevel(warning)).

set_loglevel(error) :-
  retractall(loglevel(_)),
  assert(loglevel(error)).

set_loglevel(debug) :-
  retractall(loglevel(_)),
  assert(loglevel(debug)),
  assert(loglevel(warning)),
  assert(loglevel(error)),
  assert(loglevel(info)).

set_loglevel(nothing) :-
  retractall(loglevel(_)).


log(X, Format, Args) :-
  (possible_loglevel(X) -> true ; format('~w~t~10 plspec: ~w is not a valid loglevel!', [error, X])),
  (loglevel(X)
    -> format('~w ~w~`.t~20|',[plspec, X, a]), format(Format,Args), nl
    ;  true).

log(X, Format) :-
  log(X, Format, []).
