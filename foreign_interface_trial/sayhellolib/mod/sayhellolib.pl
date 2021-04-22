% ===
% sayhellolib.pl
% ===

:- module(sayhellolib, 
          [ 
          say_hello/2
          ]).

:- use_foreign_library(foreign(sayhellolib)).
