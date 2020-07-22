:- consult('helpers/write_class_inheritance.pl').

% ===
% A Java String is Prologified into an atom
% ===

simple(str) :-
    use_module(library(jpl)),
    jpl_new('java.lang.String',['Hello, World'],JS),
    format("Obtained Java String = ~q\n",[JS]),
    (jpl_is_object(JS) -> format("JS is an object\n") ; format("JS is NOT an object\n")),
    (atom(JS) -> format("JS is an atom\n") ; format("JS is NOT an atom\n")).
    
