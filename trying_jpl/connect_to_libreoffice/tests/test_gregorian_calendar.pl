:- consult('helpers/write_class_inheritance.pl').
   
% ===
% A Gregorian Calendar stays a Gregorian Calendar
% ===

simple(cal) :-
    use_module(library(jpl)),
    jpl_new('java.util.GregorianCalendar',[],JGC),
    format("Obtained Java GregorianCalendar = ~q\n",[JGC]),
    (jpl_is_object(JGC) -> format("JGC is an object\n") ; (format("JGC is NOT an object\n"),fail)),
    write_class_inheritance_path_of_object(JGC).
    
