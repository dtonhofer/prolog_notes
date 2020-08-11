/*
 * What we want to do:
 *
 * 1) On the old source jpl.pl, extract all the throw instructions and their line numbers
 *    They look like:
 *    throw_domain_error(Pred,array_index,Fspec,eo11)
 
 * Transform them into
 * throwme(LOCATION_eol1,array_index(Fspec)) for example
 * plus a new predicate
 * exc_desc(LOCATION_eol1,array_index(Fspec),domain_error(array_index,Fspec),the text fro eol1)
 * That's not easy!
 */
 

:- use_module(library(dcg/basics)).
:- debug(parsing).

:- debug(printing).

/*
% greedily finding throwme() calls

line([TM]) --> arbitrary_with_boundary, `throwme(`, term_args(Args), `)`, !, arbitrary, { compound_name_arguments(TM,throwme,Args) }.
line([])   --> arbitrary.
%line([TM]) -->`throwme(`, term_args(Args), `)`, { compound_name_arguments(TM,throwme,Args) }.
*/

% greedily finding throw_*** calls

line([TM]) --> arbitrary_with_boundary, `throw_`,  throw_suffix(Suffix), `(`, term_args(Args), `)`, !, arbitrary, { compound_name_arguments(TM,throwme,Args) }.
line([])   --> arbitrary.
%line([TM]) -->`throwme(`, term_args(Args), `)`, { compound_name_arguments(TM,throwme,Args) }.

throw_suffix(Suffix), `(` --> 

% nongreedily throwing away prefix characters

arbitrary_with_boundary --> [].
arbitrary_with_boundary --> arbitrary,[C],{ memberchk(C,` :),;>-`) }.

% nongreedily removing arbitrary characters

arbitrary --> [].
arbitrary --> [C],{ \+ memberchk(C,`\n\r`) },arbitrary.

% the args of a term; deposit them into a list

term_args([A|As]) --> term_arg(A), `,`, term_args(As).
term_args([A])    --> term_arg(A).
term_args([])     --> [].

% a single term argument, maybe with whitespace (blanks//0 from dcg/basics)

term_arg(A) --> blanks, term_arg_trimmed(A), blanks.

% a single timmed term arg
% try "compound term" first
% prolog_var_name//1, number//1 from dcg/basics

term_arg_trimmed(X) --> compound_term(X),!.
term_arg_trimmed(X) --> simpleatom(X),!.
term_arg_trimmed(X) --> prolog_var_name(X),!.
term_arg_trimmed(X) --> number(X).

% finding an atom; not complete

simpleatom(A) --> `'`, !, inneratom_with_blanks(Cs), `'`, { atom_codes(A,Cs) }.
simpleatom(A) --> startatom(C), inneratom(Cs), !, { atom_codes(A,[C|Cs]) }.
simpleatom(A) --> startatom(C),                   { atom_codes(A,[C]) }.

startatom(C)  --> [C], { memberchk(C,`abcdefghijklmnopqrstuvwxyz_`) }.

inneratom([C|Cs]) --> [C], { memberchk(C,`ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_`) }, !, inneratom(Cs).
inneratom([])     --> [].

inneratom_with_blanks([C|Cs]) --> [C], { memberchk(C,`ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_ `) }, !, inneratom_with_blanks(Cs).
inneratom_with_blanks([])     --> [].

% the compound term is recursive

compound_term(CP) --> simpleatom(F), `(`, term_args(Args) , `)`, { compound_name_arguments(CP,F,Args) }.
compound_term(CP) --> simpleatom(P), `/`, integer(X), { compound_name_arguments(CP,'/',[P,X]) }.

% ===
% Single line
% ===

parse_line(Line,T) :-
   atom_codes(Line,Codes),
   phrase(line(T),Codes,[]).

parsefile(FileName,Result) :-
   read_file_to_codes(FileName,Codes,[]),
   phrase(file(LinesAsCodeLists),Codes,[]),
   atomify(LinesAsCodeLists,LinesAsAtoms), % should be not needed
   parse_lines(LinesAsAtoms,Result).
 
main(Ts) :-
   set_prolog_flag(answer_write_options,[max_depth(0)]),
   set_prolog_flag(debugger_write_options,[max_depth(0)]),
   parsefile("/home/calvin/_WORK_ON_PROLOG/swiplmaking2/jplwork/packages-jpl/jpl.pl",Ts).

parse_lines([A|As],[T|Ts]) :-
   parse_line(A,T),
   (T = [F] -> debug(printing,"~q",[F]) ; true),
   parse_lines(As,Ts).

parse_lines([],[]).



atomify([Cs|Css],[A|As]) :- atom_codes(A,Cs),atomify(Css,As).
atomify([],[]).

file(Lines)  --> lines(Lines).

lines([Cs|Tss])   --> arbitrary_on_line(Cs), `\n\r`,!, lines(Tss).
lines([Cs|Tss])   --> arbitrary_on_line(Cs), `\n`  ,!, lines(Tss).
lines([Cs|Tss])   --> arbitrary_on_line(Cs), `\r`  ,!, lines(Tss).
lines(Cs)         --> arbitrary_on_line(Cs).

arbitrary_on_line([])     --> [].
arbitrary_on_line([C|Cs]) --> [C],{ \+ memberchk(C,`\n\r`) },arbitrary_on_line(Cs).




/*
to_atom(R,R) :- atom(R).
to_atom(R,O) :- compound(R),compound_name_arguments(R,F,Args), F,`(`,Args2,`)`


to_atom(L,LA) :- is_list(L), to_atom_list(L,LAs),
 
to_atom_list([]     , '').
to_atom_list([X]    , [XA]) :- to_atom(X,XA


to_atom_list([X|Xs] , [XA,XAs]) :- to_atom(X,XA), to_atom(Xs,XAs).
   
*/


