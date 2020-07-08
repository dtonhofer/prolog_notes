:- debug(parsing).

file(Ts) --> text_blocks(Ts).

text_blocks(Ts) --> noncomment_block(Ts), comment_block(Ts), text_blocks(Ts).
text_blocks(Ts) --> comment_block(Ts), text_blocks(Ts).

comment_blocks --> comment_block.
comment_blocks --> comment_block, comment_blocks.

comment_block(Cs) --> `/*`, text_with_comment_blocks(Cs), `*/`.



commented_text([]), `*/` --> `*/`,!.
commented_text([]), `/*` --> `/*`,!.
commented_text([C|Cs])   --> [C],!,commented_text(Cs).
commented_text([])       --> [].




file --> block_comment_free_text(T).
file --> block_comment_free_text(T), block_comment, 

file --> arbitrary(P), `/*`, comment_content, `*/`, arbitrary(P).

/*

/*

main('*').

*/

file  --> lines.

lines --> line(PxCs,SxCs), `\n\r`,!, { printline(PxCs,SxCs) }, lines.
lines --> line(PxCs,SxCs), `\n`  ,!, { printline(PxCs,SxCs) }, lines.
lines --> line(PxCs,SxCs), `\r`  ,!, { printline(PxCs,SxCs) }, lines.
lines --> line(PxCs,SxCs),           { printline(PxCs,SxCs) }.


printline(PxCs,SxCs) :-
   atom_codes(Px,PxCs), 
   atom_codes(Sx,SxCs), 
   debug(parsing,"~q ~q",[Px,_]). 

line(PxCs,SxCs) --> noncomment_prefix(PxCs), `%`, !, arbitrary_suffix(SxCs).
line(PxCs,[])   --> noncomment_prefix(PxCs).

noncomment_prefix([C|Cs]) --> [C], { \+ member(C,`%\n\r`) }, !, noncomment_prefix(Cs).
noncomment_prefix([])     --> [].

arbitrary_suffix([C|Cs])  --> [C], { \+ member(C,`\n\r`) }, !, arbitrary_suffix(Cs).
arbitrary_suffix([])      --> [].



main :- 
   read_file_to_codes("/home/calvin/_WORK_ON_PROLOG/swiplmaking2/jplwork/packages-jpl/jpl.pl",Cs,[]),
   phrase(file, Cs, []).



