
:- use_module(library('tree_pp/build_term_for_tree.pl')).

edge(a,[]).
edge(b,a).
edge(c,a).
edge(d,b).
edge(e,b).
edge(f,c).
edge(g,c).
edge(h,d).
edge(i,d).
edge(j,e).
edge(k,e).
edge(l,f).
edge(m,f).
edge(n,g).
edge(o,g).

testit(X) :-
   build_term(X).

