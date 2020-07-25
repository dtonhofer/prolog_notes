:- use_module(library('heavycarbon/utils/vector_replace0.pl')).

:- begin_tests(vector_replace0).

test(empty)  :- vector_replace0([],[],LO,RPs),
                LO=[],RPs=[].

test(nop_op) :- vector_replace0([a,b,c,d],[],LO,RPs),
                LO=[a,b,c,d],RPs=[].

test(one)    :- vector_replace0([a],[0-xxx],LO,RPs),        
                LO=[xxx],RPs=[0-a].

test(two)    :- vector_replace0([a,b,c,d],[3-y,1-x],LO,RPs),
                LO=[a,x,c,y],RPs=[1-b,3-d].

test(full)   :- vector_replace0([a,b,c,d],[0-e,1-f,2-g,3-h],LO,RPs),
                LO=[e,f,g,h],RPs=[0-a,1-b,2-c,3-d].

test(bad,[throws(_)]) :- vector_replace0([a],[0-x,0-y],_,_).
test(bad,[throws(_)]) :- vector_replace0([a],[1-y],_,_).

test(no_infoloss) :- IN=[a,b,c,d],
                     INREP=[0-e,1-f,2-g,3-h],
                     vector_replace0(IN,INREP,OUT,OUTREP),
                     vector_replace0(OUT,OUTREP,IN,INREP).

:- end_tests(vector_replace0).

