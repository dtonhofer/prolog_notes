:- include(library('heavycarbon/support/meta_helpers_nonmodular.pl')).
:- use_module(library('heavycarbon/utils/clashfree_id_selection.pl')).


number_of_nonterminals(Dict,Count) :-
   findall(Id,Dict.get(Id,Dict,nonterminal(_,_)),Bag),
   length(Bag,Count).



 
random_structure(Dict,Id) :-
   assertion(is_dict(Dict)),
   assertion(var(Id)),
   



decide(Dict,Depth) :-
    number_of_nonterminals(Dict,NonterminalsCount),
    if_then_else(
       (NonterminalsCount > 10)
       add_terminal(Dict),
       decide_terminal_nonterminal(Dict)).

add_terminal(DictIn,DictOut,Id) :-
   const(max_id,MaxId),
   random_word(Word),
   insert_with_clashfree_id(DictIn,terminal(atom,Word),DictOut,Id,MaxId).

add_nonterminal(DictIn,DictOut,Id) :-
   const(max_id,MaxId),
   decide_nonterminal(DictIn,DictOut,Id)
   

decide_terminal_nonterminal(DictIn,DictOut,Id) :-
   random(X),
   if_then_else(
      GoOn < 0.6,
      add_nonterminal(DictIn,DictOut,Id),
      add_terminal(DictIn,DictOut,Id)).

decide_nonterminal(DictIn,DictOut,Id) :-
   random(X),
   Selection = 
      proper_list : '**************'
      open_list   : '*****'
      borked_list : '***'
      compound    : '**********'
      dict        : '******'
 
   switch(
      X < 0.1
      X < 0.2
      X < 0.3

      What=nonterminal(proper_list,Length),
      What=nonterminal(open_list,Length),
      What=nonterminal(borked_list,Length),
      What=nonterminal(compound,Arity),
      What=nonterminal(dict,Size),
 
      
   
 

   


       if_then_else(
                

   random(GoOn),
   if_then_else(
      GoOn > 0.3,
     What=terminal).

const(max_id,999999999999).

random_structure(DictIn,DictOut,Id) :-
   const(max_id,MaxId),
   decide_terminal_or_nonterminal(What),
   switch_throw_on_default(
      (What==terminal),
      (insert_with_clashfree_id(DictIn,terminal,DictOut,Id,MaxId)),
      (What==nonterminal),
      (insert_with_clashfree_id(DictIn,nonterminal(_),Dict0,Id,MaxId),
       fill_nonterminal(Dict0,Id))).
 
      
      
       




node_cases([hole
            atom
            string
            float
            true_rational
            compound
            integer
            dict
            proper_list
            open_list
            borked_list
            empty_list

random_node() :-
   

/*
generate(hole)
generate(atom) :-
generate(string) :-
generate(float) :-
generate(true_rational) :-
generate(compound) :-
generate(integer) :- random_digits(Digits)
generate(dict) :-
generate(proper_list) :-
generate(open_list) :-
generate(borked_list) :-
*/

words([alfa,bravo,charlie,delta,echo,foxtrot,golf,
       hotel,india,juliett,kilo,lima,mike,november,
       oscar,papa,quebec,romeo,sierra,tango,uniform,
       victor,whiskey,xray,yankee,zulu,nadazero,
       unaone,bissotwo,terrathree,kartefour,pantafive,
       soxisix,setteseven,oktoeight,novenine]).

random_word(Word) :-
   words(Words),
   length(Words,Len), % TODO should be buffered
   succ(Limit,Len),
   random_between(0,Limit,Idx),
   nth0(Idx,Words,Word).

random_digits(Digits) :-
   random_digits_2(D),
   Digits = D.
 
random_digits_2(Fin) :-
   random_between(0,9,Digit),
   Fin=[Digit|FinNew],
   random(GoOn),
   if_then_else(
      (GoOn > 0.3),
      random_digits_2(FinNew),
      FinNew = []).
