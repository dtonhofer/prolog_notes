:- use_module(library('heavycarbon/utils/random_atom.pl')).

% This is not really a test, just code to exercise the random_atom
% module. There is no failure criterium.

:- debug(random_atom).

:- begin_tests(random_atom).

test("exercising random_char/1") :-
   length(R,100),
   maplist(random_char,R),
   debug(random_atom,"random_char delivers: ~q",[R]).
   
test("random_text/3, produce atom of length 0") :-
   random_text(Text,0,[atom]),
   assertion((atom(Text),atom_length(Text,0))).

test("random_text/3, produce string of length 0") :-
   random_text(Text,0,[string]),
   assertion((string(Text),string_length(Text,0))).

test("random_text/3, produce default (atom) of length 0") :-
   random_text(Text,0,_),
   assertion((atom(Text),atom_length(Text,0))).

test("random_text/3, produce something of length 0 that cannot be of length 0",[error(_,_)]) :-
   random_text(_Text,0,[nonempty]).

test("random_atom/3, length 1, with options that do nothing (take 1)") :-
   random_atom(Text,1,[nonempty,atom]),
   assertion((atom(Text),atom_length(Text,1))).

test("random_atom/3, length 1, with options that do nothing (take 2)") :-
   random_atom(Text,1,[nonempty,string]),
   assertion((atom(Text),atom_length(Text,1))).

test("random_atom/3, length 1, with no options") :-
   random_atom(Text,1,_),
   assertion((atom(Text),atom_length(Text,1))).

test("random_atom/3, length 1, with bad options") :-
   random_atom(Text,1,foo),
   assertion((atom(Text),atom_length(Text,1))).

test("random_atom/3, demand length 10, run 100x") :- 
   length(R,100),
   maplist([Text]>>random_atom(Text,10,_),R),
   maplist([Text]>>(assertion((atom(Text),atom_length(Text,10)))),R),
   debug(random_atom,"random atoms of length 10: ~q",[R]).

test("random_atom/3, random length, run 100x") :- 
   length(R,100),
   maplist([Text-Len]>>random_atom(Text,Len,_),R),
   maplist([Text-Len]>>(assertion((atom(Text),atom_length(Text,Len)))),R),
   debug(random_atom,"random atoms of random length: ~q",[R]).

test("random_atom/3, random length but not length 0, run 100x") :- 
   length(R,100),
   maplist([Text-Len]>>random_atom(Text,Len,[nonempty]),R),
   maplist([Text-Len]>>(assertion((atom(Text),atom_length(Text,Len),Len>0))),R),
   debug(random_atom,"random atoms of random length, but not empty: ~q",[R]).

:- end_tests(random_atom).

% If you run the above (after uncommenting the debug/1 instruction), you get something like:
%
%
% random_char delivers: [f,j,s,d,a,g,c,t,f,s,t,p,u,w,u,c,k,z,l,d,u,f,c,h,w,h,w,c,b,c...
%
% random atoms of length 10: [hnighnehat,kewyxmijvz,urkomtlwmx,lxhjfwryjr,xtjkwcrhlq,
%    guwbgrckfe,qzgmryedvz,kplmjhlmwd,peearerbwa,rlzwjneqlr,cejsyfcxih,vjklwlmdmf,...
%
% random atoms of random length: [f-1,cfbq-4,fem-3,tva-3,ety-3,phbrc-5,dp-2,jqmh-4,rtk-3,
%    x-1,mebg-4,bpqsc-5,''-0,ohwh-4,rjq-3,hm-2,h-1,zenmor-6,oeyl-4,''-0,m-1,lsp-3,v-1,...
% 
% random atoms of random length, but not empty: [iydpn-5,bph-3,frdwtu-6,o-1,rb-2,excy-4,
%    vgum-4,eiwqsid-7,sv-2,qnxza-5,irkq-4,huzj-4,wtd-3,fpvuyn-6,yf-2,bqu-3,xxszly-6,...

