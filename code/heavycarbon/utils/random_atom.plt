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

% If you run the above (after uncommenting the debug/1 instruction), you get somethinglike:
%
% random_atom_1 delivers: [pvlhbpg,wuv,cp,ojwyl,hvbuee,p,xvm,hao,wmxa,a,pyg,dlksk,cswfu,ridgbo,
% lqe,vwsg,hcwvj,bsvj,u,suzlc,nxtc,zjq,cdcl,kdaka,lowrxz,cjin,fdfwkn,fwiplr,sw,wayxa,h,ki,ahygdd,
% o,plvkm,vdiri,gz,itduq,a,nmvpk,xyey,ob,kz,ohse,l,lwyl,dx,kh,r,wfk,qt,caxo,b,pig,ul,rkpa,pufa,
% ukokqe,wyz,gamq,ljo,lyn,gfu,gvlg,sjfxy,gnp,kbzc,re,mamj,wrbo,numpnp,ihf,vj,oijblb,wcxg,svdaxa,...
%
% random_atom_2 delivers: ['',qyn,ydpz,hgj,gm,pnr,wf,xge,rhbe,hcihd,vm,siqtumr,wnhwhfa,'',meiw,
% juut,yrz,gxt,'',hd,xvhg,xgd,j,zj,svx,ia,js,ondn,fmife,lxnb,hc,hbx,tt,mq,gada,ogb,uh,xmhip,e,
% ib,sidq,rf,vuddxn,jpp,nmau,jpj,in,j,puj,avxth,mv,a,fzkn,bq,xjw,j,vp,s,n,pcmshs,uqbd,fwrxv,
% ggibd,bfk,zwnn,nc,csme,cq,r,yfb,qahezw,jrh,jw,ai,lup,'',tmo,phmxv,oyf,fku,wbi,ua,v,wqo,...
