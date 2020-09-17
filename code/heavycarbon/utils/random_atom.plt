:- use_module(library('heavycarbon/utils/random_atom.pl')).

% This is not really a test, just code to exercise the random_atom
% module. There is no failure criterium.

% :- debug(random_atom).

:- begin_tests(random_atom).

test("random_atom_1, ran often") :- 
   length(R,1000),
   maplist(random_atom_1,R),
   debug(random_atom,"random_atom_1 delivers: ~q",[R]).

test("random_atom_2, ran often") :- 
   length(R,1000),
   maplist(random_atom_2,R),
   debug(random_atom,"random_atom_2 delivers: ~q",[R]).

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


