:- use_module(library('heavycarbon/utils/random_atom.pl')).

% This is not really a test, just code to exercise the random_atom
% module. There is no failure criterium.

% :- debug(random_atom).

:- begin_tests(random_atom).

test("run often") :- 
   length(R,1000),
   maplist(random_atom,R),
   debug(random_atom,"At termination: ~q",[R]).

:- end_tests(random_atom).

% If you run the above (after uncommenting the debug/1 instruction), you get somethinglike:
%
% At termination: [szj,shlzq,aapvk,tlhe,smsu,xn,zwvl,ubhac,tct,q,fbfyo,chuyl,yek,qtitrd,tvn,vdvf,
% iwin,ttfbi,vgcrh,pjlhkl,ezd,j,tnzxfl,lfobzuh,vzludt,nadn,ebbr,yqbw,yefl,bwo,abi,qeuhq,kudp,eqxt,
% dirt,mk,svgz,kpsuyqumo,hwqtlg,aspab,eweqr,pxwf,xitc,niz,izyeo,umiqy,bp,swko,dcnc,qjtbi,xoati,c,
% zppq,sbhrg,aiwuy,nkiqs,ouwwrb,emnpqr,kyxaf,ckdf,svdbg,ph,jbxtk,gmr,rh,lbtiykt,zogmyo,asjip,slrmhj,
% klrof,zib,og,zy,dby,msw,dhq,hqpl,xeqknqs,o,avugj,bub,tx,y,ag,ntico,dhkgc,qyd,bswhm,vvlqso,gbuf,
% ps,dbvx,dqiho,ujdag,vg,rj,lpp,ilq,ayc,bdtkv,awq,potpf,kshn,gue,fsc,wo,v,qwkr,jhi,cummvvo,awoon,
% w,mmvz,zfyd,ojifswq,ovwlyy,gwdwp,gbvk,gdvk,waxru,vnhbac,hcuy,ltpoh,kktyju,i,wzmmer,si,vna,whk,s,
% x,axyswq,zrv,kroey,gmxbv,pqdo,agkqv,grd,nuwbird,bwnqr,jbjfd,ugt,c,xyzqdf,qgjm,kmeow,puan,gdbgiml,
% qmrt,uhe,yme,c,mmaw,o,oz,mxvp,ufo,ums,w,keh,bsudqe,lrtok,dtz,rivvg,vysxfun,m,pmte,tojnei,ueljln,
% nva,u,dv,w,s,rog,hdqy,isvbnj,emhy,taadoss,criq,trm,kdmvyz,heeyow,ddnpp,tqz,qz,mbh,mfxzb,k,gwl,ezqq,
% ceox,ygb,uvzj,gqjv,nlkd,eqoi,etbd,rsz,pts,dilab,cngsbpzag,hbxxwk,njf,peqgn,omlrqs,bgxng,xa,ullr,
% fyzi,ihjx,ya,f,rzfb,emtqd,z,rhd,szi,j,...

