
 
data  studija;
 input skupina   rezultat @@;
 datalines;
2 4.3 2 4.9 2 3.6 2 4.2 2 5.1 2 4.6 2 3.8    
1 5.1 1 4.7 1 5.6 1 4.9 1 5.0 1 4.9 1 5.3 1 5.4 1 5.5
;
run;



/*** a ***/

ods graphics off;

proc ttest data=studija;
  var rezultat;
  class skupina;
 run;
 /*p-vrijednost=0.1339, ne odbacujemo Ho*/


data grupe;
 set studija;
 rename skupina=grupa;
 id=_N_;
 run;

%let n_perm=300;

/*** b ***/

 proc means data= grupe nway noprint;
  var rezultat;
  class grupa;
  output out=means mean=mean;
  run;

proc transpose data=means out=means_t;
 var mean;
 id grupa;
 run;

 data means_t;
  set means_t;
  abs_diff=abs(_1-_2);
 run;


    proc plan seed=12543 ; 
      factors     rep=&n_perm random
                  id  = 16 /noprint
 ; 
      output out=approxperm;
run;

/* proc freq; table rep*id; run;*/


 data grupe_perm;
  do i=1 to &n_perm;
   do j=1 to 7;
    grupa=2;
	output;
   end;
   do j=8 to 16;
    grupa=1;
    output; 
   end;
   end;
   keep grupa;
   run;
 
   
data approxperm;
 merge approxperm grupe_perm;
run;

proc sort data=approxperm;
 by  id rep;
run;
/** pogrešno je bez drop=grupa, ali verzija sa kum.vrij. ima tu pogrešnu verziju 
data approxperm;
 merge approxperm grupe(drop=grupa);
 by id;
 run;
***/
 data approxperm;
 merge approxperm grupe/*(drop=grupa)*/;
 by id;
 run;

proc means data=approxperm nway noprint;
 var rezultat;
 class rep grupa;
 output out=means_perm mean=mean;
 run;

 
proc transpose data=means_perm out=means_perm_t;
 var mean;
 id grupa;
 by rep;
 run;

 data means_perm_t;
  set means_perm_t;
  if _N_=1 then set means_t(keep=abs_diff);
  abs_diff_perm=abs(_1-_2);
  N_sig=(abs_diff_perm ge abs_diff);
 run;

 proc means noprint;
  var N_sig;
  output out=p_sig sum=sum n=n;
  run;

 data p_sig;
  set p_sig;
  p_sig=(sum+1)/(n+1);
  run;

  proc print; run;
/*p-vrjednost=.009966777*/
/*** c ***/

ods graphics off;

 ods output ttests=ttests;
 proc ttest data= grupe;
  var rezultat;
  class grupa;
  
  run;

 data means_t;
  set ttests;
  where upcase(method)="POOLED";
  keep tvalue;
  run;
 


    proc plan seed=12543 ; 
      factors     rep=&n_perm random
                  id  = 16 /noprint
 ; 
      output out=approxperm;
run;

 


 data grupe_perm;
  do i=1 to &n_perm;
   do j=1 to 7;
    grupa=2;
	output;
   end;
   do j=8 to 16;
    grupa=1;
    output; 
   end;
   end;
   keep grupa;
   run;
 
   
data approxperm;
 merge approxperm grupe_perm;
run;


proc sort data=approxperm;
 by  id rep;
run;

data approxperm;
 merge approxperm grupe(drop=grupa);
 by id;
 run;

 proc sort data=approxperm;
 by rep;
 run;

 ods graphics off;
ods output ttests=ttests;
 proc ttest data= approxperm  ;
  var rezultat;
  class grupa ;
  by rep;
  run;

 data means_perm_t;
  set ttests;
  where upcase(method)="POOLED";
  keep tvalue;
  rename tvalue=tvalue_perm;
  run;

 data means_perm_t;
  set means_perm_t;
  if _N_=1 then set means_t;
     prob_left =(tvalue_perm < -abs(tvalue));
   prob_right=(tvalue_perm > abs(tvalue));

   prob=sum(prob_left,prob_right);
  N_sig=(  tvalue_perm ge abs(tvalue) or tvalue_perm le -abs(tvalue));
 run;

 proc means noprint;
  var N_sig;
  output out=p_sig sum=sum n=n;
  run;

 data p_sig;
  set p_sig;
  p_sig=(sum+1)/(n+1);
  run;

  proc print; run;

/*p-vrijednost=.003322259*/

/**d   ***/

proc univariate data=means_perm_t;
var tvalue_perm;
output out=final pctlpts=2.5, 97.5 pctlpre=ci;
run;

proc print; run;

  proc datasets library=work;
   delete allperm approxperm grupe_perm;
  run;quit; 

/*95% interval za Pooled-t: -2.45570 2.29932  
  pooled t vrijednost iz a): 3.69, ne daju iste zakljuèke, vrijednost iz a) se ne nalazi u 
  ovom pouzdanom intervalu*/



 
