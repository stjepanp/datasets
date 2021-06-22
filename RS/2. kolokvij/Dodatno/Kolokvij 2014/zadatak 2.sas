

 data  lijekovi;
 input lijek $ efikasnost @@;
 datalines;
A 4.4 A 4.8 A 3.8 A 4.2 A 5.1 A 4.6 A 4.0 A 3.1
B 5.2 B 4.7 B 5.5 B 4.9 B 5.0 B 4.9
;
run;





proc ttest data=lijekovi;
 var efikasnost;
 class lijek;
 run;

/**** Rješenje 1a (p-value za jednostrani test = 0.0310/2 = 0.0155)

                                                         The TTEST Procedure

                                                         Variable:  efikasnost

                              lijek          N        Mean     Std Dev     Std Err     Minimum     Maximum

                              A              8      4.2500      0.6279      0.2220      3.1000      5.1000
                              B              6      5.0333      0.2805      0.1145      4.7000      5.5000
                              Diff (1-2)           -0.7833      0.5126      0.2768

                     lijek         Method               Mean       95% CL Mean        Std Dev      95% CL Std Dev

                     A                                4.2500      3.7250   4.7750      0.6279      0.4152   1.2780
                     B                                5.0333      4.7390   5.3277      0.2805      0.1751   0.6879
                     Diff (1-2)    Pooled            -0.7833     -1.3865  -0.1801      0.5126      0.3676   0.8462
                     Diff (1-2)    Satterthwaite     -0.7833     -1.3384  -0.2283

                                      Method           Variances        DF    t Value    Pr > |t|

                                      Pooled           Equal            12      -2.83      0.0152
                                      Satterthwaite    Unequal      10.208      -3.14      0.0103

                                                         Equality of Variances

                                           Method      Num DF    Den DF    F Value    Pr > F

                                           Folded F         7         5       5.01    0.0947



******/
 

   
/*** Bootstrap uzorkovanje za testiranje hipoteze H0: mi1=mi2 (jednakost sredina 2 nezavisna uzorka) ***/

data grupa1;
  set lijekovi;
  where lijek="A";
  ;
data grupa2;
  set lijekovi;
  where lijek="B";
  ;
data grupe;
 set grupa1 (in=a) grupa2;
 if a then grupa=1; else grupa=2;
 id=_N_;
 run;

/*** Prosjeèna vrijednost efikasnosti po grupama ***/

 proc means data=grupe nway noprint;
  var efikasnost;
  class grupa;
  output out=out mean=mean;
 run; 

 data _NULL_;
  set out;
  if grupa=1 then call symput("mean1",mean);
  else 			  call symput("mean2",mean);
  run;

  %put mean1=&mean1 mean2=&mean2;

/*** Centriranje (x-mean) unutar grupa ***/

  data grupa1_centrirana;
   set grupa1;
   efikasnost=efikasnost-&mean1;
   run;
  data grupa2_centrirana;
   set grupa2;
   efikasnost=efikasnost-&mean2;
   run;

/*** Neparametarski bootstrap (path mora biti postavljen izvoðenjem autoexec programa) ***/

%include "&path\jackboot1.sas";                                                                                                               

/*** GRUPA 1 ***/

%let var=efikasnost;
%let dat=grupa1_centrirana;
%let seed=486227;  *za grupu 1 (lijek="A"); 

%let bsamples=1000;
%let alph=0.10;


%macro analyze(data=,out=);                                                                                                                   
proc means data=&data noprint  nway;                                                                                
   var &var;                                                                                                                           
   output out=&out(drop=_freq_ _type_) mean=mean1 stderr=stde1; 
 
   %bystmt;       
 run;    
 %mend;                                                                                                                                     
  
 %boot(data=&dat,random=&seed, stat=mean1,samples=&bsamples )      

 
data boot_grupa1;
 set bootdist;
 run;
 
proc means data=grupa1 noprint  nway;                                                                                
   var &var;                                                                                                                           
   output out=actual_grupa1(drop=_freq_ _type_) mean=mean1 stderr=stde1; 

 run;    


/*** GRUPA 2 ***/

%let var=efikasnost;
%let dat=grupa2_centrirana;
%let seed= 947123; *za grupu 2 (lijek="B");
%let bsamples=1000;
%let alph=0.10;



%macro analyze(data=,out=);                                                                                                                   
proc means data=&data noprint  nway;                                                                                
   var &var;                                                                                                                           
   output out=&out(drop=_freq_ _type_) mean=mean2 stderr=stde2; 
 
   %bystmt;       
 run;    
 %mend;                                                                                                                                     
  
 %boot(data=&dat,random=&seed, stat=mean2,samples=&bsamples )      

 
data boot_grupa2;
 set bootdist;
 run;

 

proc means data=grupa2 noprint  nway;                                                                                
   var &var;                                                                                                                           
   output out=actual_grupa2(drop=_freq_ _type_) mean=mean2 stderr=stde2; 

 run;    

data actual_Welch_t;
  merge actual_grupa1 actual_grupa2;
  Welch_t_actual=(mean1-mean2)/sqrt(stde1**2 + stde2**2);
  keep Welch_t_actual;
  run;


data boot_Welch_t;
   merge  boot_grupa1 boot_grupa2;
   by _sample_;
   if _N_=1 then set actual_Welch_t;
   Welch_t=(mean1-mean2)/sqrt(stde1**2 + stde2**2);

   prob_left =(Welch_t < -abs(Welch_t_actual));
   prob_right=(Welch_t > abs(Welch_t_actual));

   prob=sum(prob_left,prob_right);
   run;

proc means data=boot_Welch_t mean p5 p95;
 var welch_t;
 run; 


/*** Dvostrani test, pa je rješenje mean za varijablu prob ***/

   proc means data= boot_Welch_t mean;
    var prob_left prob_right prob;
	run;


	/*** Rješenje

	                            The MEANS Procedure
 
                                                               Mean
                                                       ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
                                                        
                                                       prob             0.0100000
                                                       ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ




*****/
