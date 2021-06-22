/* Antonija Ðuriæ */
/* 1191227212 */

										/* ZADATAK 2 */



 data  pizza;
 input tijesto $ kvaliteta @@;
 datalines;
s 4.5 s 4.7 s 3.8 s 4.2 s 5.2 s 4.6 s 4.6
n 5.3 n 4.8 n 5.4 n 4.9 n 5.0 s 4.7
;
run;



/* A */

proc ttest data=pizza;
 var kvaliteta;
 class tijesto;
 run;


/* B */
   
/*** Bootstrap uzorkovanje za testiranje hipoteze H0: mi1=mi2 (jednakost sredina 2 nezavisna uzorka) ***/

data grupa1;
  set pizza;
  where tijesto="n";
  ;
data grupa2;
  set pizza;
  where tijesto="s";
  ;
data grupe;
 set grupa1 (in=a) grupa2;
 if a then grupa=1; else grupa=2;
 id=_N_;
 run;

/*** Prosjeèna vrijednost efikasnosti po grupama ***/

 proc means data=grupe nway noprint;
  var kvaliteta;
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
   kvaliteta=kvaliteta-&mean1;
   run;
  data grupa2_centrirana;
   set grupa2;
   kvaliteta=kvaliteta-&mean2;
   run;

/*** Neparametarski bootstrap (path mora biti postavljen izvoðenjem autoexec programa) ***/

%include "&path\Programs\jackboot1.sas";                                                                                                               

/*** GRUPA 1 ***/

%let var=kvaliteta;
%let dat=grupa1_centrirana;
%let seed=1278;  *za grupu 1 (lijek="A"); 

%let bsamples=500;
%let alph=0.05;


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

%let var=kvaliteta;
%let dat=grupa2_centrirana;
%let seed= 4156; *za grupu 2 (lijek="B");
%let bsamples=500;
%let alph=0.05;



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


ods html;
proc means data=boot_Welch_t mean p5 p95;
 var Welch_t;
 run; 

/*** Dvostrani test, pa je rješenje mean za varijablu prob ***/

   proc means data= boot_Welch_t mean;
    var prob_left prob_right prob;
	output out=nesto mean=mean;
	run;

/*
Variable 		Mean 
prob_left   	0.0120000
prob_right 		0.0060000 
prob 			0.0180000 
   
 */


