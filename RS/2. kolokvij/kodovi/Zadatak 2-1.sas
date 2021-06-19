
data  UCENICI;
 input RAZRED $ OCJENA @@;
 datalines;
A 4.4 A 4.8 A 3.8 A 4.2 A 4.7 A 4.6
B 5.0 B 4.7 B 4.8 B 4.9 B 5.0 B 4.5
;
run;


ods listing;

proc ttest data=UCENICI;
 var ocjena;
 class razred;
 run;

/**** Rješenje 1a (p-value za jednostrani test = 0.0413/2 = 0.02065)

                                       The TTEST Procedure

                                        Variable:  OCJENA

          RAZRED         N        Mean     Std Dev     Std Err     Minimum     Maximum

          A              6      4.4167      0.3710      0.1515      3.8000      4.8000
          B              6      4.8167      0.1941      0.0792      4.5000      5.0000
          Diff (1-2)           -0.4000      0.2961      0.1709

  RAZRED        Method               Mean       95% CL Mean        Std Dev      95% CL Std Dev

  A                                4.4167      4.0273   4.8060      0.3710      0.2316   0.9100
  B                                4.8167      4.6130   5.0203      0.1941      0.1211   0.4760
  Diff (1-2)    Pooled            -0.4000     -0.7809  -0.0191      0.2961      0.2069   0.5196
  Diff (1-2)    Satterthwaite     -0.4000     -0.7984 -0.00163

                   Method           Variances        DF    t Value    Pr > |t|

                   Pooled           Equal            10      -2.34      0.0413
                   Satterthwaite    Unequal      7.5455      -2.34      0.0493

                                      Equality of Variances

                        Method      Num DF    Den DF    F Value    Pr > F

                        Folded F         5         5       3.65    0.1812






******/

/*** neobavezno ***/
   ods graphics on;
   
   proc ttest data=UCENICI cochran ci=equal umpu;
     var ocjena;
  class razred;
   run;
   
   ods graphics off;


   
/*** Bootstrap uzorkovanje za testiranje hipoteze H0: mi1=mi2 (jednakost sredina 2 nezavisna uzorka) ***/

data grupa1;
  set UCENICI;
  where razred="A";
  ;
data grupa2;
  set UCENICI;
  where razred="B";
  ;
data grupe;
 set grupa1 (in=a) grupa2;
 if a then grupa=1; else grupa=2;
 id=_N_;
 run;

/*** Prosjeèna vrijednost kvalitete po grupama ***/

 proc means data=grupe nway noprint;
  var ocjena;
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
   ocjena=ocjena-&mean1;
   run;
  data grupa2_centrirana;
   set grupa2;
   ocjena=ocjena-&mean2;
   run;

/*** Neparametarski bootstrap (path mora biti postavljen izvoðenjem autoexec programa) ***/

%include "&path\jackboot.sas";                                                                                                               

/*** GRUPA 1 ***/

%let var=ocjena;
%let dat=grupa1_centrirana;
%let seed=34567;  *za grupu 1 (razred="A"); 

%let bsamples=500;
 


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

%let var=ocjena;
%let dat=grupa2_centrirana;
%let seed= 76543; *za grupu 2 (razred="B");
%let bsamples=500;
 



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

   proc means data= boot_Welch_t mean;
    var prob_left prob_right prob;
	run;

	/***ili (za procjenu 95% Int pouzdanosti za p) ***/
  

   proc freq data= boot_Welch_t  ;
    table prob_left prob_right prob/ binomial (level=2);
	run;



	/*** Rješenje 1b)

  


	        The MEANS Procedure

                                   Variable              Mean
                                   ??????????????????????????
                                   prob_left        0.0060000
                                   prob_right       0.0460000
                                   prob             0.0520000
                                   ??????????????????????????


	  proc FREQ za dodatne bodove:

	               

             							Binomial Proportion
                                            prob = 1

                                Proportion                0.0520
                                ASE                       0.0099
                                95% Lower Conf Limit      0.0325
                                95% Upper Conf Limit      0.0715

                                Exact Conf Limits
                                95% Lower Conf Limit      0.0342  za 2-stranu hipotezu
                                95% Upper Conf Limit      0.0753  za 2-stranu hipotezu

                                  Test of H0: Proportion = 0.5

                                ASE under H0              0.0224
                                Z                       -20.0352
                                One-sided Pr <  Z         <.0001
                                Two-sided Pr > |Z|        <.0001

                                      

                                      Binomial Proportion
                                         prob_left = 1

                                Proportion                0.0060
                                ASE                       0.0035
                                95% Lower Conf Limit      0.0000
                                95% Upper Conf Limit      0.0128

                                Exact Conf Limits
                                95% Lower Conf Limit      0.0012  za 1-stranu hipotezu (lijevo)
                                95% Upper Conf Limit      0.0174  za 1-stranu hipotezu (lijevo)

                                  Test of H0: Proportion = 0.5

                                ASE under H0              0.0224
                                Z                       -22.0924
                                One-sided Pr <  Z         <.0001
                                Two-sided Pr > |Z|        <.0001

                                   
**********************/
