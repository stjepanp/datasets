/*1. zadatak*/
data Gossypol;
      input Doza n;
      do i=1 to n;
         input Porast @@;
		 id+1;
         output;
         end;
      datalines;
   1 12
   179 193 133 170 213 114 104 128 158 134 108 126
   2 17
   130 87 135 116 118 165 151 59 126 64 78 94 150 160 122 110 178
   ;
   run;
data grupa1;
  set Gossypol;
  where DOZA=1;
  ;
data grupa2;
  set Gossypol;
  where DOZA=2;
  ;
data grupe;
 set grupa1 (in=a) grupa2;
 if a then grupa=1; else grupa=2;
 id=_N_;
 run;
 proc means data=grupe nway noprint;
  var Porast;
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
   Porast=Porast-&mean1;
   run;
  data grupa2_centrirana;
   set grupa2;
   Porast=Porast-&mean2;
   run;

/*** Neparametarski bootstrap ***/

%include "/home/u47429085/jackboot.sas";                                                                                                                

/*** GRUPA 1 ***/

%let var=Porast;
%let dat=grupa1_centrirana;
%let seed=47822;
%let bsamples=1000;
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

%let var=Porast;
%let dat=grupa2_centrirana;
%let seed=47822;
%let bsamples=1000;
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

   proc means data= boot_Welch_t mean;
    var prob_left prob_right prob;
	run;

	/*a) dio gledamo prob, za b) dio prob_right*/
	/*za c) dio dodajemo još jednu grupu*/
data Gossypol;
      input Doza n;
      do i=1 to n;
         input Porast @@;
		 id+1;
         output;
         end;
      datalines;
   1 12
   179 193 133 170 213 114 104 128 158 134 108 126
   2 17
   130 87 135 116 118 165 151 59 126 64 78 94 150 160 122 110 178
   3 10
   101 68 46 94 79 81 55 70 108 92
   ;
   run;
 data grupa1;
	set Gossypol;
	where doza = 1;
run;

data grupa2;
	set Gossypol;
	where doza = 2;
run;

data grupa3;
	set Gossypol;
	where doza = 3;
run;


proc means data = Gossypol nway noprint;
  var Porast;
  class Doza;
  output out = out mean = mean;
 run; 


 data _NULL_;
  set out;
  if doza = 1 then call symput("mean1",mean);
  if doza = 2 then call symput("mean2",mean);
  if doza = 3 then call symput("mean3",mean);
  run;

  %put mean1 = &mean1 mean2 = &mean2 mean3 = &mean3;


  data grupa1_centrirana;
   set grupa1;
   porast = porast - &mean1;
   run;


  data grupa2_centrirana;
   set grupa2;
   porast = porast - &mean2;
   run;


  data grupa3_centrirana;
   set grupa3;
   porast = porast - &mean3;
   run;



%let var = porast;
%let dat = grupa1_centrirana;
%let seed = 47822;
%let bsamples = 1000;
%let alpha = 0.05;


%macro analyze(data =, out =);                                                                                                                   
proc means data = &data noprint  nway;                                                                                
   var &var;                                                                                                                           
   output out = &out(drop = _freq_ _type_) mean = mean1 std = std1; 
 
   %bystmt;       
 run;    
 %mend;

%boot(data = &dat,random = &seed, stat = mean1,samples = &bsamples )      

  
data boot_grupa1;
 set bootdist;
 run;

 
proc means data = grupa1 nway noprint;                                                                                
   var &var;                                                                                                                           
   output out = actual_grupa1(drop=_freq_ _type_) mean = mean1 std = std1; 

 run;    

 

%let var = porast;
%let dat = grupa2_centrirana;
%let seed = 47822;
%let bsamples = 1000;
%let alpha = 0.05;


%macro analyze(data =, out =);                                                                                                                   
proc means data = &data noprint  nway;                                                                                
   var &var;                                                                                                                           
   output out = &out(drop = _freq_ _type_) mean = mean2 std = std2; 
 
   %bystmt;       
 run;    
 %mend;

%boot(data = &dat,random = &seed, stat = mean2,samples = &bsamples )      

  
data boot_grupa2;
 set bootdist;
 run;

 
proc means data = grupa2 nway noprint;                                                                                
   var &var;                                                                                                                           
   output out = actual_grupa2(drop=_freq_ _type_) mean = mean2 std = std2; 

 run;  



%let var = porast;
%let dat = grupa3_centrirana;
%let seed = 47822;
%let bsamples = 1000;
%let alpha = 0.05;


%macro analyze(data =, out =);                                                                                                                   
proc means data = &data noprint  nway;                                                                                
   var &var;                                                                                                                           
   output out = &out(drop = _freq_ _type_) mean = mean3 std = std3; 
 
   %bystmt;       
 run;    
 %mend;

%boot(data = &dat,random = &seed, stat = mean3,samples = &bsamples )      

  
data boot_grupa3;
 set bootdist;
 run;

 
proc means data = grupa3 nway noprint;                                                                                
   var &var;                                                                                                                           
   output out = actual_grupa3(drop=_freq_ _type_) mean = mean3 std = std3; 

 run;  



/* računamo F-statistiku na originalnim podacima */

 %let n1 = 12;
 %let n2 = 17;
 %let n3 = 10;
 %let n = 39;
 %let k = 3;
 
data actual_F;
  merge actual_grupa1 actual_grupa2 actual_grupa3;
  actual_mean_sve = 1/&n * (&n1 * mean1 + &n2 * mean2 + &n3 * mean3);
  actual_SST = &n1 * (actual_mean_sve - mean1)**2 + &n2 * (actual_mean_sve - mean2)**2 + &n3 * (actual_mean_sve - mean3)**2;
  actual_SSE = (&n1 - 1) * std1**2 + (&n2 - 1) * std2**2 + (&n3 - 1) * std3**2;
  actual_MST = actual_SST / (&k - 1);
  actual_MSE = actual_SSE / (&n - &k);
  F_actual = actual_MST / actual_MSE;
  keep F_actual;
  run;



/* računamo F-statistiku za svaki bootstrap uzorak  */

data boot_F;
   merge  boot_grupa1 boot_grupa2 boot_grupa3;
   by _sample_;
   if _N_ = 1 then set actual_F;
   mean_sve = 1/&n * (&n1 * mean1 + &n2 * mean2 + &n3 * mean3);
   SST = &n1 * (mean_sve - mean1)**2 + &n2 * (mean_sve - mean2)**2 + &n3 * (mean_sve - mean3)**2;
   SSE = (&n1 - 1) * std1**2 + (&n2 - 1) * std2**2 + (&n3 - 1) * std3**2;
   MST = SST / (&k - 1);
   MSE = SSE / (&n - &k);
   F = MST / MSE;
   pv_boot = (F > F_actual);
   drop mean_sve SST SSE MSE MST;
   run;


 ods html;

proc means data = boot_F mean;
    var pv_boot;
run;

/* Odbacujemo H0 da su ocekivanja jednaka na razini znacajnosti od 5%.*/
/*2. zadatak*/


/*** Example 2 (p.180), D.D.Boos "Introduction to the Bootstrap World", Statistical Science, 2003, Vol 18 ***/

   data pcb;
    input gs elisa @@;
	datalines;
	76 81  150 152 115 129
	50 83  192 152 166 140
	59 84  171 172 205 212
	92 92  177 172 337 309
	70 93   28 106 334 320
	99 100  58 109 309 358
   176 143 106 121 310 429
   156 145  94 122 568 510
   ;
   run;

ods graphics off;

 
   proc reg data=pcb outest=parest(where=(_type_='PARMS') keep=gs _type_ _RMSE_ intercept);
   model elisa=gs;
   output out=out r=resid;
   run; quit;
ods graphics on;

/*** Save sigma (Root Mean Squared Error) as a macro variable sigma */
/*** Save regression coefficients  (alpha, beta) as macro variables alpha, beta */

   data _NULL_;
    set parest;
     call symput("alpha",intercept);
	 call symput("beta",gs);
	 call symput("sigma",_RMSE_);
	 run;
	 %put alpha=&alpha beta=&beta sigma=&sigma;

/*** Probability of false negative when GS=200 (P(ELISA < 100 | GS=200) ***/ 

	data FalseNegativeAtGS_200;
	 fn200=probnorm((100-&alpha-&beta*200)/&sigma);
	 run;
	proc print; run;


	   /*** Parametric Bootstrap ***/

	 
%let rep=10000;
%let seed=12844;

data boot_data;

 set pcb;
 do rep=1 to &rep;
  elisaboot=&alpha+&beta*gs +&sigma*normal(&seed);
  output;
 end;
 run;

proc sort data=boot_data;
 by rep;
run;

 
 proc reg data=boot_data noprint outest=parest(where=(_type_='PARMS') keep=gs _type_ _RMSE_ intercept rep);
 model elisaboot=gs;
 by rep;
 run;
 
  
data boot_dist;
 set parest ;
 boot_fn200=probnorm((100-intercept-gs*200)/_RMSE_);
 run;

proc means data=boot_dist mean std p95;
 var boot_fn200;
 run;

/*** Nonparametric Bootstrap ****/

%let var2=elisa;
%let var1=gs;
%let dat=pcb;
%let seed=38857;
%let bsamples=10000;
%let alph=0.10;


%include "/home/u47429085/jackboot.sas";                                                                                                                
%macro analyze(data=,out=);                                                                                                                   
proc reg data=&data noprint outest=&out(where=(_type_='PARMS')                                                                                
                                                                                                                            
                  keep=&var1 _type_ _RMSE_ intercept &by)                                                                                     
;                                                                                                                                             
 model &var2=&var1; 
    %bystmt;       
 run;quit;   
data &out;
 set &out;
  boot_fn200=probnorm((100-intercept-&var1 *200)/_RMSE_);
run; 
 
   %mend;                                                                                                                                     
                                                                                                                                              
   title3 'Bootstrap Analysis';                                                                                                               
   %boot(data=&dat,random=&seed, samples=&bsamples, alpha=&alph, stat=boot_fn200)      
  %allci(alpha=&alph, stat=boot_fn200)        

 
proc means data=bootdist mean std p95;
 var boot_fn200;
 run;

/*a)*/
/*** t w/ 5 degrees of freedom ***/

 
%let rep=10000;
%let seed=23455;

data boot_data;

 set pcb;
 call streaminit(&seed);
 do rep=1 to &rep;
  elisaboot=&alpha+&beta*gs +&sigma*rand("T",5)/sqrt(5/3);
  output;
 end;
 run;

proc sort data=boot_data;
 by rep;
run;

 
 proc reg data=boot_data noprint outest=parest(where=(_type_='PARMS') keep=gs _type_ _RMSE_ intercept rep);
 model elisaboot=gs;
 by rep;
 run;
 
  
data boot_dist;
 set parest ;
 boot_fn200=CDF("T",(100-intercept-gs*200)/_RMSE_*sqrt(5/3),5);
 boot_fn200_n=probnorm((100-intercept-gs*200)/_RMSE_);
 run;

proc means data=boot_dist mean std p95;
 var boot_fn200 boot_fn200_n;
 run;
/*b) analogno za b) dio*/
/*c)*/
data out;
 set out;
 resid_poz=resid+50;
 run;


proc Reliability data=out;       
   distribution Weibull3;          
   Pplot  resid_poz; 
run;    

proc capability data=out;  
var resid; 
   histogram resid /weibull(theta=est sigma=est c=est); 
run;    
    
/** WEIBUL (parametric bootstrap) : iz INSIGHT-a procjena uz pomak=-48, c(a) = 1.34, sigma (b)=52 ***/
/** treba koristiti UNIVARIATE/RELIABILITY/CAPABILITY za procjenu parametara weibul disn i u prvom koraku i kasnije  - za procjenu prob - po weibulu ***/
data lib.weib;
 call streaminit(12345);
 mean=52*gamma(1+1/1.34);
 do i=1 to 1000;
  x=rand("WEIBULL",1.34,52)-mean;
  output;
end;

run;

/**  sa procjenama iz INSIGHT-a***/

%let rep=10000;
%let seed=12844;

data boot_data;
mean=52*gamma(1+1/1.34);
drop mean;
 set pcb;
 do rep=1 to &rep;
  elisaboot=&alpha+&beta*gs +(rand("WEIBULL",1.34,52)-mean);
  output;
 end;
 run;

/** sa procjenama iz proc reliability/capability **/
data  weib;
 call streaminit(12345);
 mean=54*gamma(1+1/1.44);
 stdev=54*sqrt(gamma(2/1.44+1)-gamma(1/1.44+1)**2);

 do i=1 to 1000;
  x=rand("WEIBULL",1.44,54)-mean;
  xstd=x/stdev;
  output;
end;

run;

proc capability data=weib;  
var x xstd; 
   histogram x xstd /weibull(theta=est sigma=est c=est); 
run;    


%let rep=10000;
%let seed=12844;

data boot_data;
mean=54*gamma(1+1/1.44);
stdev=54*sqrt(gamma(2/1.44+1)-gamma(1/1.44+1)**2);

drop mean;
 set pcb;
  call streaminit(12345);

 do rep=1 to &rep;
  elisaboot=&alpha+&beta*gs +(rand("WEIBULL",1.44,54)-mean);
  output;
 end;
 call symput("mean",mean);
 call symput("stdev",stdev);

 run;

 %put &=mean &=stdev;

proc sort data=boot_data;
 by rep;
run;

 ods graphics off;
 proc reg data=boot_data noprint outest=parest(where=(_type_='PARMS') keep=gs _type_ _RMSE_ intercept rep);
 model elisaboot=gs;
 by rep;
 run;
 ods graphics on;
  

 /*** Iako nije potpuno tocno, dovoljno je da se izracunaju False neg tj boot_fn200n koristeci kumulativnu normalnu disn (PROBNORM)**/
 /** Alternativno, mogli bi koristiti CDF("WEIBULL",...), ali tu je problem procjene pomaka **/

data boot_dist;
 set parest ;
   boot_fn200n=probnorm((100-intercept-gs*200)/_RMSE_);
  run;

proc means data=boot_dist mean std p95;
 var boot_fn200n  ;
 run;

/*d)*/
 /*** LOGNORMAL ***/

 
proc capability data=out;  
var resid; 
   histogram resid /lognormal(sigma=est scale=est shape=est theta=est);
run;    
    

/** Treba generirati x po N(scale, shape), exponencirati i dodati theta ***/

/** scale je zeta, shape je sigma **/

data lognor;
 call streaminit(12345);
 scale=4;
 shape=0.5;
 theta=-65;
 
 do i=1 to 1000;
  x=rand("NORMAL",scale,shape) ;
  y=exp(x)+theta;
  output;
end;

run;
/** provjera **/
proc capability data=lognor;  
var y; 
   histogram y /lognormal(sigma=est scale=est shape=est theta=est);
run;    
    

data boot_data;
  set pcb;
   call streaminit(12345);

 do rep=1 to &rep;
  elisaboot=&alpha+&beta*gs +(exp(rand("NORMAL",4,0.5))-65);
  output;
 end;
 
 run;

 
proc sort data=boot_data;
 by rep;
run;

 ods graphics off;
 proc reg data=boot_data noprint outest=parest(where=(_type_='PARMS') keep=gs _type_ _RMSE_ intercept rep);
 model elisaboot=gs;
 by rep;
 run;
 ods graphics on;
  
data boot_dist;
 set parest ;
   boot_fn200n=probnorm((100-intercept-gs*200)/_RMSE_);
   *boot_fn200=cdf("LOGNORMAL",(100-intercept-gs*200 +65)/_RMSE_,exp(4),0.5);

  run;

proc means data=boot_dist mean std p95;
 var boot_fn200n   ;
 run;


/** Zakljucak: najbolji pristup je neparametarski bootstrap i to sa BC ili BCa intervalom, jer je statistika FN pristrana tj. ima nezanemariv bias **/

/*3. zadatak*/

data  pizza;
 input tijesto $ kvaliteta @@;
 datalines;
s 4.4 s 4.7 s 3.8 s 4.2 s 5.2 s 4.5 s 4.5
n 5.3 n 4.8 n 5.6 n 4.9 n 5.1 n 4.8
;
run;



ods listing;
ods graphics off;
proc ttest data=pizza;
 var kvaliteta;
 class tijesto;
 run;
ods graphics on;
/**** Rješenje 1a (p-value za jednostrani test = 0.0154/2 = 0.0077)

                  Method           Variances        DF    t Value    Pr > |t|

                  Pooled           Equal            11       2.86      0.0154
                  Satterthwaite    Unequal      10.817       2.93      0.0138

                                     Equality of Variances

                       Method      Num DF    Den DF    F Value    Pr > F

                       Folded F         6         5       1.83    0.5252






******/

/*** neobavezno ***/
   ods graphics off;
   
   proc ttest data=pizza cochran ci=equal umpu;
     var kvaliteta;
  class tijesto;
   run;
   
   ods graphics on;


   
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

/*** Prosječna vrijednost kvalitete po grupama ***/

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



%include "/home/u47429085/jackboot.sas";                                                                                                              

/*** GRUPA 1 ***/

%let var=kvaliteta;
%let dat=grupa1_centrirana;
%let seed=34567;  *za grupu 1 (tijesto="n"); 

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

%let var=kvaliteta;
%let dat=grupa2_centrirana;
%let seed=77890; *za grupu 2 (tijesto="s");
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



	/*** Rješenje (prob i prob_right)


 

                                      The MEANS Procedure

                                  Variable              Mean
                                  ??????????????????????????
                                  prob_left        0.0040000
                                  prob_right       0.0020000
                                  prob             0.0060000
                                  ??????????????????????????

                                                              23:14 Saturday, June 22, 2019  29

                                      The FREQ Procedure

                                                      Cumulative    Cumulative
                prob_left    Frequency     Percent     Frequency      Percent
                ??????????????????????????????????????????????????????????????
                        0         498       99.60           498        99.60
                        1           2        0.40           500       100.00


                                     Binomial Proportion
                                        prob_left = 1

                               Proportion                0.0040
                               ASE                       0.0028
                               95% Lower Conf Limit      0.0000
                               95% Upper Conf Limit      0.0095

                               Exact Conf Limits
                               95% Lower Conf Limit      0.0005
                               95% Upper Conf Limit      0.0144

                                 Test of H0: Proportion = 0.5

                               ASE under H0              0.0224
                               Z                       -22.1818
                               One-sided Pr <  Z         <.0001
                               Two-sided Pr > |Z|        <.0001

                                       Sample Size = 500


                                                       Cumulative    Cumulative
                prob_right    Frequency     Percent     Frequency      Percent
                ???????????????????????????????????????????????????????????????
                         0         499       99.80           499        99.80
                         1           1        0.20           500       100.00



                                                              23:14 Saturday, June 22, 2019  30

                                      The FREQ Procedure

                                     Binomial Proportion
                                        prob_right = 1

                               Proportion                0.0020
                               ASE                       0.0020
                               95% Lower Conf Limit      0.0000
                               95% Upper Conf Limit      0.0059

                               Exact Conf Limits
                               95% Lower Conf Limit      0.0001
                               95% Upper Conf Limit      0.0111

                                 Test of H0: Proportion = 0.5

                               ASE under H0              0.0224
                               Z                       -22.2712
                               One-sided Pr <  Z         <.0001
                               Two-sided Pr > |Z|        <.0001

                                       Sample Size = 500


                                                    Cumulative    Cumulative
                   prob    Frequency     Percent     Frequency      Percent
                   ?????????????????????????????????????????????????????????
                      0         497       99.40           497        99.40
                      1           3        0.60           500       100.00


                                     Binomial Proportion
                                           prob = 1

                               Proportion                0.0060
                               ASE                       0.0035
                               95% Lower Conf Limit      0.0000
                               95% Upper Conf Limit      0.0128

                               Exact Conf Limits
                               95% Lower Conf Limit      0.0012
                               95% Upper Conf Limit      0.0174

                                 Test of H0: Proportion = 0.5

                               ASE under H0              0.0224
                               Z                       -22.0924
                               One-sided Pr <  Z         <.0001
                               Two-sided Pr > |Z|        <.0001

                                       Sample Size = 500


	*****/

