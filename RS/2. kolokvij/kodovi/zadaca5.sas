/*1. zadatak*/

/* bias/pristranost koeficijenta asimetrije (skewness) za male uzorke */
%let N = 20;                         /* size of each sample */
%let NumSamples = 2000;              /* number of samples   */  
data SimSK(drop=i);
call streaminit(3456);
do SampleID = 1 to &NumSamples;      /* simulation loop             */
   do i = 1 to &N;                   /* N obs in each sample        */
      Normal      = rand("Normal");  /* skewness=0 for normal, t                     */
      t           = rand("t", 5);    /* skew=2 for exp, skew=1.764 for logn */
      Exponential = rand("Expo");
      LogNormal   = exp(rand("Normal", 0, 0.503)); 
      output;
   end;
end;
run;

proc means data=SimSK noprint;
   by SampleID;
   var Normal t Exponential LogNormal;
   output out=Moments(drop=_type_ _freq_) Skewness=;
run;

proc transpose data=Moments out=Long(rename=(col1=Skewness));
   by SampleID;
run;

proc sgplot data=Long;
   title "Skewness Bias in Small Samples: N=&N";
   label _Name_ = "Distribution";
   vbox Skewness / category=_Name_ meanattrs=(symbol=Diamond);
   refline 0 6 / axis=y;
   yaxis max=6;
   xaxis discreteorder=DATA;
run;
*sve ponoviti za N=50;

/*2. zadatak
/* bias/pristranost koeficijenta splo�tenosti (kurtosis) za male uzorke */
%let N = 20;                         /* size of each sample */
%let NumSamples = 1000;              /* number of samples   */  
data SimSK(drop=i);
call streaminit(123);
do SampleID = 1 to &NumSamples;      /* simulation loop             */
   do i = 1 to &N;                   /* N obs in each sample        */
      Normal      = rand("Normal");  /* kurt=0                      */
      t           = rand("t", 5);    /* kurt=6 for t, exp, and logn */
      Exponential = rand("Expo");
      LogNormal   = exp(rand("Normal", 0, 0.503)); 
      output;
   end;
end;
run;

proc means data=SimSK noprint;
   by SampleID;
   var Normal t Exponential LogNormal;
   output out=Moments(drop=_type_ _freq_) Kurtosis=;
run;

proc transpose data=Moments out=Long(rename=(col1=Kurtosis));
   by SampleID;
run;

proc sgplot data=Long;
   title "Kurtosis Bias in Small Samples: N=&N";
   label _Name_ = "Distribution";
   vbox Kurtosis / category=_Name_ meanattrs=(symbol=Diamond);
   refline 0 6 / axis=y;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;
/* sada ponovimo sve za N=100*/

/*3.zadatak

/*5.1.

Neka je P postotak pouzdanih intervala koji sadrži 0. Ponovite program
10 puta koristeći 0 za seed, spremite postotke u varijablu P. Stavite da je broj ponavljanja
1000 i ponovno program pokrenite 10 puta. Usporedite raspon P vrijednosti. Objasnite.*/

%let N = 50;                                  /* size of each sample  */
%let NumSamples = 10000;                      /* number of samples    */
/* 1. Simulate obs from N(0,1) */
data Normal(keep=ponavljanje SampleID x);
call streaminit(0);
do ponavljanje=1 to 10;
 do SampleID = 1 to &NumSamples;               /* simulation loop      */
    do i = 1 to &N;                            /* N obs in each sample */
       x = rand('Normal');                     /* x ~ N(0,1)           */
       output;
    end;
 end;
end;
run;

/* 2. Compute statistics for each sample */
proc means data=Normal noprint;
   by ponavljanje SampleID;
   var x;
   output out=OutStats mean=SampleMean lclm=Lower uclm=Upper;
run;
/* how many CIs include parameter? */
data OutStats; set OutStats;
   ParamInCI = (Lower<0 & Upper>0);        /* indicator variable */
run;
/* Nominal coverage probability is 95%. Estimate true coverage. */
proc freq data=OutStats;
by ponavljanje;
tables ParamInCI / nocum out=Frekvencije;
run;

data Sadrze_nulu; 
set Frekvencije(where=(ParamInCI=1));
P=Percent;
keep P;
run;
proc means data=Sadrze_nulu nway;
output out=raspon min=min max=max;   
run;

/*Ponavljamo za 1000 uzoraka*/

%let N = 50;                                  /* size of each sample  */
%let NumSamples = 1000;                      /* number of samples    */
/* 1. Simulate obs from N(0,1) */
data Normal2(keep=ponavljanje SampleID x);
call streaminit(0);
do ponavljanje=1 to 10;
 do SampleID = 1 to &NumSamples;               /* simulation loop      */
    do i = 1 to &N;                            /* N obs in each sample */
       x = rand('Normal');                     /* x ~ N(0,1)           */
       output;
    end;
 end;
end;
run;

/* 2. Compute statistics for each sample */
proc means data=Normal2 noprint;
   by ponavljanje SampleID;
   var x;
   output out=OutStats2 mean=SampleMean lclm=Lower uclm=Upper;
run;
/* how many CIs include parameter? */
data OutStats2; set OutStats2;
   ParamInCI = (Lower<0 & Upper>0);        /* indicator variable */
run;
/* Nominal coverage probability is 95%. Estimate true coverage. */
proc freq data=OutStats2;
by ponavljanje;
tables ParamInCI / nocum out=Frekvencije;
run;

data Sadrze_nulu2; 
set Frekvencije(where=(ParamInCI=1));
P=Percent;
keep P;
run;
proc means data=Sadrze_nulu2 nway;
output out=raspon2 min=min max=max;   
run;

/*P-postotak intervala koji sadrži nulu

Raspon P vrijdenosti za 10000 uzoraka [94.6200000, 95.1600000] 
Raspon P vrijdenosti za 1000 uzoraka je [93.000000, 95.8000000] 



Dakle vidimo da se P vrijednost kreće oko očekivanih 95 i vidimo da kada smanjimo broj uzoraka, 
da je raspon P vrijednosti širi*/


/*5.3.Use the BINOMIAL option on the TABLES statement to show that a 95% confidence
interval about the estimate of 0.9344 does not include 0.95.*/

%let N=50;
%let NumSamples=10000;
%let seed=123;

data Exp(keep=SampleID x);
call streaminit(&seed);
do SampleID=1 to &NumSamples;
	do i=1 to &N;
		x=rand("Expo")-1;
		output;
	end;
end;
run;

proc means data=Exp noprint;
by SampleID;
var x;
output out=OutStats mean=SampleMean lclm=Lower uclm=Upper;
run;

ods graphics / width=6.5in height=4in;
proc sgplot data=OutStats(obs=100);
title "95% Confidence Intervals for the Mean";
scatter x=SampleID y=SampleMean;
highlow x=SampleID low=Lower high=Upper / legendlabel="95% CI";
refline 0 / axis=y;
yaxis display=(nolabel);
run;

data OutStats; set OutStats;
ParamInCI = (Lower<0 & Upper>0); 
run;

proc freq data=OutStats;
tables ParamInCI / nocum binomial(level=2);
run;/*postotak koji dobivamo je 93.50 sto je znacajno manje od 95%*/

/*
Exact Conf Limits 	 
95% Lower Conf Limit 	0.9300
95% Upper Conf Limit 	0.9398

Uočimo da 95%-tni interval pouzdanosti sa seed=123 ne sadrži 0.95, što smo i trebali pokazati.*/
*/

/* 4.zadatak

/*5.6: Veličina varijance populacije određuje proporciju uzoraka u kojima t test odbacuje H0.
 Pokrenite simulaciju za x2 (for c=2) iz N(0,2), N(0,5), and N(0,100). Koliko je osjetljiv pooled-variance
t test na razlike u varijancama populacija?*/

/*5.7.Ponovite simulaciju tako da prvi uztorak (c=1) sadrži 20 observacija a drugi 
uzorak 10 observacija. Usporedite rezultate sa rezultatima simulacije
kada oba uzorka sadrže 15 observacija. Koji test ima veću snagu?*/

/*Da ne kopiramo isti kod puno puta samo s jednim ili dva parametra drugačija samo ću navesti 
osnovni kod (u kojem mjenjam zadane parametre) te rezultate . */


/* test sensitivity of t test to equal variances */

%let n1 = 10;/*20 i 15*/
%let n2 = 10;/*10 i 15*/
%let NumSamples = 10000; /* number of samples */
/* Scenario 1: (x1 | c=1) ~ N(0,1); (x1 | c=2) ~ N(0,1); */
/* Scenario 2: (x2 | c=1) ~ N(0,1); (x2 | c=2) ~ N(0,10); */
data EV(drop=i);
label x1 = "Normal data, same variance" x2 = "Normal data, different variance";
call streaminit(321);
do SampleID = 1 to &NumSamples;
	c = 1; /* sample from first group */
	do i = 1 to &n1;
		x1 = rand("Normal");
		x2 = x1;
		output;
	end;
	c = 2; /* sample from second group */
	do i = 1 to &n2;
		x1 = rand("Normal");
		x2 = rand("Normal", 0, 10);/*mijenjamo 10 u 2 pa 5 pa 100*/
		output;
	end;
end;
run;
/*macro ODSOff i ODSOn*/
%macro ODSOff; /* Call prior to BY-group processing */
ods graphics off;
ods exclude all;
ods noresults;
%mend;
%macro ODSOn; /* Call prior to BY-group processing */
ods graphics on;
ods exclude none;
ods results;
%mend;

/* 2. Compute statistics */
%ODSOff /* suppress output */
proc ttest data=EV;
by SampleID;
class c; /* compare c=1 to c=2 */
var x1-x2; /* run t test on x1 and also on x2 */
ods output ttests=TTests(where=(method="Pooled"));
run;
%ODSOn /* enable output */

/* 3. Construct indicator var for tests that reject H0 at 0.05 significance */
data Results;
set TTests;
RejectH0 = (Probt <= 0.05); /* H0: mu1 = mu2 */
run;
/* 3b. Compute proportion: (# that reject H0)/NumSamples */
proc sort data=Results;
by Variable;
run;
proc freq data=Results;
by Variable;
tables RejectH0 / nocum;
run;



/*
N(0,10)
Var = x1
RejectH0 Frequency Percent 
0            9457   94.57 
1             543    5.43 
Var=x2
RejectH0 Frequency Percent 
0            9355   93.55 
1            645     6.45 

N(0,2)
Var=x1
RejectH0 Frequency Percent 
0           9457     94.57 
1           543      5.43 
Var=x2
RejectH0 Frequency Percent 
0           9437    94.37 
1           563      5.63 


N(0,5)
Var=X1
RejectH0 Frequency Percent 
0          9457     94.57 
1          543       5.43 
Var=X2
RejectH0 Frequency Percent 
0            9364   93.64 
1             636    6.36 


N(0,100)
Var=x1
RejectH0 Frequency Percent 
0         9457       94.57 
1           543       5.43 
Var=x2
RejectH0 Frequency Percent 
0            9341    93.41 
1            659      6.59 


/*Različite varijance utjecu na snagu t testa (kad uzorci dolaze iz populacija s istim ocekivanjem, a različitom varijancom, odbacujemo nul hipotezu u 6 % sl */
*/

/*Rezultati za zadatak 5.7:*/

/*

n1=20, n2=10
var=x1
RejectH0 Frequency Percent 
0         9521       95.21 
1         479        4.79 
var=x2
RejectH0 Frequency Percent 
0             8202    82.02 
1            1798    17.98 

n1=15,n2=15
var=x1
RejectH0 Frequency Percent 
0           9520    95.20 
1            480     4.80 */
/*
var=x2
RejectH0 Frequency Percent 
0          9401     94.01 
1          599      5.99 
*/

/*Vidimo da ako imamo uzorke različite veličine, vjerojatnost da odbacimo nultu hipotezu, iako je točna, je puno veća*/


/*5. zadatak

/*5.8

/* test sensitivity of t test to equal variances */
%let n1 = 10;
%let n2 = 10;
%let NumSamples = 10000; /* number of samples */
data EV(drop=i);
label x1 = "Normal data, same variance" x2 = "Normal data, different variance";
call streaminit(321);
do SampleID = 1 to &NumSamples;
	c = 1;
	do i = 1 to &n1;
		x3 = rand("Exponential"); /* mean = StdDev = 1 */
		x4 = rand("Normal", 10); /* mean=10; StdDev = 1 */
		x5 = rand("Gamma",10);
 		output;
	end;
	c = 2;
	do i = 1 to &n2;
		x3 = rand("Exponential"); /* mean = StdDev = 1 */
		x4 = 10 * rand("Exponential"); /* mean = StdDev = 10 */
		x5 = 10 * rand("Exponential");
		output;
	end;
end;
run;
/*macro ODSOff i ODSOn*/
%macro ODSOff; /* Call prior to BY-group processing */
ods graphics off;
ods exclude all;
ods noresults;
%mend;
%macro ODSOn; /* Call prior to BY-group processing */
ods graphics on;
ods exclude none;
ods results;
%mend;

/* 2. Compute statistics */
%ODSOff /* suppress output */
proc ttest data=EV;
by SampleID;
class c; /* compare c=1 to c=2 */
var x3-x5; /* run t test on x3,x4,x5*/
ods output ttests=TTests(where=(method="Pooled"));
run;
%ODSOn /* enable output */

/* 3. Construct indicator var for tests that reject H0 at 0.05 significance */
data Results;
set TTests;
RejectH0 = (Probt <= 0.05); /* H0: mu1 = mu2 */
run;
/* 3b. Compute proportion: (# that reject H0)/NumSamples */
proc sort data=Results;
by Variable;
run;
proc freq data=Results;
by Variable;
tables RejectH0 / nocum;
run;


/*
Exponential,Exponential
var=x3
RejectH0 Frequency Percent 
0          9581     95.81 
1          419       4.19 

Normal,Exponential
var=x4
RejectH0 Frequency Percent 
0          8893      88.93 
1          1107       11.07 

Exponential,Gamma
var=x5
RejectH0 Frequency Percent 
0           9124     91.24 
1            876      8.76 

Vidimo da ako su različite distribucije, t test u više slučajeva odbacuje nul-hipotezu iako je točna.
Za kombinaciju normalne i eksponencijalne greška 1. vrste veća je nego za eksponencijalnu i 
gamma distribuciju.
*/

/*6. zadatak

/*11.2: The OutEst data set also contains estimates for the RMSE for each simulated set of
data. Use PROC UNIVARIATE to analyze the _RMSE_ variable. What is the Monte Carlo estimate
for the RMSE? What is a 90% confidence interval? Test whether the ASD is normally distributed.*/


/* Simulate multiple samples from a regression model */
/*Izaberete jednu od tehnika. Ja sam koristila 2. tehniku.*/
/* Technique 1: Put explanatory variables into arrays */
%let N = 50; /* size of each sample */
%let NumSamples = 100; /* number of samples */

data RegSim1(keep= SampleID x y);
array xx{&N} _temporary_; /* do not output the array */
call streaminit(1);
do i = 1 to &N; /* create x values one time */
xx{i} = rand("Uniform");
end;
do SampleID = 1 to &NumSamples;
do i = 1 to &N;
x = xx{i}; /* use same values for each sample */
y = 1 - 2*x + rand("Normal", 0, 0.5); /* params are 1 and -2 */
output;
end;
end;
run;

/* Technique 2: Put simulation loop inside loop over observations */
data RegSim2(keep= SampleID i x y);
call streaminit(1);
do i = 1 to &N;
x = rand("Uniform"); /* use this value NumSamples times */
eta = 1 - 2*x; /* parameters are 1 and -2 */
do SampleID = 1 to &NumSamples;
y = eta + rand("Normal", 0, 0.5);
output;
end;
end;
run;
proc sort data=RegSim2;
by SampleID i;
run;

proc reg data=RegSim2 outest=OutEst NOPRINT;
by SampleID;
model y = x;
quit;
ods graphics on;
proc corr data=OutEst noprob plots=scatter(alpha=.05 .1 .25 .50);
label x="Estimated Coefficient of x"
Intercept="Estimated Intercept";
var Intercept x;
ods exclude VarInformation;
run;

proc univariate data=OutEst;
var _RMSE_;
run;
/*Monte Carlo procjena : 0.50764755*/
/*90% pouzdani interval: [0.426081,0.597481]*/
PROC UNIVARIATE DATA=OUTEST;
	VAR X;
	HISTOGRAM X / NORMAL;
	QQPLOT X / NORMAL(MU=EST SIGMA=EST);
RUN;

*7.zadatak;

 /*** CHAPTER1_2_T_1SAMPLE.SAS ***/

/*** body-mass index (BMI) = weight_in_kg) / (height_in_cm/100)**2 ***/

data body_mass_index;
 input bmi @@;
 bmi_diff=bmi-31.9;
 cards;
 32.1 33.6 31.3 31.8 28.1 34.5 35.1 34.9 29.9 49.2 36.2 30.7 38.4 45.6 48.6 26.5
;
run;
proc means data=body_mass_index;
var bmi_diff;
 output out=BMI_sk mean=mean std=std skewness=skewness kurtosis=kurtosis; 
run;

data skewkurt;
set bmi_sk;
drop _TYPE_ _FREQ_ mean std;
run;
%macro fleishman;
   /*	This program calculates the coefficients for Fleishman's power transformation in order     */
   /*   to obtain univariate non-normal variables.  For references, see Allen I. Fleishman, (1978).*/
   /*   A method for simulating non-normal distributions, Psychometrika, 43, 521-532.  Also see    */
   /*   Vale, C. David and Maurelli, Vincent A.  (1983).  Simulating multivariate non-normal       */
   /*   distributions, Psychometrika, 48, 465-471.                                                 */                   

PROC IML;

 use skewkurt; 
 read all var{skewness kurtosis} into skewkurt; 

START NEWTON;
  RUN FUN;
  DO ITER = 1 TO MAXITER
  WHILE(MAX(ABS(F))>CONVERGE);
        RUN DERIV;
        DELTA=-SOLVE(J,F);
        COEF=COEF+DELTA;
        RUN FUN;
  END;
FINISH NEWTON;
MAXITER=25;
CONVERGE=.000001;
START FUN;
  X1=COEF[1];
  X2=COEF[2];
  X3=COEF[3];
  F=(X1**2+6*X1*X3+2*X2**2+15*X3**2-1)//
    (2*X2*(X1**2+24*X1*X3+105*X3**2+2)-SKEWNESS)//
    (24*(X1*X3+X2**2*(1+X1**2+28*X1*X3)+X3**2*
      (12+48*X1*X3+141*X2**2+225*X3**2))-KURTOSIS);
FINISH FUN;
START DERIV;
  J=((2*X1+6*X3)||(4*X2)||(6*X1+30*X3))//
    ((4*X2*(X1+12*X3))||(2*(X1**2+24*X1*X3+105*X3**2+2))
     ||(4*X2*(12*X1+105*X3)))//
    ((24*(X3+X2**2*(2*X1+28*X3)+48*X3**3))||
     (48*X2*(1+X1**2+28*X1*X3+141*X3**2))||
     (24*(X1+28*X1*X2**2+2*X3*(12+48*X1*X3+141*X2**2+225*X3**2)
 
     +X3**2*(48*X1+450*X3))));
FINISH DERIV;
DO;
NUM = NROW(SKEWKURT);
DO VAR=1 TO NUM;
  SKEWNESS=SKEWKURT[VAR,1];
  KURTOSIS=SKEWKURT[VAR,2];
  COEF={1.0, 0.0, 0.0};
  RUN NEWTON;
  COEF=COEF`;
  SK_KUR=SKEWKURT[VAR,];
  COMBINE=SK_KUR || COEF;
  IF VAR=1 THEN RESULT=COMBINE;
  ELSE IF VAR>1 THEN RESULT=RESULT // COMBINE;
END;
  PRINT "COEFFICEINTS OF B, C, D FOR FLEISHMAN'S POWER TRANSFORMATION";
  PRINT "Y = A + BX + CX^2 + DX^3";
  PRINT " A = -C";
  MATTRIB RESULT COLNAME=({SKEWNESS KURTOSIS B C D})
                 FORMAT=12.9;
  PRINT RESULT;
END;
 create fleishman from result[colname={SKEWNESS KURTOSIS B C D}];
   append from result;

QUIT;
 
%mend fleishman;


%fleishman;
/** CHAPTER1_2_T_GAMMA3.sas **/

/** define random samples and get t values from defined random samples **/

%LET SEED =54957;		 
%LET NREPtot=10000;	 
%let nn=16; /*vrijednosti veličine uzorka, imamo 16 podataka*/
*%let nprint=%sysevalf(&nrep/&inccum); /*makro var. za ispisivanje to nam sad nije važno*/
%let gopt=hby=0; /* suppresses the by line */
%let nrep=&nreptot; /*** same nrep for all n za svaki n radimo 10000 ponavljanja***/ 
%let mu=3.5062500; /*očekivanje ulaznih podataka*/
%let s=6.8714354; /*st.dev. ulaznih podataka*/

proc datasets library=work;
 delete tall;
run; quit;

%macro nsize;

%let kk=1; 
%let n=%scan(&nn,&kk); 
%do %while(&n NE); 
DATA RAW;
 set fleishman;
 a=-c;
 N=&N;
 CALL STREAMINIT(&SEED);
 DO REP = 1 TO &NREP;
 DO I=1 to &N;
   genX=rannor(&seed);
   genX=a + b*genX + c* genX**2 + d*genX**3; 
   genX=&mu + &s*genX;
   X = genX;
   XT = X - &mu; /*oduzmemo očekivanje*/
  
 OUTPUT;
 END;
 END;
 label rep='repetition';
RUN;
 
/*računam očekivanu vrijednost i stand. pogrešku i t po svakoj replikaciji od njih 10000; tako dobijemo i distr. od t*/ 
proc means data=raw noprint;                                                                                              
  var xt;                                                                                                         
  by rep;                                                                                                        
  output out=t mean=mean stderr=stderr t=t; 
  id n;
run; 

proc append base=tall data=t;
run;
   %let kk=%eval(&kk+1); 
   %let n=%scan(&nn,&kk); 
%end; 
%mend nsize;

%nsize;

data tall;
 set tall;
   t_crit_01=TINV(0.01,n-1);
   fraction_crit_01_left=(t le t_crit_01);
   fraction_crit_01_right=(t ge -t_crit_01);

   t_crit_02=TINV(0.02,n-1);
   fraction_crit_02_left=(t le t_crit_02);
   fraction_crit_02_right=(t ge -t_crit_02);

   t_crit_05=TINV(0.05,n-1);
   fraction_crit_05_left=(t le t_crit_05);
   fraction_crit_05_right=(t ge -t_crit_05);

   t_crit_10=TINV(0.10,n-1);
   fraction_crit_10_left=(t le t_crit_10);
   fraction_crit_10_right=(t ge -t_crit_10);

run;

proc means data=tall nway noprint;
 var fraction_crit_01_left fraction_crit_01_right fraction_crit_02_left fraction_crit_02_right
      fraction_crit_05_left fraction_crit_05_right fraction_crit_10_left fraction_crit_10_right;
 output out=fraction mean=; 
 class n;
run;
*ods html style=sasweb file="C:\rezultati.htm";
proc print data=fraction;
 var fraction_crit_01_left fraction_crit_01_right fraction_crit_02_left fraction_crit_02_right
      fraction_crit_05_left fraction_crit_05_right fraction_crit_10_left fraction_crit_10_right;
	  id n;
	  run;
	  
/*standardni t-test za jedan uzorak */
proc ttest data=body_mass_index h0=0;
var bmi_diff;
ods output ttests=ttValues(keep=tvalue probt);
run;
/*dobiven t-value=2.04*/
data ttValues (keep=tvalue jednostrani dvostrani);
set ttValues;
dvostrani=probt;
jednostrani=probt/2;
run;

/*MC p-vrijednosti*/
data MCp(keep=t N_sig1 N_sig2);
set tall;
N_sig1=	(t ge abs(2.04));
N_sig2=(t ge abs(2.04) or t le -2.04); 
run;
/*zbrojim '1' za jednostrani test*/
proc means data=MCp noprint;
  var N_sig1;
  output out=p_sig_jednostrani sum=sum n=n;
  run;
/*zbrojim '1' za dvostrani test*/
  proc means data=MCp noprint;
  var N_sig2;
  output out=p_sig_dvostrani sum=sum n=n;
  run;
 data p_sig_MC;
  set p_sig_jednostrani;
  p_sig_jednostrani=(sum+1)/(n+1);
  set p_sig_dvostrani;
  p_sig_dvostrani=(sum+1)/(n+1);
  run;
proc print data=p_sig_MC;
run;
*p_sig_jednostrani= 0.014299, p_sig_dvostrani= 0.071893 ;

