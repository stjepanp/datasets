/************************************************************************************
*                                     Fleishman                                     *
*************************************************************************************/

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

/************************************************************************************
*                                  Fleishman KRAJ                                   *
*************************************************************************************/

data RAZRED; 
   input Name $ Height Weight @@; 
   datalines; 
Alfred  81.0 130.5   Alice  56.5  84.0   Barbara 65.3  98.0 
Carol   62.8 102.5   Henry  61.5 101.5   James   76.3 170.0 
Jane    71.8  94.5   Janet  62.5 112.5   Jeffrey 61.5  85.0 
John    59.0  99.5   Joyce  65.3 150.5   Judy    64.3  90.0 
Louise  56.3  77.0   Mary   66.5 115.0   Philip  72.5 152.0 
Robert  64.8 158.0   Ronald 67.0 137.0   Thomas  57.5  85.0 	
; 
run; 

PROC MEANS data=razred  nway mean std skewness kurtosis;
var Height;
RUN;
PROC MEANS data=razred  nway mean std skewness kurtosis;
var Weight;
RUN;
/*
Vrijednosti deskriptivnih statistika za
-height
	Mean	65.1333333 	Std Deviation	6.7239694	Skewness	0.8612172	 Kurtosis	0.5187557
	
-weight
	Mean	113.4722222	Std Deviation	29.0129141	Skewness	0.6647756  	 Kurtosis	-0.8894526
*/




/**************************************************************************************************
  Height 100 puta
***************************************************************************************************/
title 'Height - koeficijenti';

data skewkurt;
 input skewness kurtosis;
 datalines;
0.8612172	 0.5187557
;
run;

data meansig;
 input mean sig;
 datalines;
65.1333333	 6.7239694
;
run; 

%fleishman;

%let br_pon = 100;

data seedovi;
	input s;
	datalines;
0
;
run;
	
*funkcije za generiranje i ispis uzoraka sa odgovarajuca prva cetiri momenta;
%macro create(howmany);
   %do i=1 %to &howmany;
		data height_fl&i(keep=x);
			merge seedovi fleishman meansig;
			a=-c;
			do j=1 to 30;
				x=rannor(s);
				x=a + b*x + c* x**2 + d*x**3;
				x=mean + sig* x;
				output;
			end;
		run;
		proc means data = height_fl&i mean std skewness kurtosis noprint;
			var x;
			output out=momenti&i mean=mean std=std skewness=skewness kurtosis=kurtosis;
		run;
	%end;
%mend create;



%create(&br_pon);	


title 'Height - srednja vrijednost momenata';

data  height_merge;
	set  momenti1- momenti&br_pon;
run;

proc means  data =height_merge mean std;
	var mean std skewness kurtosis;
run;






/**************************************************************************************************
  Weight 100 puta
***************************************************************************************************/

title 'Weight - koeficijenti';

data skewkurt;
 input skewness kurtosis;
 datalines;
0.6647756  -0.8894526
;
run;

data meansig;
 input mean sig;
 datalines;
113.4722222	 29.0129141
;
run; 

%fleishman;
	
*Funkcija za generiranje i ispis uzorka sa odgovarajuca prva cetiri momenta, ovdje je malo drugačija verzija, koja se sastoji od dvije makro funkcije;
%macro create_w(howmany);
   %do i=1 %to &howmany;
		data weight_fl&i(keep=x);
			merge seedovi fleishman meansig;
			a=-c;
			do j=1 to 50;
				x=rannor(s);
				x=a + b*x + c* x**2 + d*x**3;
				x=mean + sig* x;
				output;
			end;
		run;
	%end;
%mend create_w;

%macro print_moments_w(howmany);
	%do i = 1 %to &howmany;
		proc means data = weight_fl&i mean std skewness kurtosis noprint;
			var x;
			output out=momentiw&i mean=mean std=std skewness=skewness kurtosis=kurtosis;
		run;
	%end;
%mend print_moments_w;

%create_w(&br_pon);	
%print_moments_w(&br_pon);


title 'Weight - srednja vrijednost momenata';

data  weight_merge;
	set  momentiw1- momentiw&br_pon;
run;

proc means  data =weight_merge mean std;
	var mean std skewness kurtosis;
run;



/* Vidimo da su srednje vrijednosti dosta blizu onima s kojima smo generirali uzorke, 
iako opet kod momenata višeg reda imamo veća odstupanja. */

/*2. zadatak*/

*Zadavanje broja permutacija;
%let n_perm=1000;

*Unosenje podataka (dvije grupe);
data grupa1;
  input ocjena @@;
  datalines;
  3.8 1.8 1.0 3.6 3.3 2.7 3.7 2.5 3.8 2.2 2.5 3.4 2.8
  ;
data grupa2;
  input ocjena @@;
  datalines;
4.0 2.5 3.6 2.5 3.6 1.7 2.8 2.6 2.7 2.5 2.6 2.2 2.5 2.3 1.3 3.2 2.6 1.0 2.6 0.0 2.8 3.0 2.5 3.1 4.0 2.9 2.7 3.9 3.4 3.6 3.1 0.7 0.7 2.2
  ;

/* Spajanje dvije grupe podataka u jedan dataset, dodajemo stupac grupa kao indikator kojoj grupi 
pripada koji podatak (1 ili 2) i stupac ID-eva */
data grupe;
 set grupa1 (in=a) grupa2;
 if a then grupa=1; else grupa=2;
 id=_N_;
 run;

/* Računamo aritmetičku sredinu ocjena za svaku grupu i spremamo u izlazni dataset means */
 proc means data= grupe nway noprint;
  var ocjena;
  class grupa;
  output out=means mean=mean;
  run;

/* Transponiranje podataka (kako bi aritmeticke sredine bile u istom retku) */
proc transpose data=means out=means_t prefix=_;
 var mean;
 id grupa;
 run;

* Računanje testne statistike;
 data means_t;
  set means_t;
  abs_diff=abs(_1-_2);
 run;


/* Generiramo n_perm permutacija u random poretku od 1 do 47
(ukupna veličina uzorka) */
    proc plan seed=60359 ; 
      factors     rep=&n_perm random
                  id  = 47 /noprint
 ; 
      output out=approxperm;
run;


/* U outputu imamo dva stupca: rep, id. Rep je oznaka permutacije,
 47 redova ima isti rep, a id kaze kako permutiramo*/

/* proc freq; table rep*id; run;*/

/* Za svaku od tih 1000 permutacija, za prvih 13 stavimo grupa=1 
a ostale grupa = 2 */
 data grupe_perm;
  do i=1 to &n_perm;
	   do j=1 to 13;
	    grupa=1;
		output;
	   end;
	   do j=14 to 47;
	    grupa=2;
	    output; 
	   end;
  end;
   keep grupa;
   run;
 
/* Dodajemo stupac s grupama u dataset approxperm */
data approxperm;
 merge approxperm grupe_perm;
run;

/* Sortiramo approxperm po id i rep */
proc sort data=approxperm;
 by  id rep;
run;

/* Spajamo setove approxperm i grupe (bez varijable grupa) */
data approxperm;
 merge approxperm grupe(drop=grupa);
 by id;
 run;

/* Računamo aritmeticke sredine po permutacijama i po grupama */
proc means data=approxperm nway noprint;
 var ocjena;
 class rep grupa;
 output out=means_perm mean=mean;
 run;

/* Koristimo transpose da bi u istom retku bili podaci za istu permutaciju */
proc transpose data=means_perm out=means_perm_t prefix=_;
 var mean;
 id grupa;
 by rep;
 run;

/* Dodajemo stupac abs_diff (opažena apsolutna vrijednost razlike) i za svaku permutaciju računamo apsolutnu vrijednost
 razlike i brojimo koliko puta je veća od opažene; */


 data means_perm_t;
  set means_perm_t;
  if _N_=1 then set means_t(keep=abs_diff);
  abs_diff_perm=abs(_1-_2);
  N_sig=(abs_diff_perm ge abs_diff);
 run;

/* Zbrajamo koliko je puta testna statistika na nekoj permutaciji
 veća od opazene. */
 proc means noprint;
  var N_sig;
  output out=p_sig sum=sum n=n;
  run;

/* Dodajemo 1 (za našu opaženu permutaciju) jer je >= je našoj opaženoj
 statistici (jednaka je) i računamo p-vrijednost permutacijskojg testa*/
 data p_sig;
  set p_sig;
  p_sig=(sum+1)/(n+1);
  run;

  proc print; run;

/* Brisemo datasetove iz work libraryja */
  proc datasets library=work;
   delete allperm approxperm grupe_perm;
  run;quit; 

/*Ispitujemo pretpostavke za test*/
proc ttest data=grupe;
 var ocjena;
 class grupa;
 run;
 
 /*jedna moguća generalizacija*/

/*Makro naredba aprox_random_test prima dva skupa podataka,
 broj permutacija, seed i naziv stupca u datotekama - x, 
 a radi dataset p_sig sa p-vrijednošću*/


%macro aprox_random_test(dat1, dat2, n_perm, mseed,x);

proc sql noprint;
   select count(*) into : n1
   from &dat1;
quit;

proc sql noprint;
   select count(*) into : n2
   from &dat2;
quit;

data grupe;
 set &dat1 (in=a) &dat2;
 if a then grupa=1; else grupa=2;
 id=_N_; 
 run;

proc sql noprint;
   select count(*) into : n
   from work.grupe;
quit;

 proc means data= grupe nway noprint;
  var &x;
  class grupa;
  output out=means mean=mean;
 run;

proc transpose data=means out=means_t prefix=_;
 var mean;
 id grupa;
 run;

 data means_t;
  set means_t;
  abs_diff=abs(_1-_2);
 run;


proc plan seed=&mseed ; 
    factors     rep=&n_perm random 
                id  = &n /noprint
 ; 
    output out=approxperm;
run;


data grupe_perm;
  do i=1 to &n_perm;
	   do j=1 to &n1;
	    grupa=1;
		output;
	   end;
	   do j=(&n1+1) to &n;
	    grupa=2;
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

proc means data=approxperm nway noprint;
 var &x;
 class rep grupa;
 output out=means_perm mean=mean;
 run;

proc transpose data=means_perm out=means_perm_t prefix=_;
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

%mend aprox_random_test;





/*3. zadatak*/


%let n_perm=1000;

data UCINKOVITOST;
         input formulacija minute @@;
         datalines;
      1 1.96   1 1.94   1 2.92   1 2.90   1 2.96   1 3.27
      1 3.25   1 3.27   1 3.27   2 3.70   2 3.74
      2 3.28   2 3.27   2 3.30   2 3.71   2 3.72   
      ;
run;

 data grupe;
 set ucinkovitost;
 rename formulacija=grupa;
 id=_N_;
 run;

 /*** a   ***/
 
 
 proc means data= grupe nway noprint;
  var minute;
  class grupa;
  output out=means mean=mean;
  run;

proc transpose data=means out=means_t prefix=_;
 var mean;
 id grupa;
 run;

 data means_t;
  set means_t;
  abs_diff=abs(_1-_2);
 run;


    proc plan seed=44855 ; 
      factors     rep=&n_perm random
                  id  = 16 /noprint
 ; 
      output out=approxperm;
run;

/* proc freq; table rep*id; run;*/


 data grupe_perm;
  do i=1 to &n_perm;
   do j=1 to 9;
    grupa=1;
	output;
   end;
   do j=10 to 16;
    grupa=2;
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

proc means data=approxperm nway noprint;
 var minute;
 class rep grupa;
 output out=means_perm mean=mean n=n;
 run;

 
proc transpose data=means_perm out=means_perm_t prefix=_;
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

  proc print; run; /*p_sig=0.003996004*/

  proc datasets library=work;
   delete allperm approxperm grupe_perm;
  run;quit; 

/*ispitivanje uvjeta za primjenu testa: jednakost varijanci? koristimo t test*/
  proc ttest data=ucinkovitost;
  var minute;
  class formulacija;
  run; /*p vrijednost od F testa je 0.0550 i za nivo značajnosti od 5 %, 
 nećemo odbaciti nul hipotezu o jednakosti varijanci. Kako je na predavanjima rečeno
  da je nivo značajnosti 5%, to je onda dobra odluka. 
  Da smo imali npr. nivo značajnosti 10% onda bismo odbacili null hipotezu, pa bismo koristili method=sattherthwaite, ili recimo 
  transformirali  podatke pomoću funkcije logx, ili log(a-x), gdje je a neka konstanta veća od maximalne vrijednosti za
  varijablu x. Nakon toga radimo sve na transformiranim podacima.*/

  /*** dodatno: p_sig se računa u svakom koraku tj. za svaku permutaciju ***/

 data cum_means_perm_t;
  set means_perm_t;

  sum+n_sig;
  n=_N_;
  p_sig=(sum+1)/(n+1);/*za n=100 p_sig=0.0198019802	*/
  sterr=sqrt(p_sig*(1-p_sig)/n); /*standardna pogreška*/
  run;

  proc sgplot data=cum_means_perm_t;
   series x=n y=p_sig;
  run; 
proc sgplot data=cum_means_perm_t;
   series x=n y=sterr;
  run; /*ovdje smo dobili graf standardne pogreke u ovisnosti o n*/

/*** b (isto, samo t-statistika)***/

  ods output ttests=means_t;
proc ttest data=grupe  ;
 var minute;
 class  grupa;
  
  run;
 
data means_t;
 set means_t;
 where method="Pooled";/*gledamo ovu metodu za originalne podatke jer su nam varijance jednake*/
 keep tvalue;
 run;

    proc plan seed=44855 ; 
      factors     rep=&n_perm random
                  id  = 16 /noprint
 ; 
      output out=approxperm;
run;

/* proc freq; table rep*id; run;*/


 data grupe_perm;
  do i=1 to &n_perm;
   do j=1 to 9;
    grupa=1;
	output;
   end;
   do j=10 to 16;
    grupa=2;
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
 merge approxperm grupe;
 by id;
 run;

 proc sort data=approxperm;
 by rep;
 run;
 
  ods output ttests=means_perm_t;
proc ttest data=approxperm  ;
 var minute;
 class  grupa;
 by rep;
  run;
 
data means_perm_t;
 set means_perm_t;
 where method="Pooled";
 keep tvalue;
 rename tvalue=tvalue_perm;
 run;

 data means_perm_t;
  set means_perm_t  ;
   
  if _N_=1 then set means_t ;
   
  N_sig=(tvalue_perm le tvalue or tvalue_perm ge -tvalue); /*tvalue je negativna u ovom primjeru*/
 run;

 proc means noprint;
  var N_sig;
  output out=p_sig sum=sum n=n;
  run;

 data p_sig;
  set p_sig;
  p_sig=(sum+1)/(n+1);
  Nminimum=4*p_sig*(1-p_sig)/(0.05-p_sig)**2; /*formula s predavanja*/

  run;

  

  proc print; run;/*** ==> dovoljno je Nminimum = 10 replikacija (permutacija) ***/

  proc datasets library=work;
   delete allperm approxperm grupe_perm;
  run;quit; 

/*** dodatno: p_sig se računa u svakom koraku tj. za svaku permutaciju ***/

  
 data cum_means_perm_t;
  set means_perm_t;

  sum+n_sig;
  n=_N_;
  p_sig=(sum+1)/(n+1);
  sterr=sqrt(p_sig*(1-p_sig)/n); /*standardna pogreška*/
  run;

  proc sgplot data=cum_means_perm_t;
   series x=n y=p_sig;
  run; 

  proc sgplot data=cum_means_perm_t;
   series x=n y=sterr;
  run;
  /**c) dio**/
  
  proc univariate data=means_perm_t noprint;
var tvalue_perm;
output out=percentiles pctlps=2.5 97.5 pctlpre=P;


run;
proc print data=percentiles;
run;

/*4. zadatak*/


%Let seed=123;
%let NumSamples=1000;

/*simuliramo podatke*/
data SumUniSize;
	call streaminit (&seed);

	do N=10 to 200 by 10;

		do SamplelD=1 to &NumSamples;

			do i=1 to N;
				x=rand("Uniform");
				output;
			end;
		end;
	end;
run;

/*racunamo mean za svaki uzorak;*/
proc means data=SumUniSize noprint;
by N SamplelD;
var x;
output out=OutStats mean=SampleMean;
run;

/*mean i standardna devijacija po velicini uzorka;*/
proc means data=Outstats Mean Std;
class N;
var SampleMean;
output out=ToPlot std=SampleStd;
run; 

/*crtanje uzoracke devijacije mean-a (što je tražena standardna pogreška) kao funkcije 
velicine uzorka;*/
proc sgplot data=ToPlot;
scatter X=N Y=SampleStd;
run;
/*Iz grafa vidimo da s rastom veličine uzorka standardna pogreška očekivanja pada.*/


/*5. zadatak*/

/*simulicija podataka;*/
%let N=50;
%let NumSamples=1000;
%let seed=123;

data Simul (drop=i);
	call streaminit(&seed);

	do SampleId=1 to &NumSamples;

		do i=1 to &N;
			Normal=rand("Normal");
			t=rand("t", 5);
			Exponential=rand("Expo");
			LogNormal=exp(rand("Normal", 0, 0.503));
			output;
		end;
	end;
run;

*računanje momenata;
proc means data=Simul noprint;
	by SampleID;
	var Normal t Exponential LogNormal;
	output out=Moments(drop=_type_ _freq_) Kurtosis=;
run;

proc transpose data=Moments out=Long(rename=(_1=Kurtosis)) prefix=_;
	by SampleID;
run;

*crtanje boxplota;
proc sgplot data=Long;
	title "Kurtosis Bias in Small Samples: N=&N";
	label _Name_="Distribution";
	vbox Kurtosis /category=_Name_ meanattrs=(symbol=Diamond);
	refline 0 6 /axis=y;
	yaxis max=30;
	xaxis discreteorder=DATA;
run;



%let N=2000;
%let NumSamples=1000;
%let seed=123;

data Simul (drop=i);
	call streaminit(&seed);

	do SampleId=1 to &NumSamples;

		do i=1 to &N;
			Normal=rand("Normal");
			t=rand("t", 5);
			Exponential=rand("Expo");
			LogNormal=exp(rand("Normal", 0, 0.503));
			output;
		end;
	end;
run;

*računanje momenata;
proc means data=Simul noprint;
	by SampleID;
	var Normal t Exponential LogNormal;
	output out=Moments(drop=_type_ _freq_) Kurtosis=;
run;

proc transpose data=Moments out=Long(rename=(col1=Kurtosis));
	by SampleID;
run;

*crtanje boxplota;
proc sgplot data=Long;
	title "Kurtosis Bias in Large Samples: N=&N";
	label _Name_="Distribution";
	vbox Kurtosis /category=_Name_ meanattrs=(symbol=Diamond);
	refline 0 6 /axis=y;
	yaxis max=30;
	xaxis discreteorder=DATA;
run;


/* Vidimo da za veće uzorke imamo manje outliera i 
"simetričnije" brkovi box plotta za kurtosis. */
