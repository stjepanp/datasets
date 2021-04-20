/*1. zadatak*/

LIBNAME dob '/home/u47429085'; /*u ovisnosti koja je kratica do vašeg radnog direktorija u SAS Studiju u browseru, morate ovaj dio promijeniti*/
DATA dob_zena;
set dob.dob_zenahr_18_30 (keep=Starost dprob);/*čitamo podatke iz datoteke dob_zena_18_30 */
RUN;
/* MACRO MRANTBL DEFINITION */ 
%macro mrantbl(dat,catvar,pvar,rep,seed);

DATA _NULL_;
 set &dat end=end;/* u varijabli end je spremljen indikator kraja datoteke, koji ima vrijednost 0 sve dok ne dodjemo do kraja kada ima vrijednost 1*/
 if end then call symput("ncat",left(put(_N_,3.))); /* kada dodjemo do kraja datoteke u makro varijablu ncat spremamo broj opservacija, tj. podataka*/
run;

DATA TEMP(KEEP=&catvar);
     SET &dat END=ENDD; /*u varijabli ENDD je spremljen indikator kraja sas datoteke dat*/

     /* set up arrays for the &ncat &catvar categories ('TEMP') */
     /* and the percent of companies in those  */
     /* categories ('PROB').                          */

     ARRAY &catvar.S(&ncat) $5 &catvar.1-&catvar&ncat;/* definiramo niz catvar.S duljine ncat, svaki clan niza je karakterna varijabla duljine 5, i clanovi niza se zovu catvar.1-catvar.ncat */
     ARRAY PROB(&ncat) PROB1-PROB&ncat;
     RETAIN &catvar.1-&catvar&ncat PROB1-PROB&ncat;/*zadržavamo vrijednosti navedenih varijabli za iduću iteraciju u data koraku*/
     &catvar.S(_N_)=&catvar; /*za svaku opservaciju catvar.S poprima vrijednost odgovarajućeg catvar */
     PROB(_N_)=&pvar;
	
     /* generate the 10,000 random categories. */
     
     IF ENDD THEN DO;/*nakon što smo učitali sve opservacije iz datoteke dat, prelazimo na simulaciju*/
        DO I=1 TO &rep;

           /* function RANTBL is called with the probabilities */
           /* stored in array 'PROB'. The integer returned by  */
           /* RANTBL is mapped into the &catvar category.       */

           &catvar=&catvar.S(RANTBL(&seed,OF PROB(*)));/*simuliramo uzorak duljine 10000 iz diskretne razdiobe sa vjerojatnostima koji su zadani u PROB*/
           OUTPUT;
        END;
        END;

/* check the random distribution to the theoretical one using */
/* the chi-square test of PROC FREQ. Obtain the frequencies   */
/* by rating category in the random sample.                   */

PROC FREQ DATA=TEMP;
     TABLE &catvar / NOPRINT OUT=FREQHIST(RENAME=(PERCENT=RNDPNT)
                                 KEEP=&catvar PERCENT COUNT);
     RUN; /*u sas datoteci FREQHIST će biti spremljene relativne frekvencije simuliranog uzorka*/

/* set up a special file where there is a record for each       */
/* category and for each sample (theoretical and random).       */
/* both theoretical and random samples have 10,000 freq. each.  */
/* variable 'FRQ' holds the frequency in each record.           */

PROC SORT DATA=&dat;
     BY &catvar;/*sortiramo podatke po vrijesnotima u catvar*/
DATA BOTH;
     MERGE &dat FREQHIST; /*spajamo teorijske i random uzorke*/
     BY &catvar;
     SAMPLE='Theoretical';
     FRQ=ROUND(&rep*&pvar);
     OUTPUT;
     SAMPLE='Random';
     FRQ=COUNT;
     OUTPUT;
     RUN;

/* execute PROC FREQ to determine whether or not the   */
/* random sample is from the theoretical distribution. */

PROC FREQ DATA=BOTH;
     TABLE SAMPLE*&catvar / CHISQ;
     WEIGHT FRQ;
     RUN;
%mend mrantbl;


        /* END MACRO MRANTBL DEFINITION */ 

        /* MACRO MRANTBL EXECUTION */ 

%mrantbl(dat=dob_zena,catvar=Starost,pvar=dprob,rep=10000,seed=123)

/* usporedba stupčastog dijagrama generiranog uzorka sa 'teorijskim' */
PROC GCHART data=both;
title 'Broj replikacija 10000';
vbar starost/ 	descrete
				group=sample
				patternid=group
				sumvar=frq;
RUN;
QUIT;

/* ponovimo sve isto za 1000 replikacija*/
%mrantbl(dat=dob_zena,catvar=Starost,pvar=dprob,rep=1000,seed=123)
PROC GCHART data=both;
title 'Broj replikacija 1000';
vbar starost/ 	descrete
				group=sample
				patternid=group
				sumvar=frq;
RUN;
QUIT;

/*2. zadatak*/
%LET lambda=1/2.5; /*sredina(mean) eksponencijalne distribucije je 1/lambda, iz čega dobijemo lambda.*/
%LET shape=4; /*parametar za Erlangovu distribuciju*/
%LET seed=7736;
%LET p=0.75; /*vjerojatnost uspjeha za Bernoullijevu sl.var.*/
%LET n=100;
*-----------------------------------------------------------------------------
| Stvaranje data setova u kojem su generirani potrebni pseudoslučajni brojevi
*-----------------------------------------------------------------------------;


DATA bernoulli;
call streaminit(&seed);
	do i=1 to &n;
		ber=rand('BERN',&p);
		OUTPUT;
	end;
RUN;
DATA mixed_distrib (DROP=i); /*ovaj dio se mogao napisati i u prethodnom data setu, nije se morao izdvojiti u poseban data set*/
	set bernoulli;
	call streaminit(&seed);
	if ber=1 then mix=rand('erlang',&shape);
	else mix=(rand('exponential'))/&lambda; /*rand ('exponential') generira podatak iz Exp(1), pa na ovaj način generiramo eksponencijalnu s parametrom lambda. */
	OUTPUT;
RUN;
PROC PRINT NOOBS;
title 'Generirana vremena potrebna za kupovinu';
var mix;
RUN;
proc means data=mixed_distrib mean std skewness kurtosis sum;
var mix;
run;

/*3. zadatak*/

*-------------------------------------------------
| Definiranje makro varijabli koje ce nam trebati
*-------------------------------------------------;
%LET seed1=6678;
%LET seed2=2211;
%LET seed3=33456;
%LET seed4=4544;
%LET seed5=4236;
%LET seed6=56643;
%LET broj=100;
%LET broj1=2; /*ova makro varijabla nam treba za testni primjer u f) dijelu zadatka*/
*-----------------
| Dani data set
*-----------------;
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

*a) dio;
data RAZRED2;
set RAZRED;
TEZINA=0.4536*WEIGHT;
VISINA=2.54*HEIGHT;
run;
PROC PRINT data=razred2;
title 'razred';
RUN;

*b) dio;
*---------------------------------------------------------------------------------
| Procjenjivanje prva 4 momenta pomoću PROC MEANS i spremanje istih u izlazni 
| data set. Za svaku varijablu radim posebni data set te ih zatim spojim u jedan.
*---------------------------------------------------------------------------------;
PROC MEANS data=razred2  nway mean std skewness kurtosis;
var VISINA;
output out=momenti1 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
PROC MEANS data=razred2  nway mean std skewness kurtosis;
var TEZINA;
output out=momenti2 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
DATA momenti;
set momenti1 momenti2;
RUN;
PROC PRINT data=momenti;
title 'Prva cetiri momenta za Visina i Tezina';
RUN; 
*-------------------------------------------------
| Macro za računanje Fleishmanovih koeficijenata
*-------------------------------------------------;
*c) i e) dio;

%macro fleishman;
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
*---------------------------------------------------------------------
| Zbog nacina na koji je napravljen makro i data set nonnor
| razdvajam skewness i kurtosis od sredine i standardne devijacije.
*---------------------------------------------------------------------; 
DATA skewkurt (drop=mean std); /*Ovdje ste mogli i ručno unijeti brojeve koje ste dobili s proc means, s tim da zaokružite na određeni broj decimala*/
set momenti1;
RUN;
DATA meansig1 (drop=skewness kurtosis);
set momenti1;
RUN;
%fleishman; *poziv makro-a;
*---------------------------------------------------------------------------
| Stvaranje data set-ova s različitim seed-ovima (ima ih 5) u kojima
| su generirani pseudoslučajni brojevi iz nenormalne distribucije
| čija su prva 4 momenta upravo oni koje smo procijenili prije.
| Za isti seed u jednom data set-u su i generirani podaci s momentima 
| procijenjenim iz TestAUC varijable i oni s momentima procijenjenim iz 
| RefAUC varijable. Razlikujemo ih po identifikacijskoj varijabli id.
|
| Nakon sto smo generirali brojeve iz nenormalne razdiobe, procijenimo 
| njihova prva 4 momenta pomocu PROC MEANS.
*---------------------------------------------------------------------------;
DATA nonnor_1;
 merge fleishman meansig1;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed1);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_1  nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_1 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
proc print data=momenti_nonnor_1;
run;
DATA nonnor_2;
 merge fleishman meansig1;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed3);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_2 nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_2 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
DATA nonnor_3;
 merge fleishman meansig1;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed4);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_3 nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_3 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
DATA nonnor_4;
 merge fleishman meansig1;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed5);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_4 nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_4 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
DATA nonnor_5;
 merge fleishman meansig1;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed6);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_5 nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_5 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
DATA svi; /*spajamo ispise za svih 5 iteracija u jedan data set*/
set momenti_nonnor_1 momenti_nonnor_2 momenti_nonnor_3 momenti_nonnor_4 momenti_nonnor_5;
run;
proc print data=svi;
run;
/*d) i e) dio*/
DATA skewkurt (drop=mean std);
set momenti2;
RUN;
DATA meansig2 (drop=skewness kurtosis);
set momenti2;
RUN;

%fleishman; *poziv makro-a;
*---------------------------------------------------------------------------
| Stvaranje data set-ova s razlicitim seed-ovima (ima ih 4) u kojima
| su generirani pseudoslucajni brojevi iz nenormalne distribucije
| cija su prva 4 momenta upravo oni koje smo procijenili prije.
| Za isti seed u jednom data set-u su i generirani podaci s momentima 
| procijenjenim iz TestAUC varijable i oni s momentima procijenjenim iz 
| RefAUC varijable. Razlikujemo ih po identifikacijskoj varijabli id.
|
| Nakon sto smo generirali brojeve iz nenormalne razdiobe, procijenimo 
| njihova prva 4 momenta pomocu PROC MEANS.
*---------------------------------------------------------------------------;
DATA nonnor_12;
 merge fleishman meansig2;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed2);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_12  nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_12 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
proc print data=momenti_nonnor_12;
run;
DATA nonnor_22;
 merge fleishman meansig2;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed3);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_22 nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_22 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
DATA nonnor_32;
 merge fleishman meansig2;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed4);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_32 nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_32 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
DATA nonnor_42;
 merge fleishman meansig2;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed5);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_42 nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_42 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
DATA nonnor_52;
 merge fleishman meansig2;
 varid=_N_;
 a=-c;
 do i=1 to 18;
    x=RANNOR(&seed6);
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + std* x;
	id=_N_;
	output;
 end;
RUN;
PROC MEANS data=nonnor_52 nway mean std skewness kurtosis;
var x;
class id;
output out=momenti_nonnor_52 mean=mean std=std skewness=skewness kurtosis=kurtosis;
RUN;
*-------------------------------------
| Da bismo generirali 100 data set-ova u kojima su pseudoslučajne
| varijable iz nenormalne distribucije takvih da uvijek upotrebljavamo
| iste zadane momente, ali različite seed-ove, kreiramo macro program koji
| 100 puta odvrti (ili koliko mu već zadamo) gornji data step i proc step.
| Da bismo svaki puta imali drugačiji seed, možemo staviti da je seed=0 pa će
| se seed očitati iz vremena računala ili sami definiramo seed-ove kao makro
| varijable.
*------------------------------------;


%MACRO nonnormalna(broj);
	%do i = 1 %to &broj;
   	data nonnormal&i;
		merge fleishman meansig1;
		varid=_N_;
	 	a=-c;
	 	%do j=1 %to 18;
    		x=RANNOR(&seed&i);
			x=a + b*x + c* x**2 + d*x**3;
			x=mean + std* x;
			id=_N_;
			output;
 		%end;
run;
	PROC MEANS data=nonnormal&i nway mean std skewness kurtosis;
	var x;
	class id;
	output out=momenti_nonnor_&i mean=mean std=std skewness=skewness kurtosis=kurtosis;

 RUN;
%end;
%MEND nonnormalna;
*kao primjer uzmimo broj1=2;

%nonnormalna(&broj1);

/*4. zadatak*/

%LET SEED =47755;	 
%LET NREP=300;	
*%LET NREP=10000; *ovaj dio otkomentirati za g) i h) dio zadatka, a zakomentirati prethodnu liniju;

/**  a */

DATA expgamma;
 CALL STREAMINIT(&SEED);
 trgovina="A";
 DO REP = 1 TO &NREP;
 ber=rand ("BERNOULLI",0.3);
 if ber then x=3*rand("EXPO");
  else X =  RAND('GAMMA',5) ;  	 
  OUTPUT;
 END;
 trgovina="B";
 DO REP = 1 TO &NREP;
 ber=rand ("BERNOULLI",0.4);
 if ber then x=4*rand("EXPO");
  else X =  RAND('GAMMA',7) ;  	 
  OUTPUT;
 END;

RUN;

proc means data=expgamma mean sum std skewness kurtosis stderr;
 var x;
 class trgovina;
 run;
/*
     The MEANS Procedure

Analysis Variable : x
trgovina	N Obs	Mean	Sum	Std Dev	Skewness	Kurtosis	Std Error
A	300	4.5904572	1377.14	2.6624801	1.4308787	7.0164180	0.1537184
B	300	5.9410755	1782.32	3.5276755	0.7053549	1.3527877	0.2036704
Odgovori: b) A trgovina: 1377.14 min=22.95h, B trgovina: 1782.32 min=29.71h 
          c) A trgovina: prosječno vrijeme kupovine je 4.59 min, tokom 12 sati usluženo 12*60/4.59=156.86, tj. 156 kupaca.
             B trgovina: prosječno vrijeme kupovine je 5.94 min, tokom 12 sati usluženo 12*60/5.94=121.21, tj. 121 kupaca.
             
*/
/* d) dio */
DATA expexp;
 CALL STREAMINIT(&SEED);
 trgovina="A";
 DO REP = 1 TO &NREP;
 ber=rand ("BERNOULLI",0.3);
 if ber then x=3*rand("EXPO");
  else X = 5*RAND("EXPO") ;  	 
  OUTPUT;
 END;
 trgovina="B";
 DO REP = 1 TO &NREP;
 ber=rand ("BERNOULLI",0.4);
 if ber then x=4*rand("EXPO");
  else X = 7*RAND("EXPO")  ;  	 
  OUTPUT;
 END;

RUN;

proc means data=expexp mean sum std skewness kurtosis stderr;
 var x;
 class trgovina;
 run;
/*  The MEANS Procedure

Analysis Variable : x
trgovina	N Obs	Mean	Sum	Std Dev	Skewness	Kurtosis	Std Error
A	300	4.1322136	1239.66	4.4536722	2.2003419	6.4089791	0.2571329
B	300	5.2518571	1575.56	5.7129148	2.2728348	8.1062492	0.3298353


 */

/*5. zadatak*/
data TLAK;
         input SKTprije SKTposlije @@;
         datalines;
   120 127   128 130   130 131   118 127
   140 132   128 125   140 141   135 137
   126 118   130 132   130 129   127 120
   122 121   141 125  
   ;
   run;



/*a) dio*/
 /* H0: mi_SKTprije=mi_SKTposlije
   H1: mi_SKTposlije<miSKTprije*/
  
PROC TTEST data=tlak;
paired SKTprije*SKTposlije;
RUN;
/* Ne odbacujemo nultu hipotezu na razini znaajnosti od 0.05. 
Objašnjenje: Dobivenu p vrijednost 0.4298 podijelimo s dva jer nama treba jednostrani test,
ona je i dalje veća od 0.05.*/

PROC MEANS data=tlak mean std;
var SKTprije SKTposlije;
RUN;
PROC CORR data=tlak;
var SKTprije SKTposlije;
RUN;
* b);
%LET SEED =5678;		
%LET NREP=100;	
%let rho=0.54358;
%let mi1=129.64286;
%let mi2=128.21429;
%let s1=7.26008;
%let s2=6.37518;

DATA NORMAL2;
 DO REP = 1 TO &NREP;
  X1 = NORMAL (&SEED);	
  X2 = NORMAL (&SEED);	
  X = X1;
  Y = &RHO * X1 + SQRT(1 - &RHO**2)* X2;
  X=round(&s1*X+&mi1);
  Y=round(&s2*Y+&mi2);
  OUTPUT;
 END;
 DROP X1 X2;
RUN;
proc means data=normal2 n mean std maxdec=2;
run;
proc corr data=normal2;
var x y;
run;

*c);

%macro ODSOff;
ods graphics off;
ods exclude all;
ods noresults;
%mend;

%macro ODSOn; 
ods graphics on;
ods excLude none;
ods results;
%mend;

%ODSOff;
DATA NORMAL3;
 DO REP = 1 TO &NREP;
 DO I=1 TO 14;
  X1 = NORMAL (&SEED);	
  X2 = NORMAL (&SEED);	
  X = X1;
  Y = &RHO * X1 + SQRT(1 - &RHO**2)* X2;
  X=round(&s1*X+&mi1);/*zaokružujemo vrijednosti jer je tlak cijeli broj*/
  Y=round(&s2*Y+&mi2);
  OUTPUT;
  END;
 END;
 DROP X1 X2;
RUN;
ods output ttests=ttests;
proc ttest data=normal3;
paired X*Y;
by REP;
run;
%ODSOn;
proc means data=ttests mean std skewness kurtosis stderr p5 p95;
var tValue;
run;
/* The MEANS Procedure

Analysis Variable : tValue t Value
Mean	    Std Dev	   Skewness	   Kurtosis	    Std Error	 5th Pctl	95th Pctl
0.9693749	1.1704730	1.0716366	2.7468309	0.1170473	-0.7664181	3.1939558

*/
/* za d) dio usporedite vrijednost t statistike iz a) dijela s prosječnom iz
 prethodnog koraka.
 Pogledajte upada li vrijednost t statistike iz a) dijela zadatka između 5. i 95. centila 
 izračunatog u prethodnom koraku i na osnovu toga odgovorite na pitanje u zadatku.
 Još nacrtajmo vrijednosti t iz c) dijela i gustoću t distribucije s 13 stupnjeva slobode*/
data t_pdf;
   do x=-4 to 4 by 0.01;
      pdf_t = pdf('t', x, 13);       /* PDF                     */
      output;
   end;
   run;
   data crtanje;
   set t_pdf ttests;
   run;
proc sgplot data=crtanje;
title "gustoca od t distribucije s 13 stupnjeva slobode";
density pdf_t;
run;
/*histogram vrijednosti tValue*/
proc sgplot data=ttests;
	histogram tValue / nbins=15;
	density tValue / type=Kernel;
	xaxis min=-3 max=8;
	yaxis grid;
run;
/*ili možemo PROC univariate koristiti*/
PROC univariate data=crtanje;
title 'Histogram vrijednosti tVAlue';
histogram tValue;
RUN;

