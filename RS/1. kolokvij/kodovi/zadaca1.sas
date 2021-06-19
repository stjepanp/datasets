/* 1.zadatak*/
%let seed1= 65789;
%let seed2=34567;
%let sigma=0.5;
%let mu=1;
%let nrep=1000;
/*a) dio*/
DATA LOGN;
CALL STREAMINIT(&SEED1);
 DO REP = 1 TO &NREP;
  x=rand('LOGNORMAL');
  OUTPUT;
 END;
RUN;
/*prvo izračunajmo vrijednosti traženih deskritivnih statistika */
proc means data=logn mean std skewness kurtosis median;
var x;
run;
/*sada nacrtajmo ppplot, a testovi se naprave automatski unutar proc univariate procedure*/

ods graphics;
 proc univariate data=LOGN;
  var x;
  ppplot  / lognormal(  color=red) square;
          /** parametri teorijske distribucije se procjenjuju iz podataka **/
  ppplot  / lognormal(   sigma=1 zeta=0 color=yellow) square;
          /** parametri teorijske distribucije su specificirani/zadani ***/
histogram  /kernel lognormal(midpercents  sigma=1 zeta=0 color=red);
  run;
 ods graphics off;
/*p vrijednost testova je veća od 0.25 pa ne odbacujemo nul hipotezu da podaci dolaze iz lognormalne distribucije*/

/* b) dio */
DATA LOGN_2;
CALL STREAMINIT(&SEED2);
 DO REP = 1 TO &NREP;
  X = RAND ("NORMAL");
X1=exp(&MU+(&SIGMA)*X); /*Na ovaj način je definirana lognormalna slučajna varijabla. Dakle, Y=e^{mu +sigma *X}, gdje je X standardna normalna sl. varijabla.*/
  OUTPUT;
 END;
RUN;

proc univariate data=LOGN_2;
  var x1;
  ppplot  / lognormal(  color=red) square;
          /** parametri teorijske distribucije se procjenjuju iz podataka **/
  ppplot  / lognormal(   sigma=&sigma zeta=&mu color=yellow) square;
          /** parametri teorijske distribucije su specificirani/zadani ***/
histogram  /kernel lognormal(midpercents  sigma=&sigma zeta=&mu color=red);
  run;
/*c)*/
  /*ručno za a) dio zadatka*/
PROC SORT DATA=LOGN OUT=QQ; BY X; RUN;

DATA QQ;
SET QQ nobs=NObs;
v = (_N_ - 0.375) / (NObs + 0.25);
q = quantile("LOGN", v);
label X = "Observed Data X" q = "Lognormal Quantiles";
run;

proc sgplot data=QQ;
   scatter x=q y=X;
   xaxis grid;  yaxis grid;
run;

/* qqplot za (b) dio zadatka */

proc sort data=LOGN_2 out=QQ; by X1; run;

data QQ;
set QQ nobs=NObs;
v = (_N_ - 0.375) / (NObs + 0.25);
q = quantile("LOGN", v, 0, 0.5);                            /*zeta=mu=0=ln(1), sigma=0.5*/
label X1 = "Observed Data X1" q = "Lognormal Quantiles";
run;

proc sgplot data=QQ;
   scatter x=q y=X1;
   xaxis grid;  yaxis grid;
run;



/*qqplot za (b) dio zadatka sa univariate procedurom - da možete usporediti rezultate i da znate kako to možemo direktno napraviti pomoću procedure proc univariate.*/

PROC UNIVARIATE DATA=LOGN_2;
qqplot X1 /LOGNORMAL(theta=0 sigma=0.5 zeta=1);
run;

/*2. zadatak*/
%let seed=22334;
%let nrep=1000;
data bernouli;
call streaminit(&seed);
do rep=1 to &nrep;
berni=rand('BERN',0.25);
output;
end;
run;
proc means data=bernouli mean std skewness kurtosis median;
var berni;
run;
proc freq data=bernouli;
run; /*uočite da je postotak jedinica (uspjeha) 0.269, što je blizu 0.25 */

/*3. zadatak*/

%let seed=44566;
%let nrep=1000;
%let sigma2=25;
%let mu=0;
data mixnorm;
call streaminit(&seed);
do rep=1 to &nrep;
bern = rand ('BERN', 0.8); /*generirate Bernoullijevu sl. var. s parametrom 0.8, ako je vrjednost 1, onda generirate iz jedinične normalne, a ako je 0, onda iz N(0,25)*/
if  bern=1 then
        x=rand('norm');
else
        x=sqrt(&sigma2)*rand('norm')+&mu;
pom=rand('norm');

output;
end;
run;

proc sgplot data=mixnorm;

title "histogram i gustoca mixnorm distribucije";
histogram x;
density x; /* crtamo histogram od x zajedno sa gustocom procjenjene normalne distribucije */

run;

proc sgplot data=mixnorm;
title "gustoca od x i standardne normalne distribucije";
density x;
density pom; /* mogli smo napisati umjesto ove linije */
*density x/ type=normal (mu=0 sigma=1);
/*na taj nacin dobijemo graf kao u papirima*/
run;


/* za momente mozemo iskoristiti proc univariate*/
proc univariate data= mixnorm;
var x pom;
run;
/*ili proc means*/
proc means data=mixnorm mean var skewness kurtosis;
var x pom;
run;
PROC UNIVARIATE DATA=mixnorm;
 VAR X;
 HISTOGRAM X/KERNEL(color=purple W=1) NORMAL(mu=0 sigma=1 color=green W=1);
RUN;

/*4.zadatak*/
%let seed=444566;
%let alpha=2;
%let beta=3;
%let nrep=1000;
DATA beta_1;
CALL STREAMINIT(&seed);
xmode=(&alpha-1)/(&alpha+&beta-2);
c=(1/beta(&alpha,&beta))*(xmode**(&alpha-1))*((1-xmode)**(&beta-1));
 DO REP = 1 TO &NREP;
y=rand('UNIFORM');
g=pdf('UNIFORM',y);
u=rand('UNIFORM');
pom=(1/beta(&alpha,&beta))*(y**(&alpha-1))*((1-y)**(&beta-1));
p=pom/(g*c);
if u<p then do;
brojac=1;
end;
else do;
brojac=0;
end;
   OUTPUT;
 END;
RUN;
proc print;
var y brojac;
run;
data konacna;
set beta_1;
where brojac=1;
keep y;
run;
proc sgplot data=konacna;
histogram y;
run;
/*q-q plot*/
PROC SORT DATA=konacna OUT=QQ; BY Y; RUN;

DATA QQ;
SET QQ nobs=NObs;
v = (_N_ - 0.375) / (NObs + 0.25);
q = quantile("BETA", v,&alpha,&beta);
label Y = "Observed Data Y" q = "Beta Quantiles";
run;

proc sgplot data=QQ;
   scatter x=q y=Y;
   xaxis grid;  yaxis grid;
run;
/*p-p plot:*/
DATA ppr;
        SET konacna;
        TCDF=CDF("BETA",Y,&ALPHA,&BETA);
RUN;
PROC SORT DATA=ppr OUT=PPS;
        BY TCDF;
RUN;

DATA PP;
        SET PPS NOBS=NOBS;
        NO=_N_/NOBS;
        V=(_N_ - 0.375)/(NOBS+0.25);
        LABEL TCDF="THEORETICAL CUMULATIVE DISTRIBUTION" NO="OBSERVED CUMULATIVE DISTRIBUTION";
RUN;

PROC SGPLOT DATA=PP;
        SCATTER X=TCDF Y=NO;
        XAXIS GRID;
        YAXIS GRID;
RUN;
/*provjera uz proc capability*/
proc capability data=konacna;
ppplot Y / beta (alpha=&alpha beta=&beta);
run;
 /*Na ovaj način ne možemo imati 1000 podataka na kraju, jer smo imali petlju koja je izgenerirala 1000 podataka, ali neke smo odbacili.*/

/*Kako je trebalo biti 1000 podataka na kraju, kod izgleda ovako:*/
%LET SEED=445566;
%LET NREP=1000;
%LET ALPHA=2;
%LET BETA=3;
%LET L=0;
%LET R=1;

/**GENERIRAMO NIZ OD 1000 PSEUDOSLUCAJNIH BROJEVA IZ BETA DISTRIBUCIJE POMOCU
JEDNOSTAVNE METODE PRIHVACANJA/ODBACIVANJA **/

%LET DISTR='UNIFORM'; /**MACRO VARIJABLA ZA GENERIRANJE VARIJABLE Y PO DISTRIBUCIJI DISTR **/
DATA AR;
        CALL STREAMINIT(&SEED);
        XMODE=(&ALPHA-1)/(&ALPHA+&BETA-2); /** F-JA GUSTOCE BETA DISTRIBUCIJE S PARAMETRIMA ALFA=2, BETA=3, POPRIMA MAX U OVOJ TOCKI **/
        C=(1/BETA(&ALPHA,&BETA))*(XMODE**(&ALPHA-1))*((1-XMODE)**(&BETA-1));
        /** GOTOVA NAREDBA:
        C=PDF('BETA',XMODE,&ALPHA,&BETA,&L,&R); **/
        N=1;
        DO WHILE (N<&nrep+1);
                Y = RAND(&DISTR); /** GENERIRAMO Y PO DISTR DISTRIBUCIJI**/
                G = PDF(&DISTR,Y); /** GY JE F-JA GUSTOCE DISTR DISTRIBUCIJE PA RAČUNAMO G=GY(Y) **/
                U = RAND('UNIFORM'); /** GENERIRAMO U PO UNIFORMNOJ DISTRIBUCIJI */
                IF (0 <= Y <= 1) THEN DO;
                        P = (1/BETA(&ALPHA,&BETA))*(Y**(&ALPHA-1))*((1-Y)**(&BETA-1));
                        /**PDF('BETA',Y,&ALPHA,&BETA,&L,&R);**/
                        PCG=P/(C*G);
                        IF (U < PCG) THEN DO;

                                N+1;
                                OUTPUT;
                        END;
                END;
        end;
RUN;


/**HISTOGRAM POMOCU PROC SGPLOT*/
PROC SGPLOT DATA=AR;
        HISTOGRAM  Y;
RUN;


/**PP PLOT POMOCU PROC SGPLOT**/
DATA POM;
        SET AR;
        TCDF=CDF("BETA",Y,&ALPHA,&BETA);
RUN;
PROC SORT DATA=POM OUT=PPS;
        BY TCDF;
RUN;

DATA PP;
        SET PPS NOBS=NOBS;
        NO=_N_/NOBS;
        V=(_N_ - 0.375)/(NOBS+0.25);
        LABEL TCDF="THEORETICAL CUMULATIVE DISTRIBUTION" NO="OBSERVED CUMULATIVE DISTRIBUTION";
RUN;

PROC SGPLOT DATA=PP;
        SCATTER X=TCDF Y=NO;
        XAXIS GRID;
        YAXIS GRID;
RUN;
/* Ne primjecujemo odstupanja od pravca, mozemo zakljuciti da je generirani uzorak iz beta distribucije
   s parametrima alpha=2 beta=3*/
/*provjera pomocu proc capability:*/
proc capability data=AR;
ppplot Y / beta (alpha=&alpha beta=&beta);
run;

/**PRIPADNOST BETA DISTRIBUCIJI**/
PROC UNIVARIATE DATA=AR;
        VAR Y;
        HISTOGRAM Y /BETA(ALPHA=2 BETA=3);
RUN;

/**GENERIRANI UZORAK PRIPADA BETA DISTRIBUCIJI JER JE P VRIJEDNOST KS TESTA >0.250 **/
