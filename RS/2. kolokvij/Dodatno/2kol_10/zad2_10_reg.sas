/*U datasetu FITNESS zapisani su rezultati mjera kondicije za 31 èlana fitness kluba. Modelom
	višestruke linearne regresije treba predvidjeti vrijednosti varijable OXYGEN (kapacitet pluæa,
	zavisna varijabla) na osnovu preostalih (prediktorskih, nezavisnih) varijabli, te odrediti
	vrijednost prilagoðenog koeficijenta determinacije (engl. adjusted R2).

a) Neparametarskom bootstrap metodom odredite 90% interval pouzdanosti za prilagoðeni
	koeficijent determinacije. Koristite percentilnu metodu za odreðivanje bootstrap intervala
	pouzdanosti(engl. Bootstrap pctl method for confidence interval). Koristite 1000 bootstrap
	ponavljanja.

b) Neparametarskom bootstrap metodom odredite 90% interval pouzdanosti za drugi
	korijen iz varijance pogreške (engl. root mean square error). Koristite percentilnu metodu za
	odreðivanje bootstrap intervala pouzdanosti(engl. Bootstrap pctl method for confidence
	interval). Koristite 1000 bootstrap ponavljanja.

U a) i b) koristite slijedeæu vrijednost seed makro varijable:
%let seed=744956;

UPUTA: U PROC REG naredbi je potrebno navesti opciju ADJRSQ za spremanje vrijednosti
prilagoðenog koeficijenta determinacije (varijabla _ADJRSQ_) u izlazni dataset EST (ako je navedena
opcija OUTEST=EST u PROC REG naredbi).
Drugi korijen iz varijance pogreške (varijabla _RMSE_) se automatski sprema u izlazni dataset EST
(ako je navedena opcija OUTEST=EST u PROC REG naredbi).*/

	%let path=D:\FAKS\diplomski\4 semestar\RS\racunarska\Racunarska statistika;
	%let outputs=D:\FAKS\diplomski\4 semestar\RS\racunarska\Racunarska statistika\Outputs;
	%let lib=D:\FAKS\diplomski\4 semestar\RS\racunarska\Racunarska statistika\data;

	libname lib "&lib";
	libname library "&lib";

	filename odsout "&outputs";

	filename boot "&path\pictures\boot.gif";
	filename probplot "&outputs\probplot.gif";
	filename boxplot "&outputs\boxplot.gif";


data fitness;
	input Age Weight Oxygen RunTime RestPulse RunPulse MaxPulse @@;
	datalines;
	44 89.47 44.609 11.37 62 178 182 40 75.07 45.313 10.07 62 185 185
	44 85.84 54.297 8.65 45 156 168 42 68.15 59.571 8.17 40 166 172
	38 89.02 49.874 9.22 55 178 180 47 77.45 44.811 11.63 58 176 176
	40 75.98 45.681 11.95 70 176 180 43 81.19 49.091 10.85 64 162 170
	44 81.42 39.442 13.08 63 174 176 38 81.87 60.055 8.63 48 170 186
	44 73.03 50.541 10.13 45 168 168 45 87.66 37.388 14.03 56 186 192
	45 66.45 44.754 11.12 51 176 176 47 79.15 47.273 10.60 47 162 164
	54 83.12 51.855 10.33 50 166 170 49 81.42 49.156 8.95 44 180 185
	51 69.63 40.836 10.95 57 168 172 51 77.91 46.672 10.00 48 162 168
	48 91.63 46.774 10.25 48 162 164 49 73.37 50.388 10.08 67 168 168
	57 73.37 39.407 12.63 58 174 176 54 79.38 46.080 11.17 62 156 165
	52 76.32 45.441 9.63 48 164 166 50 70.87 54.625 8.92 48 146 155
	51 67.25 45.118 11.08 48 172 172 54 91.63 39.203 12.88 44 168 172
	51 73.71 45.790 10.47 59 186 188 57 59.08 50.545 9.93 49 148 155
	49 76.32 48.673 9.40 56 186 188 48 61.24 47.920 11.50 52 170 176
	52 82.78 47.467 10.50 53 170 172
	;
run;

%include "&path\Programs\jackboot1.sas";
%let seed = 744956;
%let dat = fitness;
%let bsamples = 1000;
%let alph = 0.10;


/* a) dio zadatka */

proc reg data = fitness noprint adjrsq outest=est; 
 	model Oxygen = Age Weight RunTime RestPulse RunPulse MaxPulse;
run;
quit;

data originalne_vrijednosti;
	set est;
	keep _rmse_ _adjrsq_;
run;

/* Procjena za AdjR^2 je 0.8108398952, a za RMSE je 2.3169478651 */




/* b) dio zadatka - neparametarski bootstrap za interval pouzdanosti za R^2 i AdjR^2 */

%macro analyze(data =,out =);
	proc reg data = &data noprint adjrsq outest = &out;
 		model Oxygen = Age Weight RunTime RestPulse RunPulse MaxPulse;
		%bystmt;
 	run;
	quit;    
%mend;
              

/* Pouzdani interval za AdjR^2 */ 
%boot(data = &dat, random = &seed, samples = &bsamples, alpha = &alph, stat = _rsq_) /* ova naredba provodi Bootstrap uzrokovanje */      
%bootci(percentile, alpha =&alph) /* 90% pouzdani interval za AdjR^2 je [0.73425, 0.93656] */

/* Pouzdani interval za RMSE */                                                                                                                                                                                                                                                            
%boot(data = &dat, random = &seed, samples = &bsamples, alpha = &alph, stat = _rmse_)
%bootci(percentile, alpha = &alph) /* 90% pouzdani interval za RMSE je [1.93879, 3.21190] */



/* c) dio zadatka - ovdje ne postoji, ali svejedno :) */

%macro ODSOff(); /* Call prior to BY-group processing */
   ods graphics off;  ods exclude all;  ods noresults;
%mend;
 
%macro ODSOn(); /* Call after BY-group processing */
   ods graphics on;  ods exclude none;  ods results;
%mend;

sasfile fitness load; /* 1 */

proc surveyselect data = fitness out = outboot /* 2 */
	seed = 573933 /* 3 */
	method = urs /* 4 */
	samprate = 1 /* 5 */
	outhits /* 6 */
	rep = 1000; /* 7 */
run;

sasfile fitness close; /* 8 */
ods listing close; /* 9 */

%ODSOff

proc reg data = outboot noprint adjrsq outest = est; 
 	model Oxygen = Age Weight RunTime RestPulse RunPulse MaxPulse;
	by Replicate;
run;
quit;

ods listing; /* 11 */

proc univariate data = est;
	var _adjrsq_;
	output out = ci_adjrsq pctlpts = 5, 95 pctlpre = ci;
run;
 
proc univariate data = est;
	var _rmse_;
	output out = ci_rmse pctlpts = 5, 95 pctlpre = ci;
run;

%ODSOn


ods html;

data ci_adjrsq;
	merge originalne_vrijednosti ci_adjrsq;
	drop _rmse_;
run;

data ci_rmse;
	merge originalne_vrijednosti ci_rmse;
	drop _adjrsq_;
run;

title 'Pouzdani intervali za AdjR^2 (surveyselect)';
proc print data = ci_adjrsq;
run;

title 'Pouzdani intervali za RMSE (surveyselect)';
proc print data = ci_rmse;
run;
