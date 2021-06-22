/*3. Koristite podatke iz zadatka 2.
a) Parametarskom bootstrap metodom odredite bootstrap standardnu pogrešku i 90% bootstrap
	interval pouzdanosti za apsolutnu vrijednost razlike izmeðu prosjeènih vrijednosti varijable SECER za
	žitarice za djecu i prosjeènih vrijednosti varijable SECER za žitarice za odrasle.

b) Parametarskom bootstrap metodom odredite bootstrap standardnu pogrešku i 90% bootstrap
	interval pouzdanosti za apsolutnu vrijednost razlike izmeðu 10-postotne podrezane sredine (engl.
	10% trimmed mean) varijable SECER za žitarice za djecu i 10-postotne podrezane sredine varijable
	SECER za žitarice za odrasle.

U a) i b):
Primijenite slijedeæe vrijednosti makro varijabli SEED:
%let seed= 58849; *za grupu 1 (djeca);
%let seed= 28566; *za grupu 2 (odrasli);

Pretpostavite da varijabla SECER slijedi normalnu distribuciju.
Za svaku grupu izvedite 1000 bootstrap ponavljanja (replikacija).

UPUTA za b):
Umjesto procedure MEANS, koristite procedure UNIVARIATE i SORT:
proc sort data= imeulaznogdataseta;
by rep imegrupnevarijable;
run;

ods select trimmedmeans;
ods output TrimmedMeans=imeizlaznogdataseta;
proc univariate data=imeulaznogdataseta trimmed=0.10;
var imevarijable;
class imegrupnevarijable;
by rep;
run;

NAPOMENA: Nakon izvoðenja procedure UNIVARIATE æe u izlaznom datasetu
(„imeizlaznogdataseta“) 10-postotna podrezana sredina biti spremljena kao varijabla sa imenom
MEAN.*/



data zitarice_za_djecu;
	input secer @@;
	datalines;
	40.3 55 45.7 43.3 50.3 45.9 23.5 43 44.2 30.3
	;

data zitarice_za_odrasle;
	input secer @@;
	datalines;
	20 30.3 25.2 74.5 4.4 22.2 16.6 14.5 21.4 33.3 6.6 37.8 10.6 16.2 44.1
	; 
run;

data zitarice_za_djecu;
	set zitarice_za_djecu;
	starost = 'd';
run;

data zitarice_za_odrasle;
	set zitarice_za_odrasle;
	starost = 'o';
run;

data zitarice;
	set zitarice_za_djecu;
run;

proc append base = zitarice data = zitarice_za_odrasle;
run;

data zitarice;
	set zitarice;
	id = _N_;
run;


data grupa1;
	set zitarice;
	where starost = 'd';
run;

data grupa2;
	set zitarice;
	where starost = 'o';
run;


proc means data = zitarice nway noprint;
	var secer;
  	class starost;
  	output out = out mean = mean std = std;
run;

data _NULL_;
	set out;
	if starost = 'd' then call symput("mean1", mean);
  	else call symput("mean2", mean);
	if starost = 'd' then call symput("std1", std);
  	else call symput("std2", std);
run;

%put mean1 = &mean1 std1 = &std1 mean2 = &mean2 std2 = &std2;


/* parametarski bootstrap za djecu */
%let broj_uzoraka = 1000;
%let duljina_uzorka = 10;
%let seed = 58849;

data param_boot_djeca;
	call streaminit(&seed);
	do i = 1 to &broj_uzoraka;
		do j = 1 to &duljina_uzorka;
			x = &std1 * rand("normal") + &mean1;
			output;
		end;
	end;
run;


proc means data = param_boot_djeca nway noprint;
	var x;
	by i;
	output out = mean_djeca mean = mean_djeca;
run;

/* parametarski bootstrap za odrasle */
%let broj_uzoraka = 1000;
%let duljina_uzorka = 15;
%let seed = 28566;

data param_boot_odrasli;
	call streaminit(&seed);
	do i = 1 to &broj_uzoraka;
		do j = 1 to &duljina_uzorka;
			x = &std2 * rand("normal") + &mean2;
			output;
		end;
	end;
run;


proc means data = param_boot_odrasli nway noprint;
	var x;
	by i;
	output out = mean_odrasli mean = mean_odrasli;
run;



data sve_skupa;
	merge mean_djeca mean_odrasli;
	aps_raz_mean = abs(mean_djeca - mean_odrasli);
run;

proc means data = sve_skupa mean stderr lclm uclm p5 p95;
	var aps_raz_mean;
run;




/* b) dio zadatka */
/*
proc sortdata = imeulaznogdataseta;
	by rep imegrupnevarijable; 
run;
*/

ods select trimmedmeans; 
ods output TrimmedMeans = tm_djeca;
 
proc univariate data = param_boot_djeca trimmed = 0.10; 
	var x;
	*class imegrupnevarijable; 
	by i; 
run; 


ods select trimmedmeans; 
ods output TrimmedMeans = tm_odrasli;
 
proc univariate data = param_boot_odrasli trimmed = 0.10; 
	var x;
	*class imegrupnevarijable; 
	by i; 
run; 

proc print data = zitarice;
run;

proc contents data = tm_djeca;
run;

data tm_djeca;
	set tm_djeca;
	trimmean_djeca = mean;
	keep trimmean_djeca;
run;

data tm_odrasli;
	set tm_odrasli;
	trimmean_odrasli = mean;
	keep trimmean_odrasli;
run;

data tm;
	merge tm_djeca tm_odrasli;
run;

data tm;
	set tm;
	razlika_tm = abs(trimmean_djeca - trimmean_odrasli);
run;

proc means data = tm mean stderr lclm uclm p5 p95 alpha=0.1;
	var razlika_tm;
run;
