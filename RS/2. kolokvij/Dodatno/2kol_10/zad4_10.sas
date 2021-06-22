/*4. Koristite podatke iz zadatka 1.

a) Parametarskom bootstrap metodom odredite bootstrap standardnu pogrešku i 90% bootstrap
	interval pouzdanosti za apsolutnu vrijednost razlike izmeðu prosjeènih vrijednosti broja bodova za
	muškarce i prosjeènih vrijednosti broja bodova za žene.

a) Parametarskom bootstrap metodom odredite bootstrap standardnu pogrešku i 90% bootstrap
	interval pouzdanosti za apsolutnu vrijednost razlike izmeðu medijana broja bodova za muškarce i
	medijana broja bodova za žene.

U a) i b):
Primijenite slijedeæe vrijednosti makro varijabli SEED:
%let seed=633921; * za grupu 1 ( Gender=m );
%let seed= 47558; * za grupu 2 ( Gender=f );

Pretpostavite da varijabla SCORE slijedi normalnu distribuciju.
Za svaku grupu izvedite 1000 bootstrap ponavljanja (replikacija).*/

data scores;
	input Gender $ Score @@;
	datalines;
	f 75 f 76 f 80 f 77 f 80 f 77 f 73
	m 76 m 80 m 85 m 85 m 78 m 87 m 82
	; 
run;

data scores;
	set scores;
	id = _N_;
run;

data grupa1;
	set scores;
	where gender = 'm';
run;

data grupa2;
	set scores;
	where gender = 'f';
run;


proc means data = scores nway noprint;
	var score;
  	class gender;
  	output out = out mean = mean std = std median = median;
run; 

data _NULL_;
	set out;
	if gender = 'm' then call symput("mean1", mean);
  	else call symput("mean2", mean);
	if gender = 'm' then call symput("std1", std);
  	else call symput("std2", std);
	if gender = 'm' then call symput("median1", median);
  	else call symput("median2", median);
run;

%put mean1 = &mean1 std1 = &std1 median1 = &median1 mean2 = &mean2 std2 = &std2 median2 = &median2;


/* generiranje: parametarski bootstrap za muskarce */
%let broj_uzoraka = 1000;
%let duljina_uzorka = 7;
%let seed = 633921;

data param_boot_male;
	call streaminit(&seed);
	do i = 1 to &broj_uzoraka;
		do j = 1 to &duljina_uzorka;
			x = &std1 * rand("normal") + &mean1;
			output;
		end;
	end;
run;

proc means data = param_boot_male nway noprint;
	var x;
	by i;
	output out = mean_and_median_male mean = mean_male median = median_male;
run;


/*generiranje: parametarski bootstrap za zene */
%let broj_uzoraka = 1000;
%let duljina_uzorka = 7;
%let seed = 47558;

data param_boot_female;
	call streaminit(&seed);
	do i = 1 to &broj_uzoraka;
		do j = 1 to &duljina_uzorka;
			x = &std2 * rand("normal") + &mean2;
			output;
		end;
	end;
run;

proc means data = param_boot_female nway noprint;
	var x;
	by i;
	output out = mean_and_median_female mean = mean_female median = median_female;
run;


data sve_skupa;
	merge mean_and_median_male mean_and_median_female;
	aps_raz_mean = abs(mean_male - mean_female);
	aps_raz_med = abs(median_male - median_female);
run;

proc means data = sve_skupa mean stderr lclm uclm p5 p95 alpha=0.1;
	var aps_raz_mean aps_raz_med;
run;




