/*** zad3 ***/
/*Mihaela Poljak 1191213610*/

ods html close;

data lijekovi;
input lijek $ efikasnost @@;
datalines;
A 4.4 A 4.8 A 3.8 A 4.2 A 5.1 A 4.6 A 4.0 A 2.1
B 5.2 B 4.7 B 5.5 B 4.9 B 5.0 B 4.9
;
run;

data lijekovi;
set lijekovi; 
run;

data grupa1;
	set lijekovi;
	where lijek = 'A';
run;


data grupa2;
	set lijekovi;
	where lijek = 'B';
run;


proc means data = lijekovi nway noprint;
	var efikasnost;
  	class lijek;
  	output out = out mean = mean std=std stderr= stderr;
run; 


data _NULL_;
	set out;
	if lijek = 'A' then call symput("mean1", mean);
  	else call symput("mean2", mean);
	if lijek = 'A' then call symput("std1", std);
  	else call symput("std2", std);
	if lijek = 'A' then call symput("stderr1", stderr);
  	else call symput("stderr2", stderr);
run;

%put mean1 = &mean1 std1 = &std1 mean2 = &mean2 std2 = &std2;
%put stderr1=&stderr1 stderr2=&stderr2;

/*** a ***/

%let broj_uzoraka = 1000;
%let duljina_uzorka = 8;
%let seed = 123456;


data param_boot_grupa1;
	call streaminit(&seed);
	do i = 1 to &broj_uzoraka;
		do j = 1 to &duljina_uzorka;
			x = &std1 * rand("normal") + &mean1;
			output;
		end;
	end;
run;


proc means data = param_boot_grupa1 nway noprint;
	var x;
	by i;
	output out = mean_grupa1 mean = mean_grupa1 stderr=stde_grupa1;
run;


%let broj_uzoraka = 1000;
%let duljina_uzorka = 6;
%let seed = 987654;


data param_boot_grupa2;
	call streaminit(&seed);
	do i = 1 to &broj_uzoraka;
		do j = 1 to &duljina_uzorka;
			x = &std2 * rand("normal") + &mean2;
			output;
		end;
	end;
run;


proc means data = param_boot_grupa2 nway noprint;
	var x;
	by i;
	output out = mean_grupa2 mean = mean_grupa2 stderr=stde_grupa2;
run;


data sve_skupa;
	merge mean_grupa1 mean_grupa2;
	t=(mean_grupa1-mean_grupa2)/sqrt(stde_grupa1**2+stde_grupa2**2);
run;


proc means data = sve_skupa;
	var t;
	output out=rez1 mean=mean stderr=stderr p5=p5 p95=p95; /*REZULTATI: datoteka REZ1 */
run;



/*** b ***/


%let broj_uzoraka = 1000;
%let duljina_uzorka = 8;
%let seed = 123456;


data param_boot_grupa1;
	call streaminit(&seed);
	do i = 1 to &broj_uzoraka;
		do j = 1 to &duljina_uzorka;
			x = &std1 * rand("normal") + &mean1;
			output;
		end;
	end;
run;


proc means data = param_boot_grupa1 nway noprint;
	var x;
	by i;
	output out = median_grupa1 median = median_grupa1;
run;


%let broj_uzoraka = 1000;
%let duljina_uzorka = 6;
%let seed = 987654;


data param_boot_grupa2;
	call streaminit(&seed);
	do i = 1 to &broj_uzoraka;
		do j = 1 to &duljina_uzorka;
			x = &std2 * rand("normal") + &mean2;
			output;
		end;
	end;
run;


proc means data = param_boot_grupa2 nway noprint;
	var x;
	by i;
	output out = median_grupa2 median = median_grupa2;
run;


data sve_skupa;
	merge median_grupa1 median_grupa2;
	aps_raz_median = abs(median_grupa2 - median_grupa1);
run;


proc means data = sve_skupa;
	var aps_raz_median;
	output out=rez2 mean=mean stderr=stderr p5=p5 p95=p95; /*REZULTATI: datoteka REZ2 */
run;




