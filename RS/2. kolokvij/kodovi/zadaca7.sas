*1.zadatak;

%let dat = sasuser.pills_efron;
%let nboot = 500;
%let n = 24;
%let seed =775566;

/** correlation on the original data **/

proc means data=&dat noprint;
	var x;
	output out = med median = median;
run;

data _NULL_;
 set med;
 call symput("med", median);
run;

%put &med;

%macro analyze(data=,out=);
	proc means noprint data = &data;
		var x;
		output out = &out (drop=_freq_ _type_) median = median;
         %bystmt;
      run;
%mend;

%include "/home/u47429085/jackboot.sas";

/* pomocu macro-a "jack" dobivamo jackknife procjenu medijana te standardne pogreske i pristranosti za medijan */
title 'Jackknife';
%jack(data = sasuser.pills_efron)
*Jackknife procjenu medijana=32, Jackknife procjena standardne pogreške=11.9896 i pristranosti za medijan=0;