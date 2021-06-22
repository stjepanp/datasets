/*** zad2 ***/
/*Mihaela Poljak 1191213610*/

/* autoexec.sas */

	%let path=D:\FAKS\diplomski\4 semestar\RS\racunarska\Racunarska statistika;
	%let outputs=D:\FAKS\diplomski\4 semestar\RS\racunarska\Racunarska statistika\Outputs;
	%let lib=D:\FAKS\diplomski\4 semestar\RS\racunarska\Racunarska statistika\data;

	libname lib "&lib";
	libname library "&lib";

	filename odsout "&outputs";

	filename boot "&path\pictures\boot.gif";
	filename probplot "&outputs\probplot.gif";
	filename boxplot "&outputs\boxplot.gif";

/***********************************/

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
	id = _N_;
run;


/* Procedurom ttest testiramo H0 : mu_f = mu_m  vs.  H1 : mu_f razlicito mu_m */

proc ttest data = lijekovi alpha=0.05;
	var efikasnost;
	class lijek;
run;
/*OPCENITO: "pooled" - ako su varijance jednake  VS   "satherweit" - ako su varijance razlicite
  DOBILI SMO:  
  --->                            	Equality of Variances

                         Folded F       0.0187 < alfa=0.05----> varijance nisu jednake

  ---> varijance su nisu jednake

 					 Method           

                    Satterthwaite        0.0282 < alfa=0.05 ---> obacujemo H0
                       
*/


/* Bootstrap testom testiramo H0 : mu_f = mu_m  vs.  H1 : mu_f razlièito mu_m */

data grupa1;
	set lijekovi;
	where lijek = 'A';
run;

data grupa2;
	set lijekovi;
	where lijek = 'B';
run;

/*** Prosjeène ocjene po grupama ***/

proc means data = lijekovi nway noprint;
	var efikasnost;
  	class lijek;
  	output out = out mean = mean;
run; 

data _NULL_;
	set out;
	if lijek = 'A' then call symput("mean1", mean);
  	else call symput("mean2", mean);
run;

%put mean1=&mean1 mean2=&mean2;



/*** Centriranje (x-mean) unutar grupa ***/

data grupa1_centrirana;
	set grupa1;
   	efikasnost = efikasnost - &mean1;
run;

data grupa2_centrirana;
	set grupa2;
   	efikasnost = efikasnost - &mean2;
run;



/*** Neparametarski bootstrap ***/

%include "&path\Programs\jackboot1.sas";                                                                                                               



/*** GRUPA 1 ***/

%let var = efikasnost;
%let dat = grupa1_centrirana;
%let seed = 486227;
%let bsamples = 1000;
%let alpha = 0.05;

%macro analyze(data =, out =);
	proc means data=&data noprint nway;
   		var &var;                                                                        
   		output out=&out(drop=_freq_ _type_) mean=mean1 stderr=stde1;
   		%bystmt;
	run;
%mend;
  
%boot(data = &dat, random = &seed, stat = mean1, samples = &bsamples)

data boot_grupa1;
	set bootdist;
run;
 
proc means data = grupa1 noprint  nway;
	var &var;
   	output out = actual_grupa1(drop = _freq_ _type_) mean = mean1 stderr = stde1;
run;


/*** GRUPA 2 ***/

%let var = efikasnost;
%let dat = grupa2_centrirana;
%let seed = 947123;
%let bsamples = 1000;
%let alpha = 0.05;

%macro analyze(data =, out =);
	proc means data=&data noprint nway;
   		var &var;                                                                        
   		output out=&out(drop=_freq_ _type_) mean=mean2 stderr=stde2;
   		%bystmt;
	run;
%mend;
  
%boot(data = &dat, random = &seed, stat = mean2, samples = &bsamples)

data boot_grupa2;
	set bootdist;
run;
 
proc means data = grupa2 noprint  nway;
	var &var;
   	output out = actual_grupa2(drop = _freq_ _type_) mean = mean2 stderr = stde2;
run;



data actual_Welch_t;
	merge actual_grupa1 actual_grupa2;
  	Welch_t_actual = (mean1 - mean2) / sqrt(stde1**2 + stde2**2);
  	keep Welch_t_actual;
run;


data boot_Welch_t;
	merge  boot_grupa1 boot_grupa2;
   	by _sample_;
   	if _N_ = 1 then set actual_Welch_t;
   	Welch_t = (mean1 - mean2) / sqrt(stde1**2 + stde2**2);

   	prob_left = (Welch_t < -abs(Welch_t_actual));
   	prob_right = (Welch_t > abs(Welch_t_actual));

   	prob = sum(prob_left, prob_right);
run;

proc means data = boot_Welch_t ;
	var  prob;	
	output out=pv mean=mean;  /* REZULTATI: datoteka PV */
run;

proc means data = boot_Welch_t;
	var Welch_t;
	output out=intervali_mean mean=mean p5=p5 p95=p95;    /* REZULTATI: datoteka INTERVALI_MEAN */
run;

/* pv=0.068 > 0.01 -> ne odbacujemo H0*/


