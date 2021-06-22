/*Izmjerena je kolièina šeæera (u %) sadržana u razlièitim brandovima žitarica za djecu i za odrasle. Podaci za 
  žitarice za djecu su zapisani u datasetu zitarice_za_djecu, a za odrasle u datasetu zitarice_za_odrasle.

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

a) Testirajte hipotezu H0: µD = µO (da je kolièina šeæera (varijabla SECER) u žitaricama za djecu
	jednaka kolièini šeæera u žitaricama za odrasle) nasuprot 
	H1: µD > µO (da je kolièina šeæera (varijabla SECER) u žitaricama za djecu veæa od kolièine
	šeæera u žitaricama za odrasle) na razini statistièke znaèajnosti ? = 0.01 primjenom t testa za
	2 nezavisna uzorka.

UPUTA: Koristite proceduru TTEST. Prvo testirajte hipotezu o jednakosti varijanci na razini
	statistièke znaèajnosti ?=0.10, te u zavisnosti od ishoda, primijenite ili „pooled“ ili
	„Satterthwaite“ test za hipotezu H0: µD = µO.
	Može li se hipoteza H0 odbaciti na razini statistièke znaèajnosti ? = 0.01?

b) Testitajte hipotezu H0: µD = µO nasuprot H1: µD > µO na razini statistièke znaèajnosti ? = 0.01
	primjenom bootstrap testa.
	UPUTA: Koristite metodu B bootstrap uzorkovanja za testiranje hipoteza. Primijenite
	slijedeæe vrijednosti makro varijabli SEED:

	%let seed=87654; *za grupu 1 (Djeca);
	%let seed= 58667; *za grupu 2 (Odrasli);

	Za svaku grupu izvedite po 1000 bootstrap ponavljanja (replikacija).
	Može li se hipoteza H0 odbaciti na razini statistièke znaèajnosti ? = 0.01?*/

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

/*** a ***/
/* Procedurom ttest testiramo H0 : mu_d = mu_o vs.  H1 : mu_d > mu_o */

proc ttest data = zitarice sides = u alpha=0.10;
	var secer;
	class starost;
run;

/*OPCENITO: "pooled" - ako su varijance jednake  VS   "satherweit" - ako su varijance razlicite
  DOBILI SMO:  
  --->                            	Equality of Variances

                         Folded F       0.0544 < alfa=0.10----> varijance su razlièite

  ---> varijance su razlièite

 					 Method           Variances        DF    t Value    Pr > |t|

                    Pooled           Equal            18       1.70      0.0052 
                    Satterthwaite    Unequal      17.474       1.70      0.0023 < alfa=0.01 ---> odbacujemo H0    
*/


/*** b ***/
/* Bootstrap testom testiramo H0 : mu_d = mu_o  vs.  H1 : mu_d > mu_o */
/* Može li se hipoteza H0 odbaciti na razini statistièke znaèajnosti ? = 0.01? */

data grupa1;
	set zitarice;
	where starost = 'd';
run;

data grupa2;
	set zitarice;
	where starost = 'o';
run;

/*** Prosjeène ocjene po grupama ***/

proc means data = zitarice nway noprint; /* stvarne vrijednosti */
	var secer;
  	class starost;
  	output out = out mean = mean;
run; 

data _NULL_;
	set out;
	if starost = 'd' then call symput("mean1", mean);
  	else call symput("mean2", mean);
run;

%put mean1=&mean1 mean2=&mean2;



/*** Centriranje (x-mean) unutar grupa ***/

data grupa1_centrirana;
	set grupa1;
   	secer = secer - &mean1;
run;

data grupa2_centrirana;
	set grupa2;
   	secer = secer - &mean2;
run;



/*** Neparametarski bootstrap ***/

%include "&path\Programs\jackboot1.sas";

/*** GRUPA 1 ***/

%let var = secer;
%let dat = grupa1_centrirana;
%let seed = 87654;
%let bsamples = 1000;
%let alpha = 0.01;

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

%let var = secer;
%let dat = grupa2_centrirana;
%let seed = 58667;
%let bsamples = 1000;
%let alpha = 0.01;

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


/*** TESTIRANJE***/

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

   	/*prob_left = (Welch_t < -abs(Welch_t_actual));*/
   	/*prob_right = (Welch_t > abs(Welch_t_actual));*/

	p_value = (Welch_t > abs(Welch_t_actual));

   	/*prob = sum(prob_left, prob_right);*/
run;

ods html;

proc means data = boot_Welch_t mean;
	/*var prob_left prob_right prob;*/
	var p_value;
run;

/* pv=0.018 > 0.01 -> ne odbacujemo H0*/

