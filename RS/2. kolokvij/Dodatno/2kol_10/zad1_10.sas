/*Sedam muškaraca i sedam žena je polazilo teèaj golfa. Na kraju teèaja je za svakog uèesnika
	odreðen broj bodova (varijabla SCORE). Podaci su zapisani u dataset SCORES:

data scores;
input Gender $ Score @@;
datalines;
f 75 f 76 f 80 f 77 f 80 f 77 f 73
m 76 m 80 m 85 m 85 m 78 m 87 m 82
;
run;

a) Testitajte hipotezu H0: µm = µf (da je broj bodova jednak bez obzira na spol) nasuprot
	H1: µm > µf (da je broj bodova muškaraca veæi od broja bodova žena) na razini statistièke
	znaèajnosti ? = 0.01 primjenom t testa za 2 nezavisna uzorka.

UPUTA: Koristite proceduru TTEST. Prvo testirajte hipotezu o jednakosti varijanci, te u
zavisnosti od ishoda, primijenite ili „pooled“ ili „Satterthwaite“ test za hipotezu H0: µm = µf.
Može li se hipoteza H0 odbaciti?

b) Testitajte hipotezu H0: µm = µf nasuprot H1: µm > µf na razini statistièke znaèajnosti ? = 0.01
primjenom bootstrap testa.

UPUTA: Koristite metodu B bootstrap uzorkovanja za testiranje hipoteza. Primijenite
slijedeæe vrijednosti makro varijabli SEED:
%let seed=633921; * za grupu 1 ( Gender=m );
%let seed= 47558; * za grupu 2 ( Gender=f );
Za svaku grupu izvedite 1000 bootstrap ponavljanja (replikacija).
Može li se hipoteza H0 odbaciti?*/

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


/* Procedurom ttest testiramo H0 : mu_f = mu_m  vs.  H1 : mu_f > mu_m */

proc ttest data = scores sides=u alpha=0.01;
	var score;
	class gender;
run;
/*OPCENITO: "pooled" - ako su varijance jednake  VS   "satherweit" - ako su varijance razlicite
  DOBILI SMO:  
  --->                            	Equality of Variances

                         Folded F       0.2806 > alfa=0.10----> varijance su jednake

  ---> varijance su jednake

 					 Method           Variances        DF    t Value    Pr > |t|

                    Pooled           Equal            18       1.70      0.9914 > alfa=0.01 ---> ne bacujemo H0
                       
*/


/* Bootstrap testom testiramo H0 : mu_f = mu_m  vs.  H1 : mu_f > mu_m */

data grupa1;
	set scores;
	where gender = 'm';
run;

data grupa2;
	set scores;
	where gender = 'f';
run;

/*** Prosjeène ocjene po grupama ***/

proc means data = scores nway noprint;
	var score;
  	class gender;
  	output out = out mean = mean;
run; 

data _NULL_;
	set out;
	if gender = 'm' then call symput("mean1", mean);
  	else call symput("mean2", mean);
run;

%put mean1=&mean1 mean2=&mean2;



/*** Centriranje (x-mean) unutar grupa ***/

data grupa1_centrirana;
	set grupa1;
   	score = score - &mean1;
run;

data grupa2_centrirana;
	set grupa2;
   	score = score - &mean2;
run;



/*** Neparametarski bootstrap ***/

%include "&path\Programs\jackboot1.sas";                                                                                                               



/*** GRUPA 1 ***/

%let var = score;
%let dat = grupa1_centrirana;
%let seed = 633921;
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

%let var = score;
%let dat = grupa2_centrirana;
%let seed = 47558;
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

proc means data = boot_Welch_t mean;
	/*var prob_left prob_right prob;*/
	var p_value;
run;

/* pv=0.012 > 0.01 -> ne odbacujemo H0*/

