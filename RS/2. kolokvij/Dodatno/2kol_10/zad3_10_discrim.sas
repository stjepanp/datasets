/*3. Provedite slijedeæu Monte Carlo (MC) studiju za ispitivanje pogrešaka krosvalidacije linearne
diskriminativne analize:

a) Generirajte ponavljano (nrep=500) bivarijatne uzorke za grupu 1 (dataset NORMAL1) velièine n=100 sa 
	slijedeæim karakteristikama:
		varijabla x slijedi normalnu distribuciju sa sredinom 0 i varijancom 1,
		varijabla y slijedi normalnu distribuciju sa sredinom 0 i varijancom 1,
		varijable x i y potièu iz populacije sa korelacijskim koeficijentom ?xy = 0.40

	Koristite slijedeæu vrijednost seed makro varijable:  %let seed=1235;

	Generirajte ponavljano (nrep=500) bivarijatne uzorke za grupu 2 ( dataset NORMAL2)
		velièine n=100 sa slijedeæim karakteristikama:
		varijabla x slijedi normalnu distribuciju sa sredinom 1 i varijancom 1,
		varijabla y slijedi normalnu distribuciju sa sredinom 1 i varijancom 1,
		varijable x i y potièu iz populacije sa korelacijskim koeficijentom ?xy = 0.40

	Koristite slijedeæu vrijednost seed makro varijable: %let seed=5786;

	Spojite datasetove NORMAL1 i NORMAL2, sortirajte po replikacijskoj varijabli, te za svako
	ponavljanje provedite linearnu diskriminativnu analizu te izraèunajte vrijednosti pogrešaka
	klasifikacije podataka metodom krosvalidacije u grupu 1 i u grupu 2.

	Izraèunajte MC prosjeène vrijednosti i standardne pogreške pogrešaka klasifikacije podataka
	metodom krosvalidacije u grupu 1 i u grupu 2.

b) Generirajte ponavljano (nrep=500) bivarijatne uzorke za grupu 1 (dataset NORMAL1)velièine n=100 sa 
	slijedeæim karakteristikama:
		varijabla x slijedi normalnu distribuciju sa sredinom 0 i varijancom 1,
		varijabla y slijedi normalnu distribuciju sa sredinom 0 i varijancom 1,
		varijable x i y potièu iz populacije sa korelacijskim koeficijentom ?xy = 0.40

	Koristite slijedeæu vrijednost seed makro varijable:%let seed=1235;

	Generirajte ponavljano (nrep=500) bivarijatne uzorke za grupu 2 ( dataset NORMAL2) velièine n=100 sa 
	slijedeæim karakteristikama:
		varijabla x slijedi normalnu distribuciju sa sredinom 2 i varijancom 1,
		varijabla y slijedi normalnu distribuciju sa sredinom 2 i varijancom 1,
		varijable x i y potièu iz populacije sa korelacijskim koeficijentom ?xy = 0.40

	Koristite slijedeæu vrijednost seed makro varijable:%let seed=5786;

	Spojite datasetove NORMAL1 i NORMAL2, sortirajte po replikacijskoj varijabli, te za svako
	ponavljanje provedite linearnu diskriminativnu analizu i izraèunajte vrijednosti pogrešaka
	klasifikacije podataka metodom krosvalidacije u grupu 1 i u grupu 2.

	Izraèunajte MC prosjeène vrijednosti i standardne pogreške pogrešaka klasifikacije podataka
	metodom krosvalidacije u grupu 1 i u grupu 2.*/

%let broj_replikacija = 500;
%let velicina_uzorka = 100;

/* Generiranje podataka za grupu 1 */

%let mean1x = 0;
%let sd1x = 1;
%let mean1y = 0;
%let sd1y = 1;
%let rho1xy = 0.4;
%let seed1 = 1235;


data normal1;
	call streaminit(&seed1);
	do rep = 1 to &broj_replikacija;
		do i = 1 to &velicina_uzorka;
			x1 = rand("normal");	
		  	x2 = rand("normal");	
		  	x = x1;
		  	y = &rho1xy * x1 + sqrt(1 - &rho1xy**2) * x2;
		  	/** transformacije u varijable sa mi1, mi2, s1, s2 ***/
		  	x = &sd1x * x + &mean1x;
		  	y = &sd1y * y + &mean1y;
			output;
		end;
 	end;
 	drop x1 x2;
run;


/* Generiranje podataka za grupu 2 */

%let mean2x = 1;
%let sd2x = 1;
%let mean2y = 1;
%let sd2y = 1;
%let rho2xy = 0.4;
%let seed2 = 5786;

data normal2;
	call streaminit(&seed2);
	do rep = 1 to &broj_replikacija;
		do i = 1 to &velicina_uzorka;
			x1 = rand("normal");	
		  	x2 = rand("normal");	
		  	x = x1;
		  	y = &rho2xy * x1 + sqrt(1 - &rho2xy**2) * x2;
		  	/** transformacije u varijable sa mi1,mi2, s1, s2 ***/
		  	x = &sd2x * x + &mean2x;
		  	y = &sd2y * y + &mean2y;
			output;
		end;
 	end;
 	drop x1 x2;
run;


data normal1;
	set normal1;
	grupa = 'g1';
run;

data normal2;
	set normal2;
	grupa = 'g2';
run;

data podaci;
	set normal1;
run;

proc append base = podaci data = normal2;
run;

proc sort data = podaci;
	by rep;
run;

data podaci;
	set podaci;
	id = _N_;
run;




/* Diskriminativna analiza */

ods html close;
ods html;
	

ods output ErrorCrossVal = ErrorCrossVal;
PROC DISCRIM DATA = podaci
	can ncan = 4 out = can CROSSVALIDATE CROSSLIST LIST ANOVA MANOVA;
	VAR x y;
	CLASS grupa;
	ID id;
	by rep;
	PRIORS PROP;
RUN;
QUIT;

ods html close;
ods html;

proc contents data = errorcrossval;
run;


data greske_CV;
	set errorcrossval;
	keep total;
	where type = 'Rate';
run;

proc means data = greske_cv mean std stderr;
	var total;
run;







/* b) dio zadatka */


/* Generiranje podataka za grupu 1 */

%let mean1x = 0;
%let sd1x = 1;
%let mean1y = 0;
%let sd1y = 1;
%let rho1xy = 0.4;
%let seed1 = 1235;


data normal1;
	call streaminit(&seed1);
	do rep = 1 to &broj_replikacija;
		do i = 1 to &velicina_uzorka;
			x1 = rand("normal");	
		  	x2 = rand("normal");	
		  	x = x1;
		  	y = &rho1xy * x1 + sqrt(1 - &rho1xy**2) * x2;
		  	/** transformacije u varijable sa mi1, mi2, s1, s2 ***/
		  	x = &sd1x * x + &mean1x;
		  	y = &sd1y * y + &mean1y;
			output;
		end;
 	end;
 	drop x1 x2;
run;


/* Generiranje podataka za grupu 2 */

%let mean2x = 2;
%let sd2x = 1;
%let mean2y = 2;
%let sd2y = 1;
%let rho2xy = 0.4;
%let seed2 = 5786;

data normal2;
	call streaminit(&seed2);
	do rep = 1 to &broj_replikacija;
		do i = 1 to &velicina_uzorka;
			x1 = rand("normal");	
		  	x2 = rand("normal");	
		  	x = x1;
		  	y = &rho2xy * x1 + sqrt(1 - &rho2xy**2) * x2;
		  	/** transformacije u varijable sa mi1,mi2, s1, s2 ***/
		  	x = &sd2x * x + &mean2x;
		  	y = &sd2y * y + &mean2y;
			output;
		end;
 	end;
 	drop x1 x2;
run;


data normal1;
	set normal1;
	grupa = 'g1';
run;

data normal2;
	set normal2;
	grupa = 'g2';
run;

data podaci;
	set normal1;
run;

proc append base = podaci data = normal2;
run;

proc sort data = podaci;
	by rep;
run;

data podaci;
	set podaci;
	id = _N_;
run;




/* Diskriminativna analiza */

ods html close;
ods html;
	

ods output ErrorCrossVal = ErrorCrossVal;
PROC DISCRIM DATA = podaci
	can ncan = 4 out = can CROSSVALIDATE CROSSLIST LIST ANOVA MANOVA;
	VAR x y;
	CLASS grupa;
	ID id;
	by rep;
	PRIORS PROP;
RUN;
QUIT;

ods html close;
ods html;

proc contents data = errorcrossval;
run;


data greske_CV;
	set errorcrossval;
	keep total;
	where type = 'Rate';
run;

proc means data = greske_cv mean std stderr;
	var total;
run;





