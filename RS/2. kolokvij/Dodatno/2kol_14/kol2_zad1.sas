/*** zad1 ***/
/*Mihaela Poljak 1191213610*/
ods html close;

%let seed=489123;
%let nrep=10000;

/*** (a) ***/
data uniformna;
 call streaminit(&seed);
  do rep=1 to &nrep;
   xpom=rand('uniform');;
   x_unif=(2*log(1+xpom))/xpom; /*funkcija pod intefralom*/

   sum_unif + x_unif; /* suma=suma+x */
   sumk_unif + x_unif**2;
   prosjek_unif=sum_unif/rep; 
   stderr_unif = sqrt((sumk_unif - rep * prosjek_unif**2) / rep) / sqrt(rep);
  output;
 end;
run;

/*** Beta(1,2) na intervalu (0,1) , f= 1/B(a,b) * xpom^(a-1)* (1-xpom)^(b-1)  ***/
data beta;
 call streaminit(&seed);
  do rep=1 to &nrep;
   xpom=rand('beta',1,2);
   x_beta=( (2*log(1+xpom))/xpom ) / ( ( 1/beta(1,2) )*  (1-xpom) ); 

   sum_beta + x_beta; /* suma=suma+x */
   sumk_beta + x_beta**2;
   prosjek_beta=sum_beta/rep; 
   stderr_beta = sqrt((sumk_beta - rep * prosjek_beta**2) / rep) / sqrt(rep);
  output;
 end;
run;

/*** Beta(1,1.5) na intervalu (0,1) - x2=podintegralna/fi(x) , f= 1/B(a,b) * xpom^(a-1)* (1-xpom)^(b-1) ***/
data beta1;
 call streaminit(&seed);
  do rep=1 to &nrep;
   xpom=rand('beta',1,1.5);
   x_beta1=( (2*log(1+xpom))/xpom ) / ( ( 1/beta(1,1.5) ) * ((1-xpom)**0.5) ); 

   sum_beta1 + x_beta1; /* suma=suma+x */
   sumk_beta1 + x_beta1**2;
   prosjek_beta1=sum_beta1/rep; 
   stderr_beta1 = sqrt((sumk_beta1 - rep * prosjek_beta1**2) / rep) / sqrt(rep);
  output;
 end;
run;

/***  Triangularnu na intervalu (0,1), uz vrijednost parametra h=0.01 ***/
/* f= 2(x-a) / (b-a)(c-a) ako je x < c=h=0.01
      2(b-x) / (b-a)(b-c) ako je x > c=h=0.01 */

data triangularna;
	call streaminit(&seed);
	do rep=1 to &nrep;
    xpom = rand("triangle", 0.01);
	if xpom < 0.01 then
		x_triang = ( (2*log(1+xpom))/xpom ) / ((2 * xpom) / 0.01);
	else
		x_triang = ( (2*log(1+xpom))/xpom ) / ((2 - 2 * xpom)/(1-0.01));

   sum_triang + x_triang; /* suma=suma+x */
   sumk_triang + x_triang**2;
   prosjek_triang=sum_triang/rep; 
   stderr_triang = sqrt((sumk_triang - rep * prosjek_triang**2) / rep) / sqrt(rep);
  output;
end;
run;

/***  Triangularnu na intervalu (0,1), uz vrijednost parametra h=0.001 ***/
/* f= 2(x-a) / (b-a)(c-a) ako je x < c=h=0.001
      2(b-x) / (b-a)(b-c) ako je x > c=h=0.001 */

data triangularna1;
	call streaminit(&seed);
	do rep=1 to &nrep;
    xpom = rand("triangle", 0.001);
	if xpom < 0.001 then
		x_triang1 = ( (2*log(1+xpom))/xpom ) / ((2 * xpom) / 0.001);
	else
		x_triang1 = ( (2*log(1+xpom))/xpom ) / ((2 - 2 * xpom)/(1-0.001));

   sum_triang1 + x_triang1; /* suma=suma+x */
   sumk_triang1 + x_triang1**2;
   prosjek_triang1=sum_triang1/rep; 
   stderr_triang1 = sqrt((sumk_triang1 - rep * prosjek_triang1**2) / rep) / sqrt(rep);
  output;
end;
run;

/***  Podrezanu („truncated“) normalnu distribuciju sa vrijednostima parametara µ=0,
sigma=1.5 (N(0,1.5^2)) , podrezanu („truncated“) na intervalu (0,1). ***/

%let mu = 0; 
%let sigma = 1.5;

data normal_trunc1; 
	call streaminit(&seed); 
	do rep = 1 to &nrep; 
	pom = &sigma * probit(rand("uniform") * (probnorm((1 - &mu) / &sigma) - probnorm(-&mu / &sigma)) + probnorm(-&mu / &sigma)) + &mu; 
	x_normal_trunc1 = ( (2*log(1+pom))/pom )  / ((pdf('normal', pom, &mu, &sigma)) / (probnorm((1 - &mu) / &sigma) - probnorm(-&mu / &sigma))); 

	sum_normal_trunc1 + x_normal_trunc1;
	sumkv_normal_trunc1 + x_normal_trunc1**2;
	prosjek_normal_trunc1 = sum_normal_trunc1 / rep;
	stderr_normal_trunc1 = sqrt((sumkv_normal_trunc1 - rep * prosjek_normal_trunc1**2) / rep) / sqrt(rep);
output; 
end;  
run;



/* Radi lakseg crtanja u isti dataset stavimo kumulativne prosjeke*/
data K_PROSJECI; 
 merge uniformna beta beta1 triangularna triangularna1 normal_trunc1;
 keep rep prosjek_unif prosjek_beta prosjek_beta1 prosjek_triang prosjek_triang1 prosjek_normal_trunc1;
run;

/* Radi lakseg crtanja u isti dataset stavimo greske*/
data GRESKE; 
 merge uniformna beta beta1 triangularna triangularna1 normal_trunc1;
 keep rep stderr_unif stderr_beta stderr_beta1 stderr_triang stderr_triang1 stderr_normal_trunc1;
run;


data pogreske;
	set greske;
	keep stderr_unif stderr_beta stderr_beta1 stderr_triang stderr_beta stderr_triang1 stderr_normal_trunc1;
	where rep = &nrep;
run;

title 'Procjena standardnih pogreska nakon 10000 replikacija';
proc print data = pogreske;
run;

/*** EFIKASNOSTI ***/
data efikasnost;
	set pogreske;
	var_unif    = (stderr_unif * sqrt(&nrep - 1))**2;
	var_beta    = (stderr_beta * sqrt(&nrep - 1))**2;
	var_beta1   = (stderr_beta1 * sqrt(&nrep - 1))**2;
	var_triang  = (stderr_triang * sqrt(&nrep - 1))**2;
	var_triang1 = (stderr_triang1 * sqrt(&nrep - 1))**2;
	var_normal_trunc1=(stderr_normal_trunc1 * sqrt(&nrep - 1))**2;
run;

data efikasnost;
	set efikasnost;
	efikasnost_beta = var_unif / var_beta;
	efikasnost_beta1 = var_unif / var_beta1;
	efikasnost_triang = var_unif / var_triang;
	efikasnost_triang1 = var_unif / var_triang1;
	efikasnost_normal_trunc1 = var_unif / var_normal_trunc1;

	keep  efikasnost_beta efikasnost_beta1 efikasnost_triang efikasnost_triang1 efikasnost_normal_trunc1;
run;

title 'Efikasnost pojedinih metoda u odnosu na uniformnu metodu';
proc print data = efikasnost;
run;
