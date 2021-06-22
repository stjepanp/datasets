/*Monte Carlo metodom procijenite slijedeæi odreðeni integral:
                   I=int[0,1] x^2.5 exp{-x/4}
Koristite slijedeæe Importance Sampling (IS) funkcije (tj. funkcije distribucija):
a) Uniformnu na intervalu 0,1 (U(0,1))
b) Beta na intervalu (0,1), uz vrijednost parametara a=2.5, b=1
c) Beta na intervalu (0,1), uz vrijednost parametara a=2, b=2
d) Triangularnu na intervalu (0,1), uz vrijednost parametra h=0.5
e) Triangularnu na intervalu (0,1), uz vrijednost parametra h=0.9
f) Gamma distribuciju („truncated“) na intervalu (0,1), uz vrijednost parametra a=2

Za svaku importance sampling funkciju generirajte 10000 replikacija/ponavljanja.

Koristite SEED= 4772565 (za sve importance sampling funkcije).

U svakom ponavljanju izraèunajte kumulativni prosjek i kumulativnu standardnu pogrešku.
Odredite procjene standardne pogreške za metode a)-f) nakon 10000 replikacija, te
efikasnosti metoda b)-f) u odnosu na a).

UPUTA ZA f: Kod generiranja po gamma distribuciji zadržavaju se samo one generirane
vrijednosti koje su u intervalu (0,1). Funkciju gustoæe vjerojatnosti takve „truncated“ gamma
distribucije na (0,1) intervalu potrebno je prilagoditi kako bi zadovoljavala svojstva funkcije
gustoæe vjerojatnosti. Koristite funkciju PROBGAM.*/


%let seed=4772565;
%let nrep=10000;

/*** (a) ***/
data uniformna;
 call streaminit(&seed);
  do rep=1 to &nrep;
   xpom=rand('uniform');;
   x_unif=xpom**2.5 * exp(-xpom / 4); /*funkcija pod intefralom*/

   sum_unif + x_unif; /* suma=suma+x */
   sumk_unif + x_unif**2;
   prosjek_unif=sum_unif/rep; 
   stderr_unif = sqrt((sumk_unif - rep * prosjek_unif**2) / rep) / sqrt(rep);
  output;
 end;
run;

/*** Beta(2.5,1) na intervalu (0,1) - x2=x^x/fi(x) , f= 1/B(a,b) * xpom^(a-1)* (1-xpom)^(b-1)  ***/
data beta;
 call streaminit(&seed);
  do rep=1 to &nrep;
   xpom=rand('beta',2.5,1);
   x_beta=xpom**2.5 * exp(-xpom / 4) / ( ( 1/beta(2.5,1) )*  (xpom**1.5)); 

   sum_beta + x_beta; /* suma=suma+x */
   sumk_beta + x_beta**2;
   prosjek_beta=sum_beta/rep; 
   stderr_beta = sqrt((sumk_beta - rep * prosjek_beta**2) / rep) / sqrt(rep);
  output;
 end;
run;

/*** Beta(2,2) na intervalu (0,1) - x2=x^x/fi(x) , f= 1/B(a,b) * xpom^(a-1)* (1-xpom)^(b-1) ***/
data beta1;
 call streaminit(&seed);
  do rep=1 to &nrep;
   xpom=rand('beta',2,2);
   x_beta1=xpom**2.5 * exp(-xpom / 4) / ( ( 1/beta(2,2) ) *  xpom * (1-xpom) ); 

   sum_beta1 + x_beta1; /* suma=suma+x */
   sumk_beta1 + x_beta1**2;
   prosjek_beta1=sum_beta1/rep; 
   stderr_beta1 = sqrt((sumk_beta1 - rep * prosjek_beta1**2) / rep) / sqrt(rep);
  output;
 end;
run;

/***  Triangularnu na intervalu (0,1), uz vrijednost parametra h=0.5 ***/
/* f= 2(x-a) / (b-a)(c-a) ako je x < c=h=0.5
      2(b-x) / (b-a)(b-c) ako je x > c=h=0.5 */

data triangularna;
	call streaminit(&seed);
	do rep=1 to &nrep;
    xpom = rand("triangle", 0.5);
	if xpom < 0.5 then
		x_triang = (xpom**2.5 * exp(-xpom / 4))  / (4 * xpom);
	else
		x_triang = (xpom**2.5 * exp(-xpom / 4))  / (4 - 4 * xpom);

   sum_triang + x_triang; /* suma=suma+x */
   sumk_triang + x_triang**2;
   prosjek_triang=sum_triang/rep; 
   stderr_triang = sqrt((sumk_triang - rep * prosjek_triang**2) / rep) / sqrt(rep);
  output;
end;
run;

/***  Triangularnu na intervalu (0,1), uz vrijednost parametra h=0.9 ***/
/* f= 2(x-a) / (b-a)(c-a) ako je x < c=h=0.9
      2(b-x) / (b-a)(b-c) ako je x > c=h=0.9 */

data triangularna1;
	call streaminit(&seed);
	do rep=1 to &nrep;
    xpom = rand("triangle", 0.9);
	if xpom < 0.9 then
		x_triang1 = (xpom**2.5 * exp(-xpom / 4))  / ((2 * xpom)/0.9);
	else
		x_triang1 = (xpom**2.5 * exp(-xpom / 4))  / ((2-2*xpom)/0.1);

   sum_triang1 + x_triang1; /* suma=suma+x */
   sumk_triang1 + x_triang1**2;
   prosjek_triang1=sum_triang1/rep; 
   stderr_triang1 = sqrt((sumk_triang1 - rep * prosjek_triang1**2) / rep) / sqrt(rep);
  output;
end;
run;


/* Truncated gamma razdioba na [0, 1] s parametrima alpha = 2, beta = 1 */ 
data gamma_trunc1; 
	call streaminit(&seed); 
    do rep = 1 to &nrep; 
		pom = gaminv((rand("uniform") * probgam(1, 2)), 2); 
		x_gamma1 = (pom**2.5 * exp(-pom / 4)) / ((pom * exp(-pom)) / (probgam(1, 2))); 
    
   sum_gamma1 + x_gamma1; /* suma=suma+x */
   sumk_gamma1 + x_gamma1**2;
   prosjek_gamma1=sum_gamma1/rep; 
   stderr_gamma1 = sqrt((sumk_gamma1 - rep * prosjek_gamma1**2) / rep) / sqrt(rep);
  output;
end;
run;


/* Radi lakseg crtanja u isti dataset stavimo kumulativne prosjeke*/
data K_PROSJECI; 
 merge uniformna beta beta1 triangularna triangularna1 gamma_trunc1;
 keep rep prosjek_unif prosjek_beta prosjek_beta1 prosjek_triang prosjek_triang1  prosjek_gamma1;
run;

/* Radi lakseg crtanja u isti dataset stavimo greske*/
data GRESKE; 
 merge uniformna beta beta1 triangularna triangularna1  gamma_trunc1;
 keep rep stderr_unif stderr_beta stderr_beta1 stderr_triang stderr_triang1 stderr_gamma1;
run;


/*** crtamo prosjeke ***/
goptions reset=all ftext=swiss cback=grey ctext=white;
title '4 krivulje kumulativnih prosjeka  (na y osi), sa brojem replikacija na x osi ';

symbol1 interpol=line color=red;
symbol2 interpol=line color=blue;
symbol3 interpol=line color=orange;
symbol4 interpol=line color=green;
symbol5 interpol=line color=magenta;
symbol6 interpol=line color=black;

legend label=none value=(font=swiss color=black 'Uniformna'  'Beta' 'Beta1' 'Triangularna' 'Triangularna1' 'Gamma-trunced')
       position=(top right inside) mode=share  cborder=black;

axis1 label=('REPLIKACIJE')
      offset=(5)
      width=3;

axis2 label=('PROSJECI')
      width=3;

proc gplot data=K_PROSJECI;
	plot prosjek_unif*rep prosjek_beta*rep prosjek_beta1*rep prosjek_triang*rep prosjek_triang1*rep prosjek_gamma1*rep/overlay 
                                                                                    haxis=axis1
                                                                                    vaxis=axis2
                                                                     				frame
                                                                     				legend=legend1;
run;



/*** crtamo greske ***/
goptions reset=all ftext=swiss cback=grey ctext=white;
title 'Nacrtajte sve 4 krivulje kumulativnih standardnih pogresaka (na y osi), sa brojem replikacija na x osi';

symbol1 interpol=line color=red;
symbol2 interpol=line color=blue;
symbol3 interpol=line color=orange;
symbol4 interpol=line color=green;
symbol5 interpol=line color=magenta;
symbol6 interpol=line color=black;

legend label=none value=(font=swiss color=black 'Uniformna'  'Beta' 'Beta1' 'Triangularna' 'Triangularna1' 'Gamma-trunced')
       position=(top right inside) mode=share cborder=black;

axis1 label=('REPLIKACIJE')
      offset=(2)
      width=3;

axis2 label=('POGRESKE')
      width=3;

proc gplot data=GRESKE;
	plot stderr_unif*rep stderr_beta*rep stderr_beta1*rep stderr_triang*rep stderr_triang1*rep stderr_gamma1*rep/overlay 
                                                                     		   haxis=axis1
                                                                     		   vaxis=axis2
                                                                     		   frame
                                                                     		   legend=legend1;
run;


data pogreske;
	set greske;
	keep stderr_unif stderr_beta stderr_beta1 stderr_triang stderr_beta stderr_triang1  stderr_gamma1 ;
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
	var_gamma1    = (stderr_gamma1 * sqrt(&nrep - 1))**2;
	
run;


data efikasnost;
	set efikasnost;
	efikasnost_beta = var_unif / var_beta;
	efikasnost_beta1 = var_unif / var_beta1;
	efikasnost_triang = var_unif / var_triang;
	efikasnost_triang1 = var_unif / var_triang1;
	efikasnost_gamma1 = var_unif / var_gamma1;

	keep  efikasnost_beta efikasnost_beta1 efikasnost_triang efikasnost_triang1 efikasnost_gamma1;
run;

title 'Efikasnost pojedinih metoda u odnosu na uniformnu metodu';
proc print data = efikasnost;
run;

