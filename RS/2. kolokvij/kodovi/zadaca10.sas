/*1. zadatak*/
%LET SEED = 575867; /** sami odredimo ili je zadano  **/
%LET NREP = 10000; /** 10000 replikacija/ponavljanja **/

/** a) dio zadatka - uniformna na intervalu (0,1) **/
DATA UNIFORM;
 DO REP = 1 TO &NREP;
  X = UNIFORM (&SEED);
  suma_u+x; /** kumulira se, kao da pise suma=suma+x **/
  prosjek_u = suma_u/rep; /** kumulativni prosjeci **/
  sumakv_u+x**2; /** suma kvadrata **/
  std_u = sqrt((sumakv_u-rep*prosjek_u**2)/(rep-1)); /** std(x1-xN), N=1,..,10000 **/
  greska_u = std_u/sqrt(rep); /** kumulativne standardne pogreske, SE=std(X)/sqrt(N) **/
  OUTPUT;
 END;
RUN;


/** b) dio zadatka - triangularna na intervalu (0,1) **/
/** prvo moramo definirati min, max i mod **/
%let min_t = 0;
%let max_t = 1;
%let mod_t = 0.5; /** simetricna triangularna distribucija pa je mod=(a+b)/2 **/

DATA TRIAG;
DO REP = 1 TO &NREP;
  x = (&max_t-&min_t)*rantri(&seed,(&mod_t-&min_t)/(&max_t-&min_t))+&min_t;
if (x le &mod_t) then
pom1=x*(&max_t-&min_t)*(&mod_t-&min_t)/(2*(x-&min_t));

else
pom1=x*(&max_t-&min_t)*(&max_t-&mod_t)/(2*(&max_t-x));

suma_t+pom1;
  prosjek_t = suma_t/rep;
  sumakv_t+pom1**2;
  std_t = sqrt((sumakv_t-rep*prosjek_t**2)/(rep-1));
  greska_t = std_t/sqrt(rep);
  OUTPUT;
 END;
RUN;


/** c) dio zadatka - kontaminirana beta distribucija gdje je fi(x)=2*(1-alfa)*x+alfa , alfa=0.01 **/
%let alfa1 = 0.01;



DATA CB1;
DO REP = 1 TO &NREP;
bern = rand ('BERN', 1-&alfa1);
if bern=1 then
  X = rand('beta',2,1);  /** X~beta(2,1) **/
else
  X=uniform(&seed); /*X~U(0,1) s vjerojatnoscu alfa1*/
 
 
  pom1 = X/(2*(1-&alfa1)*X+&alfa1); /** pom=x/fi(x) **/
  suma_cb1+pom1;
  prosjek_cb1 = suma_cb1/rep;
  sumakv_cb1+pom1**2;
  std_cb1 = sqrt((sumakv_cb1-rep*prosjek_cb1**2)/(rep-1));
  greska_cb1 = std_cb1/sqrt(rep);
  OUTPUT;
 END;
RUN;


/** d) dio zadatka - kontaminirana beta distribucija gdje je fi(x)=2*(1-alfa)*x+alfa , alfa=0.05 **/
%let alfa2 = 0.05;

DATA CB2;
DO REP = 1 TO &NREP;
 bern = rand ('BERN', 1-&alfa2);
if bern=1 then
  x = rand('beta',2,1);  /** X~beta(2,1) **/
else
  x=uniform(&seed); /*X~U(0,1) s vjerojatnoscu alfa1*/
 
 
  pom2 = x/(2*(1-&alfa2)*x+&alfa2);
  suma_cb2+pom2;
  prosjek_cb2 = suma_cb2/rep;
  sumakv_cb2+pom2**2;
  std_cb2 = sqrt((sumakv_cb2-rep*prosjek_cb2**2)/(rep-1));
  greska_cb2 = std_cb2/sqrt(rep);
  OUTPUT;
 END;
RUN;


/** stavljamo ono sto nam treba u isti dataset
	=> zelimo prikazati na istom grafu kumulativne sredine, i isto tako pogreske **/
DATA ZAJEDNO;
MERGE UNIFORM TRIAG CB1 CB2;
KEEP REP prosjek_u greska_u prosjek_t greska_t prosjek_cb1 greska_cb1 prosjek_cb2 greska_cb2;
RUN;

/* stavimo kumulativne prosjeke u isti dataset */
data KUM_PROSJECI; 
 merge uniform triag cb1 cb2;
 keep rep prosjek_u prosjek_t prosjek_cb1 prosjek_cb2;
run;

/* stavimo greske u isti dataset*/
data GRESKE; 
 merge uniform triag cb1 cb2;
 keep rep greska_u greska_t greska_cb1 greska_cb2;
run;

/* crtamo prosjeke */
goptions reset=all ftext=swiss cback=grey ctext=white;
title 'krivulje kumulativnih prosjeka  (na y osi), sa brojem replikacija na x osi ';

symbol1 interpol=line color=blue;
symbol2 interpol=line color=orange;
symbol3 interpol=line color=green;
symbol4 interpol=line color=red;

legend label=none value=(font=swiss color=black 'Uniformna' 'Triangularna' 'Beta1' 'Beta2')
       position=(top right inside) mode=share cborder=black;

axis1 label=('REPLIKACIJE')
      order=(0 to 10000 by 1000)
      offset=(2)
      width=3;

axis2 label=('PROSJECI')
      order=(0.47 to 0.70 by 0.01)
      width=3;

proc gplot data=KUM_PROSJECI;
	plot prosjek_u*rep prosjek_t*rep prosjek_cb1*rep prosjek_cb2*rep/overlay 
                                                                     haxis=axis1
                                                                     vaxis=axis2
                                                                     frame
                                                                     legend=legend1;
run;


/* crtamo greske */
goptions reset=all ftext=swiss cback=grey ctext=white;
title 'krivulje kumulativnih standardnih pogresaka (na y osi), sa brojem replikacija na x osi';

symbol1 interpol=line color=blue;
symbol2 interpol=line color=orange;
symbol3 interpol=line color=green;
symbol4 interpol=line color=red;

legend label=none value=(font=swiss color=black 'Uniformna' 'Triangularna' 'Beta1' 'Beta2')
       position=(top right inside) mode=share cborder=black;

axis1 label=('REPLIKACIJE')
      order=(0 to 10000 by 1000)
      offset=(2)
      width=3;

axis2 label=('POGRESKE')
      width=3;

proc gplot data=GRESKE;
	plot greska_u*rep greska_t*rep greska_cb1*rep greska_cb2*rep/overlay 
                                                                     haxis=axis1
                                                                     vaxis=axis2
                                                                     frame
                                                                     legend=legend1;
run;

/** efikasnost metode racunamo kao omjer varijanci var(E1)/var(E0)
	- var(E0)=varijanca nakon primjene tehnike za redukciju varijabiliteta
	- var(E1)=varijanca direktne (osnovne) metode, bazirane na r replikacija
	- mi gledamo efikasnost metoda b)-c) u odnosu na a)**/
%let var_u = 1/12; /** VarU=(b-a)^2/12 ....X~U(0,1) **/
%let var_t = (&min_t**2+&mod_t**2+&max_t**2-&min_t*&mod_t-&min_t*&max_t-&mod_t*&max_t)/18; /** VarT=(min^2+max^2+mod^2-min*max-min*mod-max*mod)/18
										X~T(0,1,0.9), min=0, max=1, mod=0.9 **/
/*%let var_t=0.3852;*/
proc means data=cb1 mean var;
var pom1;
run; /*odavde citam var_cb1*/
%let var_cb1 = 0.000038722;

DATA EFIKASNOST;
ef_ut = &var_u/&var_t; /** efikasnost metode b) u odnosu na metodu a) **/
ef_ucb1 = &var_u/&var_cb1;
RUN;

PROC PRINT;
RUN;
/**Obs	   ef_ut	  ef_ucb1
   1	.006172840	2152.09**/
  
/*2. zadatak*/

/*slicno kao prvi. pom varijable se dobiju tako da se x^x podijeli sa odgovarajućom funkcijom gustoće*/

