*1.zadatak;

%let dat = lib.pills_efron;
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


/* pomocu macro-a "boot" dobivamo neparametarsku bootstrap procjenu medijana te standardne pogreske i pristranosti za medijan */
title 'Bootstrap';
%boot(data = lib.pills_efron, random=&seed, samples=&nboot) /*data-set; seed; broj_ponavljanja*/


/*2. zadatak*/

ods html close;
ods html;

/** CHAPTER1_3_bootstrap **/

/** parametric bootstrap **/

%let dat = lib.normal2_rho_0_562;
%let nboot = 500;
%let n = 15;
%let seed = 2234;

/** correlation on the original data **/

title 'Parametric bootstrap estimates';
proc corr noprint data=&dat
         out=corr(where=(_type_='CORR' & _name_='X')
                  rename=(Y=corr)
                  keep=Y _type_ _name_ );
         var X Y;
run;

data _NULL_;
 set corr end=last;
 if last then call symput('corr',put(corr,best10.));
run;

%put &corr;

/** parametric bootstrap **/

DATA NORMAL2;
 DO REP = 1 TO &NBOOT;
  do i=1 to &n;
  X1 = NORMAL (&SEED);	
  X2 = NORMAL (&SEED);	
  X = X1;
  Y = &CORR * X1 + SQRT(1 - &CORR**2)* X2;
  OUTPUT;
 end;
 END;
 DROP X1 X2 i;
RUN; 

proc corr noprint data=normal2
         out=pbootdist(where=(_type_='CORR' & _name_='X')
                  rename=(Y=bootcorr)
                  keep=Y _TYPE_ _NAME_ rep);
         var X Y;
		 by rep;
run;

data pbootdist;
 set pbootdist;
 rename rep=_sample_;
 method='Parametric   ';
run; 

proc means data = pbootdist nway mean std stderr;
	var bootcorr;
run;
/*
Parametric bootstrap estimates   

                                         The MEANS Procedure

                                     Analysis Variable : bootcorr

                                     Mean	Std Dev	Std Error
                               0.5519069	0.1852439	0.0082844
*/
/** smoothed normal bootstrap = Fisher's formula for the sampling density of corr in bivariate normal case **/

/** nonparametric bootstrap **/

title 'Nonparametric bootstrap estimates';
%macro analyze(data=,out=);
      proc corr noprint data=&data
         out=&out(where=(_type_='CORR' & _name_='X')
                  rename=(Y=bootcorr)
                  keep=Y _TYPE_ _NAME_ &by);
         var X Y;
         %bystmt;
      run;
%mend;

%include "/home/u47429085/jackboot.sas";
   %boot(data=lib.normal2_rho_0_562,random=&seed, samples=&nboot)
proc means data = bootdist nway mean std stderr;
	var bootcorr;
run;
/*Nonparametric bootstrap estimates                                 17
                                                                         13:47 Wednesday, June 1, 2016

                                         The MEANS Procedure

                                     Analysis Variable : bootcorr

                                     Mean	Std Dev	Std Error
                               0.5292181	0.2184899	0.0097712
*/
/*Ovdje je neparametarski bootstrap napravljen pomoću macro-a boot, mogli ste pomoću surveyselect (kako je i pisalo u uputama).*/
/*b)*/
data boot;
 drop _TYPE_ _NAME_;
 set pbootdist bootdist;
 if method=' ' then method='Nonparametric';
 run;

proc means data = boot p5 p95;
	var bootcorr;
	class method;
run;
/* The MEANS Procedure

                                    Analysis Variable : bootcorr
      method	N Obs	5th Pctl	95th Pctl
Nonparametric	500	   0.1147021	0.8163793
Parametric	    500	   0.1869080	0.8064704
*/


  goptions reset=global gunit=pct 
     cback=white ctext=black vsize=4in hsize=4in
     colors=(blue green red cyan yellow black gray) ftext=simplex
     ftitle = simplex htitle=3 htext=2;


  goptions display dev=gif gsfname=histplot /*gcopies=0*/  gsfmode=replace
  ;                                                                                                      
title1 "Parametric and Nonparametric bootstrap estimates of corr.coef.";
   pattern v=solid c=green;                                                                                                               

   proc capability data=boot noprint;       
      comphistogram bootcorr / class        = method href=&corr chref=red lhref=1
                     
                       kernel(color = white fill) 
                       cfill        = blue 
                       pfill        = solid 
                       cframe       = gray 
                       cframeside   = gray 
                     
                       turnvlabels; 
   inset mean std="Std Dev" p5 p95/ pos   = ne 
                              format= 6.3 
                              ctext = black 
                              cfill = white; 
run; 

   
title1 "Parametric bootstrap estimate of corr.coef.";
   pattern v=solid c=green;                                                                                                               

   proc capability data=boot noprint;     
   where method='Nonparametric'; 
      histogram bootcorr /  href=&corr chref=red lhref=1
                     
                       kernel(color = white fill) 
                       cfill        = blue 
                       pfill        = solid 
                       cframe       = gray 
                     
                       ; 
   inset mean std="Std Dev" p5 p95/ pos   = ne 
                              format= 6.3 
                              ctext = black 
                              cfill = white; 
run; 


goptions reset=all; 
* 3. zadatak;

data iris;
	set sashelp.iris(where=(Species = 'Virginica'));
	keep SepalLength SepalWidth;
run;

%include "/home/u47429085/jackboot.sas"; 

/* a) */

title 'Procjena modela obicnom linearnom regresijom';

proc reg data=iris plots(only) = () outest = out;
	model SepalWidth = SepalLength;
run;

/* Procjena regresijskog modela je y = 14.46305 + 0.23189*x  (x = SepalLength, y = SepalWidth). */
/* Standardna pogreska regresijskog koeficijenta iznosi s_x = 0.0651. */


%macro analyze(data=, out=);
	proc reg noprint data=&data
		outest = &out(where=(_TYPE_='PARMS') rename=(SepalLength=bootreg) keep=SepalLength _TYPE_ &by);
		model SepalWidth = SepalLength;
        %bystmt;
	run;
%mend;

%let nboot=1000;
%let seed=3774;

title 'Neparametarske bootstrap procjene i sumarne statistike';

%boot(data=iris, random=&seed, samples=&nboot, chart=0);

/* Neparametarska bootstrap procjena regresijskog koeficijenta je 0.23063, pa je procjena pristranosti -0.001257766  */
  
/* b) */

title 'Neparametarski percentilni bootstrap P.I. za regresijski koeficijent';

%bootci(method=PCTL, stat=bootreg, alpha=0.10);   /* 90% P.I. je [0.10420, 0.36331] */
%bootci(method=PCTL, stat=bootreg, alpha=0.05);   /* 95% P.I. je [0.07244, 0.38840] */


/* c) */

title 'Neparametarski BC bootstrap P.I. za regresijski koeficijent';

%bootci(method=BC, stat=bootreg, alpha=0.10);   /* 90% P.I. je [0.10958, 0.37011] */
%bootci(method=BC, stat=bootreg, alpha=0.05);   /* 95% P.I. je [0.07657, 0.39162] */

 
  
/* d) */

%let seed=4455;
%let nboot=1000;

proc surveyselect data=iris out=outboot noprint
	seed=&seed
	method=urs
	samprate=1
	outhits
	rep=&nboot; 
run;

proc reg data=outboot noprint
	outest = outall(where=(_TYPE_='PARMS') rename=(SepalLength=bootreg) keep=SepalLength _TYPE_);
	model SepalWidth = SepalLength;
	by Replicate;
run;

proc univariate data=outall noprint;
	var bootreg;
	output out=final pctlpts=2.5, 5, 95, 97.5 pctlpre=ci_ ;
run;

proc means data=outall mean std noprint;
	var bootreg;
	output out = final2 mean=mean std=std;
run;

data final2;
	set final2;
	bias = mean - 0.23189;
	keep mean std bias;
run;

data sve;
	set final2;
	set final;
run;

title 'Neparametarske bootstrap procjene i pouzdani intervali';

proc print data=sve;
run;

/* e) */


ods graphics / reset width=6.4in height=4.8in imagemap;

proc sgplot data=work.outall;
	title height=14pt "Bootstrap razdioba procjenitelja regresijskog koeficijenta iz d) dijela zadatka";
	histogram bootreg /;
	density bootreg;
	yaxis grid;
run;

ods graphics / reset;
title;

/* Na histogram je dodan graf funkcije gustoce N(0.23189, 0.0651^2) razdiobe (to je teorijska razdioba procjenitelja regresijskog
   koeficijenta).  */









