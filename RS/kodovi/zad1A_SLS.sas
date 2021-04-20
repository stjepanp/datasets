
%let n=25;
%let nrep=1000;
%let seed=37749;

%let x0=0;


 
%let mi=2;
%let s=1.5;
 
/** a ***/

* sredina lognormalne disn: exp(mi + s**2/2) ;
* skewness (exp(s**2)+2)*sqrt(exp(s**2)-1)   ;
data _null_;
 mi_lognor=exp(&mi + &s**2/2);
 skewness= (exp(&s**2)+2)*sqrt(exp(&s**2)-1);
 var=exp(2*&mi+&s**2)*(exp(&s**2)-1);  

 call symput("x0",mi_lognor);
 put mi_lognor skewness var;
 run;

 %put &=x0;
/*E(x)=22.759895094 Skewness(X)=33.468046797 var(x)=4396.7560156;

/**b **/

/*** Hipoteza: M=X0 (=0)***/


data dio1a;
call streaminit(&seed);
do rep=1 to &nrep;
 do i=1 to &n;
  
  
 x=exp(rand("NORMAL")*&s + &mi);
 xtest=x-&x0;
 output;
 end;
 end;
 run;

 
proc means data=dio1a  nway noprint;
 var xtest;
 by rep;
 output out=out mean=mean median=median skewness=skewness std=std var=var t=t probt=probt;
 run;


 

 data out1;
  set out;
  jesig=(probt le 0.05);
  run;

 proc freq data=out1;
  table jesig /binomial(level='1');
  run;
/*empirijska procjena: 0.2150

  95% interval pouzdanosti:
  Exact Conf Limits   
95% Lower Conf Limit 0.1899 
95% Upper Conf Limit 0.2418 ;

  /** c **/
  
proc means data=out mean std;
 var mean median var  ;
 run;
/* Variable Mean Std Dev 
mean 
median 
var 
 -0.2528804 
-14.7267309 
3530.17 
 11.7808908 
3.2149208 
12185.21 ;
 

 
  /** d **/
/** primjer programa: CHAPTER1_2_T_NORMAL3_FRACTION_CRITICAL_VALUES.sas (Exercises)***/
  
data tall;
 set out;
 n=&n;
   t_crit_01=TINV(0.01,n-1);
   fraction_crit_01_left=(t le t_crit_01);
   fraction_crit_01_right=(t ge -t_crit_01);

   t_crit_025=TINV(0.025,n-1);
   fraction_crit_025_left=(t le t_crit_025);
   fraction_crit_025_right=(t ge -t_crit_025);

   t_crit_05=TINV(0.05,n-1);
   fraction_crit_05_left=(t le t_crit_05);
   fraction_crit_05_right=(t ge -t_crit_05);
run;

proc means data=tall nway noprint;
 var fraction_crit_01_left fraction_crit_01_right fraction_crit_025_left fraction_crit_025_right
      fraction_crit_05_left fraction_crit_05_right;
 output out=fraction mean=; 
 class n;
run;

proc print data=fraction;
 var fraction_crit_01_left fraction_crit_01_right fraction_crit_025_left fraction_crit_025_right
      fraction_crit_05_left fraction_crit_05_right;
	  id n;
	  run;
 
   /* n fraction_crit_01_left fraction_crit_01_right fraction_crit_025_left fraction_crit_025_right fraction_crit_05_left fraction_crit_05_right 
25 0.173 0 0.215 0 0.264 .002 ;


