/* Antonija Ðuriæ */
/* 1191227212 */

										/* ZADATAK 3 */



 data  pizza;
 input tijesto $ kvaliteta @@;
 datalines;
s 4.5 s 4.7 s 3.8 s 4.2 s 5.2 s 4.6 s 4.6
n 5.3 n 4.8 n 5.4 n 4.9 n 5.0 s 4.7
;
run;



data grupa1;
  set pizza;
  where tijesto="n";
  ;
data grupa2;
  set pizza;
  where tijesto="s";
  ;
data grupe;
 set grupa1 (in=a) grupa2;
 if a then grupa=1; else grupa=2;
 id=_N_;
 run;


/*** Prosjeèna vrijednost EFIKASNOSTI po grupama ***/

 proc means data=grupe nway noprint;
  var kvaliteta;
  class grupa;
  output out=out mean=mean std=std;
 run; 

 data _NULL_;
  set out;
  if grupa=1 then do; 
             call symput("mean1",mean);
             call symput("std1",std);
			 end;
  else 	do;
			 call symput("mean2",mean);
             call symput("std2",std);
			 end;
  run;

  %put mean1=&mean1 mean2=&mean2 std1=&std1 std2=&std2;

%let seed1=1245;  *za grupu 1  ; 
%let seed2=7653;  *za grupu 2  ;


data all;
call streaminit(&seed1);
 do rep = 1 to 500;
 do i=1 to 6;
  x = rand("normal")*&std1 + &mean1;
  grupa=1;
  output;
 end;
 end;

 call streaminit(&seed2);
 do rep = 1 to 500;
 do i=1 to 7;
  x = rand("normal")*&std1 + &mean1;
  grupa=2;
  output;
 end;
 end;
run;
 
 proc means data=all nway noprint;
  var x;
  class rep grupa;
  output out=out mean=mean stderr=stderr;
  run;

/*** A  ***/

  /*** means (brojnik za t) ***/

  proc transpose data=out out=means;
   var mean;
   id grupa;
   by rep;
   run;

   data means;
    set means;
	brojnik_za_t= _1 - _2;
	run;

 /*** stderrs (nazivnik za t)***/

  proc transpose data=out out=stderrs;
   var stderr;
   id grupa;
   by rep;
   run;

   data stderrs;
    set stderrs;
	nazivnik_za_t=sqrt( _1**2 + _2**2);
	run;

  data t;
   merge means stderrs;
   t=brojnik_za_t/nazivnik_za_t;
   run;

   /*** Napomena; ovaj zadnji data step gore se može izvesti i sa proc sql **/

title '90% bootstrap interval pouzdanosti za t vrijednost'; 

proc means data=t p5 p95 std mean stderr maxdec=3;
	 var t;
	 run;

title; 



/*** B (apsolutna razlika medijana) ***/

proc means data=all nway noprint;
 var x;
 class rep grupa;
 output out=out1 median=medijan;
 run;
 

  proc transpose data=out1 out=medians;
   var medijan;
   id grupa;
   by rep;
   run;

 
   data medians;
    set medians;
	absmedians=abs(_1 - _2);
	run;

	title '90% bootstrap interval pouzdanosti za aps.vrijednost razlike medijana'; 
	proc means data=medians p5 p95 std mean stderr maxdec=3;
	 var absmedians;
	 run;

	title;
