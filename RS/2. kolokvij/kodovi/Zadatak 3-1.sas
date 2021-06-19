


data  UCENICI;
 input RAZRED $ OCJENA @@;
 datalines;
A 4.4 A 4.8 A 3.8 A 4.2 A 4.7 A 4.6
B 5.0 B 4.7 B 4.8 B 4.9 B 5.0 B 4.5
;
run;



data grupa1;
  set ucenici;
  where razred="A";
  ;
data grupa2;
  set ucenici;
  where razred="B";
  ;
data grupe;
 set grupa1 (in=a) grupa2;
 if a then grupa=1; else grupa=2;
 id=_N_;
 run;


/*** Prosjecna vrijednost OCJENA po grupama ***/

 proc means data=grupe nway noprint;
  var ocjena;
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

%let seed1=23456;  *za grupu 1  ; 
%let seed2=65432;  *za grupu 2  ;



  data all;

  call streaminit(&seed1);
 
  do rep=1 to 500;
     do i=1 to 6;
	 x=RAND("NORMAL")*&std1 + &mean1;
	 grupa=1;
     output;
	 end;
	 end;

  call streaminit(&seed2);

 
  do rep=1 to 500;
     do i=1 to 6;
	 x=RAND("NORMAL")*&std2 + &mean2;
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

   /*** Napomena; ovaj zadnji step gore se može izvesti i sa proc sql **/

title '90% bootstrap interval pouzdanosti za t vrijednost'; 

proc means data=t p5 p95 stderr mean;
	 var t;
	 run;

title; 
/*** Rješenje 3a:

                                     

The MEANS Procedure

                                        Analysis Variable : t

                         5th Pctl       95th Pctl       Std Error            Mean
                     ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
                       -5.2369326      -0.7452849       0.0655133      -2.6927874
                     ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ

    
**********/



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
	proc means data=medians p5 p95 stderr mean;
	 var absmedians;
	 run;

	title;


	/*** Rješenje 3b

	
                      The MEANS Procedure

                                    Analysis Variable : absmedians

                         5th Pctl       95th Pctl       Std Error            Mean
                     ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
                        0.1026381       0.7359817       0.0083931       0.4188859
                     ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ

 
*************/

