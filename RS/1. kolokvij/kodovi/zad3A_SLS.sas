
data Fit; 
   input weight waist; 
   datalines; 
191  36   
189  37    
162  35    
189  35   
182  36   
211  38    
167  34    
176  31       
154  33    
169  34   
166  33   
154  34   
247  46    
193  36    
202  37   
157  32    
156  33    
138  33
161  31 
; 
run;
 

   
   proc means data=fit mean t stderr std skewness kurtosis;
    var  waist;
	run;
 

/* Program CHAPTER1_1_NONNORMAL_FLEISHMAN_EX.SAS  */

/* desired skewness, kurtosis */

data skewkurt;
 input skewness kurtosis;
 datalines;
 2.03 6.19  
  ;
run;

 

/* desired mean, sigma */

data meansig;
 input mean sig;
 datalines;
34.95  3.34
 ;
run;

/* Deriving Fleishman Coefficients for Desired Skewness and Kurtosis (assuming macro FLEISHMAN has been defined) */
/*** submitirati fleishman macro ***/

%fleishman;

%let nrep=300; 
%let seed=12456;

/* application of fleishman formula and linear transformation to desired mean, sigma */

data nonnor;
 merge fleishman meansig;
 varid=_N_;
 call streaminit(&seed);
 a=-c;
 do rep=1 to &nrep;
  do j=1 to 19;
    x=RAND("NORMAL");
	x=a + b*x + c* x**2 + d*x**3;
	x=mean + sig* x;
	output;
   end;
 end;
run;

 


proc means data=nonnor nway noprint;
 var x;
 class rep;
 output out=out mean=mean stderr=stderr median=median range=range p25=p25 p75=p75;
 run;

 
 proc means data=out mean std p5 p95;
  var median p25 p75;
  run;
 
 /*Variable Mean Std Dev 5th Pctl 95th Pctl 
median 
p25 
p75 
 33.9821213 
32.5614487 
36.3976921 
 0.7496916 
0.4406850 
1.3761025 
 32.9437704 
32.0389153 
34.4927607 
 35.4315521 
33.3395953 
39.0139581 
 
*/

