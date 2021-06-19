

 

/*** a ***/
%let limit1=0;
%let limit2=1;
%let seed=557338;



data integral1;
call streaminit(&seed);

 do i=1 to 4000;
  x=&limit1+rand("UNIFORM")*(&limit2-&limit1);
  y1=tanh(sqrt(x**2 + 2))/(sqrt(x**2+2)*(x**2+1));
  y2=&limit2-&limit1;
  y=y1/y2;
  sum+y;
  int1=sum/i;
  diff2+(y-int1)**2;
  if i>1 then do;
   var=diff2/(i-1);
   stde=sqrt(var/i);
  end;
  output;
 end;
 run;

  title "a";

 proc print data=integral1;
 where i=4000;
 var int1 stde var;
 format int1 stde 8.4;
 run;

 /*** ovaj dio (efikasnost) može biti izraèunat i sa kalkulatorom ***/
 data _NULL_;
  set integral1;
  if i=4000 then call symput("var_a",var);
  run;
  %put var_a=&var_a;

  


 /*** b ***/
%let abeta=1;
%let bbeta=2;


data integral2;
call streaminit(&seed);

 do i=1 to 4000;
  x=rand("BETA",&abeta,&bbeta) ;
  y1=tanh(sqrt(x**2 + 2))/(sqrt(x**2+2)*(x**2+1));
  y2=PDF("BETA",x,&abeta,&bbeta);
  y=y1/y2;
  sum+y;
  int1=sum/i;
  diff2+(y-int1)**2;
  if i>1 then do;
   var=diff2/(i-1);
   stde=sqrt(var/i);
   efikasnost=&var_a/var;
  end;
  output;
 end;
 run;

 title "b";
 proc print data=integral2;
 where i=4000;
 var int1 stde var efikasnost;
 format int1 stde efikasnost 8.4;
 run;

  /*** c ***/
%let abeta=1;
%let bbeta=1.2;


data integral3;
call streaminit(&seed);

 do i=1 to 4000;
  x=rand("BETA",&abeta,&bbeta) ;
  y1=tanh(sqrt(x**2 + 2))/(sqrt(x**2+2)*(x**2+1));
  y2=PDF("BETA",x,&abeta,&bbeta);
  y=y1/y2;
  sum+y;
  int1=sum/i;
  diff2+(y-int1)**2;
  if i>1 then do;
   var=diff2/(i-1);
   stde=sqrt(var/i);
   efikasnost=&var_a/var;
  end;
   output;
 end;
 run;

 
 title "c";
 proc print data=integral3;
 where i=4000;
 var int1 stde var efikasnost;
 format int1 stde efikasnost 8.4;
 run;


 /*** d ***/
 %let h=0.01;

 
data integral4;
call streaminit(&seed);

 do i=1 to 4000;
  x=rand("TRIANGLE",&h) ;
  y1=tanh(sqrt(x**2 + 2))/(sqrt(x**2+2)*(x**2+1));
  y2=(x <= &h)*2*x/&h + (x>&h) * 2*(1-x)/(1-&h);  **(...) je logièki izraz sa rezultatom 0 ili 1;
  y=y1/y2;
  sum+y;
  int1=sum/i;
  diff2+(y-int1)**2;
  if i>1 then do;
   var=diff2/(i-1);
   stde=sqrt(var/i);
   efikasnost=&var_a/var;
  end;
  output;
 end;
 run; 

  title "d";
 proc print data=integral4;
 where i=4000;
 var int1 stde var efikasnost;
 format int1 stde efikasnost 8.4;
 run;


 /*** e ***/
 %let h=0.001;

 
data integral5;
call streaminit(&seed);

 do i=1 to 4000;
  x=rand("TRIANGLE",&h) ;
  y1=tanh(sqrt(x**2 + 2))/(sqrt(x**2+2)*(x**2+1));
  y2=(x <= &h)*2*x/&h + (x>&h) * 2*(1-x)/(1-&h);  **(...) je logièki izraz sa rezultatom 0 ili 1;
  y=y1/y2;
  sum+y;
  int1=sum/i;
  diff2+(y-int1)**2;
  if i>1 then do;
   var=diff2/(i-1);
   stde=sqrt(var/i);
   efikasnost=&var_a/var;
  end;
  output;
 end;
 run; 

 
  title "e";
 proc print data=integral5;
 where i=4000;
 var int1 stde var efikasnost;
 format int1 stde efikasnost 8.4;
 run;


   /*** f ***/

%let m=0;
%let s=0.6;

/** Truncated normal na intervalu (0,1) ***/

 data _NULL_;
  probn1=probnorm((&limit1-&m)/&s);
  probn2=probnorm((&limit2-&m)/&s);
  probn=probn2-probn1;
  call symput("probn",probn);
  put probn1 probn2 probn;
  run;



data integral6;
call streaminit(&seed);

 do while (i<4000);
  
  x=rand("NORMAL",&m,&s) ;
  if x>= &limit1 and x <= &limit2 then do;
  i+1;
  y1=tanh(sqrt(x**2 + 2))/(sqrt(x**2+2)*(x**2+1));
  y2=pdf('NORMAL',x,&m,&s)/&probn;
  y=y1/y2;
  sum+y;
  int1=sum/i;
  diff2+(y-int1)**2;
  if i>1 then do;
   var=diff2/(i-1);
   stde=sqrt(var/i);
   efikasnost=&var_a/var;
  end;
  output;
  end;
 end;
 run;

 
  title "f";
 proc print data=integral6;
 where i=4000;
 var int1 stde var efikasnost;
 format int1 stde efikasnost 8.4;
 run;

 title;

