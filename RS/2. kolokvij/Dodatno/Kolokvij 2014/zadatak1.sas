

/*** a ***/
%let limit1=0;
%let limit2=1;
%let seed=489123;


data integral1 last1(keep=int1 stde1 var1);
call streaminit(&seed);

 do i=1 to 10000;
  x=&limit1+rand("UNIFORM")*(&limit2-&limit1);
  if x=0 then y1=2;
  else
  y1=2*log(1+x)/x;
  y2=&limit2-&limit1;
  y=y1/y2;
  sum+y;
  int1=sum/i;   /* kumulativni prosjek */
  diff2+(y-int1)**2;
  var1=diff2/(i-1);  /* kumulativna stand pogre� */
  stde1=sqrt(var1/i);  /* procj stand pogre� */
  output integral1;
  if i=10000 then output last1;
 end;
 run;
  


 /*** b ***/
%let abeta=1;
%let bbeta=2;


data integral2 last2(keep=int2 stde2 var2);
call streaminit(&seed);

 do i=1 to 10000;
  x=rand("BETA",&abeta,&bbeta) ;
  if x=0 then y1=2;
  else
  y1=2*log(1+x)/x;
  y2=PDF("BETA",x,&abeta,&bbeta);
  y=y1/y2;
  sum+y;
  int2=sum/i;
  diff2+(y-int2)**2;
  var2=diff2/(i-1);
  stde2=sqrt(var2/i);
  output integral2;
  if i=10000 then output last2;
 end;
 run;

  /*** c ***/
%let abeta=1;
%let bbeta=1.5;


data integral3 last3(keep=int3 stde3 var3);
call streaminit(&seed);

 do i=1 to 10000;
  x=rand("BETA",&abeta,&bbeta) ;
  if x=0 then y1=2;
  else
  y1=2*log(1+x)/x;
  y2=PDF("BETA",x,&abeta,&bbeta);
  y=y1/y2;
  sum+y;
  int3=sum/i;
  diff2+(y-int3)**2;
  var3=diff2/(i-1);
  stde3=sqrt(var3/i);
  output integral3;
  if i=10000 then output last3;

 end;
 run;


 /*** d ***/
 %let h=0.01;

 
data integral4 last4(keep=int4 stde4 var4);
call streaminit(&seed);

 do i=1 to 10000;
  x=rand("TRIANGLE",&h) ;
  if x=0 then y1=2;
  else
  y1=2*log(1+x)/x;
  y2=(x <= &h)*2*x/&h + (x>&h) * 2*(1-x)/(1-&h);  **(...) je logi�ki izraz sa rezultatom 0 ili 1;
  y=y1/y2;
  sum+y;
  int4=sum/i;
  diff2+(y-int4)**2;
  var4=diff2/(i-1);
  stde4=sqrt(var4/i);
  output integral4;
  if i=10000 then output last4;

 end;
 run; 

 /*** e ***/
 %let h=0.001;

 
data integral5 last5(keep=int5 stde5 var5);
call streaminit(&seed);

 do i=1 to 10000;
  x=rand("TRIANGLE",&h) ;
  if x=0 then y1=2;
  else
  y1=2*log(1+x)/x;
  y2=(x <= &h)*2*x/&h + (x>&h) * 2*(1-x)/(1-&h);  **(...) je logi�ki izraz sa rezultatom 0 ili 1;
  y=y1/y2;
  sum+y;
  int5=sum/i;
  diff2+(y-int5)**2;
  var5=diff2/(i-1);
  stde5=sqrt(var5/i);
  output integral5;
  if i=10000 then output last5;

 end;
 run; 


   /*** f ***/

%let m=0;
%let s=1.5;

/** Truncated normal na intervalu (0,1) ***/

 data _NULL_;
  probn1=probnorm((&limit1-&m)/&s);
  probn2=probnorm((&limit2-&m)/&s);
  probn=probn2-probn1;
  call symput("probn",probn);
  put probn1 probn2 probn;
  run;



data integral6 last6(keep=int6 stde6 var6);
call streaminit(&seed);

 do while (i<10000);
  
  x=rand("NORMAL",&m,&s) ;
  if x>= &limit1 and x <= &limit2 then do;
  i+1;
  if x=0 then y1=2;
  else
  y1=2*log(1+x)/x;
  y2=pdf('NORMAL',x,&m,&s)/&probn;
  y=y1/y2;
  sum+y;
  int6=sum/i;
  diff2+(y-int6)**2;
  var6=diff2/(i-1);
  stde6=sqrt(var6/i);
  output integral6;
   if i=10000 then output last6;

  end;
 end;
  run;

/*** NAPOMENA: Spajanje kona�nih rezultata (za i=10000) se mo�e izvesti i ru�no (pro�itati rezultate - tj. bez ovog zadnjeg koraka spajanja) ***/

data sve;
 merge last1 last2 last3 last4 last5 last6;
 array eff eff2-eff6;
 array var var2-var6;
 do over eff;
  eff=var1/var;
 end;
 run;
 

 proc print data=sve noobs;
  var stde1-stde6;
  format stde1-stde6 10.4;
  run;

  /*** Rje�enje

                                  stde1         stde2         stde3         stde4         stde5         stde6

                                0.0018        0.0148        0.0065        0.0163        0.0163        0.0008

****/
 
 proc print data=sve noobs;
  var eff2-eff6;
  format eff2-eff6 10.4;
  run;

  /**** Rje�enje

                                          eff2          eff3          eff4          eff5          eff6

                                       0.0141        0.0734        0.0117        0.0116        4.7221
****/
