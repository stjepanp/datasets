*1. zadatak;
proc iml;
start ImanConoverTransform(Y, C);
   X = Y; 
   N = nrow(X);
   R = J(N, ncol(X));
   /* compute scores of each column */
   do i = 1 to ncol(X);
      h = quantile("Normal", rank(X[,i])/(N+1) );
      R[,i] = h;
   end;
   /* these matrices are transposes of those in Iman & Conover */
   Q = root(corr(R)); 
   P = root(C); 
   S = solve(Q,P);                      /* same as  S = inv(Q) * P; */
   M = R*S;             /* M has rank correlation close to target C */

   /* reorder columns of X to have same ranks as M.
      In Iman-Conover (1982), the matrix is called R_B. */
   do i = 1 to ncol(M);
      rank = rank(M[,i]);
      tmp = X[,i];       /* TYPO in first edition */
      call sort(tmp);
      X[,i] = tmp[rank];
   end;
   return( X );
finish;

/* Step 1: Specify marginal distributions */
call randseed(1);
N = 100;
A = j(N,4);   y = j(N,1);
distrib = {"Normal" "Lognormal" "Expo" "Uniform"};
do i = 1 to ncol(distrib);
   call randgen(y, distrib[i]);
   A[,i] = y;
end;

/* Step 2: specify target rank correlation */
C = { 1.00  0.75 -0.70  0,
      0.75  1.00 -0.95  0,
     -0.70 -0.95  1.00 -0.2,
      0     0    -0.2   1.0};

X = ImanConoverTransform(A, C);
RankCorr = corr(X, "Spearman");
print RankCorr[format=5.2];

/* write to SAS data set */
create MVData from X[c=("x1":"x4")];  append from X;  close MVData;
quit;


proc copula data=MVData;
	var x1-x4;
	fit normal;
	simulate / seed=1234 ndraws=100
	marginals=empirical outuniform=UnifData;
run;

data Sim;
set UnifData;
normal = quantile("Normal", x1);
lognormal = quantile("LogNormal", x2);
expo = quantile("Exponential", x3);
uniform = x4;
run;



proc corr data=Sim Spearman noprob plots=matrix(hist) fisher;
title "Simulated Data";
var normal lognormal expo uniform;
run;

/*
 0.75 je u <0.641592,  0.820675>
-0.70 je u <-0.780178,-0.570199>
 0    je u <-0.136890, 0.254533>
-0.95 je u <-0.965382,-0.924846>
 0    je u <-0.120842, 0.269730>
-0.2  je u <-0.410921,-0.038691>

Dakle svi parametri se nalaze u 95% pouzdanim intervalima

*2. zadatak;
data podaci;
	set SASHELP.CARS;
	keep INVOICE MPG_City WEIGHT;
run;

proc copula data=podaci;
	var INVOICE MPG_City WEIGHT;
	fit normal;
	simulate / seed=1234 ndraws=200
	marginals=empirical outuniform=UnifData;
run;


data Sim;
	set UnifData;
	INVOICE = quantile("Exponential", INVOICE);
	MPG_City = quantile("LogNormal", MPG_City);
	WEIGHT = quantile("LogNormal", WEIGHT);
run;

proc corr data=podaci Spearman noprob plots=matrix(hist);
title "Pocetni podaci";
var INVOICE MPG_City WEIGHT;
run;

proc corr data=Sim Spearman noprob plots=matrix(hist);
	title "Simulirani podaci";
	var INVOICE MPG_City WEIGHT;
run;

/* Simulirani podaci slice na pocetne, korelacije se dobro podudaraju.
   Potpuna podudarnost se ne postize buduci da podaci vjerojatno ne dolaze iz
   distribucija koje smo koristili u data Sim koraku*/

*/



