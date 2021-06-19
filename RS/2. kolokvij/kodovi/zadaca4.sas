/*4. zadaća*/
/*1.zadatak*/

/********************************************************************
 Reordering Multivariate Data: The Iman-Conover Method
 *******************************************************************/

/* Use Iman-Conover method to generate MV data with known marginals
   and known rank correlation. */
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

/* Step 3: fit normal copula
   Step 4: simulate data, transformed to uniformity */
proc copula data=MVData;
   var x1-x4;
   fit normal;
   simulate / seed=1234  ndraws=100
              marginals=empirical  outuniform=UnifData;
run;
/* Step 5: use inverse CDF to invert uniform marginals */
data Sim;
set UnifData;
normal = quantile("Normal", x1);
lognormal = quantile("LogNormal", x2);
expo = quantile("Exponential", x3);
uniform = x4;
run;
/* Compute 95% confidence intervals for the Spearman correlation for simulated data */
proc corr data=Sim Spearman noprob plots=matrix(hist) fisher;
   title "Simulated Data";
   var normal lognormal expo uniform;
run;
/*2. zadatak*/
data podaci;
set sashelp.cars (keep=INVOICE MPG_City Weight);
run;
/*a) dio*/
proc copula data=podaci;
   var INVOICE MPG_City Weight;
   fit normal;
   simulate / seed=1234  ndraws=200
              marginals=empirical  outuniform=UnifData;
run;
/*b) dio*/
data Sim;
set UnifData;
expo_inv = quantile("Exponential", INVOICE);
lognormal_mpg = quantile("LogNormal", MPG_City);
lognormal_wei = quantile("LogNormal", Weight);
run;
/*c) dio*/
proc corr data=podaci Spearman noprob plots=matrix(hist);
   title "Data from SASHELP.CARS";
   var INVOICE MPG_City Weight;
run;

proc corr data=Sim Spearman noprob plots=matrix(hist);
   title "Simulated Data";
   var expo_inv lognormal_mpg lognormal_wei;
run;
