data {

  int<lower=1> N;            // all data points
  int J;                     // total of individuals

  real age[N] ;  //age vector
  real l[N] ;     //response variable

  int<lower=1,upper=J> ind[N];           //which individual
  real age_max[J];               // vector with maximum age per individual

  real linf_m;     // prior for linf
  real linf_min;   // minimum for linf

  real lmax;      // maximum length

}

parameters {

  // VBGC parameters
  real<lower=linf_min> linf[J];        //linf per individual, bigger than linf_min
  real t0;                        // t0 same for all individuals
  real<lower=0> k[J];           // k should be bigger that 0

  // parameters for regression log(k)~log(inf)
  real sl;
  real gp;

  // likelihood of y
  real<lower=0> sigma;

}

model  {

  //priors
  target += student_t_lpdf(sl | 3, -2.31, 0.22);   // prior of sl from estimate in Morais et al. (2018)
  target += student_t_lpdf(gp | 3, 0, 1);
  target += student_t_lpdf(t0 | 3,  0, 1);
  target += cauchy_lpdf(sigma | 0, 5);

  for (j in 1:J){
    linf[j] ~ normal(linf_m, 3);
    k[j] ~ normal(exp(sl*log(linf[j]) + gp), 0.1);  // k is correlated with linf
  }

  //model
  for (n in 1:N) {
     l[n] ~ normal(linf[ind[n]] * (1 - exp( - k[ind[n]] * (age[n] - t0))), sigma);
    }

}
generated quantities {

  real mu_linf;   // weighted average for linf, based on age of individuals
  real mu_k;
  real kmax;

  vector[N] y_rep; // prediction per individual
  vector[N] y_m;   // prediction based on weighted averages for linf, k
  vector[N] y_max; // prediction based on maximum length and kmax

  vector[J] linf_w;

   for (j in 1:J){
     linf_w[j] = linf[j] * age_max[j];
   }

  mu_linf = sum(linf_w)/sum(age_max);
  mu_k = exp(gp + (sl*log(mu_linf)));
  kmax = exp(gp + (sl*log(lmax)));

   for (n in 1:N) {
     y_rep[n] = linf[ind[n]] * (1 - exp( - k[ind[n]] * (age[n] - t0)));
     y_m[n] = mu_linf * (1 - exp( - mu_k * (age[n] - t0)));
     y_max[n] = lmax * (1 - exp( - kmax * (age[n] - t0)));
   }

}
