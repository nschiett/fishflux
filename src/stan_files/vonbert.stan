functions {
real normal_lb_rng(real mu, real sigma, real lb) {
    real p = normal_cdf(lb, mu, sigma);  // cdf for bounds
    real u = uniform_rng(p, 1);
    return (sigma * inv_Phi(u)) + mu;  // inverse cdf for value
}
}

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
  real<lower=linf_min> linf_glob;
  real t0;                        // t0 same for all individuals
  real<lower=0> k[J];           // k should be bigger that 0

  // parameters for regression log(k)~log(inf)
  real<upper = 0> sl;
  real<lower = 0> gp;

  // likelihood of y
  real<lower=0> sigma;
  real<lower=0> linf_sigma;
  real<lower=0> k_sigma;


}
transformed parameters{
    real<lower=0> k_glob;
    k_glob = exp(sl*log(linf_glob) + gp);
}

model  {

  //priors
  target += student_t_lpdf(sl | 3, -2.31, 0.22);   // prior of sl from estimate in Morais et al. (2018)
  target += student_t_lpdf(gp | 3, 3, 2);
  target += student_t_lpdf(t0 | 3,  0, 1);
  target += cauchy_lpdf(sigma | 0, 5);

  linf_glob ~ normal(linf_m, 3);
  target += normal_lpdf(linf_sigma | 0, 1);
  target += normal_lpdf(k_sigma | 0, 0.1);

  for (j in 1:J){
    linf[j] ~ normal(linf_glob, linf_sigma);
    k[j] ~ normal(k_glob, k_sigma);
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

  real linf_global;
  real k_global;

  vector[N] y_rep; // prediction per individual
  vector[N] y_m;   // prediction based on weighted averages for linf, k
  vector[N] y_max; // prediction based on maximum length and kmax

  vector[N] y_global; // prediction


  vector[J] linf_w;

   for (j in 1:J){
     linf_w[j] = linf[j] * age_max[j];
   }


  mu_linf = sum(linf_w)/sum(age_max);

  linf_global = normal_lb_rng(linf_glob, linf_sigma, linf_min);
  mu_k = exp(gp + (sl*log(mu_linf)));
  kmax = exp(gp + (sl*log(lmax)));
  k_global = normal_rng(k_glob, k_sigma );

   for (n in 1:N) {
     y_rep[n] = linf[ind[n]] * (1 - exp( - k[ind[n]] * (age[n] - t0)));
     y_m[n] = mu_linf * (1 - exp( - mu_k * (age[n] - t0)));
     y_max[n] = lmax * (1 - exp( - kmax * (age[n] - t0)));
     y_global[n] = linf_global * (1 - exp( - k_global * (age[n] - t0)));
   }

}
