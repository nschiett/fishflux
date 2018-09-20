data {                      // Data block
int<lower=1> N;            // all data points

int J;                // total of individuals

vector[N] age;  //covariate vector
vector[N] l;    //response variable

vector[N] s;  //vector for structure, should be 1

int<lower=1,upper=J> ind[N];           //which individual

//prior adapt

real k_m;
real linf_m;
real linf_min;

}

parameters {                // Parameters block
real<lower=0> k;           // k should be bigger that 0
real<lower=linf_min> linf;        //linf has to be bigger than 14 cm
real t0;

real<lower=0> sigmasq_k;
real<lower=0> sigmasq_linf;
real<lower=0> sigmasq_t0;

real<lower=0> sigma;

// effects individuals
real z_k[J];
real z_linf[J];
real z_t0[J];

}

transformed parameters {
  real<lower=0> sigma_k;
  real<lower=0> sigma_linf;
  real<lower=0> sigma_t0;

  sigma_k = sqrt(sigmasq_k);
  sigma_linf = sqrt(sigmasq_linf);
  sigma_t0 = sqrt(sigmasq_t0);

}

model  {
  vector[N] nlp_linf = linf * s;
  vector[N] nlp_k = k * s;
  vector[N] nlp_t0 = t0 * s;

//priors
  target += normal_lpdf(linf | linf_m, 5);
  target += normal_lpdf(k | k_m, 0.5);
  target += normal_lpdf(t0 | 0, 0.01);
  target += cauchy_lpdf(sigma | 0, 5);
  target += inv_gamma_lpdf(sigmasq_linf |0.001, 0.001);
  target += normal_lpdf(z_linf | 0, 10);
  target += inv_gamma_lpdf(sigmasq_k | 0.001, 0.001);
  target += normal_lpdf(z_k | 0, 1);
  target += inv_gamma_lpdf(sigmasq_t0 |0.001, 0.001);
  target += normal_lpdf(z_t0 | 0, 1);

  for (n in 1:N) {
    nlp_linf[n] += z_linf[ind[n]];
    nlp_k[n] += z_k[ind[n]];
    nlp_t0[n] += z_t0[ind[n]];
    // compute non-linear predictor
    l[n] ~ normal(nlp_linf[n] * (1 - exp( - nlp_k[n] * (age[n] - nlp_t0[n]))), sigma);
  }
}
generated quantities {
  vector[N] y_rep;
  vector[N] nlp_linf = linf * s;
  vector[N] nlp_k = k * s;
  vector[N] nlp_t0 = t0 * s;
  real mu_linf;
  real mu_k;
  real mu_t0;

    for (n in 1:N) {
      nlp_linf[n] += z_linf[ind[n]];
      nlp_k[n] += z_k[ind[n]];
      nlp_t0[n] += z_t0[ind[n]];
      y_rep[n] = (nlp_linf[n] * (1 - exp( - nlp_k[n] * (age[n] - nlp_t0[n]))));
  }

 mu_linf = mean(nlp_linf);
 mu_k = mean(nlp_k);
 mu_t0 = mean(nlp_t0);



}

