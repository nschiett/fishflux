
functions {
real normal_lb_ub_rng(real mu, real sigma, real lb, real ub) {
    real p1 = normal_cdf(lb, mu, sigma);  // cdf with lower bound
    real p2 = normal_cdf(ub, mu, sigma);  // cdf with upper bound
    real u = uniform_rng(p1, p2);
    return (sigma * inv_Phi(u)) + mu;  // inverse cdf
}

real get_log_sd(real mu, real sigma){
  real log_sigma = sqrt(log(((sigma^2)/(mu^2)) + 1));
  if (log_sigma <= 0){
    log_sigma = 0.0000000001;
  }
  return(log_sigma);
}

real get_log_mu(real mu, real sigma){
  real log_sigma = sqrt(log(((sigma^2)/(mu^2)) + 1));
  real log_mu = log(mu) - (0.5 * (log_sigma^2));
  return(log_mu);
}


}



data {

                     //means of all parameters

                     real TL_m;
                     real AEc_m;
                     real AEn_m;
                     real AEp_m;
                     real Fc_m;
                     real Fn_m;
                     real Fp_m;
                     real Linf_m;
                     real k_m;
                     real t0_m;
                     real f_m;
                     real asp_m;
                     real troph_m;
                     real lwa_m;
                     real lwb_m;
                     real w_prop_m;
                     real temp_m;
                     real Tn_m;
                     real Tp_m;
                     real Qc_m;
                     real Qn_m;
                     real Qp_m;
                     real a_m;
                     real B0_m;

                     //sd of all parameters

                     real TL_sd;
                     real AEc_sd;
                     real AEn_sd;
                     real AEp_sd;
                     real Fc_sd;
                     real Fn_sd;
                     real Fp_sd;
                     real Linf_sd;
                     real k_sd;
                     real t0_sd;
                     real f_sd;
                     real asp_sd;
                     real troph_sd;
                     real lwa_sd;
                     real lwb_sd;
                     real w_prop_sd;
                     real temp_sd;
                     real Tn_sd;
                     real Tp_sd;
                     real Qc_sd;
                     real Qn_sd;
                     real Qp_sd;
                     real a_sd;
                     real B0_sd;

                     // correlations ro
                     real ro_Qc_Qn;
                     real ro_Qc_Qp;
                     real ro_Qn_Qp;
                     real ro_Fc_Fn;
                     real ro_Fc_Fp;
                     real ro_Fn_Fp;
                     real ro_lwa_lwb;
                     real ro_a_B0;

                     }

transformed data{
  real logQc_m = get_log_mu(Qc_m, Qc_sd);
  real logQc_sd = get_log_sd(Qc_m, Qc_sd);
  real logQn_m = get_log_mu(Qn_m, Qn_sd);
  real logQn_sd = get_log_sd(Qn_m, Qn_sd);
  real logQp_m = get_log_mu(Qp_m, Qp_sd);
  real logQp_sd = get_log_sd(Qp_m, Qp_sd);

  real logFc_m = get_log_mu(Fc_m, Fc_sd);
  real logFc_sd = get_log_sd(Fc_m, Fc_sd);
  real logFn_m = get_log_mu(Fn_m, Fn_sd);
  real logFn_sd = get_log_sd(Fn_m, Fn_sd);
  real logFp_m = get_log_mu(Fp_m, Fp_sd);
  real logFp_sd = get_log_sd(Fp_m, Fp_sd);

  real loglwa_m = get_log_mu(lwa_m, lwa_sd);
  real loglwa_sd = get_log_sd(lwa_m, lwa_sd);
  real loglwb_m = get_log_mu(lwb_m, lwb_sd);
  real loglwb_sd = get_log_sd(lwb_m, lwb_sd);

  real loga_m = get_log_mu(a_m, a_sd);
  real loga_sd = get_log_sd(a_m, a_sd);
  real logB0_m = get_log_mu(B0_m, B0_sd);
  real logB0_sd = get_log_sd(B0_m, B0_sd);
}

model{

                     }


generated quantities {

                     //all paramaters

                     real<lower=0.001> TL;
                     real<lower=0> AEc;
                     real<lower=0> AEn;
                     real<lower=0> AEp;
                     real<lower=0> Fc;
                     real<lower=0> Fn;
                     real<lower=0> Fp;
                     real<lower=0> Linf;
                     real<lower=0> k;
                     real t0;
                     real<lower=0,upper=10> f;
                     real<lower=0> asp;
                     real<lower=0> troph;
                     real<lower=0> lwa;
                     real<lower=0> lwb;
                     real<lower=0> w_prop;
                     real<lower=0> temp;
                     real<lower=0> Tn;
                     real<lower=0> Tp;
                     real<lower=0> Qc;
                     real<lower=0> Qn;
                     real<lower=0> Qp;
                     real<lower=0> a;
                     real<lower=0> B0;

                     //derived variables

                     real<lower=0> m_max;   //max weight
                     real l1;      //same as TL
                     real a1;      //age at length l1
                     real a2;      // age at next time interval
                     real l2;      // predicted length at age a2
                     real w1;      // weight for l1
                     real w2;      // weight for l2
                     real wd1;     // dry weight equivalent w1
                     real wd2;     // dry weight equivalent w2
                     real Wd;      // gain in dry weight over time step
                     real Ww;      // gain in wet weight over time step

                     real Qc1;      // mass C of fish at l1 in g
                     real Qn1;      // mass N of fish at l1 in g
                     real Qp1;      // mass P of fish at l1 in g
                     real C_g;     // C gain for growth in g
                     real N_g;     // N gain for growth in g
                     real P_g;     // P gain for growth in g

                     real Em;      //cost of growth in J/g
                     real gC_to_J; // conversion factor
                     real Ec;      // combustion energy of biomass (Joules / g)
	                   real Bm;
	                   real B_main;  // maintenance metabolic rate
	                   real B_syn;   // cost of growth
	                   real B_rest;  // resting metabolic rate (Joules / day)
	                   real B_tot;   // assimilation rate in joule per day
	                   real Cm;      // amount of mass C needed for metabolism

                     real N_t;     // N needed for cell renewal
                     real P_t;     // P needed for cell renewal

                     real N_n;
                     real P_n;
                     real C_n;

                    // needed nutrients
                     real st_np;
                     real st_cn;
                     real st_cp;

                    // food
                     real stf_np;
                     real stf_cn;
                     real stf_cp;

                     int lim;      // limiting element


                     real C_in;    // ingestion
                     real N_in;
                     real P_in;

                     real C_eg;
                     real N_eg;    // egestion
                     real P_eg;

                     real N_ex;    // excretion
                     real P_ex;

                     real C_r;     // total respiration

                     real N_l;     // leftover excretion
                     real P_l;

                     real IN;      // ingestion in g dry weight
                     real IN_cnp;

          ////////// Body CNP estimation /////////

                     // covariance matrices
                     matrix[3,3] Sigma_Qcnp;
                     matrix[3,3] Sigma_Fcnp;
                     matrix[2,2] Sigma_lw;
                     matrix[2,2] Sigma_ab;
                     // vectors of parameter means
                     vector[3] mu_Qcnp;
                     vector[3] mu_Fcnp;
                     vector[2] mu_lw;
                     vector[2] mu_ab;

                     // vectors of parameter estimates from multinormal sampling
                     vector[3] Qcnp;
                     vector[3] Fcnp;
                     vector[2] lw;
                     vector[2] ab;

                     // fill vectors of parameter means
                     mu_Qcnp[1] = logQc_m;
                     mu_Qcnp[2] = logQn_m;
                     mu_Qcnp[3] = logQp_m;

                     mu_Fcnp[1] = logFc_m;
                     mu_Fcnp[2] = logFn_m;
                     mu_Fcnp[3] = logFp_m;

                     mu_lw[1] = loglwa_m;
                     mu_lw[2] = loglwb_m;

                     mu_ab[1] = loga_m;
                     mu_ab[2] = logB0_m;

                     // construct cov matrices
                     Sigma_Qcnp[1,1] = logQc_sd^2;
                     Sigma_Qcnp[2,2] = logQn_sd^2;
                     Sigma_Qcnp[3,3] = logQp_sd^2;
                     Sigma_Qcnp[1,2] = logQc_sd * logQn_sd * ro_Qc_Qn;
                     Sigma_Qcnp[2,1] = logQc_sd * logQn_sd * ro_Qc_Qn;
                     Sigma_Qcnp[1,3] = logQc_sd * logQp_sd * ro_Qc_Qp;
                     Sigma_Qcnp[3,1] = logQc_sd * logQp_sd * ro_Qc_Qp;
                     Sigma_Qcnp[2,3] = logQn_sd * logQp_sd * ro_Qn_Qp;
                     Sigma_Qcnp[3,2] = logQn_sd * logQp_sd * ro_Qn_Qp;

                     Sigma_Fcnp[1,1] = logFc_sd^2;
                     Sigma_Fcnp[2,2] = logFn_sd^2;
                     Sigma_Fcnp[3,3] = logFp_sd^2;
                     Sigma_Fcnp[1,2] = logFc_sd * logFn_sd * ro_Fc_Fn;
                     Sigma_Fcnp[2,1] = logFc_sd * logFn_sd * ro_Fc_Fn;
                     Sigma_Fcnp[1,3] = logFc_sd * logFp_sd * ro_Fc_Fp;
                     Sigma_Fcnp[3,1] = logFc_sd * logFp_sd * ro_Fc_Fp;
                     Sigma_Fcnp[2,3] = logFn_sd * logFp_sd * ro_Fn_Fp;
                     Sigma_Fcnp[3,2] = logFn_sd * logFp_sd * ro_Fn_Fp;

                     Sigma_lw[1,1] = loglwa_sd^2;
                     Sigma_lw[2,2] = loglwb_sd^2;
                     Sigma_lw[1,2] = loglwa_sd * loglwb_sd * ro_lwa_lwb;
                     Sigma_lw[2,1] = loglwa_sd * loglwb_sd * ro_lwa_lwb;

                     Sigma_ab[1,1] = loga_sd^2;
                     Sigma_ab[2,2] = logB0_sd^2;
                     Sigma_ab[1,2] = loga_sd * logB0_sd * ro_a_B0;
                     Sigma_ab[2,1] = loga_sd * logB0_sd * ro_a_B0;

                     // sample from multinormal distributions
                     Qcnp = multi_normal_rng(mu_Qcnp, Sigma_Qcnp);
                     Fcnp = multi_normal_rng(mu_Fcnp, Sigma_Fcnp);
                     lw = multi_normal_rng(mu_lw, Sigma_lw);
                     ab = multi_normal_rng(mu_ab, Sigma_ab);

                     // back transform estimates

                     Qc = exp(Qcnp[1]);
                     Qn = exp(Qcnp[2]);
                     Qp = exp(Qcnp[3]);

                     Fc = exp(Fcnp[1]);
                     Fn = exp(Fcnp[2]);
                     Fp = exp(Fcnp[3]);

                     lwa = exp(lw[1]);
                     lwb = exp(lw[2]);

                     a = exp(ab[1]);
                     B0 = exp(ab[2]);

                     // Sample other parameters

                      TL = normal_lb_ub_rng(TL_m, TL_sd, 0.5, 1000);
                      AEc = normal_lb_ub_rng(AEc_m, AEc_sd, 0.0001, 1);
                      AEn = normal_lb_ub_rng(AEn_m, AEn_sd, 0.0001, 1);
                      AEp = normal_lb_ub_rng(AEp_m, AEp_sd, 0.0001, 1);
                      Linf = normal_lb_ub_rng(Linf_m, Linf_sd, 1, 1000);
                      k = normal_lb_ub_rng(k_m, k_sd,0.0001,3);
                      t0 = normal_rng(t0_m, t0_sd);
                      f = normal_lb_ub_rng(f_m, f_sd,0.1, 6);
                      asp = normal_lb_ub_rng(asp_m, asp_sd,0.001, 8);
                      troph = normal_lb_ub_rng(troph_m, troph_sd,1, 5);
                      w_prop = normal_lb_ub_rng(w_prop_m, w_prop_sd, 0.001, 1);
                      temp = normal_rng(temp_m, temp_sd);
                      Tn = normal_lb_ub_rng(Tn_m, Tn_sd, 0.000000000000001, 0.1);
                      Tp = normal_lb_ub_rng(Tp_m, Tp_sd, 0.000000000000001, 0.1);

                     //Quantify derived values

                     m_max = lwa * Linf^lwb;  //maximum weight based on linf

                     l1  = TL;                                    // TL1, Total length of the fish at the moment
                     w1  = lwa * (l1^lwb);                        // conversion to weight in g
                     wd1 = w1 * w_prop;                           // conversion to dry weight in g

                     // Growth per day

                     if (TL < Linf){
                     a1  = log(1.0 - (l1/Linf))/(-k) + t0;          // Age1, Predicted age of the fish at length TL1
                     a2  = a1 + (1.0 / 365);                        // Age2, Age1 + 1 day
                     l2  = Linf * (1.0 - exp(-k * (a2 - t0)));      // TL2, Predicted total length at age 2
                     w2  = lwa * (l2^lwb);
                     wd2 = w2 * w_prop;
                     Wd  = wd2 - wd1;                             // Growth in dry weight
                     Ww  = w2 - w1;                               // Growth in wet weight
                     }
                     if (TL>= Linf){
                     // if bigger than linf, growth is zero
                     a1  = 100;  // arbitrary age, infinity
                     a2  = a1;
                     l2  = l1;
                     w2  = w1;
                     wd2 = wd1;
                     Wd = 0;
                     Ww = 0;
                     }

                     Qc1  = Qc * wd1 / 100;
                     Qn1  = Qn * wd1 / 100;
                     Qp1  = Qp * wd1 / 100;
                     C_g = Qc * Wd / 100;
                     N_g = Qn * Wd / 100;
                     P_g = Qp * Wd / 100;

                     // metabolism

                     	 Em       = exp(4.38 + 0.1032 * log(temp) + 0.73 * log(troph) + 0.41 * log(asp + 1.0));  //cost of growth in J/g

                     	 gC_to_J  = 39e3;                         // conversion factor
                     	 Ec       = 24e3;                         // combustion energy of biomass (Joules / g)
	                     Bm       = B0 * gC_to_J * m_max^(a - 1.0);
	                     B_main   = Bm * w1;                       // maintenance metabolic rate
	                     B_syn    = Em * Ww;                       // cost of growth
	                     B_rest   = B_main + B_syn;                // resting metabolic rate (Joules / day)
	                     B_tot    = B_rest * f;                    // assimilation rate in joule per day
	                     Cm       = B_tot / gC_to_J;               // amount of mass C needed for metabolism

                     // biomass turnover

                       N_t = Tn * Qn1;                            // N needed for cell renewal
                       P_t = Tp * Qp1;                            // P needed for cell renewal

                     // Needed ingestion of each element
                       N_n = (N_g + N_t) / AEn;
                       P_n = (P_g + P_t) / AEp;
                       C_n = (C_g + Cm) / AEc;

                     // Stoichiometry and determining limiting element
                      // needed nutrients
                       st_np = N_n / P_n;
                       st_cn = C_n / N_n;
                       st_cp = C_n / P_n;

                      // food
                       stf_np = Fn / Fp;
                       stf_cn = Fc / Fn;
                       stf_cp = Fc / Fp;

                     // limiting nutrients: C=1, N=2, P=3

                       if (st_cn > stf_cn && st_cp > stf_cp) {
                                lim = 1;
                       } else if  (st_cn < stf_cn && st_np > stf_np){
                                lim = 2;
                       } else {
                                lim = 3;
                       }

                     // ingestion, based upon limiting element

                       if (lim==3){                       // P limiting
                               P_in = P_n;
                               N_in = P_in * stf_np;
                               C_in = P_in * stf_cp;
                       } else if (lim==2){                // N limiting
                               N_in = N_n;
                               P_in = N_in / stf_np;
                               C_in = N_in * stf_cn;
                       } else{                            // C limiting
                               C_in = C_n;
                               P_in = C_in / stf_cp;
                               N_in = C_in / stf_cn;
                       }

                     // egestion
                       C_eg = C_in * (1-AEc);
                       N_eg = N_in * (1-AEn);
                       P_eg = P_in * (1-AEp);

                     // excretion
                       N_ex = N_in - N_eg - N_g;
                       P_ex = P_in - P_eg - P_g;

                     // respiration
                       C_r = C_in - C_eg - C_g;

                     // leftover excretion
                       N_l = N_ex - N_t;
                       P_l = P_ex - P_t;

                     // ingestion in dry weight
                       IN = C_in * 100 / Fc;
                       IN_cnp = C_in + N_in + P_in;

                     }
