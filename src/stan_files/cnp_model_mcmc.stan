
functions {
real normal_lb_ub_rng(real mu, real sigma, real lb, real ub) {
    real p1 = normal_cdf(lb, mu, sigma);  // cdf with lower bound
    real p2 = normal_cdf(ub, mu, sigma);  // cdf with upper bound
    real u = uniform_rng(p1, p2);
    return (sigma * inv_Phi(u)) + mu;  // inverse cdf
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
                     real C_m;
                     real N_m;
                     real P_m;
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
                     real C_sd;
                     real N_sd;
                     real P_sd;
                     real a_sd;
                     real B0_sd;

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
                     real<lower=(TL+1)> Linf;
                     real<lower=0.001> k;
                     real t0;
                     real<lower=1,upper=4> f;
                     real<lower=0> asp;
                     real<lower=0> troph;
                     real<lower=0.001> lwa;
                     real<lower=0.001> lwb;
                     real<lower=0.001> w_prop;
                     real<lower=0> temp;
                     real<lower=0> Tn;
                     real<lower=0> Tp;
                     real<lower=0> C;
                     real<lower=0> N;
                     real<lower=0> P;
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

                     real C1;      // mass C of fish at l1 in g
                     real N1;      // mass N of fish at l1 in g
                     real P1;      // mass P of fish at l1 in g
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

                      TL = normal_lb_ub_rng(TL_m, TL_sd, 0, 1000);
                      AEc = normal_lb_ub_rng(AEc_m, AEc_sd, 0, 1);
                      AEn = normal_lb_ub_rng(AEn_m, AEn_sd, 0, 1);
                      AEp = normal_lb_ub_rng(AEp_m, AEp_sd, 0, 1);
                      Fc = normal_lb_ub_rng(Fc_m, Fc_sd, 0, 100);
                      Fn = normal_lb_ub_rng(Fn_m, Fn_sd, 0, 100);
                      Fp = normal_lb_ub_rng(Fp_m, Fp_sd, 0, 100);
                      Linf = normal_lb_ub_rng(Linf_m, Linf_sd, 0, 1000);
                      k = normal_lb_ub_rng(k_m, k_sd,0,3);
                      t0 = normal_rng(t0_m, t0_sd);
                      f = normal_lb_ub_rng(f_m, f_sd,1, 4);
                      asp = normal_lb_ub_rng(asp_m, asp_sd,0, 8);
                      troph = normal_lb_ub_rng(troph_m, troph_sd,1, 5);
                      lwa = normal_lb_ub_rng(lwa_m, lwa_sd, 0, 1);
                      lwb = normal_lb_ub_rng(lwb_m, lwb_sd, 1, 5);
                      w_prop = normal_lb_ub_rng(w_prop_m, w_prop_sd, 0, 1);
                      temp = normal_rng(temp_m, temp_sd);
                      Tn = normal_lb_ub_rng(Tn_m, Tn_sd, 0, 1);
                      Tp = normal_lb_ub_rng(Tp_m, Tp_sd, 0, 1);
                      C = normal_lb_ub_rng(C_m, C_sd, 0, 100);
                      N = normal_lb_ub_rng(N_m, N_sd, 0, 100);
                      P = normal_lb_ub_rng(P_m, P_sd, 0, 100);
                      a = normal_lb_ub_rng(a_m, a_sd, 0.2, 1.2);
                      B0 = normal_lb_ub_rng(B0_m, B0_sd, 0, 1);


                     //Quantify derived values

                     m_max = lwa * Linf^lwb;  //maximum weight based on linf

                     // Growth per day

                     l1  = TL;                                    // TL1, Total length of the fish at the moment
                     a1  = log(1.0 - (l1/Linf))/(-k) + t0;          // Age1, Predicted age of the fish at length TL1
                     a2  = a1 + (1.0 / 365);                        // Age2, Age1 + 1 day
                     l2  = Linf * (1.0 - exp(-k * (a2 - t0)));      // TL2, Predicted total length at age 2
                     w1  = lwa * (l1^lwb);                        // conversion to weight in g
                     w2  = lwa * (l2^lwb);
                     wd1 = w1 * w_prop;                           // conversion to dry weight in g
                     wd2 = w2 * w_prop;
                     Wd  = wd2 - wd1;                             // Growth in dry weight
                     Ww  = w2 - w1;                               // Growth in wet weight
                     // Correct possible negative growth
                     if (Wd < 0) {
                       Wd = 0;
                     }
                     if (Ww < 0) {
                       Ww = 0;
                     }

                     C1  = C * wd1 / 100;
                     N1  = N * wd1 / 100;
                     P1  = P * wd1 / 100;
                     N_g = N * Wd / 100;
                     P_g = P * Wd / 100;
                     C_g = C * Wd / 100;



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

                       N_t = Tn * N1;                            // N needed for cell renewal
                       P_t = Tp * P1;                            // P needed for cell renewal

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
