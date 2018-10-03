#' A function to predict N and P excretion, egestion, ingestion rate, etc
#'
#' This function combines MTE and stoichiometric theory in order to predict
#' necessary ingestion and excretion processes.
#'
#' Returns a list, containing a dataframe with with all model components
#' (result), specifications of metabolism, and the limiting element.
#'
#' @param TL total length of a fish in cm
#' @param AEn N-specific assimilation efficiency
#' @param AEp P-specific assimilation efficiency
#' @param AEc C-specific assimilation efficiency
#' @param Fn N content of food in percent
#' @param Fp P content of food in percent
#' @param Fc C content of food in percent
#' @param t0 Von Bertalanffy growth parameter
#' @param Linf Von Bertalanffy growth parameter (Should be higher than largest
#' fish)
#' @param k Von Bertalanffy growth parameter
#' @param f activity scope. Ranges from 1 to 4
#' @param asp aspect ratio of caudal fin
#' @param troph trophic level
#' @param lwa,lwb parameters for length-weight relationship
#' @param w_prop ratio between dry weight and wet weight of fish
#' @param temp temperature in degrees celcius
#' @param Tn,Tp N,P specific biomass turnover rates
#' @param N,C,P N,C,P content of fish in percent
#' @param a Resting metabolic rate mass-scaling exponent
#' @param B0 Constant for resting metabolic rate. If NA, function will
#' calculate an average.
#' @keywords excretion fish, stoichiometry,
#' @examples
#'
#'
#' sp <- "Scarus psittacus"
#' para <- fishflux::model_parameters(sp,"Scaridae",temp = 27)
#' para
#' test <- fishflux::cnp_model(TL = 10:19,Fn = 1.3,Fp = 0.1,Fc = 65,
#'                t0 = para$t0, Linf = para$Linf,k = para$k,
#'                asp = para$asp,troph = para$troph,f = 3,w_prop = para$w_prop,
#'                lwa = para$lwa_m,lwb = para$lwb_m,temp = 27,N = 11,P = 3,C = 39, a = para$a_m, B0 = para$B0_m)
#'
#' @export cnp_model
cnp_model <- function(TL, AEn = 0.8, AEp = 0.7, AEc = 0.8,
                      Fn, Fp, Fc, t0, Linf, k, f, asp, troph,
                      lwa, lwb, w_prop = 0.27, temp = 27, Tn = 0.0057,
                      Tp = 0.00016, N, C, P, a = 0.77, B0){

  #Maximum weight
  m_max  <- lwa * (Linf ^ lwb)

  #Growth per day
  l1  <- TL                                     #TL1, Total length of the fish
  a1  <- (log(1 - (l1 / Linf)) / (-k)) + t0     #Age1, Predicted age of the fish at length TL1
  a2  <- a1 + (1 / 365)                         #Age2, Age1 + 1 day
  l2  <- Linf * (1 - exp(-k * (a2 - t0)))       #TL2, Predicted total length at age 2
  w1  <- lwa * (l1 ^ lwb)                         #conversion to weight in g
  w2  <- lwa * (l2 ^ lwb)
  wd1 <- w1 * w_prop                            #conversion to dry weight in g
  wd2 <- w2 * w_prop
  Wd  <- wd2 - wd1                              #Growth in dry weight
  Ww  <- w2 - w1                                #Growth in wet weight

  N1  <- N * wd1 / 100                      #mass N of fish in g
  P1  <- P * wd1 / 100                      #mass P of fish in g
  N_g <- N * Wd / 100                       #N needed for growth in g
  P_g <- P * Wd / 100                       #P needed for growth in g
  C_g <- C * Wd / 100                       #C needed for growth in g

  #metabolism
  metabolism <- fishflux::metabolic_rate(temp = temp, m_max = m_max, m = w1,
                                         asp = asp, troph = troph, f = f,
                                         B0 = B0, a = a, growth_g_day = Ww)
  C_m <- metabolism$Total_metabolic_rate_C_g_d

  #Biomass turnover
  N_t <- Tn * N1                #N needed for cell renewal
  P_t <- Tp * P1                #P needed for cell renewal

  #Needed ingestion of each element
  N_n <- (N_g + N_t) / AEn
  P_n <- (P_g + P_t) / AEp
  C_n <- (C_g + C_m) / AEc

  #Stoichiometry and determining limiting element
    ##needed nutrients
    st_np <- N_n / P_n
    st_cn <- C_n / N_n
    st_cp <- C_n / P_n

    ##food
    stf_np <- Fn / Fp
    stf_cn <- Fc / Fn
    stf_cp <- Fc / Fp

    ##limiting nutrients
    x <- 1:length(l1)
    lim <- sapply(x, FUN = function(i){
      if (st_cn[i] > stf_cn & st_cp[i] > stf_cp){
        lim <- "C"
      } else if  (st_cn[i] < stf_cn & st_np[i] > stf_np){
        lim <- "N"
      } else{
        lim <- "P"
      }
      return(lim)
    })

  #Ingestion according to limiting element
  ing <- lapply(x, FUN = function(i){
    if (lim[i] == "P"){
      I_P <- P_n[i]
      I_N <- I_P * stf_np
      I_C <- I_P * stf_cp
    } else if (lim[i] == "N"){
      I_N <- N_n[i]
      I_P <- I_N / stf_np
      I_C <- I_N * stf_cn
    } else{
      I_C <- C_n[i]
      I_P <- I_C / stf_cp
      I_N <- I_C / stf_cn
    }
    return(data.frame(I_C = I_C, I_N = I_N, I_P = I_P))
  }
  )
  ing <- plyr::ldply(ing)
  C_in <- ing$I_C
  P_in <- ing$I_P
  N_in <- ing$I_N

  #excretion
  C_eg <- C_in * (1 - AEc)
  N_eg <- N_in * (1 - AEn)
  P_eg <- P_in * (1 - AEp)

  #egestion
  N_ex <- N_in - N_eg - N_g
  P_ex <- P_in - P_eg - P_g

  #respiration
  C_r <- C_in - C_eg - C_g

  #leftover excretion
  N_l <- N_ex - N_t
  P_l <- P_ex - P_t

  #ingestion in dry weight
  IN <-  C_in * 100 / Fc
  IN_cnp <-  C_in + N_in + P_in


  result <- data.frame(TL, biomass = w1, C_in, N_in, P_in, N_ex,
                        N_eg, P_ex, P_eg, N_l, P_l, C_m, C_g, N_g,
                        P_g, N_t, P_t, C_eg, C_r, IN, IN_cnp, lim)
  return(result)
}
