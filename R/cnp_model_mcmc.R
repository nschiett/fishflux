#' A function to predict N and P excretion, CNP egestion, CNP ingestion rate, using MCMC and stan
#'
#' This function combines MTE and stoichiometric theory in order to predict nescessary ingestion and excretion processes. A probability distribution is obtained by including uncertainty of parameters and using MCMC sampling with stan.
#'
#' @param TL      total length(s) in cm
#' @param param   list of all parameter means (add "_m") and standard deviations (add "_sd") Default parameters are set with very low sd's.
#' parameters:
#' \itemize{
#' \item{Qc_m, Qc_sd:} percentage C of dry mass fish
#' \item{Qn_m, Qn_sd:} percentage N of dry mass fish
#' \item{Qp_m, Qp_sd:} percentage P of dry mass fish
#' \item{Fc_m, Fc_sd:} percentage C of dry mass food
#' \item{Fn_m, Fn_sd:} percentage N of dry mass food
#' \item{Fp_m, Fp_sd:} percentage P of dry mass food
#' \item{AEc_m, AEc_sd:} C-specific assimilation efficiency
#' \item{AEn_m, AEn_sd:} N-specific assimilation efficiency
#' \item{AEp_m, AEp_sd:} P-specific assimilation efficiency
#' \item{Linf_m, Linf_sd:} Von Bertalanffy Growth parameter, theoretical maximum size in TL
#' \item{k_m, k_sd:} Von Bertalanffy Growth parameter, growth rate
#' \item{lwa_m, lwa_sd:} Parameter length-weight relationship
#' \item{lwb_m, lwb_sd:} Parameter length-weight relationship
#' \item{w_prop_m, wprop_sd:} Ratio between dry weight and wet weight of fish
#' \item{Tn_m, Tn_sd:} N-specific turnover rate
#' \item{Tp_m, Tp_sd:} P-specific turnover rate
#' \item{B0_m, B0_sd:} Normalization constant for resting metabolic rate
#' \item{a_m, a_sd:} Resting metabolic rate mass-scaling exponent
#' \item{f_m, f_sd:} Activity scope
#' \item{asp_m, asp_sd:} Aspect ratio
#' \item{troph_m, troph_sd:} Trophic level
#' \item{temp_m, temp_sd:} Temperature
#' }
#' @param cor     A list of correlations between certain parameters: ro_Qc_Qn, ro_Qc_Qp, ro_Qn_Qp,
#' ro_Fc_Fn, ro_Fc_Fp, ro_Fn_Fp, ro_lwa_lwb, ro_a_B0
#' @param iter    A positive integer specifying the number of iterations. The default is 2000.
#' @param ...     Arguments of rstan::sampling()
#'
#' @details     Returns a list with two objects: A stanfit object and a dataframe with a summary of all model components.
#'  See fishflux::extract() to extract a summary of predicted variables and
#'  fishflux::limitation() to get information on the limiting element.
#' @keywords    fish, stoichiometry, excretion, mcmc
#' @export cnp_model_mcmc
#'
#' @examples
#'
#' model <- fishflux::cnp_model_mcmc(TL = 10, param = list(Qc_m = 40, Qn_m = 10, Qp_m = 4, f_m = 3))

cnp_model_mcmc <- function(TL, param, iter=1000,
                           cor = list(ro_Qc_Qn = 0.5, ro_Qc_Qp = -0.3, ro_Qn_Qp = -0.2,
                                      ro_Fc_Fn = 0.2, ro_Fc_Fp = -0.1, ro_Fn_Fp = -0.1,
                                      ro_lwa_lwb = 0.9, ro_a_B0 = 0.9), ...){

  require(rstan)

  ##standard parameters, all sd's are quite low here!
  params_st <- list(TL_m = 10,
                    AEc_m = 0.8,
                    AEn_m = 0.8,
                    AEp_m = 0.7,
                    Fc_m = 2.5,
                    Fn_m = 0.3,
                    Fp_m = 0.1,
                    Linf_m = 20,
                    k_m = 0.4,
                    t0_m = 0,
                    f_m = 2,
                    asp_m = 2,
                    troph_m = 2,
                    lwa_m = 0.0137,
                    lwb_m = 3.083,
                    w_prop_m = 0.309,
                    temp_m = 27,
                    Tn_m = 0.01,
                    Tp_m = 0.0007,
                    Qc_m = 40,
                    Qn_m = 10,
                    Qp_m = 4,
                    a_m = 0.8,
                    B0_m = 0.002,

                    TL_sd = 0.0000000001,
                    AEc_sd = 0.0000000001,
                    AEn_sd = 0.0000000001,
                    AEp_sd = 0.0000000001,
                    Fc_sd = 0.0000000001,
                    Fn_sd = 0.0000000001,
                    Fp_sd = 0.0000000001,
                    Linf_sd = 0.0000000001,
                    k_sd = 0.0000000001,
                    t0_sd = 0.0000000001,
                    f_sd = 0.0000000001,
                    asp_sd = 0.0000000001,
                    troph_sd = 0.0000000001,
                    lwa_sd = 0.0000000001,
                    lwb_sd = 0.0000000001,
                    w_prop_sd = 0.0000000001,
                    temp_sd = 0.0000000001,
                    Tn_sd = 0.0000000001,
                    Tp_sd = 0.0000000001,
                    Qc_sd = 0.0000000001,
                    Qn_sd = 0.0000000001,
                    Qp_sd = 0.0000000001,
                    a_sd = 0.0000000001,
                    B0_sd = 0.0000000001
  )

  #check input variable names
  if (TRUE %in% (!names(param) %in% names(params_st))){
    wrong <- names(param)[!(names(param) %in% names(params_st))]
    error <- paste("The following input parameters do not exist: ", paste(wrong, collapse = ", "),
                   "  Check ?fishflux::cnp_model_mcmc for a description of valid input parameters")
    stop(error)
  }


  if (missing(TL)){
    stop("please provide TL: total length")
  }



  if ("Qc_m" %in% names(param)){
    if (param$Qc_m <= 0 | param$Qc_m >= 100){
      stop("Qc_m should be between 0 and 100")
   }
  }

  if ("Qn_m" %in% names(param)){
    if (param$Qn_m <= 0 | param$Qn_m >= 100){
      stop("Qn_m should be between 0 and 100")
    }
  }

  if ("Qp_m" %in% names(param)){
    if (param$Qp_m <= 0 | param$Qp_m >= 100){
      stop("Qp_m should be between 0 and 100")
    }
  }

  if ("Fc_m" %in% names(param)){
    if (param$Fc_m <= 0 | param$Fc_m >= 100){
      stop("Fc_m should be between 0 and 100")
    }
  }

  if ("Fn_m" %in% names(param)){
    if (param$Fn_m <= 0 | param$Fn_m >= 100){
      stop("Fn_m should be between 0 and 100")
    }
  }

  if ("Fp_m" %in% names(param)){
    if (param$Fp_m <= 0 | param$Fp_m >= 100){
      stop("Fp_m should be between 0 and 100")
    }
  }

  if ("AEc_m" %in% names(param)){
    if (param$AEc_m <= 0 | param$AEc_m >= 1){
      stop("AEc_m should be between 0 and 1")
    }
  }

  if ("Fp_m" %in% names(param)){
    if (param$AEn_m <= 0 | param$AEn_m >= 1){
      stop("AEn_m should be between 0 and 1")
    }
  }

  if ("AEp_m" %in% names(param)){
    if (param$AEp_m <= 0 | param$AEp_m >= 1){
      stop("AEp_m should be between 0 and 1")
    }
  }

  ## mcmc function

  cnp_mcmc <- function(TL=TL, param = param, iter = iter, ...){

  ## add TL to parameter list
  param[["TL_m"]] <- TL

  p_given <- names(param)
  p_all <- names(params_st)
  unknown <- p_all[!p_all %in% p_given]

  if (length(unknown > 0)){
    warning("not inputting certain parameters may give wrong results")
    for (v in unknown){
      warning("adding standard values for ", v)
    }
  }

  params_missing <- params_st[which(p_all %in% unknown)]
  param <- append(param, params_missing)
  param <- append(param, cor)


  ##add others plus replace Qcnp

  stanfit <-  rstan::sampling(stanmodels$cnp_model_mcmc, data = param,
                              iter = iter, algorithm = "Fixed_param", chains = 1, ...)

  ee <- rstan::extract(stanfit)
  par <- names(ee)

  result <-
    plyr::ldply(lapply(par, function(par){
      summary <-
        data.frame(
          TL = mean(ee[["TL"]]),
          variable = par,
          mean = mean(ee[[par]]),
          median = median(ee[[par]]),
          se = sd(ee[[par]])/sqrt(length(ee[[par]])),
          sd = sd(ee[[par]]),
          Q_2.5 = quantile(ee[[par]], 0.025),
          Q_97.5 = quantile(ee[[par]], 0.975),
          Q_25 = quantile(ee[[par]], 0.25),
          Q_75 = quantile(ee[[par]], 0.75))
      return(summary)
    }))

  ## limiting element
  lim <- result[result$variable == "lim", "mean"]
  lim <- round(lim)
  lim <- sapply(lim, FUN = function(x){
    if (x == 1){
      return("C")
    } else if (x == 2){
      return("N")
    } else{
      return("P")
    }
  })

  return(list(stanfit, summary = result, lim = lim))

  }


  if (length(TL) == 1){ ## option for only one length ##
  result <- cnp_mcmc(TL, param, iter)
  return(result)

  } else{ ## option for vector of lengths ##
    result <- parallel::mclapply(TL, param = param, iter = iter, FUN = cnp_mcmc)

    stanfit <- lapply(result, FUN = function(x){x[[1]]})
    summary <- lapply(result, FUN = function(x){x[[2]]})
    summary <- plyr::ldply(summary)

    ## limiting element
    lim <- round(summary[summary$variable == "lim", "mean"])
    lim <- sapply(lim, FUN = function(x){
      if (x == 1){
        return("C")
      } else if (x == 2){
        return("N")
      } else{
        return("P")
      }
    })
    return(list(stanfit = stanfit, summary = summary, lim = lim))
  }
}
