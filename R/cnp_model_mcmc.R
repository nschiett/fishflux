#' A function to predict N and P excretion, CNP egestion, CNP ingestion rate, using MCMC and stan
#'
#' This function combines MTE and stoichiometric theory in order to predict nescessary ingestion and excretion processes. A probability distribution is obtained by including uncertainty of parameters and using MCMC sampling with stan.
#'
#' @param TL      total length of a fish in cm
#' @param param   list of all parameter means ("_m") and standard deviations ("_sd") Default parameters are set with very low sd's. See \link[fishflux]{cnp_model}  for a list of all requested parameters
#' @param iter    A positive integer specifying the number of iterations. The default is 2000.
#' @param ...     Arguments of rstan::sampling()
#' @details       Returns a list with three objects: A stanfit object, a dataframe with a summary of all model components and a vector containing the limiting element.
#' @keywords      fish, stoichiometry, excretion, mcmc
#' @export cnp_model_mcmc
#'
#' @examples
#'
#' model <- fishflux::cnp_model_mcmc(TL = 5:10, param = list(C_m = 40, N_m = 10, P_m = 4, f_m = 3))

cnp_model_mcmc <- function(TL, param, iter=1000, ...){

  require(rstan)

  #check input variable names
  if (TRUE %in% (!names(param) %in% names(params_st))){
    wrong <- names(param)[!(names(param) %in% names(params_st))]
    error <- paste("The following input parameters do not exist: ", paste(wrong, collapse = ", "),
                   "  Check ?fishflux::cnp_model_mcmc for a description of valid input parameters")
    stop(error)
  }

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
                    C_m = 40,
                    N_m = 10,
                    P_m = 4,
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
                    C_sd = 0.0000000001,
                    N_sd = 0.0000000001,
                    P_sd = 0.0000000001,
                    a_sd = 0.0000000001,
                    B0_sd = 0.0000000001
  )


  if (missing(TL)){
    stop("please provide TL: total length")
  }

  # check for bad parameter inputs

  if ("Linf" %in% names(param)){
   if (param$TL_m >= param$Linf_m){
     stop("TL_m cannot be higher than Linf_m")
   }
  }

  if ("C_m" %in% names(param)){
    print("ok")
    if (param$C_m <= 0 | param$C_m >= 100){
      stop("C_m should be between 0 and 100")
      print("a")
   }
  }

  if ("N_m" %in% names(param)){
    if (param$N_m <= 0 | param$N_m >= 100){
      stop("N_m should be between 0 and 100")
    }
  }

  if ("P_m" %in% names(param)){
    if (param$P_m <= 0 | param$P_m >= 100){
      stop("P_m should be between 0 and 100")
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




  stanfit <-  rstan::sampling(stanmodels$cnp_model_mcmc, data = param,
                              iter = iter, algorithm = "Fixed_param", chains = 1, ...)

  result <- as.data.frame(rstan::summary(stanfit)$summary)
  result$variable <- rownames(result)
  result$TL_input <- TL

  ## limiting element
  lim <- round(result[result$variable == "lim", "mean"])
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
    lim <- round(summary[summary$variable == "lim", "mean=fdw"])
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
