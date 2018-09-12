#' A function to predict N and P excretion, CNP egestion, CNP ingestion rate, using MCMC and stan
#'
#' This function combines MTE and stoichiometric theory in order to predict nescessary ingestion and excretion processes. A probability distribution is obtained by including uncertainty of parameters and using MCMC sampling with stan.
#'
#' @param TL      total length of a fish in cm
#' @param param   list of all parameter means ("_m") and standard deviations ("_m") Default parameters are given with very low sd's. See \link[fishflux]{cnp_model}  for a list of all requested parameters
#' @param iter    A positive integer specifying the number of iterations. The default is 2000.
#' @details       Returns a list with three objects: A stanfit object, a dataframe with a summary of all model components and a vector containing the limiting element.
#' @keywords      fish, stoichiometry, excretion, mcmc
#' @export cnp_model_mcmc
#'
#' @examples
#'
#' model <- cnp_model_mcmc(TL = 5:10, param = list(C_m = 40, N_m = 10, P_m = 4))

cnp_model_mcmc <- function(TL, param , iter=1000){

  require(rstan)

  ##standard parameters, all sd's are quite low here!
  params_st <- list(TL_m = 10 ,
                    AEc_m = 0.8 ,
                    AEn_m = 0.8 ,
                    AEp_m = 0.7 ,
                    Fc_m = 2.5 ,
                    Fn_m = 0.3 ,
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
                    Tn_m = 0.0057,
                    Tp_m = 0.00016,
                    C_m = 40,
                    N_m = 10,
                    P_m = 4,
                    a_m = 0.8,
                    B0_m = 0.002,

                    TL_sd = 0.0001 ,
                    AEc_sd = 0.0005 ,
                    AEn_sd = 0.0005 ,
                    AEp_sd = 0.0005 ,
                    Fc_sd = 0.0005 ,
                    Fn_sd = 0.0005 ,
                    Fp_sd = 0.0005,
                    Linf_sd = 0.0000001,
                    k_sd = 0.0005,
                    t0_sd = 0.0001,
                    f_sd = 0.0005,
                    asp_sd = 0.0005,
                    troph_sd = 0.000002,
                    lwa_sd = 0.000001,
                    lwb_sd = 0.000005,
                    w_prop_sd = 0.0005,
                    temp_sd = 0.0005,
                    Tn_sd = 0.00000000000000001,
                    Tp_sd = 0.00000000000000001,
                    C_sd = 0.0005,
                    N_sd = 0.0005,
                    P_sd = 0.0005,
                    a_sd = 0.0005,
                    B0_sd = 0.0000005
  )


  if (missing(TL)){
    stop("please provide TL: total length")
    }

  ## mcmc function

  cnp_mcmc <- function(TL=TL, param = param, iter = iter){

  ## add TL to parameter list
  param[["TL_m"]] <- TL

  p_given <- names(param)
  p_all <- names(params_st)
  unknown <- p_all[!p_all%in% p_given]

  if (length(unknown>0)){
    warning("not inputting certain parameters may give wrong results")
    for (v in unknown){
      warning("adding standard values for ", v)
    }
  }

  params_missing <- params_st[which(p_all%in%unknown)]
  param <- append(param,params_missing)

  stanfit <-  rstan::sampling(stanmodels$cnp_model_mcmc, data=param, iter=iter, algorithm="Fixed_param", chains=1)

  result <- as.data.frame(rstan::summary(stanfit)$summary)
  result$variable <- rownames(result)
  result$TL_input <- TL

  ## limiting element
  lim <- round(result[result$variable=="lim","mean"])
  lim <- sapply(lim, FUN = function(x){
    if (x==1){
      return("C")
    } else if (x==2){
      return("N")
    } else{
      return("P")
    }
  })

  return(list(stanfit,summary = result, lim = lim))

  }


  if (length(TL) == 1){ ## option for only one length ##

  result <- cnp_mcmc(TL,param,iter)
  return(result)

  } else{ ## option for vector of lengths ##

    result <- parallel::mclapply(TL,param=param,iter=iter, FUN = cnp_mcmc)

    stanfit <-lapply(result,FUN = function(x){x[[1]]})
    summary <- lapply(result,FUN = function(x){x[[2]]})
    summary <- plyr::ldply(summary)

    ## limiting element
    lim <- round(summary[summary$variable=="lim","mean"])
    lim <- sapply(lim, FUN = function(x){
      if (x==1){
        return("C")
      } else if (x==2){
        return("N")
      } else{
        return("P")
      }
    })

    return(list(stanfit = stanfit, summary = summary, lim = lim))
  }

}

