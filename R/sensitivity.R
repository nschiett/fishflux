#' A function to check the sensitivity of cnp_model predictions based on the variation of input parameters
#'
#' This function runs the cnp_model fixing all parameters SD's but one to test for sensitivity
#'
#' @param TL      total length of a fish in cm
#' @param param   list of all parameter means ("_m") and standard deviations ("_sd") Default parameters are set with very low sd's. See \link[fishflux]{cnp_model}  for a list of all requested parameters
#' @param iter    A positive integer specifying the number of iterations. The default is 1000
#' @param par     Charachter vector specifying which input parameter sd's should be used for sensitivity.
#' @param out     Charachter vector specifying which output parameter sd's should be returned.
#' @details       Returns a dataframe with sd's of model predictions. Row names indicate the variable, who's sd was used for the model run.
#'                Plots a heatplot with sd values for predictions.
#' @keywords      fish, stoichiometry, excretion, mcmc
#' @export sensitivity
#'
#' @examples
#'
#' fishflux::sensitivity(TL=10, param = list(k_sd=0.2, Fn_sd=0.2, Fc_sd = 0.1), par = c("k_sd","Fn_sd","Fc_sd"), out = c("C_in", "N_in", "P_in", "C_g", "N_g", "P_g", "C_r", "N_ex", "P_ex", "C_eg", "N_eg", "P_eg", "IN"))

sensitivity <- function(TL, param, iter = 1000, par,
                        out = c("C_in", "N_in", "P_in", "C_g",
                                "N_g", "P_g", "C_r", "N_ex",
                                "P_ex", "C_eg", "N_eg", "P_eg", "IN"), ...){

require(ggplot2)

  #parameter SD's and means
  pm <- c("TL_m", "AEc_m", "AEn_m", "AEp_m", "Fc_m",
          "Fn_m", "Fp_m", "Linf_m", "k_m", "t0_m",
          "f_m", "asp_m", "troph_m", "lwa_m", "lwb_m",
          "w_prop_m", "temp_m", "Tn_m", "Tp_m", "Qc_m",
          "Qn_m", "Qp_m", "a_m", "B0_m")
  psd <- c("TL_sd", "AEc_sd", "AEn_sd", "AEp_sd",
           "Fc_sd", "Fn_sd", "Fp_sd", "Linf_sd",
           "k_sd", "t0_sd", "f_sd", "asp_sd", "troph_sd",
           "lwa_sd", "lwb_sd", "w_prop_sd", "temp_sd",
           "Tn_sd", "Tp_sd", "Qc_sd", "Qn_sd", "Qp_sd", "a_sd", "B0_sd")

  parm <- par[par %in% pm]
  parsd <- par[par %in% psd]

  param_m <- param[parm]
  param_sd <- param[parsd]

  sd_low <- 0.000000001

  param_sdl <- param_sd
  param_sdl[1:length(parsd)] <- sd_low
  param_msdl <- append(param_m, param_sdl)

  #run cnp_model for all sd's with rest very low
  sd <- parsd
  res_sd <- as.data.frame(
    parallel::mcmapply(sd, FUN = function(sd){
    param_msdl[sd] <- param_sd[sd]
    mod <- fishflux::cnp_model_mcmc(TL, param_msdl, iter, ...)$summary
    ext <- mod[match(out, mod$variable), "Q_97.5"] - mod[match(out, mod$variable), "Q_2.5"]
    return(ext)
  }))

  row.names(res_sd) <- sapply(out, function(x){
    rn <- paste(x, "_CI", sep = "")
    return(rn)
    }
    )

  res_sd <- as.data.frame(t(res_sd))

  #plot
  require(ggplot2)
  res <- res_sd
  res$input_sd <- row.names(res)
  res <- tidyr::gather(res, key, value, -input_sd)
  plot <- ggplot(res) +
    geom_tile(aes(x = key, y = input_sd, fill = value)) +
    scale_fill_continuous(trans = "log") +
    geom_text(aes(x = key, y = input_sd, label = formatC(value, format = "e", digits = 1))) +
    labs(x = "", y = "", fill = "Width 95% CI") +
    theme_bw()
  print(plot)

  return(res_sd)

}
