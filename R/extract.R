#' A function to extract specific model output parameters from result
#'
#'
#'
#' @param mod     Output from cnp_mod_mcmc()
#' @param par     Character vector specifying which output parameter that should be returned.
#' Main model output parameters:
#' \itemize{
#' \item{C_n:} Minimal required carbon (g/day)
#' \item{N_n:} Minimal required nitrogen (g/day)
#' \item{P_n:} Minimal required phosphorus (g/day)
#' \item{C_g:} Carbon-specific growth rate (g/day)
#' \item{N_g:} Nitrogen-specific growth rate (g/day)
#' \item{P_g:} Phosphorus-specific growth rate (g/day)
#' \item{B_r:} Resting metabolic rate (J/d)
#' \item{B_m:} Maintenance metabolic rate (J/day)
#' \item{B_t:} Total field metabolic rate (J/day)
#' \item{C_m:} Total field metabolic rate (g/day) (~ B_t)
#' \item{T_n:} Nitrogen-specific maintenance rate (g/day)
#' \item{T_p:} Phosphorus-specific maintenance rate (g/day)
#' \item{C_in:} Ingestion rate of C (g/day)
#' \item{N_in:} Ingestion rate of N (g/day)
#' \item{P_in:} Ingestion rate of P (g/day)
#' \item{C_eg:} Egestion rate of C (g/day)
#' \item{N_eg:} Egestion rate of N (g/day)
#' \item{P_eg:} Egestion rate of P (g/day)
#' \item{C_r:} Total respiration rate (g/day)
#' \item{N_ex:} Excretion rate of N (g/day)
#' \item{P_ex:} Excretion rate of P (g/day)
#' }
#' @details       Returns a dataframe with a summary of the selected output parameters
#' @keywords      fish, stoichiometry, excretion, mcmc
#' @export extract
#'
#' @examples
#'
#' model <- fishflux::cnp_model_mcmc(TL = 5:10, param = list(C_m = 40, N_m = 10, P_m = 4))
#' fishflux::extract(model, c("N_ex","P_ex"))
#'
#'

extract <- function(mod, par){

  summary <- mod$summary
  TL <- unique(summary$TL)

  list <- lapply(par, FUN = function(p){
    sub <- dplyr::filter(summary, variable == p)
    sub <- data.frame(sub$mean, sub$median, sub$sd, sub$`Q_2.5`, sub$`Q_97.5`,  sub$`Q_25`,  sub$`Q_75` )
    colnames(sub) <- c(paste(p, "mean", sep = "_"),
                       paste(p, "median", sep = "_"),
                       paste(p, "sd", sep = "_"),
                       paste(p, "2.5%", sep = "_"),
                       paste(p, "97.5%", sep = "_"),
                       paste(p, "25%", sep = "_"),
                       paste(p, "75%", sep = "_"))
    return(sub)
    })

  df <- do.call(cbind, list)

  tl <- data.frame(TL = TL)
  df <- cbind(tl, df)
  return(df)
}
