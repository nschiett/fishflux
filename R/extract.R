#' A function to extract specific model output parameters from result
#'
#'
#'
#' @param mod     Output from cnp_mod_mcmc()
#' @param par     Character vector specifying which output parameter that should be returned
#' @details       Returns a dataframe with a summary of the selected output parameters
#' @keywords      fish, stoichiometry, excretion, mcmc
#' @export extract
#'
#' @examples
#'
#' model <- fishflux::cnp_model_mcmc(TL = 5:10, param = list(C_m = 40, N_m = 10, P_m = 4, f=3))
#' extract(model, c("N_ex","P_ex"))
#'
#'

extract <- function(mod, par){

  summary <- mod$summary
  TL <- unique(summary$TL_input)

  list <- lapply(par, FUN = function(p){
    name <- paste(p,"s",sep="_")
    sub <- dplyr::filter(summary, variable == p)
    sub <- data.frame(sub$mean, sub$sd, sub$`2.5%`, sub$`97.5%`)
    colnames(sub) <- c(paste(p,"mean",sep="_"), paste(p,"sd",sep="_"), paste(p,"2.5%",sep="_"), paste(p,"97.5%",sep="_"))
    return(sub)
    })

  df <- do.call(cbind, list)

  tl <- data.frame(TL=TL)
  df <- cbind(tl,df)
  return(df)
}





