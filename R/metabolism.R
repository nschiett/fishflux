#' A function to generate B0 and a
#'
#' All model parameters below were estimated by Barneche & Allen 2018 Ecology
#' Letters doi: 10.1111/ele.12947. These parameters are for the best model
#' (Model 2 in the paper online supplementary material) of fish resting
#' metabolic rates reported in the paper, which also includes trophic level as
#' a covariate.
#'
#' @param family family fish
#' @param temp  Temperature in degrees Celsius
#' @param troph_m Trophic level mean (from 1 to 5)
#' @param troph_sd Trophic level sd (optional)
#' @details     All model parameters below were estimated by Barneche & Allen 2018 Ecology Letters doi: 10.1111/ele.12947. These parameters are for the best model (Model 2 in the paper online supplementary material) of fish resting metabolic rates reported in the paper, which also includes trophic level as a covariate.
#' @keywords    Fish, metabolism
#' @export metabolism
#' @examples
#'
#' fishflux::metabolism(family= "Pomacentridae", temp = 27, troph_m = 2)


metabolism <- function (family, temp, troph_m, troph_sd=0.0000000001) {

  require(rstan)

  ## get b0 and a from database
  metpar <- fishflux::metpar
  metpars <- metpar[metpar$family==family,]

  if (nrow(metpars)>0){
  message("values for b0 and a on family level")
  }

  if (nrow(metpars)==0){
    metpars <- metpar[metpar$family=="all",]
    message("average values for b0 and a used")
  }

  metpars$troph_m = troph_m
  metpars$troph_sd = troph_sd
  metpars$temp = temp
  metpars <- as.list(metpars[,-1])

  ## predict B0
  stanfit <-  rstan::sampling(stanmodels$get_B0, data=metpars, iter=1000, algorithm="Fixed_param", chains=1)
  result <- as.data.frame(rstan::summary(stanfit)$summary)
  result <- result[1,c("mean","sd")]
  colnames(result) <- c("B0_m", "B0_sd")
  result <- cbind(result, metpars)
  result <- result[, ! names(result) %in% c("troph_m","troph_sd","temp")]
  rownames(result) <- NULL
  return(result)
}


