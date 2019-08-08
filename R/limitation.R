#' A function to evaluate element limitation of the model
#'
#' This function allows you extract the proportions of the iterations for which c, n and p are the limiting element in the model.
#' @param mod Model output from cnp_model_mcmc().
#' @param plot Argument to specify if results should be shown in a plot.
#' @details Returns a data frame with:
#' \describe{
#'   \item{tl}{Total length, in cm}
#'   \item{nutrient}{c, n or p}
#'   \item{prop_lim}{the proportion of iterations for which there is limitation by the element}
#' }
#' @keywords fish, plot, limitation
#' @import ggplot2
#' @import purrr
#' @export limitation
#' @examples
#' mod <- fishflux::cnp_model_mcmc(TL = 5:15, param = list(Qc_m = 40, Qn_m = 10, Qp_m = 4,
#'                                                         Dc_sd = 0.1, Dn_sd = 0.05, Dp_sd = 0.05))
#' limitation(mod)


limitation <- function(mod, plot = TRUE){

  requireNamespace("ggplot2")
  requireNamespace("purrr")

  lim <- lapply(mod$stanfit, function(x){
    ee <- rstan::extract(x,"lim")[[1]]
    c <- length(which(ee==1))/length(ee)
    n <- length(which(ee==2))/length(ee)
    p <- length(which(ee==3))/length(ee)
    return(data.frame(c = c,
                      n = n,
                      p = p))
  }) %>%

    dplyr::bind_rows() %>%
    dplyr::mutate(tl = unique(mod$summary$TL)) %>%

    tidyr::gather("nutrient", "prop_lim", - tl)

  if (plot){
  p <- ggplot(lim) +
    geom_point(aes(x = tl, y = prop_lim, color = nutrient)) +
    geom_line(aes(x = tl, y = prop_lim, color = nutrient)) +
    labs(x = "TL (cm)", y = "Proportion of iterations", color = "Limiting element") +
    theme_bw() +
    theme(legend.position = "bottom")
  print(p)
  }

  return(lim)

}




