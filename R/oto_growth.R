#' A function to extract growth parameters from otolith data
#'
#'
#' @param length Numerical vector with length
#' @param age    Numerical vector with age
#' @param id     Character vector with fish id
#' @param linf_m Prior for linf
#' @param lmax   maximum size. Based on this value, maximum growth rate kmax will be computed.
#' @param plot   option to plot model fit (TRUE or FALSE)
#' @param ...     Additional arguments, see ?rstan::sampling()
#' @details      Returns a dataframe with estimates for linf, k and t0, sl and gp.
#' There is a hierarchical structure for linf and k, so that there is a unique estimate for these parameters per individual (linf_j, k_j).
#' linf and k are the population level estimates of linf and k. kmax is the standardised growth parameter, depending on lmax
#' (kmax = exp(sl * log(lmax) + gp), see Morais and Bellwood (2018) for details)
#'
#' @keywords      fish, growth, Von Bertalanfy
#' @import ggplot2
#' @import rstan
#' @export oto_growth
#'
#' @examples
#'
#' zs <- fishflux::zebsco_growth
#' fishflux::oto_growth(length = zs$length, age = zs$age, id = zs$id, lmax = 25, linf_m = 15, iter = 2000, chains = 1)
#'



oto_growth <- function(length, age, id, lmax = 20, linf_m, plot = TRUE, ...){

requireNamespace("ggplot2")
requireNamespace("rstan")


data <- list(
  N = length(length),
  N_1 = length(unique(id)),
  y = length,
  x = age,
  J = as.integer(as.factor(as.character(id))),
  linf_prior = linf_m,
  lmax = lmax,
  X = rep(1, length(length))
)

fit <- rstan::sampling(stanmodels$vonbert, data = data, ...)

summary <-  as.data.frame(rstan::summary(fit)$summary)

result <- summary[c("k", "linf", "t0", "sl", "gp", "kmax"),1:8]

ee <- rstan::extract(fit)
y_m <- ee$y_m
y_rep <- ee$y_rep
pred <- data.frame(
  age = age,
  ypred_m = apply(y_m,2,mean),
  ypred_lq = apply(y_m,2,quantile, 0.025),
  ypred_uq = apply(y_m,2,quantile, 0.975),
  yrep_m = apply(y_rep,2,mean),
  yrep_lq = apply(y_rep,2,quantile, 0.025),
  yrep_uq = apply(y_rep,2,quantile, 0.975)
)

if(plot){
p <-
  ggplot() +
  geom_point(aes(x = age, y = length)) +
  geom_ribbon(aes(x = age, ymin = ypred_lq, ymax = ypred_uq), alpha = 0.4, data = pred) +
  geom_line(aes(x = age, y = ypred_m), data = pred) +
  theme_bw()

print(p)
}
return(list(summary = result, fitted = pred, stanfit = fit))

}
