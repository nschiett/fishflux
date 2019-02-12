#' A function to extract growth parameters from otolith data
#'
#'
#' @param length Numerical vector with length
#' @param age    Numerical vector with age
#' @param id     Character vector with fish id
#' @param k_m    Prior for k, default is 0.5
#' @param linf_m Prior for linf
#' @param linf_min possibility to add a minimum for linf, default is 0
#' @param lmax   maximum size
#' @param ...     Additional arguments, see ?rstan::sampling()
#' @details      Returns a dataframe with estimates for Linf, k and t0
#' @keywords      fish, growth, Von Bertalanfy
#' @export oto_growth
#'
#' @examples
#'
#' sarmi <- fishflux::sarmi
#' fishflux::oto_growth(length = sarmi$length, age = sarmi$age, id = sarmi$id, lmax = 20, linf_m = 16, k_m = 0.4, iter = 2000, chains = 1)
#'



oto_growth <- function(length, age, id, linf_min = 0, lmax = 20, linf_m, k_m = 0.5, ...){

require(ggplot2)
require(rstan)


data <- list(
  N = length(length),
  J = length(unique(id)),
  l = length,
  age = age,
  ind = as.integer(as.factor(id)),
  linf_min = linf_min,
  linf_m = linf_m,
  lmax = lmax,
  age_max = sapply(unique(id), function(x){max(age[which(id == x)] , na.rm = TRUE)})
)

fit <- rstan::sampling(stanmodels$vonbert, data = data, ...)

summary <-  as.data.frame(rstan::summary(fit)$summary)

result <- summary[c("mu_k", "mu_linf", "t0", "kmax"),1:8]

ee <- rstan::extract(fit)
ypred <- ee$y_m
pred <- data.frame(
  age = age,
  ypred_m = apply(ypred,2,mean),
  ypred_lb = apply(ypred,2,quantile, 0.025),
  ypred_ub = apply(ypred,2,quantile, 0.975)
)


plot <-
  ggplot() +
  geom_point(aes(x = age, y = length)) +
  geom_ribbon(aes(x = age, ymin = ypred_lb, ymax = ypred_ub), alpha = 0.4, data = pred) +
  geom_line(aes(x = age, y = ypred_m), data = pred) +
  theme_bw()

print(plot)

return(list(summary = result, stanfit = fit))

}
