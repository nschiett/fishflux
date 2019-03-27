#' A function to extract growth parameters from otolith data
#'
#'
#' @param length Numerical vector with length
#' @param age    Numerical vector with age
#' @param id     Character vector with fish id
#' @param linf_m Prior for linf
#' @param linf_min possibility to add a minimum for linf, default is 0
#' @param lmax   maximum size. Based on this value, maximum growth rate kmax will be computed.
#' @param plot   option to plot model fit (TRUE or FALSE)
#' @param ...     Additional arguments, see ?rstan::sampling()
#' @details      Returns a dataframe with estimates for linf, k and t0.
#' There is a hierarchical structure for linf and k, so that there is a unique estimate for these parameters per individual.
#' linf_global and k_global are the full distributions for linf and k with linf_glob and k_glob the averages of these distributions.
#' mu_linf and mu_k are the weighted averages of linf and k, where each estimates for all individuals are weighted linearly to their maximum age.
#' @keywords      fish, growth, Von Bertalanfy
#' @export oto_growth
#'
#' @examples
#'
#' zs <- fishflux::zebsco_growth
#' fishflux::oto_growth(length = zs$length, age = zs$age, id = zs$id, lmax = 25, linf_m = 20, iter = 2000, chains = 4, control = list(adapt_delta = 0.99))
#'



oto_growth <- function(length, age, id, linf_min = 0, lmax = 20, linf_m, k_m = 0.5, plot = TRUE, ...){

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

result <- summary[c("mu_k", "mu_linf", "t0", "kmax", "k_glob","linf_glob", "k_global", "linf_global"),1:8]

ee <- rstan::extract(fit)
y_m <- ee$y_m
y_g <- ee$y_global
pred <- data.frame(
  age = age,
  ypred_m = apply(y_m,2,mean),
  ypred_lb = apply(y_m,2,quantile, 0.025),
  ypred_ub = apply(y_m,2,quantile, 0.975),
  yglob_m = apply(y_g,2,mean),
  yglob_lb = apply(y_g,2,quantile, 0.025),
  yglob_ub = apply(y_g,2,quantile, 0.975)
)

if(plot){
p <-
  ggplot() +
  geom_point(aes(x = age, y = length)) +
  geom_ribbon(aes(x = age, ymin = yglob_lb, ymax = yglob_ub), alpha = 0.2, data = pred) +
  geom_ribbon(aes(x = age, ymin = ypred_lb, ymax = ypred_ub), alpha = 0.4, data = pred) +
  geom_line(aes(x = age, y = ypred_m), data = pred) +
  theme_bw()

print(p)
}
return(list(summary = result, stanfit = fit))

}
