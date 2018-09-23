#' A function to extract growth parameters from otolith data
#'
#'
#' @param length Numerical vector with length
#' @param age    Numerical vector with age
#' @param id     Character vector with fish id
#' @param k_m    Prior for k, default is 0.5
#' @param linf_m Prior for linf
#' @param linf_min possibility to add a minimum for linf, default is 0
#' @details      Returns a dataframe with estimates for Linf, k and t0
#' @keywords      fish, growth, Von Bertalanfy
#' @export oto_growth
#'
#' @examples
#'
#' sarmi <- fishflux::sarmi
#' fishflux::oto_growth(length = sarmi$length, age = sarmi$age, id = sarmi$id, linf_m = 16, k_m = 0.4, iter = 2000, warmup = 1000)
#'



oto_growth <- function(length, age, id, linf_min = 0, linf_m, k_m = 0.5, iter = 2000, warmup = 1000){

require(ggplot2)

data <- list(
  N = length(length),
  J = length(unique(id)),
  l = length,
  age = age,
  ind = as.integer(as.factor(id)),
  s = rep(1,length(length)),
  linf_min = linf_min,
  linf_m = linf_m,
  k_m = k_m
)

fit <- rstan::sampling(stanmodels$vonbert, data=data, iter=iter, warmup=warmup, chains=4, seed=123,control = list(adapt_delta = 0.99,max_treedepth = 15))

summary <- as.data.frame(rstan::summary(fit)$summary)

result <- data.frame(
  k_m = summary["mu_k","mean"],
  linf_m = summary["mu_linf","mean"],
  t0_m = summary["mu_t0","mean"],
  k_sd = summary["mu_k","sd"],
  linf_sd = summary["mu_linf","sd"],
  t0_sd = summary["mu_t0","sd"]
)

predict <- result$linf_m*(1 - exp(-result$k_m * (sarmi$age - result$t0_m)))
predict_l <- (result$linf_m-1.96*result$linf_sd)*(1 - exp(-(result$k_m-1.96*result$k_sd) * (sarmi$age - (result$t0_m-1.96*result$t0_sd))))
predict_u <- (result$linf_m+1.96*result$linf_sd)*(1 - exp(-(result$k_m+1.96*result$k_sd) * (sarmi$age - (result$t0_m+1.96*result$t0_sd))))


plot <-
  ggplot()+
  geom_point(aes(x=age,y=length))+
  geom_ribbon(aes(x=age, ymin = predict_l, ymax = predict_u), alpha = 0.4)+
  geom_line(aes(x=age, y=predict))+
  theme_bw()

print(plot)

return(list(result= result, stanfit = fit))

}


