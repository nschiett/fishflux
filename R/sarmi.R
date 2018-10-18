#' Example otolith data *Sargocentron microstoma*
#'
#' Has a otolith data with length and age estimations for 17 individuals.
#' This data can be used to approximate the VB growth curve.
#' the data frame contains length, age (from otolith readinga nd back calculations) and id of the fish.
#'
#'
#' @docType data
#'
#' @usage data(sarmi)
#'
#' @keywords datasets
#'
#' @examples
#' data(sarmi)
#' \donttest{fishflux::oto_growth(length = sarmi$length, age = sarmi$age, id = sarmi$id, linf_m = 16, k_m = 0.4)}
"sarmi"
