#' A function to find the ratio of dry weight and wet weight of fish in local database
#'
#' This function searches the ratio of dry weight and wet weight of fish
#' on the family level. If the family is not available, an average is returned.
#'
#' Returns a dataframe with the weight ratio (ww) and it's sd (ww_sd).
#'
#' @param family family
#' @keywords fish, find some parameters needed for cnp_model
#' @export wprop
#' @examples
#'
#' wprop(family="Scaridae")

wprop <- function(family){
  wprop <- fishflux::weight_prop
  ww <- wprop[wprop$Family==family,"weight_prop"]
  ww_sd <- wprop[wprop$Family==family,"weight_prop_sd"]
  if (length(ww) ==0){
    ww <- mean(wprop$weight_prop)
    ww_sd <- sd(wprop$weight_prop)
    warning("family not in database, average used")
  }
  return(data.frame(ww=ww, ww_sd=ww_sd))
}
