#' A function to find a set of parameters
#'
#'
#' @param sp Species name
#' @param family family
#' @param otolith TRUE or FALSE, if TRUE, function will only search fishbase for growth parameters that are based upon otolith analysis
#' @param temp temperature
#' @param ... Additional arguments. See find_lw()
#' @details Returns a dataframe with all parameters that can be estimated
#' @keywords fish, find some parameters needed for cnp_model
#' @export model_parameters
#' @examples
#'
#' fishflux::model_parameters(sp = "Scarus psittacus", family = "Scaridae", temp = 27)

model_parameters <- function(sp, family, otolith = TRUE, temp, ...){

  #check species
  fishflux::check_name_fishbase(sp)

  #dry_weight/wet_weight
  wprop <- fishflux::wprop(family = family)

  #length weight
  length_weight <- fishflux::find_lw(sp, ...)

  #growth parameters
  growth <- fishflux::growth_params(sp = sp, otolith = otolith)

  #trophic level
  troph <- fishflux::trophic_level(sp)

  #aspect ratio
  asp <- fishflux::aspect_ratio(sp)

  #metabolism
  met <- fishflux::metabolism(family = family, troph_m = troph$trophic_level,
                              temp = temp )

  #combine
  parameters <- data.frame(species  = sp,
                           t0       = mean(growth$t0, na.rm = TRUE),
                           Linf     = mean(growth$Linf, na.rm = TRUE),
                           k        = mean(growth$k, na.rm = TRUE),
                           asp      = asp$aspect_ratio,
                           troph    = troph$trophic_level,
                           lwa_m    = length_weight$lwa_m,
                           lwa_sd   = length_weight$lwa_sd,
                           lwb_m    = length_weight$lwb_m,
                           lwb_sd   = length_weight$lwb_sd,
                           mdw_m    = wprop$mdw,
                           f0_m     = met$B0_m,
                           f0_sd    = met$B0_sd,
                           a_m      = met$a_m,
                           a_sd     = met$a_sd)

  return(parameters)
}
