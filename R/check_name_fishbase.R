#' Returns error if name is incorrect
#'
#' This is a wrapper function to which will return an error
#' (via \code{\link[fishflux]{name_errors}}) if the provided species
#' name is wrong.
#'
#' @param sp A character value containing the species name
#' @keywords fish, taxonomy, fishbase
#' @export
#' @examples
#'
#' check_name_fishbase("Lutjanus griseus")
#' plyr::ldply(lapply(c("Chlorurus spilurus","Zebrasoma copas"), check_name_fishbase))



#' Returns error if name is incorrect
#'
#' This is a wrapper function to which will return an error (via
#' \code{\link[fishflux]{name_errors}}) if the provided species name is wrong.
#'
#'
#' @param sp A character value containing the species name
#' @keywords fish, fishbase taxonomy,
#' @examples
#'
#'
#' check_name_fishbase("Lutjanus griseus")
#' plyr::ldply(lapply(c("Chlorurus spilurus","Zebrasoma copas"), check_name_fishbase))
#'
#' @export check_name_fishbase
check_name_fishbase <- function (sp) {
  if (length(suppressMessages(fishflux::name_errors(sp))) > 0) {
    stop("Species name is incorrect")
  }
}
