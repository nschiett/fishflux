#' A function to find errors in fish species names
#'
#' This function allows you to check if there are errors
#' in your fish species list and returns inaccurate scientific names
#' @param sp A vector containing all your scientific species names.
#' 
#' @keywords fish scientific names
#' 
#' @importFrom rfishbase validate_names
#' 
#' @returns A vector with the incorrect species names. 
#' 
#' @examples
#' \dontrun{
#' library(fishflux)
#' name_errors(c("Chlorurus spilurus", "Zebrasoma scopas"))
#' name_errors(c("Chlorurus spilurus", "Zebrasoma copas"))}
#' 
#' @export
name_errors  <- function(sp) {
  sp_correct <- suppressWarnings(validate_names(sp))
  sp_error   <- sp[ ! (sp %in% sp_correct)]
  if (length(sp_error) == 0) {
    message("All species names are correct")
  } else {
    message("Inaccurate species names found:")
    sp_error
  }
}
