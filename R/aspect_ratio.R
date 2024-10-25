#' A function to find aspect ratio
#'
#' A function to find aspect ratio of a species on either species or genus
#' level using rfishbase. It returns a data frame containing the aspect ratio
#' and the level at which the aspect ratio was found (species or genus).
#'
#' @param sp A character value containing the species name
#' 
#' @returns dataframe with species, aspect ratio and taxonomy level. 
#' 
#' @keywords fish aspect-ratio fishbase
#' 
#' @importFrom rfishbase morphometrics load_taxa
#' @importFrom dplyr filter pull select summarise group_by select %>%
#' @importFrom rlang .env .data
#'
#' @examples
#' \dontrun{
#' library(fishflux)
#' library(dplyr)
#' aspect_ratio("Lutjanus griseus")
#' lapply(c("Chlorurus spilurus","Zebrasoma scopas"), aspect_ratio) %>%
#'   bind_rows(.id = ".id")
#' }
#' 
#' @export
aspect_ratio <- function(sp) {
    check_name_fishbase(sp)
    ma <- morphometrics(sp)
    if (length(ma) == 0) {
        genus <- strsplit(sp, " ")[[1]][1]
        gn    <- load_taxa() %>%
          filter(.data$Genus %in% .env$genus) %>%
          pull(Species)
        ma    <- morphometrics(gn)
        asp   <- select(ma, Species, AspectRatio, SL, TL)
        asp   <- summarise(group_by(asp, Species), AspectRatio = mean(AspectRatio))
        asp   <- mean(asp$AspectRatio, na.rm = TRUE)
        level <- "genus"
    } else {
        asp   <- select(ma, Species, AspectRatio, SL, TL)
        asp   <- mean(asp$AspectRatio, na.rm = TRUE)
        level <- "species"
    }
    data.frame(species = sp,
               aspect_ratio = asp,
               level = level)
}
