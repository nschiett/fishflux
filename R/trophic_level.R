#' A function to find trophic level
#'
#' A function to find trophic level of a species on either species or genus
#' level using rfishbase. It returns a data frame containing the trophic level
#' and the level at which the trophic level was found (species or genus).
#'

#' @param sp A character value containing the species name
#' @keywords fish, trophic level, fishbase
#' @export trophic_level
#' @examples
#'
#' trophic_level("Lutjanus griseus")
#' plyr::ldply(lapply(c("Chlorurus spilurus","Zebrasoma scopas"), trophic_level))

trophic_level <- function (sp) {
    fishflux::check_name_fishbase(sp)
    ecogn <- rfishbase::ecology(sp)
    if (length(ecogn) == 0) {
        genus <- rfishbase::species(sp)$Genus
        gn    <- rfishbase::species_list(Genus = genus)
        ecogn <- rfishbase::ecology(gn)
        level <- "genus"
    } else {
        level <- "species"
    }
    troph       <- dplyr::select(ecogn, .data$Species, .data$DietTroph, .data$FoodTroph)
    troph$troph <- rowMeans(troph[, 2:3], na.rm = TRUE)
    troph       <- mean(troph$troph, na.rm = TRUE)
    data.frame(species       = sp,
               trophic_level = troph,
               level         = level)
}
