#' A function to find aspect ratio
#'
#' A function to find aspect ratio of a species on either species or genus
#' level using rfishbase. It returns a data frame containing the aspect ratio
#' and the level at which the aspect ratio was found (species or genus).
#'

#' @param sp A character value containing the species name
#' @keywords fish, aspect ratio, fishbase
#' @export
#' @examples
#'
#' aspect_ratio("Lutjanus griseus")
#' plyr::ldply(lapply(c("Chlorurus spilurus","Zebrasoma scopas"), aspect_ratio))
#' @export aspect_ratio

aspect_ratio <- function (sp) {
    fishflux::check_name_fishbase(sp)
    ma <- rfishbase::morphometrics(sp)
    if (length(ma) == 0){
        genus <- strsplit(sp, " ")[[1]][1]
        gn    <- rfishbase::species_list(Genus = genus)
        ma    <- rfishbase::morphometrics(gn)
        asp   <- dplyr::select(ma, Species, AspectRatio, SL, TL)
        asp   <- dplyr::summarise(dplyr::group_by(asp, Species), AspectRatio = mean(AspectRatio))
        asp   <- mean(asp$AspectRatio, na.rm = TRUE)
        level <- "genus"
    } else{
        asp   <- dplyr::select(ma, Species, AspectRatio, SL, TL)
        asp   <- mean(asp$AspectRatio, na.rm = TRUE)
        level <- "species"
    }
    data.frame(species      = sp,
               aspect_ratio = asp,
               level        = level)
}
