#' A function to estimate CNP and sd for the highest phylogenetic level
#' available
#'
#' A function to estimate CNP and sd for the highest phylogenetic level
#' available
#'
#'
#' @param sp A charachter value containing the species name
#' @param family A charachter value containing family name
#' @keywords CNP fish,
#' @examples
#'
#'
#' fishflux::cnp_fishf("Lutjanus griseus")
#'
#'
#' @export cnp_fishf
cnp_fishf <- function(sp=NA,family=NA){
  data <- fishflux::cnp_fish
  if(is.na(sp)){
    if (is.na(family)){
      stop("Please provide Family of species")
    }
    d <- dplyr::filter(data,Family==family)
    if (nrow(d)<5){
      d <- data
      r <- data.frame(
        C=mean(d$C),
        N=mean(d$N),
        P=mean(d$P),
        C_sd=sd(d$C),
        N_sd=sd(d$N),
        P_sd=sd(d$P),
        nrep=nrow(d),
        level="all"
      )
      return(r)
    } else{
      r <- data.frame(
        C=mean(d$C),
        N=mean(d$N),
        P=mean(d$P),
        C_sd=sd(d$C),
        N_sd=sd(d$N),
        P_sd=sd(d$P),
        nrep=nrow(d),
        level="Family"
      )
      return(r)
    }
  }
  else{
    spe <- rfishbase::species(sp)
    gen <- unique(spe$Genus)
    d <- dplyr::filter(data,species==sp)
    if (nrow(d)<5){
      d <- dplyr::filter(data,Genus==gen)
      if (nrow(d)<5){
        if (is.na(family)){
          stop("Please provide Family of species")
        }
        d <- dplyr::filter(data,Family==family)
        if (nrow(d)<5){
          d <- data
          r <- data.frame(
            C=mean(d$C),
            N=mean(d$N),
            P=mean(d$P),
            C_sd=sd(d$C),
            N_sd=sd(d$N),
            P_sd=sd(d$P),
            nrep=nrow(d),
            level="all"
          )
          return(r)
        } else{
          r <- data.frame(
            C=mean(d$C),
            N=mean(d$N),
            P=mean(d$P),
            C_sd=sd(d$C),
            N_sd=sd(d$N),
            P_sd=sd(d$P),
            nrep=nrow(d),
            level="Family"
          )
          return(r)
        }
      }else{
        r <- data.frame(
          C=mean(d$C),
          N=mean(d$N),
          P=mean(d$P),
          C_sd=sd(d$C),
          N_sd=sd(d$N),
          P_sd=sd(d$P),
          nrep=nrow(d),
          level="Genus"
        )
        return(r)
      }
    }else{
      r <- data.frame(
        C=mean(d$C),
        N=mean(d$N),
        P=mean(d$P),
        C_sd=sd(d$C),
        N_sd=sd(d$N),
        P_sd=sd(d$P),
        nrep=nrow(d),
        level="Species"
      )
      return(r)
    }
  }
}


