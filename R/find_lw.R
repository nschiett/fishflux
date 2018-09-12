#' A function to find length-weight relationship parameters a and b
#'
#' A function to find estimates length-weight relationship parameters available on fishbase. It returns a list of means and standard deviations of a and b obtained from:
#'*Froese, R., J. Thorson and R.B. Reyes Jr., 2013. A Bayesian approach for estimating length-weight relationships in fishes. J. Appl. Ichthyol. (2013):1-7.*
#'Please cite Froese et al. (2013), when using these values.
#'
#' @param sp A charachter value containing the species name
#' @keywords fish, l-w relationship, fishbase, bayesian
#' @export
#' @examples
#'
#' # find length-weight relationship parameters for one species
#' find_lw("Lutjanus griseus")
#'
#' # find length-weight relationship parameters for multiple species and return in dataframe
#' plyr::ldply(lapply(c("Chlorurus spilurus","Zebrasoma scopas"), find_lw))
#' @export find_lw

find_lw <- function(sp){

  sp_ <- gsub(" ","-",sp)

  page <-  readLines(paste('https://www.fishbase.de/summary/',sp_,'.html',sep=""))

  l <- grep("Bayesian length-weight:",page)
  line <- page[l]
  line <- gsub("\t","",line)
  line <- gsub("Bayesian length-weight: ","",line)
  line <- gsub(", in cm Total Length, based on LWR estimates for this species (Ref. <A href='../references/FBRefSummary.php?ID=93245'>93245</A>).","",line)
  ob <- strsplit(line," ")
  ob <- ob[[1]]

  lwa_m <- as.numeric(gsub("a=","",ob[1]))
  lwa_up <- as.numeric(gsub("),","",ob[4]))
  lwa_sd <- (lwa_up-lwa_m)/1.96

  lwb_m <- as.numeric(gsub("b=","",ob[5]))
  lwb_up <- as.numeric(gsub("),","",ob[8]))
  lwb_sd <- (lwb_up-lwb_m)/1.96

  return(data.frame(species=sp,lwa_m=lwa_m,lwa_sd=lwa_sd,lwb_m=lwb_m,lwb_sd=lwb_sd))

}
