#' A function to plot results model
#'
#' This function allows you to plot an overview of the model results in function of the total length of fish
#' @param mod Model output from cnp_model or cnp_model_mcmc.
#' @param option Argument to specify which type of model results to plot. Can be either a charachter or a charachter vector with multiple choices.
#'               Choices are one or more of the following:
#'               "ingestion", "excretion", "egestion", "growth", "respiration", or "overview", containing all of the before-mentioned results.
#'               Default is "overview".
#' @param display Parameter to specify which element to show result for: "c", "n", "p" or "cnp", which will return three plots.
#'                Default is "cnp".
#' @keywords fish, plot, TL
#' @export plot_cnp
#' @examples
#' sp <- "Scarus psittacus"
#' para <- model_parameters(sp,"Scaridae")$parameters
#' para
#' mod1 <- fishflux::cnp_model(TL = 10:19,Fn = 1.3,Fp = 0.1,Fc = 50,
#'                 t0 = para$t0,Linf = para$Linf,k = para$k,
#'                asp = para$asp,troph = para$troph,f = 3,w_prop = para$w_prop,
#'                lwa = para$lwa,lwb = para$lwb,temp = 27,N = para$N,P=para$P,C=para$C)
#'  plot_cnp(test)
#'
#' mod2 <- cnp_model_mcmc(TL = 5:15, param = list(C_m = 40, N_m = 10, P_m = 4, Fn_sd = 0.05))
#' plot_cnp(mod2)




plot_cnp  <- function(mod, option = c("overview","ingestion","excretion","egestion","growth","respiration"), ts=20, display = c("cnp", "c", "n", "p")){

  require("ggplot2")
  require("cowplot")


  # set defaults

  if (missing(option)){
    option <- "overview"
  }

  if (missing(display)){
    display = "cnp"
  }

  # check input
  if (!option %in% c("overview","ingestion","excretion","egestion","growth","respiration")){
    stop("Input for argument option is not allowed. For more info on input arguments, run ?fishflux::plot_cnp")
  }

  if (!display %in% c("cnp", "c", "n", "p")){
    stop("Input for argument display is not allowed. For more info on input arguments, run ?fishflux::plot_cnp")
  }

  if (length(display)>1){
    stop("display should be a charachter, not a vector. For more info on input arguments, run ?fishflux::plot_cnp")
  }


  ## prepare general empty ggplot objects
  cb <- c("#1a1a1a",
           "#e69f00",
           "#56b4e9",
           "#009e73",
           "#f0e442",
           "#0072b2",
           "#9d2f00") ##color scale
  scaleFUN <- function(x) sprintf("%.3f", x)  ##function for scales of y axis

  c <- ggplot() +
    scale_y_continuous(labels = scaleFUN)+
    labs( x="TL (cm)",y="C (g/d)")+
    theme_bw()+
    theme(axis.title  = element_text(size=ts),
          title       = element_text(size=ts),
          axis.text   = element_text(size=ts-5),
          legend.text = element_text(size=ts-5))

  n <- ggplot() +
    scale_y_continuous(labels = scaleFUN)+
    labs( x="TL (cm)",y="N (g/d)")+
    theme_bw()+
    theme(axis.title  = element_text(size=ts),
          title       = element_text(size=ts),
          axis.text   = element_text(size=ts-5),
          legend.text = element_text(size=ts-5))

  p <- ggplot() +
    scale_y_continuous(labels = scaleFUN)+
    labs( x="TL (cm)",y="P (g/d)")+
    theme_bw()+
    theme(axis.title  = element_text(size=ts),
          title       = element_text(size=ts),
          axis.text   = element_text(size=ts-5),
          legend.text = element_text(size=ts-5))

  legend <- ggplot() +
    scale_x_continuous(limits = c(0.4,1.6))+
    geom_line(aes(x=c(1,1.5),y="f"),size = 1.5, color = "white")+
    geom_line(aes(x=c(1,1.5),y="1"),size = 1.5, color = "white")+
    theme_void()

  ## option  for model_cnp_mcmc ##

  if (is.list(mod)){

    result <- mod[[2]]

    if ("overview" %in% option){
      c <- c +
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="C_in","97.5%"],ymin=result[result$variable=="C_in","2.5%"]),alpha=0.1)+
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="C_r","97.5%"],ymin=result[result$variable=="C_r","2.5%"]),alpha=0.1,fill = cb[6])+
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="C_g","97.5%"],ymin=result[result$variable=="C_g","2.5%"]),alpha=0.1,fill = cb[4])+
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="C_eg","97.5%"],ymin=result[result$variable=="C_eg","2.5%"]),alpha=0.1,fill = cb[7])+

        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="C_in","mean"]),size = 1.5)+
        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="C_r","mean"]),size = 1.5, color = cb[6])+
        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="C_g","mean"]),size = 1.5, color = cb[4])+
        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="C_eg","mean"]),size = 1.5, color = cb[7])

      n <- n +
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="N_in","97.5%"],ymin=result[result$variable=="N_in","2.5%"]),alpha=0.1)+
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="N_g","97.5%"],ymin=result[result$variable=="N_g","2.5%"]),alpha=0.1,fill = cb[4])+
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="N_eg","97.5%"],ymin=result[result$variable=="N_eg","2.5%"]),alpha=0.1,fill = cb[7])+
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="N_ex","97.5%"],ymin=result[result$variable=="N_ex","2.5%"]),alpha=0.1,fill = cb[5])+

        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="N_in","mean"]),size = 1.5)+
        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="N_g","mean"]),size = 1.5, color = cb[4])+
        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="N_eg","mean"]),size = 1.5, color = cb[7])+
        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="N_ex","mean"]),size = 1.5, color = cb[5])

      p <- p +
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="P_in","97.5%"],ymin=result[result$variable=="P_in","2.5%"]),alpha=0.1)+
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="P_g","97.5%"],ymin=result[result$variable=="P_g","2.5%"]),alpha=0.1,fill = cb[4])+
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="P_eg","97.5%"],ymin=result[result$variable=="P_eg","2.5%"]),alpha=0.1,fill = cb[7])+
        geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="P_ex","97.5%"],ymin=result[result$variable=="P_ex","2.5%"]),alpha=0.1,fill = cb[5])+

        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="P_in","mean"]),size = 1.5)+
        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="P_g","mean"]),size = 1.5, color = cb[4])+
        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="P_eg","mean"]),size = 1.5, color = cb[7])+
        geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="P_ex","mean"]),size = 1.5, color = cb[5])

      legend <- legend +
        geom_line(aes(x=c(1,1.5),y="e"),size = 1.5, color = "black")+
        geom_text(aes(x=0.5,y="e",label = "Ingestion"), color = "black",size=6,hjust=0)+
        geom_line(aes(x=c(1,1.5),y="b"),size = 1.5, color = cb[5])+
        geom_text(aes(x=0.5,y="b",label = "Excretion"),size=6,hjust=0)+
        geom_line(aes(x=c(1,1.5),y="c"),size = 1.5, color = cb[7])+
        geom_text(aes(x=0.5,y="c",label = "Egestion"), color = "black",size=6,hjust=0)+
        geom_line(aes(x=c(1,1.5),y="d"),size = 1.5, color = cb[4])+
        geom_text(aes(x=0.5,y="d",label = "Growth"),size=6,hjust=0)+
        geom_line(aes(x=c(1,1.5),y="a"),size = 1.5, color = cb[6])+
        geom_text(aes(x=0.5,y="a",label = "Respiration"), color = "black",size=6, hjust=0)

    } else{
      if ("ingestion" %in% option){

       c <- c +
         geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="C_in","97.5%"],ymin=result[result$variable=="C_in","2.5%"]),alpha=0.1)+
         geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="C_in","mean"]),size = 1.5)

       n <- n +
         geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="N_in","97.5%"],ymin=result[result$variable=="N_in","2.5%"]),alpha=0.1)+
         geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="N_in","mean"]),size = 1.5)

       p <- p +
         geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="P_in","97.5%"],ymin=result[result$variable=="P_in","2.5%"]),alpha=0.1)+
         geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="P_in","mean"]),size = 1.5)

       legend <- legend +
         geom_line(aes(x=c(1,1.5),y="e"),size = 1.5, color = "black")+
         geom_text(aes(x=0.5,y="e",label = "Ingestion"), color = "black",size=6,hjust=0)

      }

      if ("respiration" %in% option){
        c <- c +
          geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="C_r","97.5%"],ymin=result[result$variable=="C_r","2.5%"]),alpha=0.1,fill = cb[6])+
          geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="C_r","mean"]),size = 1.5, color = cb[6])

        legend <- legend +
          geom_line(aes(x=c(1,1.5),y="a"),size = 1.5, color = cb[6])+
          geom_text(aes(x=0.5,y="a",label = "Respiration"), color = "black",size=6, hjust=0)

      }

      if ("growth" %in% option){
        c <- c +
          geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="C_g","97.5%"],ymin=result[result$variable=="C_g","2.5%"]),alpha=0.1,fill = cb[4])+
          geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="C_g","mean"]),size = 1.5, color = cb[4])

        n <- n +
          geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="N_g","97.5%"],ymin=result[result$variable=="N_g","2.5%"]),alpha=0.1,fill = cb[4])+
          geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="N_g","mean"]),size = 1.5, color = cb[4])

        p <-p +
          geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="P_g","97.5%"],ymin=result[result$variable=="P_g","2.5%"]),alpha=0.1,fill = cb[4])+
          geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="P_g","mean"]),size = 1.5, color = cb[4])

        legend <- legend +
          geom_line(aes(x=c(1,1.5),y="d"),size = 1.5, color = cb[4])+
          geom_text(aes(x=0.5,y="d",label = "Growth"),size=6,hjust=0)

        }

      if ("egestion" %in% option){
        c <- c +
          geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="C_eg","97.5%"],ymin=result[result$variable=="C_eg","2.5%"]),alpha=0.1,fill = cb[7])+
          geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="C_eg","mean"]),size = 1.5, color = cb[7])

        n <- n +
          geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="N_eg","97.5%"],ymin=result[result$variable=="N_eg","2.5%"]),alpha=0.1,fill = cb[7])+
          geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="N_eg","mean"]),size = 1.5, color = cb[7])

        p <- p +
          geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="P_eg","97.5%"],ymin=result[result$variable=="P_eg","2.5%"]),alpha=0.1,fill = cb[7])+
          geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="P_eg","mean"]),size = 1.5, color = cb[7])

        legend <- legend +
          geom_line(aes(x=c(1,1.5),y="c"),size = 1.5, color = cb[7])+
          geom_text(aes(x=0.5,y="c",label = "Egestion"), color = "black",size=6,hjust=0)
      }

      if ("excretion" %in% option){
        n <- n +
          geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="N_ex","97.5%"],ymin=result[result$variable=="N_ex","2.5%"]),alpha=0.1,fill = cb[5])+
          geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="N_ex","mean"]),size = 1.5, color = cb[5])

        p <- p +
          geom_ribbon(aes(x=result[result$variable=="TL","mean"],ymax=result[result$variable=="P_ex","97.5%"],ymin=result[result$variable=="P_ex","2.5%"]),alpha=0.1,fill = cb[5])+
          geom_line(aes(x=result[result$variable=="TL","mean"],y=result[result$variable=="P_ex","mean"]),size = 1.5, color = cb[5])

        legend <- legend +
          geom_text(aes(x=0.5,y="b",label = "Excretion"),size=6,hjust=0)+
          geom_line(aes(x=c(1,1.5),y="b"),size = 1.5, color = cb[5])
      }
    }


      if (display == "cnp"){
        cnp <- cowplot::plot_grid(c,n,p,legend,nrow = 2)
        cnp
        return(cnp)
      }

      if (display == "c"){
        c <- cowplot::plot_grid(c, legend, nrow = 1, rel_widths = c(2,1))
        c
        return(c)
      }
      if (display == "n"){
        n <- cowplot::plot_grid(n, legend, nrow = 1, rel_widths = c(2,1))
        n
        return(n)
      }
      if (display == "p"){
        p <- cowplot::plot_grid(p, legend, nrow = 1, rel_widths = c(2,1))
        p
        return(p)
      }
  }

 ## for model_cnp output
    if (is.data.frame(mod))  {
      if (overview %in% option){
        c <- c +
          geom_line(aes(x=mod$TL,y=mod$C_in), size = 1.5)+
          geom_line(aes(x=mod$TL,y=mod$C_r),size = 1.5, color = cb[6])+
          geom_line(aes(x=mod$TL,y=mod$C_g),size = 1.5, color = cb[4])+
          geom_line(aes(x=mod$TL,y=mod$C_eg),size = 1.5, color = cb[7])

        n <- n +
          geom_line(aes(x=mod$TL,y=mod$N_in), size = 1.5)+
          geom_line(aes(x=mod$TL,y=mod$N_g),size = 1.5, color = cb[4])+
          geom_line(aes(x=mod$TL,y=mod$N_eg),size = 1.5, color = cb[7])+
          geom_line(aes(x=mod$TL,y=mod$N_ex),size = 1.5, color = cb[5])

        p <- p +
          geom_line(aes(x=mod$TL,y=mod$P_in))+
          geom_line(aes(x=mod$TL,y=mod$P_g),size = 1.5, color = cb[4])+
          geom_line(aes(x=mod$TL,y=mod$P_eg),size = 1.5, color = cb[7])+
          geom_line(aes(x=mod$TL,y=mod$P_ex),size = 1.5, color = cb[5])


        legend <- legend +
          geom_line(aes(x=c(1,1.5),y="e"),size = 1.5, color = "black")+
          geom_text(aes(x=0.5,y="e",label = "Ingestion"), color = "black",size=6,hjust=0)+
          geom_line(aes(x=c(1,1.5),y="b"),size = 1.5, color = cb[5])+
          geom_text(aes(x=0.5,y="b",label = "Excretion"),size=6,hjust=0)+
          geom_line(aes(x=c(1,1.5),y="c"),size = 1.5, color = cb[7])+
          geom_text(aes(x=0.5,y="c",label = "Egestion"),color = "black",size=6,hjust=0)+
          geom_line(aes(x=c(1,1.5),y="d"),size = 1.5, color = cb[4])+
          geom_text(aes(x=0.5,y="d",label = "Growth"),size=6,hjust=0)+
          geom_line(aes(x=c(1,1.5),y="a"),size = 1.5, color = cb[6])+
          geom_text(aes(x=0.5,y="a",label = "Respiration"), color = "black",size=6, hjust=0)

      } else{
        if ("ingestion" %in% option){

          c <- c +
            geom_line(aes(x=mod$TL,y=mod$C_in), size = 1.5)

          n <- n +
            geom_line(aes(x=mod$TL,y=mod$N_in), size = 1.5)

          p <- p +
            geom_line(aes(x=mod$TL,y=mod$P_in), size = 1.5)

          legend <- legend +
            geom_line(aes(x=c(1,1.5),y="e"),size = 1.5, color = "black")+
            geom_text(aes(x=0.5,y="e",label = "Ingestion"), color = "black",size=6,hjust=0)

        }

        if ("respiration" %in% option){
          c <- c +
            geom_line(aes(x=mod$TL,y=mod$C_r),size = 1.5, color = cb[6])

          legend <- legend +
            geom_line(aes(x=c(1,1.5),y="a"),size = 1.5, color = cb[6])+
            geom_text(aes(x=0.5,y="a",label = "Respiration"),size = 1.5, color = "black",size=6, hjust=0)

        }

        if ("growth" %in% option){
          c <- c +
            geom_line(aes(x=mod$TL,y=mod$C_g),size = 1.5, color = cb[4])

          n <- n +
            geom_line(aes(x=mod$TL,y=mod$N_g),size = 1.5, color = cb[4])

          p <-p +
            geom_line(aes(x=mod$TL,y=mod$P_g),size = 1.5, color = cb[4])

          legend <- legend +
            geom_line(aes(x=c(1,1.5),y="d"),size = 1.5, color = cb[4])+
            geom_text(aes(x=0.5,y="d",label = "Growth"),size=6,hjust=0)

        }

        if ("egestion" %in% option){
          c <- c +
            geom_line(aes(x=mod$TL,y=mod$C_eg),size = 1.5, color = cb[7])

          n <- n +
            geom_line(aes(x=mod$TL,y=mod$N_eg),size = 1.5, color = cb[7])

          p <- p +
            geom_line(aes(x=mod$TL,y=mod$P_eg),size = 1.5, color = cb[7])

          legend <- legend +
            geom_line(aes(x=c(1,1.5),y="c"),size = 1.5, color = cb[7])+
            geom_text(aes(x=0.5,y="c",label = "Egestion"),size = 1.5, color = "black",size=6,hjust=0)
        }

        if ("excretion" %in% option){
          n <- n +
            geom_line(aes(x=mod$TL,y=mod$N_ex),size = 1.5, color = cb[5])

          p <- p +
            geom_line(aes(x=mod$TL,y=mod$P_ex),size = 1.5, color = cb[5])

          legend <- legend +
            geom_text(aes(x=0.5,y="b",label = "Excretion"),size=6,hjust=0)+
            geom_line(aes(x=c(1,1.5),y="b"),size = 1.5, color = cb[5])
        }
      }

        if (display == "cnp"){
          cnp <- cowplot::plot_grid(c,n,p,legend,nrow = 2)
          cnp
          return(cnp)
        }

        if (display == "c"){
          c
          return(c)
        }
        if (display == "n"){
          n
          return(n)
        }
        if (display == "p"){
          return(p)
        }
    }
  }

