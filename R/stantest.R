# # ###test model with cnp_model_mcmc
#
# param <- list(TL_m = 10 ,
#               AEc_m = 0.8 ,
#               AEn_m = 0.8 ,
#               AEp_m = 0.7 ,
#               Fc_m = 2.5 ,
#               Fn_m = 0.3 ,
#               Fp_m = 0.1,
#               Linf_m = 23,
#               k_m = 0.5,
#               t0_m = 0,
#               f_m = 2,
#               asp_m = 1.9,
#               troph_m = 2,
#               lwa_m = 0.0137,
#               lwb_m = 3.083,
#               w_prop_m = 0.309,
#               temp_m = 27,
#               Tn_m = 0.0057,
#               Tp_m = 0.00016,
#               C_m = 41.6,
#               N_m = 8.9,
#               P_m = 3.7,
#               a_m = 0.9,
#               B0_m = 0.001517984,
#               C_sd = 3.72,
#               N_sd = 0.91,
#               P_sd = 0.61,
#               k_sd = 0.1,
#               Linf_sd = 2,
#               Fn_sd = 0.1,
#               Fn_sd = 0.05
#
# )
#
# test <- cnp_model_mcmc(10,param)
# test2 <- cnp_model_mcmc(15,param)
#
# test <- lapply(4:16,FUN =cnp_model_mcmc,param=param,iter=1000)
#
#
# test2 <- plyr::ldply(test)
#
# library(ggplot2)
# p1 <- ggplot()+
#         geom_line(aes(x=test2[test2$variable=="TL","mean"],y=test2[test2$variable=="N_ex","mean"]), size = 2)+
#         geom_line(aes(x=test2[test2$variable=="TL","mean"],y=test2[test2$variable=="N_ex","2.5%"]), colour = "grey")+
#         geom_line(aes(x=test2[test2$variable=="TL","mean"],y=test2[test2$variable=="N_ex","97.5%"]), colour = "grey")+
#         labs(title="N_ex with stan" , x="TL (cm)",y="N_ex")+
#         theme_bw()
#
# p2 <- ggplot()+
#         geom_line(aes(x=test2[test2$variable=="TL","mean"],y=test2[test2$variable=="P_ex","mean"]))+
#         labs(title="P_ex with stan" , x="TL (cm)",y="P_ex")
# p1
# p2
# ##same in cnp_model
#
# test3 <- cnp_model(TL = 4:16 ,
#                     AEc = 0.8 ,
#                     AEn = 0.8 ,
#                     AEp = 0.7 ,
#                     Fc = 2.5 ,
#                     Fn = 0.3 ,
#                     Fp = 0.1,
#                     Linf = 23,
#                     k = 0.5,
#                     t0 = 0,
#                     f = 2,
#                     asp = 1.9,
#                     troph = 2,
#                     lwa = 0.0137,
#                     lwb = 3.083,
#                     w_prop = 0.309,
#                     temp = 27,
#                     Tn = 0.0057,
#                     Tp = 0.00016,
#                     C = 41.6,
#                     N = 8.9,
#                     P = 3.7,
#                     a = 0.9,
#                     B0 = 0.001517984
#                    )
# View(test3$result)
#
# p3 <- ggplot(data = test3$result)+
#   geom_line(aes(x=TL,y=N_ex))+
#   labs(title="N_ex cnp_model")
#
# p4 <- ggplot(data = test3$result)+
#   geom_line(aes(x=TL,y=P_ex))+
#   labs(title="P_ex cnp_model")
#
# cowplot::plot_grid(p1,p2,p3,p4)
#
# ##compare all variables for fixed TL
# test <- lapply(4:10,FUN =cnp_model_mcmc,param=param,iter=1)
#
# test2 <- plyr::ldply(test)
# means <- dplyr::select(test2,variable,mean,TL_input)
# means <- tidyr::spread(means,variable,mean)
#
#
