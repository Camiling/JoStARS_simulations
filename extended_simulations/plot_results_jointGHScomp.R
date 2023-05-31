rm(list=ls())
load("extended_simulations/data/stabJGL_simulations_extended_jointGHS_ebic0.Rdata")
load("extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3.Rdata")

library(ggplot2)

# We replace K=3 with K=2 in the below.

# Rename for simplicity
res.K2 = res.K2.jointGHS
res.K3 = res.K3.jointGHS


fracs.disagreement = c(0,0.2,0.4,0.6,0.8,1)
n.points = length(fracs.disagreement)
perc.disagreement = 100*fracs.disagreement

k=1

# For each disagreement fraction, get measures
precisions.K2 = unlist(lapply(res.K2, FUN = function(s) mean(s[[1]]$mean.precisions[k])))
precisions.K2.2 = unlist(lapply(res.K2, FUN = function(s) mean(s[[2]]$mean.precisions[k])))
precisions.K2.3 = unlist(lapply(res.K2, FUN = function(s) mean(s[[3]]$mean.precisions[k])))
precisions.K2.4 = unlist(lapply(res.K2, FUN = function(s) mean(s[[4]]$mean.precisions[k])))
precisions.K2.jointghs = unlist(lapply(res.K2, FUN = function(s) mean(s[[5]]$mean.precisions.jointghs[k])))

precisions.K3 = unlist(lapply(res.K3, FUN = function(s) mean(s[[1]]$mean.precisions[k])))
precisions.K3.2 = unlist(lapply(res.K3, FUN = function(s) mean(s[[2]]$mean.precisions[k])))
precisions.K3.3 = unlist(lapply(res.K3, FUN = function(s) mean(s[[3]]$mean.precisions[k])))
precisions.K3.4 = unlist(lapply(res.K3, FUN = function(s) mean(s[[4]]$mean.precisions[k])))
precisions.K3.jointghs = unlist(lapply(res.K3, FUN = function(s) mean(s[[5]]$mean.precisions.jointghs[k])))

recalls.K2 = unlist(lapply(res.K2, FUN = function(s) mean(s[[1]]$mean.recalls[k])))
recalls.K2.2 = unlist(lapply(res.K2, FUN = function(s) mean(s[[2]]$mean.recalls[k])))
recalls.K2.3 = unlist(lapply(res.K2, FUN = function(s) mean(s[[3]]$mean.recalls[k])))
recalls.K2.4 = unlist(lapply(res.K2, FUN = function(s) mean(s[[4]]$mean.recalls[k])))
recalls.K2.jointghs = unlist(lapply(res.K2, FUN = function(s) mean(s[[5]]$mean.recalls.jointghs[k])))


recalls.K3 = unlist(lapply(res.K3, FUN = function(s) mean(s[[1]]$mean.recalls[k])))
recalls.K3.2 = unlist(lapply(res.K3, FUN = function(s) mean(s[[2]]$mean.recalls[k])))
recalls.K3.3 = unlist(lapply(res.K3, FUN = function(s) mean(s[[3]]$mean.recalls[k])))
recalls.K3.4 = unlist(lapply(res.K3, FUN = function(s) mean(s[[4]]$mean.recalls[k])))
recalls.K3.jointghs = unlist(lapply(res.K3, FUN = function(s) mean(s[[5]]$mean.recalls.jointghs[k])))


# Also standard deviations
precisions.sd.K2 = unlist(lapply(res.K2, FUN = function(s) sd(s[[1]]$precisions[,k],na.rm=T)))
precisions.sd.K2.2 = unlist(lapply(res.K2, FUN = function(s) sd(s[[2]]$precisions[,k],na.rm=T)))
precisions.sd.K2.3 = unlist(lapply(res.K2, FUN = function(s) sd(s[[3]]$precisions[,k],na.rm=T)))
precisions.sd.K2.4 = unlist(lapply(res.K2, FUN = function(s) sd(s[[4]]$precisions[,k],na.rm=T)))
precisions.sd.K2.jointghs = unlist(lapply(res.K2, FUN = function(s) sd(s[[5]]$precisions.jointghs[,k],na.rm=T)))



precisions.sd.K3 = unlist(lapply(res.K3, FUN = function(s) sd(s[[1]]$precisions[,k],na.rm=T)))
precisions.sd.K3.2 = unlist(lapply(res.K3, FUN = function(s) sd(s[[2]]$precisions[,k],na.rm=T)))
precisions.sd.K3.3 = unlist(lapply(res.K3, FUN = function(s) sd(s[[3]]$precisions[,k],na.rm=T)))
precisions.sd.K3.4 = unlist(lapply(res.K3, FUN = function(s) sd(s[[4]]$precisions[,k],na.rm=T)))
precisions.sd.K3.jointghs = unlist(lapply(res.K3, FUN = function(s) sd(s[[5]]$precisions.jointghs[,k],na.rm=T)))


recalls.sd.K2 = unlist(lapply(res.K2, FUN = function(s) sd(s[[1]]$recalls[,k],na.rm=T)))
recalls.sd.K2.2 = unlist(lapply(res.K2, FUN = function(s) sd(s[[2]]$recalls[,k],na.rm=T)))
recalls.sd.K2.3 = unlist(lapply(res.K2, FUN = function(s) sd(s[[3]]$recalls[,k],na.rm=T)))
recalls.sd.K2.4 = unlist(lapply(res.K2, FUN = function(s) sd(s[[4]]$recalls[,k],na.rm=T)))
recalls.sd.K2.jointghs = unlist(lapply(res.K2, FUN = function(s) sd(s[[5]]$recalls.jointghs[,k],na.rm=T)))

recalls.sd.K3 = unlist(lapply(res.K3, FUN = function(s) sd(s[[1]]$recalls[,k],na.rm=T)))
recalls.sd.K3.2 = unlist(lapply(res.K3, FUN = function(s) sd(s[[2]]$recalls[,k],na.rm=T)))
recalls.sd.K3.3 = unlist(lapply(res.K3, FUN = function(s) sd(s[[3]]$recalls[,k],na.rm=T)))
recalls.sd.K3.4 = unlist(lapply(res.K3, FUN = function(s) sd(s[[4]]$recalls[,k],na.rm=T)))
recalls.sd.K3.jointghs = unlist(lapply(res.K3, FUN = function(s) sd(s[[5]]$recalls.jointghs[,k],na.rm=T)))



#Plotting data frame


df.K.all = data.frame(precision=c(precisions.K2, precisions.K2.jointghs,
                                  precisions.K3, precisions.K3.jointghs),
                      recall = c(recalls.K2, recalls.K2.jointghs,
                                 recalls.K3, recalls.K3.jointghs),
                      sd.prec = c(precisions.sd.K2, precisions.sd.K2.jointghs,
                                  precisions.sd.K3, precisions.sd.K3.jointghs),
                      sd.rec = c(recalls.sd.K2, recalls.sd.K2.jointghs,
                                 recalls.sd.K3, recalls.sd.K3.jointghs),
                      method = factor(rep(c(rep(paste0('stabJGL', expression(beta_1 == 0.005 )), n.points),
                                            rep('jointGHS',n.points)),2)),
                      K = factor(c(rep('K=2', n.points*6), rep('K=3', 6*n.points)),levels=c('K=2','K=3')),
                      disagreement=c(rep(perc.disagreement,4)))

df.K.otherthresh = data.frame(precision=c(precisions.K2.2, precisions.K2.3,precisions.K2.4,
                                          precisions.K3.2, precisions.K3.3,precisions.K3.4),
                              recall = c(recalls.K2.2, recalls.K2.3,recalls.K2.4,
                                         recalls.K3.2, recalls.K3.3, recalls.K3.4),
                              sd.prec = c(precisions.sd.K2.2, precisions.sd.K2.3, precisions.sd.K2.4,
                                          precisions.sd.K3.2, precisions.sd.K3.3, precisions.sd.K3.4),
                              sd.rec = c(recalls.sd.K2.2, recalls.sd.K2.3, recalls.sd.K2.4,
                                         recalls.sd.K3.2, recalls.sd.K3.3, recalls.sd.K3.4),
                              method = factor(rep(c(rep(paste0('stabJGL ', expression(beta_1 == 0.01 )), n.points),
                                                    rep(paste0('stabJGL ', expression(beta_1 == 0.03 )),n.points),
                                                    rep(paste0('stabJGL ', expression(beta_1 == 0.05 )),n.points)),2)),
                              K = factor(c(rep('K=2', n.points*9), rep('K=3', 9*n.points)),levels=c('K=2','K=3')),
                              disagreement=c(rep(perc.disagreement,6)))

df.K = rbind(df.K.all, df.K.otherthresh)
my.labs = c('jointGHS', "stabJGL \u03b2 = 0.005", "stabJGL \u03b2 = 0.01", "stabJGL \u03b2 = 0.03", "stabJGL \u03b2 = 0.05")


g.prec.sd.smaller=ggplot2::ggplot(df.K, aes(y=precision, x=disagreement, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('dodgerblue','darkgoldenrod2','darkorange', 'brown1','brown3'), 
                                                                                    labels=my.labs)+scale_linetype(labels=my.labs)+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) + facet_wrap(~K) + ylim(0,1)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='bottom',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K, aes(y=recall, x=disagreement, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('dodgerblue','darkgoldenrod2','darkorange', 'brown1','brown3'),
                                                                                    labels=my.labs)+scale_linetype(labels=my.labs)+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) + facet_wrap(~K)+ ylim(0,1)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")


gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(6,6,1))

# Only plotting for K=2

df.K = rbind(df.K.all, df.K.otherthresh)
df.K = df.K  %>% filter(K =='K=2')
my.labs = c('jointGHS', "stabJGL \u03b21 = 0.005", "stabJGL \u03b21 = 0.01", "stabJGL \u03b21 = 0.03", "stabJGL \u03b21 = 0.05")


g.prec.sd.smaller=ggplot2::ggplot(df.K, aes(y=precision, x=disagreement, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('dodgerblue','darkgoldenrod2','darkorange', 'brown1','brown3'), 
                                                                                    labels=my.labs)+scale_linetype(labels=my.labs)+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) +  ylim(0,1)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='right',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K, aes(y=recall, x=disagreement, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('dodgerblue','darkgoldenrod2','darkorange', 'brown1','brown3'),
                                                                                    labels=my.labs)+scale_linetype(labels=my.labs)+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) + ylim(0,1)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")

lay.mat = matrix(c(1,2,
                   3,2),ncol=2,byrow=T)

quartz(type = 'pdf', file = 'extended_simulations/plots/simstudy_plot_vs_jointGHS.pdf',width=8,height=7)
#gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(5,5,0.1))
gridExtra::grid.arrange(g.prec.sd.smaller,legend,g.rec.sd.smaller,layout_matrix=lay.mat, widths=c(6,2))
dev.off()










