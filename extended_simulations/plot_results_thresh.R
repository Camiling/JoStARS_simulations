rm(list=ls())
load("extended_simulations/data/stabJGL_simulations_extended_thresh_K2.Rdata")
load("extended_simulations/data/stabJGL_simulations_extended_C.Rdata")
library(ggplot2)
library(dplyr)
library(grDevices)

# We replace K=3 with K=2 in the below.

# Rename for simplicity
res.K2 = res.thresh.K2


fracs.disagreement = c(0,0.2,0.4,0.6,0.8,1)
n.points = length(fracs.disagreement)
perc.disagreement = 100*fracs.disagreement
perc.similarity = 100-perc.disagreement

k=1

# For each disagreement fraction, get measures
precisions.K2 = unlist(lapply(res.K2, FUN = function(s) mean(s[[1]]$mean.precisions[k])))
precisions.K2.2 = unlist(lapply(res.K2, FUN = function(s) mean(s[[2]]$mean.precisions[k])))
precisions.K2.3 = unlist(lapply(res.K2, FUN = function(s) mean(s[[3]]$mean.precisions[k])))
precisions.K2.4 = unlist(lapply(res.K2, FUN = function(s) mean(s[[4]]$mean.precisions[k])))
precisions.K2.jgl = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.precisions.jgl[k])))
precisions.K2.ggl = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.precisions.ggl[k])))
precisions.K2.ssjgl = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.precisions.ssjgl[k])))
precisions.K2.jointghs = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.precisions.jointghs[k])))
precisions.K2.glasso = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.precisions.glasso[k])))

recalls.K2 = unlist(lapply(res.K2, FUN = function(s) mean(s[[1]]$mean.recalls[k])))
recalls.K2.2 = unlist(lapply(res.K2, FUN = function(s) mean(s[[2]]$mean.recalls[k])))
recalls.K2.3 = unlist(lapply(res.K2, FUN = function(s) mean(s[[3]]$mean.recalls[k])))
recalls.K2.4 = unlist(lapply(res.K2, FUN = function(s) mean(s[[4]]$mean.recalls[k])))
recalls.K2.jgl = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.recalls.jgl[k])))
recalls.K2.ggl = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.recalls.ggl[k])))
recalls.K2.ssjgl = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.recalls.ssjgl[k])))
recalls.K2.jointghs = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.recalls.jointghs[k])))
recalls.K2.glasso = unlist(lapply(res.K2.2, FUN = function(s) mean(s$mean.recalls.glasso[k])))

# Also standard deviations
precisions.sd.K2 = unlist(lapply(res.K2, FUN = function(s) sd(s[[1]]$precisions[,k],na.rm=T)))
precisions.sd.K2.2 = unlist(lapply(res.K2, FUN = function(s) sd(s[[2]]$precisions[,k],na.rm=T)))
precisions.sd.K2.3 = unlist(lapply(res.K2, FUN = function(s) sd(s[[3]]$precisions[,k],na.rm=T)))
precisions.sd.K2.4 = unlist(lapply(res.K2, FUN = function(s) sd(s[[4]]$precisions[,k],na.rm=T)))
precisions.sd.K2.jgl = unlist(lapply(res.K2.2, FUN = function(s) sd(s$precisions.jgl[,k],na.rm=T)))
precisions.sd.K2.ggl = unlist(lapply(res.K2.2, FUN = function(s) sd(s$precisions.ggl[,k],na.rm=T)))
precisions.sd.K2.ssjgl = unlist(lapply(res.K2.2, FUN = function(s) sd(s$precisions.ssjgl[,k],na.rm=T)))
precisions.sd.K2.jointghs = unlist(lapply(res.K2.2, FUN = function(s) sd(s$precisions.jointghs[,k],na.rm=T)))
precisions.sd.K2.glasso = unlist(lapply(res.K2.2, FUN = function(s) sd(s$precisions.glasso[,k],na.rm=T)))

recalls.sd.K2 = unlist(lapply(res.K2, FUN = function(s) sd(s[[1]]$recalls[,k],na.rm=T)))
recalls.sd.K2.2 = unlist(lapply(res.K2, FUN = function(s) sd(s[[2]]$recalls[,k],na.rm=T)))
recalls.sd.K2.3 = unlist(lapply(res.K2, FUN = function(s) sd(s[[3]]$recalls[,k],na.rm=T)))
recalls.sd.K2.4 = unlist(lapply(res.K2, FUN = function(s) sd(s[[4]]$recalls[,k],na.rm=T)))
recalls.sd.K2.jgl = unlist(lapply(res.K2.2, FUN = function(s) sd(s$recalls.jgl[,k],na.rm=T)))
recalls.sd.K2.ggl = unlist(lapply(res.K2.2, FUN = function(s) sd(s$recalls.ggl[,k],na.rm=T)))
recalls.sd.K2.ssjgl = unlist(lapply(res.K2.2, FUN = function(s) sd(s$recalls.ssjgl[,k],na.rm=T)))
recalls.sd.K2.jointghs = unlist(lapply(res.K2.2, FUN = function(s) sd(s$recalls.jointghs[,k],na.rm=T)))
recalls.sd.K2.glasso = unlist(lapply(res.K2.2, FUN = function(s) sd(s$recalls.glasso[,k],na.rm=T)))


#Plotting data frame

df.K.all = data.frame(precision=c(precisions.K2, precisions.K2.jgl,  precisions.K2.ggl,precisions.K2.ssjgl, precisions.K2.jointghs, precisions.K2.glasso),
                      recall = c(recalls.K2, recalls.K2.jgl,  recalls.K2.ggl,recalls.K2.ssjgl, recalls.K2.jointghs, recalls.K2.glasso),
                      sd.prec = c(precisions.sd.K2, precisions.sd.K2.jgl,  precisions.sd.K2.ggl,precisions.sd.K2.ssjgl, 
                                  precisions.sd.K2.jointghs, precisions.sd.K2.glasso),
                      sd.rec = c(recalls.sd.K2, recalls.sd.K2.jgl, recalls.sd.K2.ggl,recalls.sd.K2.ssjgl, recalls.sd.K2.jointghs, recalls.sd.K2.glasso),
                      method = factor(c(rep(paste0('stabJGL ', expression(beta_1 == 0.01 )), n.points),rep('FGL',n.points),rep('GGL',n.points),rep('SSJGL',n.points),
                                            rep('JointGHS',n.points),rep('Glasso',n.points)),
                                      levels = c('FGL', 'GGL','Glasso', 'SSJGL', paste0('stabJGL ', expression(beta_1 == 0.01 )),'JointGHS')),
                      disagreement=c(rep(perc.disagreement,6)),similarity=c(rep(perc.similarity,6)))

df.K.otherthresh = data.frame(precision=c(precisions.K2.2, precisions.K2.3,precisions.K2.4),
                              recall = c(recalls.K2.2, recalls.K2.3,recalls.K2.4),
                              sd.prec = c(precisions.sd.K2.2, precisions.sd.K2.3, precisions.sd.K2.4),
                              sd.rec = c(recalls.sd.K2.2, recalls.sd.K2.3, recalls.sd.K2.4),
                              method = factor(c(rep(paste0('stabJGL ', expression(beta_1 == 0.05 )), n.points),
                                                    rep(paste0('stabJGL ', expression(beta_1 == 0.1 )),n.points),
                                                    rep(paste0('stabJGL ', expression(beta_1 == 0.2 )),n.points))),
                              disagreement=c(rep(perc.disagreement,3)), similarity=c(rep(perc.similarity,3)))

df.K.full = rbind(df.K.all, df.K.otherthresh)
df.K = df.K.full  %>% filter(method != 'JointGHS')

my.labs = c('FGL', 'GGL','Glasso', 'SSJGL', "stabJGL \u03b21 = 0.01", "stabJGL \u03b21 = 0.05", "stabJGL \u03b21 = 0.1", "stabJGL \u03b21 = 0.2")


g.prec.sd.smaller=ggplot2::ggplot(df.K, aes(y=precision, x=disagreement, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('lightskyblue3','plum','darkgrey','darkkhaki',
                                                                                             'darkgoldenrod2','darkorange', 'brown1','brown3'),labels=my.labs)+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) + ylim(0,1)+scale_linetype(labels=my.labs)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='bottom',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K, aes(y=recall, x=disagreement, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('lightskyblue3', 'plum','darkgrey','darkkhaki',
                                                                                             'darkgoldenrod2','darkorange', 'brown1','brown3'),labels=my.labs)+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) +  ylim(0,1)+scale_linetype(labels=my.labs)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")

quartz(type = 'pdf', file = 'extended_simulations/plots/simstudy_plot_thresh.pdf')
gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(6,6,1))
dev.off()

# Plot only stabJGL threshold results

df.K = df.K.full  %>% filter(! method %in% c('JointGHS','SSJGL','Glasso','GGL','FGL'))
my.labs = c("stabJGL \u03b21 = 0.01", "stabJGL \u03b21 = 0.05", "stabJGL \u03b21 = 0.1", "stabJGL \u03b21 = 0.2")


g.prec.sd.smaller=ggplot2::ggplot(df.K, aes(y=precision, x=disagreement, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('darkgoldenrod2','darkorange', 'brown1','brown3'),labels=my.labs)+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) + ylim(0,1)+scale_linetype(labels=my.labs)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='bottom',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K, aes(y=recall, x=disagreement, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('darkgoldenrod2','darkorange', 'brown1','brown3'),labels=my.labs)+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) +  ylim(0,1)+scale_linetype(labels=my.labs)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")

quartz(type = 'pdf', file = 'extended_simulations/plots/simstudy_plot_thresh_onlystabJGL.pdf',width=8,height=7)
gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(6,6,1))
dev.off()


# Then: plot with similarity on x axis, not disagreement -----------------------------

df.K = df.K.full  %>% filter(method != 'JointGHS')
my.labs = c('FGL', 'GGL','Glasso', 'SSJGL', "stabJGL \u03b21 = 0.01", "stabJGL \u03b21 = 0.05", "stabJGL \u03b21 = 0.1", "stabJGL \u03b21 = 0.2")



g.prec.sd.smaller=ggplot2::ggplot(df.K, aes(y=precision, x=similarity, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('lightskyblue3','plum','darkgrey','darkkhaki',
                                                                                             'darkgoldenrod2','darkorange', 'brown1','brown3'),labels=my.labs)+
  labs(x='Similarity %')+ scale_x_continuous(breaks = perc.similarity) + ylim(0,1)+scale_linetype(labels=my.labs)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='bottom',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K, aes(y=recall, x=similarity, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('lightskyblue3','plum','darkgrey','darkkhaki',
                                                                                             'darkgoldenrod2','darkorange', 'brown1','brown3'),labels=my.labs)+
  labs(x='Similarity %')+ scale_x_continuous(breaks = perc.similarity) +  ylim(0,1)+scale_linetype(labels=my.labs)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")

quartz(type = 'pdf', file = 'extended_simulations/plots/simstudy_plot_similarity_thresh.pdf')
gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(6,6,1))
dev.off()


# Only stabJGL threshol res

df.K = df.K.full  %>% filter(! method %in% c('JointGHS','SSJGL','Glasso','GGL','FGL'))
my.labs = c("stabJGL \u03b21 = 0.01", "stabJGL \u03b21 = 0.05", "stabJGL \u03b21 = 0.1", "stabJGL \u03b21 = 0.2")

g.prec.sd.smaller=ggplot2::ggplot(df.K, aes(y=precision, x=similarity, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('darkgoldenrod2','darkorange', 'brown1','brown3'),labels=my.labs)+
  labs(x='Similarity %')+ scale_x_continuous(breaks = perc.similarity) + ylim(0,1)+scale_linetype(labels=my.labs)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='bottom',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K, aes(y=recall, x=similarity, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('darkgoldenrod2','darkorange', 'brown1','brown3'),labels=my.labs)+
  labs(x='Similarity %')+ scale_x_continuous(breaks = perc.similarity) +  ylim(0,1)+scale_linetype(labels=my.labs)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")

quartz(type = 'pdf', file = 'extended_simulations/plots/simstudy_plot_similarity_thresh_onlystabJGL.pdf',width=8,height=7)
gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(6,6,1))
dev.off()


















