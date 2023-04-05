rm(list=ls())
source('simulation_functions/help_functions.R')
#load("data/JoStARS_simulations.Rdata")
load("extended_simulations/data/JoStARS_simulations_extended_B.Rdata")
load("extended_simulations/data/JoStARS_simulations_extended_C.Rdata")

# We replace K=3 with K=2 in the below.

# Rename for simplicity
res.K3 = res.K2.2
#res.K3 = res.K3 # Case 1: p=100, K=3, 
res.K4 = res.K4.1 # Case B: p=100, K=4, 

fracs.disagreement = c(0,0.2,0.4,0.6,0.8,1)
n.points = length(fracs.disagreement)
perc.disagreement = 100*fracs.disagreement
perc.similarity = 100-perc.disagreement
k=1


# For each disagreement fraction, get measures
precisions.K3 = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.precisions[k])))
precisions.K3.jgl = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.precisions.jgl[k])))
precisions.K3.ggl = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.precisions.ggl[k])))
precisions.K3.ssjgl = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.precisions.ssjgl[k])))
precisions.K3.jointghs = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.precisions.jointghs[k])))
precisions.K3.glasso = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.precisions.glasso[k])))

recalls.K3 = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.recalls[k])))
recalls.K3.jgl = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.recalls.jgl[k])))
recalls.K3.ggl = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.recalls.ggl[k])))
recalls.K3.ssjgl = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.recalls.ssjgl[k])))
recalls.K3.jointghs = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.recalls.jointghs[k])))
recalls.K3.glasso = unlist(lapply(res.K3, FUN = function(s) mean(s$mean.recalls.glasso[k])))

precisions.K4 = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.precisions[k])))
precisions.K4.jgl = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.precisions.jgl[k])))
precisions.K4.ggl = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.precisions.ggl[k])))
precisions.K4.ssjgl = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.precisions.ssjgl[k])))
precisions.K4.jointghs = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.precisions.jointghs[k])))
precisions.K4.glasso = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.precisions.glasso[k])))

recalls.K4 = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.recalls[k])))
recalls.K4.jgl = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.recalls.jgl[k])))
recalls.K4.ggl = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.recalls.ggl[k])))
recalls.K4.ssjgl = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.recalls.ssjgl[k])))
recalls.K4.jointghs = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.recalls.jointghs[k])))
recalls.K4.glasso = unlist(lapply(res.K4, FUN = function(s) mean(s$mean.recalls.glasso[k])))

# Also standard deviations
precisions.sd.K3 = unlist(lapply(res.K3, FUN = function(s) sd(s$precisions[,k])))
precisions.sd.K3.jgl = unlist(lapply(res.K3, FUN = function(s) sd(s$precisions.jgl[,k])))
precisions.sd.K3.ggl = unlist(lapply(res.K3, FUN = function(s) sd(s$precisions.ggl[,k])))
precisions.sd.K3.ssjgl = unlist(lapply(res.K3, FUN = function(s) sd(s$precisions.ssjgl[,k])))
precisions.sd.K3.jointghs = unlist(lapply(res.K3, FUN = function(s) sd(s$precisions.jointghs[,k])))
precisions.sd.K3.glasso = unlist(lapply(res.K3, FUN = function(s) sd(s$precisions.glasso[,k])))

recalls.sd.K3 = unlist(lapply(res.K3, FUN = function(s) sd(s$recalls[,k])))
recalls.sd.K3.jgl = unlist(lapply(res.K3, FUN = function(s) sd(s$recalls.jgl[,k])))
recalls.sd.K3.ggl = unlist(lapply(res.K3, FUN = function(s) sd(s$recalls.ggl[,k])))
recalls.sd.K3.ssjgl = unlist(lapply(res.K3, FUN = function(s) sd(s$recalls.ssjgl[,k])))
recalls.sd.K3.jointghs = unlist(lapply(res.K3, FUN = function(s) sd(s$recalls.jointghs[,k])))
recalls.sd.K3.glasso = unlist(lapply(res.K3, FUN = function(s) sd(s$recalls.glasso[,k])))

precisions.sd.K4 = unlist(lapply(res.K4, FUN = function(s) sd(s$precisions[,k])))
precisions.sd.K4.jgl = unlist(lapply(res.K4, FUN = function(s) sd(s$precisions.jgl[,k])))
precisions.sd.K4.ggl = unlist(lapply(res.K4, FUN = function(s) sd(s$precisions.ggl[,k])))
precisions.sd.K4.ssjgl = unlist(lapply(res.K4, FUN = function(s) sd(s$precisions.ssjgl[,k])))
precisions.sd.K4.jointghs = unlist(lapply(res.K4, FUN = function(s) sd(s$precisions.jointghs[,k])))
precisions.sd.K4.glasso = unlist(lapply(res.K4, FUN = function(s) sd(s$precisions.glasso[,k])))

recalls.sd.K4 = unlist(lapply(res.K4, FUN = function(s) sd(s$recalls[,k])))
recalls.sd.K4.jgl = unlist(lapply(res.K4, FUN = function(s) sd(s$recalls.jgl[,k])))
recalls.sd.K4.ggl = unlist(lapply(res.K4, FUN = function(s) sd(s$recalls.ggl[,k])))
recalls.sd.K4.ssjgl = unlist(lapply(res.K4, FUN = function(s) sd(s$recalls.ssjgl[,k])))
recalls.sd.K4.jointghs = unlist(lapply(res.K4, FUN = function(s) sd(s$recalls.jointghs[,k])))
recalls.sd.K4.glasso = unlist(lapply(res.K4, FUN = function(s) sd(s$recalls.glasso[,k])))

#Plotting data frame

df.K.all = data.frame(precision=c(precisions.K3, precisions.K3.jgl,  precisions.K3.ggl,precisions.K3.ssjgl, precisions.K3.jointghs, precisions.K3.glasso,
                              precisions.K4, precisions.K4.jgl,  precisions.K4.ggl,precisions.K4.ssjgl, precisions.K4.jointghs, precisions.K4.glasso),
                  recall = c(recalls.K3, recalls.K3.jgl,  recalls.K3.ggl,recalls.K3.ssjgl, recalls.K3.jointghs, recalls.K3.glasso,
                             recalls.K4, recalls.K4.jgl,  recalls.K4.ggl,recalls.K4.ssjgl, recalls.K4.jointghs, recalls.K4.glasso),
                  sd.prec = c(precisions.sd.K3, precisions.sd.K3.jgl,  precisions.sd.K3.ggl,precisions.sd.K3.ssjgl, precisions.sd.K3.jointghs, precisions.sd.K3.glasso,
                              precisions.sd.K4, precisions.sd.K4.jgl,  precisions.sd.K4.ggl,precisions.sd.K4.ssjgl, precisions.sd.K4.jointghs, precisions.sd.K4.glasso),
                  sd.rec = c(recalls.sd.K3, recalls.sd.K3.jgl, recalls.sd.K3.ggl,recalls.sd.K3.ssjgl, recalls.sd.K3.jointghs, recalls.sd.K3.glasso,
                              recalls.sd.K4, recalls.sd.K4.jgl, recalls.sd.K4.ggl,recalls.sd.K4.ssjgl, recalls.sd.K4.jointghs, recalls.sd.K4.glasso),
                  method = factor(rep(c(rep('JoStARS', n.points),rep('FGL',n.points),rep('GGL',n.points),rep('SSJGL',n.points),
                                        rep('JointGHS',n.points),rep('Glasso',n.points)),2),
                                  levels = c('FGL', 'GGL','JoStARS','SSJGL','Glasso', 'JointGHS')),
                  K = factor(c(rep('K=2', n.points*6), rep('K=4', 6*n.points)),levels=c('K=2','K=4')),
                  disagreement=c(rep(perc.disagreement,12)),similarity=c(rep(perc.similarity,12)))



df.K = df.K.all  %>% filter(method != 'JointGHS')


g.prec.sd.smaller=ggplot2::ggplot(df.K, aes(y=precision, x=disagreement, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('olivedrab3','dodgerblue','darkorange','maroon2','blueviolet'))+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) + facet_wrap(~K) + ylim(0,1)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='bottom',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K, aes(y=recall, x=disagreement, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('olivedrab3','dodgerblue','darkorange','maroon2','blueviolet'))+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) + facet_wrap(~K)+ ylim(0,1)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")

pdf(file='plots/simstudy_plot.pdf',9,7)
gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(6,6,1))
dev.off()


# Plot without SSJGL
df.K.red = df.K.all  %>% filter(! method %in% c('JointGHS','SSJGL'))

g.prec.sd.smaller=ggplot2::ggplot(df.K.red, aes(y=precision, x=disagreement, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('olivedrab3','dodgerblue','darkorange','blueviolet'))+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) + facet_wrap(~K) + ylim(0,1)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='bottom',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K.red, aes(y=recall, x=disagreement, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('olivedrab3','dodgerblue','darkorange','blueviolet'))+
  labs(x='Disagreement %')+ scale_x_continuous(breaks = perc.disagreement) + facet_wrap(~K)+ ylim(0,1)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")

pdf(file='plots/simstudy_plot_noSSJGL.pdf',9,7)
gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(6,6,1))
dev.off()

# Then: plot with similarity on x axis, not disagreement -----------------------------



df.K = df.K.all  %>% filter(method != 'JointGHS')


g.prec.sd.smaller=ggplot2::ggplot(df.K, aes(y=precision, x=similarity, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('olivedrab3','dodgerblue','darkorange','maroon2','blueviolet'))+
  labs(x='Similarity%')+ scale_x_continuous(breaks = perc.similarity) + facet_wrap(~K) + ylim(0,1)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='bottom',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K, aes(y=recall, x=similarity, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('olivedrab3','dodgerblue','darkorange','maroon2','blueviolet'))+
  labs(x='Similarity %')+ scale_x_continuous(breaks = perc.similarity) + facet_wrap(~K)+ ylim(0,1)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")

pdf(file='plots/simstudy_plot_similarity.pdf',9,7)
gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(6,6,1))
dev.off()


# Plot without SSJGL
df.K.red = df.K.all  %>% filter(! method %in% c('JointGHS','SSJGL'))

g.prec.sd.smaller=ggplot2::ggplot(df.K.red, aes(y=precision, x=similarity, group=method, colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('olivedrab3','dodgerblue','darkorange','blueviolet'))+
  labs(x='Similarity %')+ scale_x_continuous(breaks = perc.similarity) + facet_wrap(~K) + ylim(0,1)+
  geom_errorbar(aes(ymin=precision-sd.prec, ymax=precision+sd.prec),width=2)+theme_bw()+ theme(legend.position='bottom',text = element_text(size = 15))

g.rec.sd.smaller = ggplot2::ggplot(df.K.red, aes(y=recall, x=similarity, group=method,colour=method))+ labs(title=" ")+theme(plot.title = element_text(hjust = 0.5))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values=c('olivedrab3','dodgerblue','darkorange','blueviolet'))+
  labs(x='Similarity %')+ scale_x_continuous(breaks = perc.similarity) + facet_wrap(~K)+ ylim(0,1)+
  geom_errorbar(aes(ymin=recall-sd.rec, ymax=recall+sd.rec),width=2)+ theme_bw()+theme(legend.position = "none",text = element_text(size = 15))

tmp = ggplot2::ggplot_gtable(ggplot2::ggplot_build(g.prec.sd.smaller))
leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend = tmp$grobs[[leg]]
g.prec.sd.smaller = g.prec.sd.smaller + theme(legend.position = "none")

pdf(file='plots/simstudy_plot_noSSJGL_similarity.pdf',9,7)
gridExtra::grid.arrange(g.prec.sd.smaller,g.rec.sd.smaller,legend,ncol=1,heights=c(6,6,1))
dev.off()


