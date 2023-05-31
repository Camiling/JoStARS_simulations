rm(list=ls())
library(ggplot2)
#source('simulation_functions/help_functions.R')
load("extended_simulations/data/time_simulations_joint_K2.Rdata")
load("extended_simulations/data/time_simulations_joint_K3.Rdata")
load("extended_simulations/data/time_simulations_joint_K4.Rdata")
library(dplyr)

# Plot results from time simulations

p=c(20,50,100,150,200,250)
n.p = length(p)



# All joint methods

df.time.K2 = data.frame(time=c(time.res.joint.K2$times.jointGHS, time.res.joint.K2$times.JGL, time.res.joint.K2$times.SSJGL, time.res.joint.K2$times.GGL,time.res.joint.K2$times.stabJGL), p=rep(p,5),
                        method=c(rep('jointGHS',n.p),rep('FGL',n.p),rep('SSJGL',n.p),rep('GGL',n.p),rep('stabJGL',n.p)))

df.time.K3 = data.frame(time=c(time.res.joint.K3$times.jointGHS, time.res.joint.K3$times.JGL, time.res.joint.K3$times.SSJGL, time.res.joint.K3$times.GGL,time.res.joint.K3$times.stabJGL), p=rep(p,5),
                        method=c(rep('jointGHS',n.p),rep('FGL',n.p),rep('SSJGL',n.p),rep('GGL',n.p),rep('stabJGL',n.p)))

df.time.K4 = data.frame(time=c(time.res.joint.K4$times.jointGHS, time.res.joint.K4$times.JGL, time.res.joint.K4$times.SSJGL, time.res.joint.K4$times.GGL,time.res.joint.K4$times.stabJGL), p=rep(p,5),
                        method=c(rep('jointGHS',n.p),rep('FGL',n.p),rep('SSJGL',n.p),rep('GGL',n.p),rep('stabJGL',n.p)))


df.time = rbind(df.time.K2,df.time.K3,df.time.K4)
df.time$K=c(rep('K=2', n.p*5),rep('K=3', n.p*5),rep('K=4', n.p*5))

df.time.no.jointGHS = df.time %>% filter(method != 'jointGHS')


pdf(file='extended_simulations/plots/time_joint_log.pdf',8,4)
ggplot2::ggplot(df.time.no.jointGHS, aes(y=time, x=p, group=method))+ labs(title=" ")+theme_bw()+theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 15))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values = c('lightskyblue3', 'plum','darkkhaki','brown1'))+
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p)+scale_y_continuous(trans='log10',breaks = c(0,5, 50,500,5000,50000))+facet_wrap(~K)
dev.off()

df.time.no.onlyJGL = df.time %>% filter(method %in% c('FGL','stabJGL'))

pdf(file='extended_simulations/plots/time_joint_log_onlyJGL.pdf',8,4)
ggplot2::ggplot(df.time.no.onlyJGL, aes(y=time, x=p, group=method))+ labs(title=" ")+theme_bw()+theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 15))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values = c('lightskyblue3','brown1'))+
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p)+scale_y_continuous(trans='log10',breaks = c(0,5, 50,500,5000,50000))+facet_wrap(~K)
dev.off()


df.time.no.onlyJGLSSJGL = df.time %>% filter(method %in% c('FGL','stabJGL','SSJGL'))

pdf(file='extended_simulations/plots/time_joint_log_onlyJGLSSJGL.pdf',8,4)
ggplot2::ggplot(df.time.no.onlyJGLSSJGL, aes(y=time, x=p, group=method))+ labs(title=" ")+theme_bw()+theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 15))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values = c('lightskyblue3','darkkhaki','brown1'))+
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p)+scale_y_continuous(trans='log10',breaks = c(0,5, 50,500,5000,50000))+facet_wrap(~K)
dev.off()

pdf(file='extended_simulations/plots/time_joint_log_with_jointGHS.pdf',8,4)
ggplot2::ggplot(df.time, aes(y=time, x=p, group=method))+ labs(title=" ")+theme_bw()+theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 15))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+ scale_color_manual(values = c('lightskyblue3', 'plum','dodgerblue','darkkhaki','brown1'))+
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p)+scale_y_continuous(trans='log10',breaks = c(0,5, 50,500,5000,50000))+facet_wrap(~K)
dev.off()






