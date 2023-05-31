rm(list=ls())
load("extended_simulations/data/time_simulations_extended_1_joint.Rdata")
load("extended_simulations/data/time_simulations_extended_2_large.Rdata")
load("extended_simulations/data/time_simulations_extended_3_large.Rdata")
load("extended_simulations/data/time_simulations_extended_4_large.Rdata")
load("extended_simulations/data/time_simulations_extended_5_large.Rdata")
library(dplyr)
library(ggplot2)

# First for K=2

p=c(600,700,800,900,1000,1100,1200,1300,1400)

df.time.large = data.frame(time=c(time.res.large.1,time.res.large.2,time.res.large.3,time.res.large.4,time.res.large.5), p=p)


pdf(file='extended_simulations/plots/time_stabJGL.pdf',6,6)
g1 = ggplot2::ggplot(df.time.large, aes(y=time, x=p))+ labs(title=" ")+theme_bw()+theme(legend.position = 'none', text = element_text(size = 15))+
  geom_line(colour="azure4", linewidth=1)+ 
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p) +scale_y_continuous(trans='log10')
g1
dev.off()


# Then for K=3

load("extended_simulations/data/time_simulations_extended_K3_1_large.Rdata")
load("extended_simulations/data/time_simulations_extended_K3_2_large.Rdata")
load("extended_simulations/data/time_simulations_extended_K3_3_large.Rdata")

p=c(500,600,700,800,900,1000)

df.time.large.K3 = data.frame(time=c(time.res.large.K3,time.res.large.K3.2,time.res.large.K3.3), p=p)


pdf(file='extended_simulations/plots/time_stabJGL_K3.pdf',10,8)
g2 = ggplot2::ggplot(df.time.large.K3, aes(y=time, x=p))+ labs(title=" ")+theme_bw()+theme(legend.position = 'none', text = element_text(size = 15))+
  geom_line(colour="azure4", linewidth=1)+ 
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p) +scale_y_continuous(trans='log10')
g2
dev.off()


pdf(file='extended_simulations/plots/time_stabJGL_K2_and_K3.pdf',10,5)
ggpubr::ggarrange(g1,g2, labels=c('(a)','(b)'),hjust=-11)
dev.off()

# plot together with facet
df.time.large.all = rbind(df.time.large,df.time.large.K3)
df.time.large.all$K = c(rep('K=2', nrow(df.time.large)), rep('K=3', nrow(df.time.large.K3)))
p=c(500,600,700,800,900,1000,1100,1200,1300,1400)

pdf(file='extended_simulations/plots/time_stabJGL_K2_and_K3_samescale.pdf',10,5)
ggplot2::ggplot(df.time.large.all, aes(y=time, x=p))+ labs(title=" ")+theme_bw()+theme(legend.position = 'none', text = element_text(size = 15))+
  geom_line(colour="azure4", linewidth=1)+ facet_wrap(~K,scales = "free_x")+  
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p) +scale_y_continuous(trans='log10')
dev.off()

# Then plot against jointGHS ------------------------------------
load("extended_simulations/data/time_simulations_extended_1_joint_jointGHS.Rdata")
load("extended_simulations/data/time_simulations_extended_2_large_jointGHS.Rdata")
load("extended_simulations/data/time_simulations_extended_3_large_jointGHS.Rdata")
load("extended_simulations/data/time_simulations_extended_4_large_jointGHS.Rdata")
load("extended_simulations/data/time_simulations_extended_5_large_jointGHS.Rdata")
load("extended_simulations/data/time_simulations_extended_K3_1_large_jointGHS.Rdata")
load("extended_simulations/data/time_simulations_extended_K3_2_large_jointGHS.Rdata")
load("extended_simulations/data/time_simulations_extended_K3_3_large_jointGHS.Rdata")
load("extended_simulations/data/time_simulations_extended_small_joint_jointGHS.Rdata")
load("extended_simulations/data/time_simulations_extended_small_joint_stabJGL.Rdata")
load("extended_simulations/data/time_simulations_extended_small_joint_jointGHS_K3.Rdata")
load("extended_simulations/data/time_simulations_extended_small_joint_stabJGL_K3.Rdata")

# First for K=2

p=c(600,700,800,900,1000,1100,1200,1300,1400)
df.time.large.jointGHS = data.frame(time=c(time.res.large.1.jointGHS,time.res.large.2.jointGHS,time.res.large.3.jointGHS,time.res.large.4.jointGHS,time.res.large.5.jointGHS), p=p)
df.time.large.comb = rbind(df.time.large,df.time.large.jointGHS)
df.time.large.comb$method = c(rep('stabJGL',length(p)), rep('jointGHS',length(p)))

# Then for K=3

#time.res.large.K3.2.jointGHS = time.res.large.K3.jointGHS
time.res.large.K3.3.jointGHS = time.res.large.K3.jointGHS
p.2=c(500,600,700,800,900,1000)
df.time.large.K3.jointGHS = data.frame(time=c(time.res.large.K3.jointGHS,time.res.large.K3.2.jointGHS,time.res.large.K3.3.jointGHS), p=p.2)
df.time.large.K3.comb = rbind(df.time.large.K3,df.time.large.K3.jointGHS)
df.time.large.K3.comb$method = c(rep('stabJGL',length(p.2)), rep('jointGHS',length(p.2)))

df.full = rbind(df.time.large.comb,df.time.large.K3.comb)
df.full$K = c(rep('K=2',nrow(df.time.large.comb)),rep('K=3',nrow(df.time.large.K3.comb)))

# Smaller dfs
#time.res.large.small.stabJGL_K3 = time.res.large.small.jointGHS_K3
p.small=c(100,200,300,400)
df.smaller = data.frame(time = c(time.res.large.small.jointGHS,time.res.large.small.stabJGL,time.res.large.small.jointGHS_K3,time.res.large.small.stabJGL_K3),
                        p=p.small,method=c(rep('jointGHS',length(p.small)),rep('stabJGL',length(p.small)),rep('jointGHS',length(p.small)),rep('stabJGL',length(p.small))),
                        K=c(rep('K=2',2*length(p.small)),rep('K=3',2*length(p.small)) ))

df.full.2 = rbind(df.full,df.smaller)
df.full.2.2 = df.full.2 %>% filter(! (p==500 & method == 'jointGHS'))
df.full.2.3 = df.full.2.2 %>% filter(p<700)

p=c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400)
g1.comb = ggplot2::ggplot(df.full.2.3, aes(y=time, x=p, group=method))+ labs(title=" ")+theme_bw()+theme(text = element_text(size = 15))+
  geom_line(aes(colour=method, linetype=method), linewidth=1.2)+scale_color_manual(values = c('dodgerblue','brown1'))+ facet_wrap(~K,scales = "free_x")+ 
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p) +scale_y_continuous(trans='log10')

pdf(file='extended_simulations/plots/time_jointGHS_vs_stabJGL.pdf',8,4)
g1.comb
dev.off()




