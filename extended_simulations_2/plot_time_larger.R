rm(list=ls())
load("extended_simulations_2/data/time_simulations_extended_1_joint.Rdata")
load("extended_simulations_2/data/time_simulations_extended_2_large.Rdata")
load("extended_simulations_2/data/time_simulations_extended_3_large.Rdata")
load("extended_simulations_2/data/time_simulations_extended_4_large.Rdata")
library(dplyr)
library(ggplot2)

# First for K=2

p=c(2000,2500,1500,1000)

df.time.large = data.frame(time=c(time.res.large.1,time.res.large.2,time.res.large.3,time.res.large.4), p=p)


pdf(file='extended_simulations_2/plots/time_stabJGL_larger.pdf',6,6)
g1 = ggplot2::ggplot(df.time.large, aes(y=time, x=p))+ labs(title=" ")+theme_bw()+theme(legend.position = 'none', text = element_text(size = 15))+
  geom_line(colour="azure4", linewidth=1)+ 
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p) +scale_y_continuous(trans='log10')
g1
dev.off()


# Then for K=10

load("extended_simulations_2/data/time_simulations_extended_K10_1_large.Rdata")
load("extended_simulations_2/data/time_simulations_extended_K10_2_large.Rdata")
load("extended_simulations_2/data/time_simulations_extended_K10_3_large.Rdata")

p=c(200,100,150)

df.time.large.K10 = data.frame(time=c(time.res.large.K3,time.res.large.K3.2,time.res.large.K3.3), p=p)


pdf(file='extended_simulations_2/plots/time_stabJGL_K10.pdf',10,8)
g2 = ggplot2::ggplot(df.time.large.K10, aes(y=time, x=p))+ labs(title=" ")+theme_bw()+theme(legend.position = 'none', text = element_text(size = 15))+
  geom_line(colour="azure4", linewidth=1)+ 
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p) +scale_y_continuous(trans='log10')
g2
dev.off()


pdf(file='extended_simulations_2/plots/time_stabJGL_K2_and_K10.pdf',10,5)
ggpubr::ggarrange(g1,g2, labels=c('(a)','(b)'),hjust=-11)
dev.off()

# plot together with facet
df.time.large.all = rbind(df.time.large,df.time.large.K10)
df.time.large.all$K = c(rep('K=2', nrow(df.time.large)), rep('K=10', nrow(df.time.large.K10)))
p=c(100,150,200,1000,1500,2000,2500)

pdf(file='extended_simulations_2/plots/time_stabJGL_K2_and_K10_samescale.pdf',10,5)
ggplot2::ggplot(df.time.large.all, aes(y=time, x=p))+ labs(title=" ")+theme_bw()+theme(legend.position = 'none', text = element_text(size = 15))+
  geom_line(colour="azure4", linewidth=1)+ facet_wrap(~K,scales = "free_x")+  
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p) +scale_y_continuous(trans='log10')
dev.off()

# Different ticks
df.time.large.all = rbind(df.time.large,df.time.large.K10)
df.time.large.all$K = c(rep('K=2', nrow(df.time.large)), rep('K=10', nrow(df.time.large.K10)))
p=c(100,150,200,1000,1500,2000,2500)

pdf(file='extended_simulations_2/plots/time_stabJGL_K2_and_K10_samescale_ticks.pdf',10,5)
ggplot2::ggplot(df.time.large.all, aes(y=time, x=p))+ labs(title=" ")+theme_bw()+theme(legend.position = 'none', text = element_text(size = 15))+
  geom_line(colour="azure4", linewidth=1)+ facet_wrap(~K,scales = "free_x")+  
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p) +scale_y_continuous(trans='log10',breaks=c(1,10,100,1000,10000,100000))
dev.off()





