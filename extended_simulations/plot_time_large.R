rm(list=ls())
load("extended_simulations/data/time_simulations_extended_1_joint.Rdata")
load("extended_simulations/data/time_simulations_extended_2_large.Rdata")
load("extended_simulations/data/time_simulations_extended_3_large.Rdata")
load("extended_simulations/data/time_simulations_extended_4_large.Rdata")
load("extended_simulations/data/time_simulations_extended_5_large.Rdata")

# First for K=2

p=c(600,700,800,900,1000,1100,1200,1300,1400)

df.time.large = data.frame(time=c(time.res.large.1,time.res.large.2,time.res.large.3,time.res.large.4,time.res.large.5), p=p)


pdf(file='extended_simulations/plots/time_JoStARS.pdf',6,6)
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


pdf(file='extended_simulations/plots/time_JoStARS_K3.pdf',10,8)
g2 = ggplot2::ggplot(df.time.large.K3, aes(y=time, x=p))+ labs(title=" ")+theme_bw()+theme(legend.position = 'none', text = element_text(size = 15))+
  geom_line(colour="azure4", linewidth=1)+ 
  labs(y="CPU time (s)")+ scale_x_continuous(breaks = p) +scale_y_continuous(trans='log10')
g2
dev.off()


pdf(file='extended_simulations/plots/time_JoStARS_K2_and_K3.pdf',10,5)
ggpubr::ggarrange(g1,g2, labels=c('(a)','(b)'),hjust=-11)
dev.off()

