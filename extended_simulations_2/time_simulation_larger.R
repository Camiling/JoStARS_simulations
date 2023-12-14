rm(list=ls())
library(jointGHS) # Must be installed from Camiling/jointGHS on github
library(fastGHS) # Must be installed from Camiling/fastGHS on github
library(tailoredGlasso) # Must be installed from Camiling/tailoredGlasso on github
library(huge)
library(igraph)
library(JGL)
library(ggplot2)
library(gridExtra)
library(doParallel)
library(foreach)
library(parallel)
library(Rcpp)
library(stabJGL)
source('simulation_functions/help_functions.R')
source('extended_simulations/perform_time_simulation_large.R')

# Write print function

nCores = 20 # If using HPC

# Perform simulation study

perform_time_sim_1_joint = F
perform_time_sim_2_joint = F
perform_time_sim_3_joint = F
perform_time_sim_4_joint = F
perform_time_sim_5_joint = F
perform_time_sim_6_joint = F
perform_time_sim_7_joint = T


# JOINT SIMULATION

# Note that this simulation requires sufficient memory allocation.

if(perform_time_sim_1_joint){
  p=c(2000)
  K=2
  #n.vals=c(p,p)^2
  n.vals = c(500,500)
  time.res.large.1 = perform_time_simulation_joint_onlystabJGL(p,K=2,n.vals,nCores, nlambda1=20,nlambda2=20)
  save(time.res.large.1, file="extended_simulations_2/data/time_simulations_extended_1_joint.Rdata")
}

if(perform_time_sim_2_joint){
  p=c(2500)
  K=2
  #n.vals=c(p,p)^2
  set.seed(113)
  n.vals = c(500,500)
  time.res.large.2 = perform_time_simulation_joint_onlystabJGL(p,K=2,n.vals,nCores, nlambda1=20,nlambda2=20)
  save(time.res.large.2, file="extended_simulations_2/data/time_simulations_extended_2_large.Rdata")
}

if(perform_time_sim_3_joint){
  p=c(1500)
  K=2
  #n.vals=c(p,p)^2
  set.seed(1)
  n.vals = c(500,500)
  time.res.large.3 = perform_time_simulation_joint_onlystabJGL(p,K=2,n.vals,nCores, nlambda1=20,nlambda2=20)
  save(time.res.large.3, file="extended_simulations_2/data/time_simulations_extended_3_large.Rdata")
}

if(perform_time_sim_4_joint){
  p=c(1000) 
  K=2
  #n.vals=c(p,p)^2
  set.seed(131)
  n.vals = c(500,500)
  time.res.large.4 = perform_time_simulation_joint_onlystabJGL(p,K=2,n.vals,nCores, nlambda1=20,nlambda2=20)
  save(time.res.large.4, file="extended_simulations_2/data/time_simulations_extended_4_large.Rdata")
}



if(perform_time_sim_5_joint){
  p=c(200)
  K=10
  n.vals=rep(500,K)
  set.seed(1)
  time.res.large.K3 = perform_time_simulation_joint_onlystabJGL(p,K,n.vals,nCores, nlambda1=20,nlambda2=20)
  save(time.res.large.K3, file="extended_simulations_2/data/time_simulations_extended_K10_1_large.Rdata")
}

if(perform_time_sim_6_joint){
  p=c(100)
  K=10
  n.vals=rep(500,K)
  set.seed(1)
  time.res.large.K3.2 = perform_time_simulation_joint_onlystabJGL(p,K,n.vals,nCores, nlambda1=20,nlambda2=20)
  save(time.res.large.K3.2, file="extended_simulations_2/data/time_simulations_extended_K10_2_large.Rdata")
}

if(perform_time_sim_7_joint){
  p=c(150)
  K=10
  n.vals=rep(500,K)
  set.seed(1)
  time.res.large.K3.3 = perform_time_simulation_joint_onlystabJGL(p,K,n.vals,nCores, nlambda1=20,nlambda2=20)
  save(time.res.large.K3.3, file="extended_simulations_2/data/time_simulations_extended_K10_3_large.Rdata")
}


