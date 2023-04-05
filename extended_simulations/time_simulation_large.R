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
library(JoStARS)
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
perform_time_sim_7_joint = F
perform_time_sim_8_joint = T

# JOINT SIMULATION

if(perform_time_sim_1_joint){
  p=c(600,700)
  K=2
  n.vals=c(500, 500)
  time.res.large.1 = perform_time_simulation_joint_onlyJoStARS(p,K=2,n.vals,nCores)
  save(time.res.large.1, file="extended_simulations/data/time_simulations_extended_1_joint.Rdata")
}

if(perform_time_sim_2_joint){
  p=c(800,900)
  K=2
  n.vals=c(500,500)
  time.res.large.2 = perform_time_simulation_joint_onlyJoStARS(p,K=2,n.vals,nCores)
  save(time.res.large.2, file="extended_simulations/data/time_simulations_extended_2_large.Rdata")
}

if(perform_time_sim_3_joint){
  p=c(1000)
  K=2
  n.vals=c(500, 500)
  time.res.large.3 = perform_time_simulation_joint_onlyJoStARS(p,K=2,n.vals,nCores)
  save(time.res.large.3, file="extended_simulations/data/time_simulations_extended_3_large.Rdata")
}

if(perform_time_sim_4_joint){
  p=c(1100,1200)
  K=2
  n.vals=c(500, 500)
  time.res.large.4 = perform_time_simulation_joint_onlyJoStARS(p,K=2,n.vals,nCores)
  save(time.res.large.4, file="extended_simulations/data/time_simulations_extended_4_large.Rdata")
}

if(perform_time_sim_5_joint){
  p=c(1300,1400)
  K=2
  n.vals=c(500, 500)
  time.res.large.5 = perform_time_simulation_joint_onlyJoStARS(p,K=2,n.vals,nCores)
  save(time.res.large.5, file="extended_simulations/data/time_simulations_extended_5_large.Rdata")
}

if(perform_time_sim_6_joint){
  p=c(500,600)
  K=3
  n.vals=c(500, 500,500)
  time.res.large.K3 = perform_time_simulation_joint_onlyJoStARS(p,K,n.vals,nCores)
  save(time.res.large.K3, file="extended_simulations/data/time_simulations_extended_K3_1_large.Rdata")
}

if(perform_time_sim_7_joint){
  p=c(700,800)
  K=3
  n.vals=c(500, 500,500)
  time.res.large.K3.2 = perform_time_simulation_joint_onlyJoStARS(p,K,n.vals,nCores)
  save(time.res.large.K3.2, file="extended_simulations/data/time_simulations_extended_K3_2_large.Rdata")
}

if(perform_time_sim_8_joint){
  p=c(900,1000)
  K=3
  n.vals=c(500, 500,500)
  time.res.large.K3.3 = perform_time_simulation_joint_onlyJoStARS(p,K,n.vals,nCores)
  save(time.res.large.K3.3, file="extended_simulations/data/time_simulations_extended_K3_3_large.Rdata")
}



