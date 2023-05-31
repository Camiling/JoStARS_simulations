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
source("SSJGL/R/SSJGL.R")
source("SSJGL/R/JGL.R")
source("SSJGL/R/admm.iters.R")
source("SSJGL/R/eval.R")
source("SSJGL/R/gete.R")
source("extended_simulations/perform_time_simulation.R")
source('simulation_functions/help_functions.R')



# Write print function

nCores = 20 # If using HPC
do.K2 = FALSE
do.K3 = TRUE
do.K4 = FALSE

# Perform simulation study

perform_time_sim_joint= TRUE
p=c(20,50,100,150,200,250)

if(perform_time_sim_joint){
  # First for K=2 networks
  if(do.K2){
    K=2
    n.vals = c(100,150)
    set.seed(33)
    time.res.joint.K2= perform_time_simulation_joint(p,K,n.vals,nCores)
    save(time.res.joint.K2, file="extended_simulations/data/time_simulations_joint_K2.Rdata")
  }
  # First for K=3 networks
  if(do.K3){
    K=3
    n.vals = c(100,150,150)
    set.seed(122)
    time.res.joint.K3= perform_time_simulation_joint(p,K,n.vals,nCores)
    save(time.res.joint.K3, file="extended_simulations/data/time_simulations_joint_K3.Rdata")
  }
  # First for K=4 networks
  if(do.K4){
    K=4
    n.vals = c(100,100,150,150)
    set.seed(123)
    time.res.joint.K4= perform_time_simulation_joint(p,K,n.vals,nCores)
    save(time.res.joint.K4, file="extended_simulations/data/time_simulations_joint_K4.Rdata")
  }
}


