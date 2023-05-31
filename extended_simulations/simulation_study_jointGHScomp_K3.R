rm(list=ls())
source('simulation_functions/perform_stabJGL_simulation.R')
source('simulation_functions/help_functions.R')

# Extended simulations B and C for jointGHS

nCores = 50 # If using HPC

# Choose which scenarios to simulate
run.case1.K3 = F
run.case2.K3 = F
run.case3.K3 = F
run.case4.K3 = F
run.case5.K3 = F
run.case5.K3.2 = F
run.case5.K3.3 = F
run.case5.K3.4 = F
run.case5.K3.5 = T
run.case6.K3 = F
print.results = F

# 100 simulations per case
#N = 100
N=100
p = 100


# K = 3 data sets, of various similarity -------------
K=3
n.vals = c(150,200,250)
fracs.disagreement = c(0,0.2,0.4,0.6,0.8,1)
stars.thresh.vals = c(0.005,0.01,0.03,0.05)
ebic.gamma.val = 0.1


if(run.case1.K3){
  # Case 1: datasets from same distribution
  seeds=rep(1234,length(stars.thresh.vals)+1)
  res.1 = lapply(1:length(stars.thresh.vals), FUN = function(i) perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[1], 
                                                                                                nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                                                ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.1[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[1], 
                                                                           nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                           include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                           ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.1, file="extended_simulations/data/stabJGL_simulations_jointGHS_K3_1.Rdata")
}
if(run.case2.K3){
  # Case 2: datasets from similar distributions (80% edge agreement)
  #set.seed(1234)
  seeds=rep(111,length(stars.thresh.vals)+1)
  res.2 = lapply(1:length(stars.thresh.vals), FUN = function(i) perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[2], 
                                                                                                nCores = nCores, stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F, 
                                                                                                ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.2[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[2], 
                                                                           nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                           include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                           ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.2, file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_2.Rdata")
}
if(run.case3.K3){
  # Case 3: datasets from slightly related distributions (60% edge agreement)
  seeds=rep(1212,length(stars.thresh.vals)+1)
  res.3 = lapply(1:length(stars.thresh.vals), FUN = function(i)  perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[3], 
                                                                                                 nCores = nCores, stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                 include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F, 
                                                                                                 ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.3[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[3], 
                                                                           nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                           include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                           ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.3, file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_3.Rdata")
}
if(run.case4.K3){
  # Case 4: datasets from slightly related distributions (40% edge agreement)
  seeds=rep(1233,length(stars.thresh.vals)+1)
  res.4 = lapply(1:length(stars.thresh.vals), FUN = function(i) perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[4], 
                                                                                                nCores = nCores, stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                                                ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.4[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[4], 
                                                                           nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                           include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                           ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.4, file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_4.Rdata")
}
if(run.case5.K3){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  seeds=rep(32,length(stars.thresh.vals)+1)
  res.5 = lapply(1:length(stars.thresh.vals), FUN = function(i)  perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5],
                                                                                                 nCores = nCores, stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                 include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F, 
                                                                                                 ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.5[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5], 
                                                                           nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                           include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                           ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.5, file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_5.Rdata")
}

if(run.case5.K3.2){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  seeds=rep(321,length(stars.thresh.vals)+1)
  res.5 = lapply(1:length(stars.thresh.vals), FUN = function(i)  perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5],
                                                                                                      nCores = nCores, stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                      include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F, 
                                                                                                      ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.5[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5], 
                                                                                nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                                include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                                ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.5, file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_5_2.Rdata")
}

if(run.case5.K3.3){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  seeds=rep(33,length(stars.thresh.vals)+1)
  res.5 = lapply(1:length(stars.thresh.vals), FUN = function(i)  perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5],
                                                                                                      nCores = nCores, stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                      include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F, 
                                                                                                      ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.5[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5], 
                                                                                nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                                include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                                ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.5, file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_5_3.Rdata")
}

if(run.case5.K3.4){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  seeds=rep(44,length(stars.thresh.vals)+1)
  res.5 = lapply(1:length(stars.thresh.vals), FUN = function(i)  perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5],
                                                                                                      nCores = nCores, stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                      include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F, 
                                                                                                      ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.5[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5], 
                                                                                nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                                include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                                ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.5, file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_5_4.Rdata")
}
if(run.case5.K3.5){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  seeds=rep(42,length(stars.thresh.vals)+1)
  res.5 = lapply(1:length(stars.thresh.vals), FUN = function(i)  perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5],
                                                                                                      nCores = nCores, stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                      include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F, 
                                                                                                      ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.5[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5], 
                                                                                nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                                include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                                ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.5, file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_5_5.Rdata")
}



if(run.case6.K3){
  # Case 6: datasets from unrelated distributions
  seeds=rep(11,length(stars.thresh.vals)+1)
  res.6 = lapply(1:length(stars.thresh.vals), FUN = function(i) perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[6], 
                                                                                                nCores = nCores, stars.thresh = stars.thresh.vals[i], include.stabJGL=T,
                                                                                                include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F, 
                                                                                                ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[i]))
  res.6[[(length(stars.thresh.vals)+1)]] = perform_stabJGL_simulation_orig_seed(K,n.vals, p, N, frac.disagreement = fracs.disagreement[6], 
                                                                           nCores = nCores,stars.thresh = stars.thresh.vals[i], include.stabJGL=F,
                                                                           include.jointGHS=T, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F,
                                                                           ebic.gamma = ebic.gamma.val,v=0.5,u=0.01, seed=seeds[(length(stars.thresh.vals)+1)])
  save(res.6, file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_6.Rdata")
}

if(print.results){
  # Print results
  load("extended_simulations/data/stabJGL_simulations_jointGHS_K3_1.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_2.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_3.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_4.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_5_4.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3_6.Rdata")
  res.K3.jointGHS = list(res.1, res.2, res.3, res.4, res.5, res.6)
  save(res.K3.jointGHS,file="extended_simulations/data/stabJGL_simulations_extended_jointGHS_K3.Rdata")
  #load("extended_simulations/data/stabJGL_simulations_extended_jointGHS.Rdata")
  print_results_stabJGL(res.K3.jointGHS, fracs.disagreement, show.interval=F, show.sd=T, show.lambda = F,include.jointGHS = T, include.stabJGL = T,
                        include.GGL = F, include.JGL = F, include.glasso = F, include.SSJGL = F)
}


