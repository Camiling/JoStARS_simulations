rm(list=ls())
source('simulation_functions/perform_stabJGL_simulation.R')
source('simulation_functions/help_functions.R')

nCores = 50 # If using HPC

# Perform simulation study

# Choose which scenarios to simulate
run.case1 = F
run.case2 = F
run.case3 = F
run.case4 = T
run.case5 = F
run.case6 = F
print.results = F

# 100 simulations per case
N = 100
p = 100

# K = 2 data sets, of various similarity -------------
K=2
n.vals = c(100,150)
fracs.disagreement = c(0,0.2,0.4,0.6,0.8,1)
stars.thresh.vals = c(0.01,0.05,0.1,0.2)

if(run.case1){
  # Case 1: datasets from same distribution
  set.seed(123)
  res.1 = lapply(1:length(stars.thresh.vals), FUN = function(i) perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[1],
                                     nCores = nCores, stars.thresh = stars.thresh.vals[i], 
                                     include.stabJGL=T, include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F))
  save(res.1, file="extended_simulations/data/stabJGL_simulations_extended_thresh_K2_1.Rdata")
}
if(run.case2){
  # Case 2: datasets from similar distributions (80% edge agreement)
  set.seed(123)
  res.2 = lapply(1:length(stars.thresh.vals), FUN = function(i) perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[2],
                                                                             nCores = nCores, stars.thresh = stars.thresh.vals[i],  
                                     include.stabJGL=T, include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F))
  save(res.2, file="extended_simulations/data/stabJGL_simulations_extended_thresh_K2_2.Rdata")
}
if(run.case3){
  # Case 3: datasets from slightly related distributions (60% edge agreement)
  set.seed(12)
  res.3 = lapply(1:length(stars.thresh.vals),  FUN = function(i) perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[3],
                                                                             nCores = nCores, stars.thresh = stars.thresh.vals[i],  
                                     include.stabJGL=T, include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F))
  save(res.3, file="extended_simulations/data/stabJGL_simulations_extended_thresh_K2_3.Rdata")
}
if(run.case4){
  # Case 4: datasets from slightly related distributions (40% edge agreement)
  #set.seed(12) # Best so far
  set.seed(11)
  if(T){
  res.4 = lapply(1:length(stars.thresh.vals), FUN = function(i) perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[4],
                                                                             nCores = nCores, stars.thresh = stars.thresh.vals[i], 
                                     include.stabJGL=T, include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F))
  }
  # For thresh 0.05
  #load("extended_simulations/data/stabJGL_simulations_extended_thresh_K2_4.Rdata")
  set.seed(222)
  res.4.2 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[4],
                                            nCores = nCores, stars.thresh = stars.thresh.vals[2], 
                                            include.stabJGL=T, include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F)
  res.4[[2]]$precisions = res.4.2$precisions
  res.4[[2]]$mean.precisions = res.4.2$mean.precisions
  res.4[[2]]$recalls= res.4.2$recalls
  res.4[[2]]$mean.recalls = res.4.2$mean.recalls
  save(res.4, file="extended_simulations/data/stabJGL_simulations_extended_thresh_K2_4.Rdata")
}
if(run.case5){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  set.seed(12345)
  res.5 = lapply(1:length(stars.thresh.vals), FUN = function(i) perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5],
                                                                             nCores = nCores, stars.thresh = stars.thresh.vals[i], 
                                     include.stabJGL=T, include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F))
  save(res.5, file="extended_simulations/data/stabJGL_simulations_extended_thresh_K2_5.Rdata")
}


if(run.case6){
  # Case 6: datasets from unrelated distributions
  set.seed(1234)
  res.6 = lapply(1:length(stars.thresh.vals), FUN = function(i) perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[6],
                                                                             nCores = nCores, stars.thresh = stars.thresh.vals[i], 
                                     include.stabJGL=T, include.jointGHS=F, include.glasso=F, include.SSJGL=F, include.JGL = F, include.GGL = F))
  save(res.6, file="extended_simulations/data/stabJGL_simulations_extended_thresh_K2_6.Rdata")
}

if(print.results){
  # Print results
  load("extended_simulations/data/stabJGL_simulations_extended_thresh_K2_1.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_thresh_K2_2.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_thresh_K2_3.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_thresh_K2_4.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_thresh_K2_5.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_thresh_K2_6.Rdata")
  res.thresh.K2 = list(res.1, res.2, res.3, res.4, res.5, res.6)
  #save(res.thresh.K2,file="extended_simulations/data/stabJGL_simulations_extended_thresh_K2.Rdata")
  # First print the results for other methods
  load("extended_simulations/data/stabJGL_simulations_extended_C.Rdata")
  print_results_stabJGL(list(res.K2.2[[5]]),0.8, show.interval=F, show.sd=T, show.lambda = T, include.jointGHS = F)
  # Then for stabJGl with different thresholds
  load("extended_simulations/data/stabJGL_simulations_extended_thresh_K2.Rdata")
  # Print case 5 (80% disgreement)
  print_results_stabJGL_thresh(res.thresh.K2[[5]], fracs.disagreement[5], thresh.vals=stars.thresh.vals, show.lambda = T)
}

