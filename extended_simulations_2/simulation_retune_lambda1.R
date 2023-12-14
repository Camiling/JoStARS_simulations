# Redoing the simulations in simulation_study.R 

rm(list=ls())
source('simulation_functions/perform_stabJGL_simulation.R')
source('simulation_functions/help_functions.R')

nCores = 50 # If using HPC

# Perform simulation study

# Choose which scenarios to simulate
run.case1 = T
run.case2 = T
run.case3 = T
run.case4 = T
run.case5 = T
run.case6 = T
print.results = F

# 100 simulations per case
#N = 100
N=100
p = 100

# K = 3 data sets, of various similarity -------------
K=3
n.vals = c(150,200,300)
fracs.disagreement = c(0,0.2,0.4,0.6,0.8,1)

if(run.case1){
  # Case 1: datasets from same distribution
  set.seed(1234)
  res.1 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[1], nCores = nCores, stars.thresh = 0.1, 
                                          include.SSJGL = F,include.GGL = F,include.glasso = F, include.JGL = F,include.jointGHS = F, retune.lambda1=TRUE)
  save(res.1, file="extended_simulations_2/data/stabJGL_simulations_1_retune.RData")
}
if(run.case2){
  # Case 2: datasets from similar distributions (80% edge agreement)
  set.seed(1234)
  res.2 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[2], nCores = nCores, stars.thresh = 0.1,
                                          include.SSJGL = F,include.GGL = F,include.glasso = F, include.JGL = F,include.jointGHS = F, retune.lambda1=TRUE)
  save(res.2, file="extended_simulations_2/data/stabJGL_simulations_2_retune.RData")
}
if(run.case3){
  # Case 3: datasets from slightly related distributions (60% edge agreement)
  set.seed(1234)
  res.3 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[3], nCores = nCores, stars.thresh = 0.1,
                                          include.SSJGL = F,include.GGL = F,include.glasso = F, include.JGL = F,include.jointGHS = F, retune.lambda1=TRUE)
  save(res.3, file="extended_simulations_2/data/stabJGL_simulations_3_retune.RData")
}
if(run.case4){
  # Case 4: datasets from slightly related distributions (40% edge agreement)
  set.seed(1234)
  res.4 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[4], nCores = nCores, stars.thresh = 0.1, 
                                          include.SSJGL = F,include.GGL = F,include.glasso = F, include.JGL = F,include.jointGHS = F, retune.lambda1=TRUE)
  save(res.4, file="extended_simulations_2/data/stabJGL_simulations_4_retune.RData")
}
if(run.case5){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  set.seed(1234)
  res.5 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5], nCores = nCores, stars.thresh = 0.1,
                                          include.SSJGL = F,include.GGL = F,include.glasso = F, include.JGL = F,include.jointGHS = F, retune.lambda1=TRUE)
  save(res.5, file="extended_simulations_2/data/stabJGL_simulations_5_retune.RData")
}
if(run.case6){
  # Case 6: datasets from unrelated distributions
  set.seed(1234)
  res.6 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[6], nCores = nCores, stars.thresh = 0.1, retune.lambda1=TRUE)
  save(res.6, file="extended_simulations_2/data/stabJGL_simulations_6_retune.RData")
}

if(print.results){
  # Print results
  load("extended_simulations_2/data/stabJGL_simulations_1_retune.RData")
  load("extended_simulations_2/data/stabJGL_simulations_2_retune.RData")
  load("extended_simulations_2/data/stabJGL_simulations_3_retune.RData")
  load("extended_simulations_2/data/stabJGL_simulations_4_retune.RData")
  load("extended_simulations_2/data/stabJGL_simulations_5_retune.RData")
  load("extended_simulations_2/data/stabJGL_simulations_6_retune.RData")
  res.K3 = list(res.1, res.2, res.3, res.4, res.5, res.6)
  #save(res.K3,file="extended_simulations_2/data/stabJGL_simulations_retune.RData")
  load("extended_simulations_2/data/stabJGL_simulations_retune.RData")
  print_results_stabJGL(res.K3, fracs.disagreement, show.interval=F, show.sd=T, show.lambda = T, include.jointGHS = F, include.GGL = F,
                        include.glasso = F,include.JGL = F, include.SSJGL = F)
}




