rm(list=ls())
source('simulation_functions/perform_stabJGL_simulation.R')
source('simulation_functions/help_functions.R')

nCores = 50 # If using HPC

# Perform simulation study

# Choose which scenarios to simulate
run.case1 = F
run.case2 = F
run.case3 = F
run.case4 = F
run.case5 = F
run.case6 = T
print.results = F

# 100 simulations per case
N = 100
p = 300

# K = 2 data sets, of various similarity -------------
K=2
n.vals = c(200,250)
fracs.disagreement = c(0,0.2,0.4,0.6,0.8,1)

if(run.case1){
  # Case 1: datasets from same distribution
  set.seed(1234)
  res.1 = perform_stabJGL_simulation(K,n.vals, p, N, frac.disagreement = fracs.disagreement[1], nCores = nCores, include.SSJGL = F, stars.thresh = 0.1)
  save(res.1, file="extended_simulations/data/stabJGL_simulations_extended_D_1.Rdata")
}
if(run.case2){
  # Case 2: datasets from similar distributions (80% edge agreement)
  set.seed(1234)
  res.2 = perform_stabJGL_simulation(K,n.vals, p, N, frac.disagreement = fracs.disagreement[2], nCores = nCores, include.SSJGL = F, stars.thresh = 0.1)
  save(res.2, file="extended_simulations/data/stabJGL_simulations_extended_D_2.Rdata")
}
if(run.case3){
  # Case 3: datasets from slightly related distributions (60% edge agreement)
  set.seed(1234)
  res.3 = perform_stabJGL_simulation(K,n.vals, p, N, frac.disagreement = fracs.disagreement[3], nCores = nCores, include.SSJGL = F, stars.thresh = 0.1)
  save(res.3, file="extended_simulations/data/stabJGL_simulations_extended_D_3.Rdata")
}
if(run.case4){
  # Case 4: datasets from slightly related distributions (40% edge agreement)
  set.seed(1234)
  res.4 = perform_stabJGL_simulation(K,n.vals, p, N, frac.disagreement = fracs.disagreement[4], nCores = nCores, include.SSJGL = F, stars.thresh = 0.1)
  save(res.4, file="extended_simulations/data/stabJGL_simulations_extended_D_4.Rdata")
}
if(run.case5){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  set.seed(1234)
  res.5 = perform_stabJGL_simulation(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5], nCores = nCores, include.SSJGL = F, stars.thresh = 0.1)
  save(res.5, file="extended_simulations/data/stabJGL_simulations_extended_D_5.Rdata")
}
if(run.case6){
  # Case 6: datasets from unrelated distributions
  set.seed(1234)
  res.6 = perform_stabJGL_simulation(K,n.vals, p, N, frac.disagreement = fracs.disagreement[6], nCores = nCores, include.SSJGL = F, stars.thresh = 0.1)
  save(res.6, file="extended_simulations/data/stabJGL_simulations_extended_D_6.Rdata")
}

if(print.results){
  # Print results
  load("extended_simulations/data/stabJGL_simulations_extended_D_1.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_D_2.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_D_3.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_D_4.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_D_5.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_D_6.Rdata")
  res.K2= list(res.1, res.2, res.3, res.4, res.5, res.6)
  #save(res.K2,file="extended_simulations/data/stabJGL_simulations_extended_D.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_D.Rdata")
  print_results_stabJGL(res.K2, fracs.disagreement, show.interval=F, show.sd=T, show.lambda = T, include.jointGHS = F, include.SSJGL = F)
}


