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
run.case6 = F
run.case1.bic = F
run.case2.bic = F
run.case3.bic = F
run.case4.bic = F
run.case5.bic = F
run.case6.bic = T
print.results = F

# 100 simulations per case
N = 100
p = 100

# K = 4 data sets, of various similarity -------------
K=4
n.vals = c(150,200,250,300)
fracs.disagreement = c(0,0.2,0.4,0.6,0.8,1)
ebic.gamma=0.2

if(run.case1){
  # Case 1: datasets from same distribution
  set.seed(1234)
  res.1 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[1], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.1, file="extended_simulations/data/stabJGL_simulations_extended_B_1_JGLeBIC.Rdata")
}
if(run.case2){
  # Case 2: datasets from similar distributions (80% edge agreement)
  set.seed(1234)
  res.2 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[2], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.2, file="extended_simulations/data/stabJGL_simulations_extended_B_2_JGLeBIC.Rdata")
}
if(run.case3){
  # Case 3: datasets from slightly related distributions (60% edge agreement)
  set.seed(1234)
  res.3 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[3], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.3, file="extended_simulations/data/stabJGL_simulations_extended_B_3_JGLeBIC.Rdata")
}
if(run.case4){
  # Case 4: datasets from slightly related distributions (40% edge agreement)
  set.seed(1234)
  res.4 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[4], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.4, file="extended_simulations/data/stabJGL_simulations_extended_B_4_JGLeBIC.Rdata")
}
if(run.case5){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  set.seed(1234)
  res.5 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.5, file="extended_simulations/data/stabJGL_simulations_extended_B_5_JGLeBIC.Rdata")
}
if(run.case6){
  # Case 6: datasets from unrelated distributions
  set.seed(1234)
  res.6 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[6], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.6, file="extended_simulations/data/stabJGL_simulations_extended_B_6_JGLeBIC.Rdata")
}

if(print.results){
  # Print results
  load("extended_simulations/data/stabJGL_simulations_extended_B_1_JGLeBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_2_JGLeBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_3_JGLeBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_4_JGLeBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_5_JGLeBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_6_JGLeBIC.Rdata")
  res.K4.1 = list(res.1, res.2, res.3, res.4, res.5, res.6)
  save(res.K4.1,file="extended_simulations/data/stabJGL_simulations_extended_B_JGLeBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_JGLeBIC.Rdata")
  print_results_stabJGL(res.K4.1, fracs.disagreement, show.interval=F, show.sd=T, show.lambda = T, include.jointGHS = F, include.GGL = F,include.glasso = F,
                        include.SSJGL = F, include.stabJGL = F)
}

# Then for ordinary BIC

ebic.gamma=0

if(run.case1.bic){
  # Case 1: datasets from same distribution
  set.seed(1234)
  res.1 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[1], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.1, file="extended_simulations/data/stabJGL_simulations_extended_B_1_JGLBIC.Rdata")
}
if(run.case2.bic){
  # Case 2: datasets from similar distributions (80% edge agreement)
  set.seed(1234)
  res.2 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[2], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.2, file="extended_simulations/data/stabJGL_simulations_extended_B_2_JGLBIC.Rdata")
}
if(run.case3.bic){
  # Case 3: datasets from slightly related distributions (60% edge agreement)
  set.seed(1234)
  res.3 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[3], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.3, file="extended_simulations/data/stabJGL_simulations_extended_B_3_JGLBIC.Rdata")
}
if(run.case4.bic){
  # Case 4: datasets from slightly related distributions (40% edge agreement)
  set.seed(1234)
  res.4 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[4], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.4, file="extended_simulations/data/stabJGL_simulations_extended_B_4_JGLBIC.Rdata")
}
if(run.case5.bic){
  # Case 5: datasets from slightly related distributions (20% edge agreement)
  set.seed(1234)
  res.5 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[5], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.5, file="extended_simulations/data/stabJGL_simulations_extended_B_5_JGLBIC.Rdata")
}
if(run.case6.bic){
  # Case 6: datasets from unrelated distributions
  set.seed(1234)
  res.6 = perform_stabJGL_simulation_orig(K,n.vals, p, N, frac.disagreement = fracs.disagreement[6], nCores = nCores,include.stabJGL=F,include.jointGHS=F, include.glasso=F, 
                                          include.SSJGL=F, include.JGL = TRUE, include.GGL = F, ebic.gamma=ebic.gamma,JGL.eBIC=TRUE)
  save(res.6, file="extended_simulations/data/stabJGL_simulations_extended_B_6_JGLBIC.Rdata")
}

if(print.results){
  # Print results
  load("extended_simulations/data/stabJGL_simulations_extended_B_1_JGLBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_2_JGLBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_3_JGLBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_4_JGLBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_5_JGLBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_6_JGLBIC.Rdata")
  res.K4.1 = list(res.1, res.2, res.3, res.4, res.5, res.6)
  save(res.K4.1,file="extended_simulations/data/stabJGL_simulations_extended_B_JGLBIC.Rdata")
  load("extended_simulations/data/stabJGL_simulations_extended_B_JGLBIC.Rdata")
  print_results_stabJGL(res.K4.1, fracs.disagreement, show.interval=F, show.sd=T, show.lambda = T, include.jointGHS = F, include.GGL = F,include.glasso = F,
                        include.SSJGL = F, include.stabJGL = F)
}

