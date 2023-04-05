#' Perform joint network simulation to assess time use
#' 
#' This function performs simulations, assessing time use for all joint methods
#' 
#' @return simulation results, including sparsity, precision, recall and specificity
perform_time_simulation_joint = function(p,K,n.vals,nCores=5){
  n.p = length(p)
  # Create data sets
  data.sets=list()
  for(i in 1:length(p)){
    data.tmp = list()
    huge.init = huge.generator(n.vals[1],p[i],graph='scale-free',verbose = F,v=1,u=0.01)
    data.tmp[[1]] = scale(huge.init$data)
    for(k in 2:K){
      valid=F
      while(!valid){ # Ensure valid precision matrices
        huge.tmp = mutate.graph(huge.init,fraction=0.5,scale=T, generate.data = F)
        cov.mat = huge.tmp$cov.mat
        # Avoid rounding errors leading to matrices not being symmetric
        if(!matrixcalc::is.symmetric.matrix(cov.mat)){
          cov.mat = round(cov.mat,8)
        }
        valid = matrixcalc::is.positive.definite(cov.mat)
      }
      data.tmp[[k]] = scale(mvtnorm::rmvnorm(n.vals[k], mean=rep(0,p[i]), cov.mat))
    }
    data.sets[[i]] = data.tmp
  }
  # Perform GGL
  registerDoParallel(nCores)
  res.list = foreach (i=1:n.p) %dopar% {
    system.time(JGL_select_AIC(Y=data.sets[[i]],penalty='group',nlambda1=20,lambda1.min=0.01,lambda1.max=1,nlambda2=20,lambda2.min=0,lambda2.max=0.1,lambda2.init=0.01,
                               penalize.diagonal=FALSE))['elapsed'];
  }
  registerDoSEQ()
  times.GGL = unlist(res.list)
  #times.GGL = rep(0,n.p)
  #for(i in 1:n.p){
  #  times.GGL[i] = system.time(JGL_select_AIC(Y=data.sets[[i]],penalty='group',nlambda1=20,lambda1.min=0.01,lambda1.max=1,nlambda2=20,lambda2.min=0,lambda2.max=0.1,lambda2.init=0.01,
  #                             penalize.diagonal=FALSE))['elapsed']
  #} 
  cat('GGL done \n') 
  # Perform JoStARS
  times.jostars = rep(0,n.p)
  for(i in 1:n.p){
    times.jostars[i] = system.time(JoStARS::JoStARS(Y=data.sets[[i]],var.thresh = 0.05,subsample.ratio = NULL,rep.num = 20, nlambda1=20,scale=F,
                                               lambda1.min=0.01,lambda1.max=1, nlambda2=20,lambda2.min=0,lambda2.max = 0.1, lambda2.init = 0.01,
                                               ebic.gamma=0,verbose=F,penalize.diagonal=FALSE,parallelize = T,nCores=20))['elapsed']
    registerDoSEQ()
  }
  cat('JoStARS done \n')
  # Perform jointGHS
  registerDoParallel(nCores)
  res.list = foreach (i=1:n.p) %dopar% {
    system.time(jointGHS::jointGHS(X=data.sets[[i]], AIC_selection = F,fix_tau = T, tau_sq= 1, epsilon = 1e-3 , verbose=FALSE))['elapsed'];
  }
  registerDoSEQ()
  times.jointGHS = unlist(res.list)
  cat('jointGHS done \n')
  # Perform SSJGL
  registerDoParallel(nCores)
  res.list = foreach (i=1:n.p) %dopar% {
    system.time(SSJGL(Y=data.sets[[i]],penalty='fused',lambda0=1, lambda1=1,lambda2=1, v1 = 1, v0s = 1/(1 + c(1:10) * 5), tol.em=1e-4, a=1, b=p[i],
                      doubly=TRUE, normalize=TRUE))['elapsed'];
  }
  registerDoSEQ()
  times.SSJGL = unlist(res.list)
  cat('SSJGL done \n')
  # Perform JGL
  registerDoParallel(nCores)
  res.list = foreach (i=1:n.p) %dopar% {
    system.time(JGL_select_AIC(Y=data.sets[[i]],penalty='fused',nlambda1=20,lambda1.min=0.01,lambda1.max=1,nlambda2=20,lambda2.min=0,lambda2.max=0.1,lambda2.init=0.01,
                               penalize.diagonal=FALSE))['elapsed'];
  }
  registerDoSEQ()
  times.JGL = unlist(res.list)
  cat('JGL done \n')

  
  return(list(times.jostars = times.jostars,times.jointGHS=times.jointGHS, times.SSJGL=times.SSJGL, times.JGL = times.JGL, times.GGL = times.GGL))
}




