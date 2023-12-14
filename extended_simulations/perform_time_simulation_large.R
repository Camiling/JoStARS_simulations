
#' Perform joint network simulation to assess time use
#' 
#' This function performs simulations, assessing time use for stabJGL
#' 
#' @return simulation results, including sparsity, precision, recall and specificity
perform_time_simulation_joint_onlystabJGL = function(p,K,n.vals,nCores=20, nlambda1=20, nlambda2=20){
  n.p = length(p)
  # Create data sets
  data.sets=list()
  for(i in 1:length(p)){
    data.tmp = list()
    huge.init = huge.generator(n.vals[1],p[i],graph='scale-free',verbose = F)
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
  # Perform stabJGL
  times.stabJGL = rep(0,n.p)
  for(i in 1:n.p){
    times.stabJGL[i] = system.time(stabJGL::stabJGL(Y=data.sets[[i]],var.thresh = 0.05,subsample.ratio = NULL,rep.num = 20, nlambda1=nlambda1,scale=F,
                                                    lambda1.min=0.01,lambda1.max=1, nlambda2=nlambda2,lambda2.min=0,lambda2.max = 0.1, lambda2.init = 0.01,
                                                    ebic.gamma=0,verbose=F,penalize.diagonal=FALSE,parallelize = T,nCores=nCores))['elapsed']
    registerDoSEQ()
  }
  return(times.stabJGL)
}


#' Perform joint network simulation to assess time use
#' 
#' This function performs simulations, assessing time use for  jointGHS
#' 
#' @return simulation results, including sparsity, precision, recall and specificity
perform_time_simulation_joint_onlyjointGHS = function(p,K,n.vals,nCores=5){
  n.p = length(p)
  # Create data sets
  data.sets=list()
  for(i in 1:length(p)){
    data.tmp = list()
    huge.init = huge.generator(n.vals[1],p[i],graph='scale-free',verbose = F)
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
  # Perform jointGHS
  times.jointGHS = rep(0,n.p)
  for(i in 1:n.p){
    times.jointGHS[i] = system.time(jointGHS::jointGHS(X=data.sets[[i]],epsilon=1e-3,AIC_selection = F, fix_tau = T, tau_sq= 1,verbose=F,stop_overflow = T))['elapsed']
    registerDoSEQ()
  }
  return(times.jointGHS)
}