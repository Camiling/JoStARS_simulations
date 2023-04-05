
#' Perform joint network simulation to assess time use
#' 
#' This function performs simulations, assessing time use for JoStARS
#' 
#' @return simulation results, including sparsity, precision, recall and specificity
perform_time_simulation_joint_onlyJoStARS = function(p,K,n.vals,nCores=5){
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
  # Perform JoStARS
  times.jostars = rep(0,n.p)
  for(i in 1:n.p){
    times.jostars[i] = system.time(JoStARS::JoStARS(Y=data.sets[[i]],var.thresh = 0.05,subsample.ratio = NULL,rep.num = 20, nlambda1=20,scale=F,
                                                    lambda1.min=0.01,lambda1.max=1, nlambda2=20,lambda2.min=0,lambda2.max = 0.1, lambda2.init = 0.01,
                                                    ebic.gamma=0,verbose=F,penalize.diagonal=FALSE,parallelize = T,nCores=20))['elapsed']
    registerDoSEQ()
  }
  return(times.jostars)
}