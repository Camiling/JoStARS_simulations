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

#' Perform jointGHS simulations
#' 
#' This function performs simulations for jointGHS, averaging over the results. 
#' 
#' @param K number of data sets/networks
#' @param n.vals the number of observations in each data set. A vector
#' @param p the number of nodes
#' @param N the number of simulations to perform
#' @param seeds seeds to use in each of the \eqn{N} simulations. A vector of length \eqn{N}. 
#' @param nCores how many cores should be used
#' @param frac.disagreement the fraction of edges that the networks should disagree on
#' @param method how should the similarity between the prec matrices be? Symmetric by default, meaning all are equally different. If not, one will stand out as completely unrelated.
#' @param include.stabJGL should stabJGL be performed?
#' @param include.jointGHS should the jointGHS be performed?
#' @param include.glasso should single-network graphical lasso be performed?
#' @param include.SSJGL should we perform SSJGL?
#' @param include.JGL should the fused joint graphical lasso be included?
#' @param include.GGL should the group joint graphical lasso be included?
#' @param penalize.diagonal should the diagonal be penalized in the graphical lasso-based methods?
#' @param verbose logical indicator of printing information at each iteration
#' @param scale should the data be scaled?
#' @param singleVSjoint is the simulation comparing single and joint GHS?
#' @return simulation results, including sparsity, precision, recall and specificity
perform_stabJGL_simulation = function(K, n.vals, p, N=100, seeds=sample(1:1000,N), nCores = 3, frac.disagreement = 0, method='symmetric', include.stabJGL=TRUE,
                                       include.jointGHS=TRUE, include.glasso=TRUE, include.SSJGL=TRUE, include.JGL = TRUE, include.GGL = TRUE,
                                       penalize.diagonal=FALSE, verbose=TRUE, scale=TRUE, singleVSjoint=FALSE,stars.thresh = 0.05, nlambda1=20,lambda1.min=0.01,
                                       lambda1.max=1,nlambda2=20,lambda2.min=0,lambda2.max=0.1,lambda2.init=0.01,ebic.gamma=0, larger.partialcor=F){
 
  res=list()
  # stabJGL results
  n.thresh = length(stars.thresh)
  res$opt.sparsities = matrix(0,N,K)
  res$precisions =  matrix(0,N,K)
  res$specificities =  matrix(0,N,K)
  res$recalls =  matrix(0,N,K)
  res$lambda1 = rep(0,N)
  res$lambda2 = rep(0,N)
  if(n.thresh>1){
    # up to two other thresholds
    res$opt.sparsities.2 = matrix(0,N,K)
    res$precisions.2 =  matrix(0,N,K)
    res$specificities.2 =  matrix(0,N,K)
    res$recalls.2 =  matrix(0,N,K)
    res$lambda1.2 = rep(0,N)
    res$lambda2.2 = rep(0,N)
    res$opt.sparsities.3 = matrix(0,N,K)
    res$precisions.3 =  matrix(0,N,K)
    res$specificities.3 =  matrix(0,N,K)
    res$recalls.3 =  matrix(0,N,K)
    res$lambda1.3 = rep(0,N)
    res$lambda2.3 = rep(0,N)
    res$opt.sparsities.4 = matrix(0,N,K)
    res$precisions.4 =  matrix(0,N,K)
    res$specificities.4 =  matrix(0,N,K)
    res$recalls.4 =  matrix(0,N,K)
    res$lambda1.4 = rep(0,N)
    res$lambda2.4 = rep(0,N)
  }
  # JointGHS results
  res$opt.sparsities.jointghs = matrix(0,N,K)
  res$precisions.jointghs =  matrix(0,N,K)
  res$specificities.jointghs =  matrix(0,N,K)
  res$recalls.jointghs =  matrix(0,N,K)
  # SSJGL results 
  res$opt.sparsities.ssjgl = matrix(0,N,K)
  res$precisions.ssjgl =  matrix(0,N,K)
  res$specificities.ssjgl =  matrix(0,N,K)
  res$recalls.ssjgl =  matrix(0,N,K)
  # JGL results (fused, tuned by AIC)
  res$opt.sparsities.jgl = matrix(0,N,K)
  res$precisions.jgl =  matrix(0,N,K)
  res$specificities.jgl =  matrix(0,N,K)
  res$recalls.jgl =  matrix(0,N,K)
  res$lambda1.jgl = rep(0,N)
  res$lambda2.jgl = rep(0,N)
  # GGL results (group JGL, tuned by AIC)
  res$opt.sparsities.ggl = matrix(0,N,K)
  res$precisions.ggl =  matrix(0,N,K)
  res$specificities.ggl =  matrix(0,N,K)
  res$recalls.ggl =  matrix(0,N,K)
  res$lambda1.ggl = rep(0,N)
  res$lambda2.ggl = rep(0,N)
  # Glasso results
  res$opt.sparsities.glasso = matrix(0,N,K)
  res$precisions.glasso =  matrix(0,N,K)
  res$specificities.glasso =  matrix(0,N,K)
  res$recalls.glasso =  matrix(0,N,K)
  res$lambda.glasso = matrix(0,N,K)
  
  # Start by generating the precision matrices 
  cov.matrices = list()
  prec.matrices = list()
  
  # Start by generating the first prec matrix
  #huge.init = huge::huge.generator(n.vals[1],p,graph='scale-free',verbose = F,v=1,u=0.01) # REMOVED THIS NOW
  if(larger.partialcor) huge.init = huge::huge.generator(n.vals[1],p,graph='scale-free',verbose = F,v=1,u=0.01) 
  else if(n.thresh>1 & include.jointGHS) huge.init = huge::huge.generator(n.vals[1],p,graph='scale-free',verbose = F,v=0.5,u=0.01) 
  else huge.init = huge::huge.generator(n.vals[1],p,graph='scale-free',verbose = F)
  theta.init = huge.init$omega
  theta.init[which(abs(theta.init)<1e-5,arr.ind=T)] = 0
  spars.init = huge.init$sparsity
  cov.matrices[[1]] = huge.init$sigma
  prec.matrices[[1]] = theta.init
  
  # Added this 
  if(scale) cov.matrices[[1]] = cov2cor(cov.matrices[[1]])
  if(scale) prec.matrices[[1]] = cov2cor(prec.matrices[[1]])
  
  # Avoid rounding errors leading to matrices not being symmetric
  if(!matrixcalc::is.symmetric.matrix(cov.matrices[[1]])){
    cov.matrices[[1]] = round(cov.matrices[[1]],8)
  }
  if(method=='symmetric'){
    for(k in 2:K){
      valid=F
      while(!valid){ # Ensure valid precision matrices
        if(larger.partialcor) huge.tmp = mutate.graph(huge.init,frac.disagreement,scale=F, generate.data = F, larger.partialcor = larger.partialcor, thresh.cov=F)
        else if(n.thresh>1 & include.jointGHS) huge.tmp = mutate.graph(huge.init,frac.disagreement,scale=F, generate.data = F, thresh.cov=F,v=0.5,u=0.01)
        else huge.tmp = mutate.graph(huge.init,frac.disagreement,scale, generate.data = F, larger.partialcor = F)
        cov.matrices[[k]] = huge.tmp$cov.mat
        prec.matrices[[k]] = huge.tmp$prec.mat
        # Avoid rounding errors leading to matrices not being symmetric
        if(!matrixcalc::is.symmetric.matrix(cov.matrices[[k]])){
          cov.matrices[[k]] = round(cov.matrices[[k]],8)
          valid = matrixcalc::is.symmetric.matrix(cov.matrices[[k]])
        }
        if(valid){
          valid = matrixcalc::is.positive.definite(cov.matrices[[k]])
        }
      }
      if(larger.partialcor | (n.thresh>1 & include.jointGHS)) {
        cov.matrices[[k]] = cov2cor(cov.matrices[[k]])
        prec.matrices[[k]] = cov2cor(prec.matrices[[k]])
      }
    }
    cat('Done generating networks \n')
  }
  else{ # First K-1 graphs are similar
    for(k in 2:(K-1)){
      huge.tmp = mutate.graph(huge.init,frac.disagreement,scale)
      cov.matrices[[k]] = huge.tmp$cov.mat
      prec.matrices[[k]] = huge.tmp$prec.mat
      # Avoid rounding errors leading to matrices not being symmetric
      if(!matrixcalc::is.symmetric.matrix(cov.matrices[[k]])){
        cov.matrices[[k]] = round(cov.matrices[[k]],8)
      }
    }
    # Last graph is completely different
    huge.tmp = mutate.graph(huge.init,fraction = 1, scale)
    cov.matrices[[K]] = huge.tmp$cov.mat
    prec.matrices[[K]] = huge.tmp$prec.mat
    # Avoid rounding errors leading to matrices not being symmetric
    if(!matrixcalc::is.symmetric.matrix(cov.matrices[[K]])){
      cov.matrices[[K]] = round(cov.matrices[[K]],8)
    }
  }
  res.list.2 = list()
  if(n.thresh>1){ # Do stabJGL in parallel for multiple thresholds
    res.list=list()
    # First stabJGL for different thresholds
    for(i in 1:N){
      res.list[[i]] = stabJGL_simulation_one_iteration(n.vals=n.vals,cov.matrices=cov.matrices,prec.matrices=prec.matrices,scale=scale,
                                       include.stabJGL = include.stabJGL, include.jointGHS=F, include.glasso=F, include.SSJGL=F, 
                                       include.JGL=F,include.GGL=F, penalize.diagonal=penalize.diagonal,seed=seeds[i], 
                                       singleVSjoint=F,stars.thresh = stars.thresh,nlambda1=nlambda1,lambda1.min=lambda1.min,
                                       lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init, 
                                       ebic.gamma=ebic.gamma, nCores=nCores)
    }
    cat('stabJGL done \n')
    # Then other methods in parallel
    registerDoParallel(max(floor(nCores/K)),2)
    res.list.2 = foreach (i=1:N) %dopar% {
      tryCatch({stabJGL_simulation_one_iteration(n.vals=n.vals,cov.matrices=cov.matrices,prec.matrices=prec.matrices,scale=scale,
                                                 include.stabJGL = F, include.jointGHS=include.jointGHS, include.glasso=include.glasso, include.SSJGL=include.SSJGL, 
                                                 include.JGL=include.JGL,include.GGL=include.GGL, penalize.diagonal=penalize.diagonal,seed=seeds[i], 
                                                 singleVSjoint,stars.thresh = stars.thresh,nlambda1=nlambda1,lambda1.min=lambda1.min,
                                                 lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init, 
                                                 ebic.gamma=ebic.gamma)}, error=function(cond) { return(NULL)});
    }
    registerDoSEQ()
    cat('jointGHS done \n')
  }
  else{
    registerDoParallel(nCores)
    res.list = foreach (i=1:N) %dopar% {
      tryCatch({stabJGL_simulation_one_iteration(n.vals=n.vals,cov.matrices=cov.matrices,prec.matrices=prec.matrices,scale=scale,
                                                 include.stabJGL = include.stabJGL, include.jointGHS=include.jointGHS, include.glasso=include.glasso, include.SSJGL=include.SSJGL, 
                                                 include.JGL=include.JGL,include.GGL=include.GGL, penalize.diagonal=penalize.diagonal,seed=seeds[i], 
                                                 singleVSjoint,stars.thresh = stars.thresh,nlambda1=nlambda1,lambda1.min=lambda1.min,
                                                 lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init, 
                                                 ebic.gamma=ebic.gamma)}, error=function(cond) { return(NULL)});
    }
    registerDoSEQ()
  }
  # Save results from each replicates
  for(i in 1:N){
    est.tmp = res.list[[i]]
    est.tmp.2 = res.list.2[[i]]
    
    # Results from stabJGL
    if(include.stabJGL){
      res$opt.sparsities[i,] = est.tmp$opt.sparsities 
      res$precisions[i,] = est.tmp$precisions
      res$recalls[i,] = est.tmp$recalls
      res$specificities[i,] =  est.tmp$specificities
      res$lambda1[i] = est.tmp$opt.lambda1
      res$lambda2[i] = est.tmp$opt.lambda2
      if(n.thresh>1){
        res$opt.sparsities.2[i,] = est.tmp$opt.sparsities.2 
        res$precisions.2[i,] = est.tmp$precisions.2
        res$recalls.2[i,] = est.tmp$recalls.2
        res$specificities.2[i,] =  est.tmp$specificities.2
        res$lambda1.2[i] = est.tmp$opt.lambda1.2
        res$lambda2.2[i] = est.tmp$opt.lambda2.2
        res$opt.sparsities.3[i,] = est.tmp$opt.sparsities.3 
        res$precisions.3[i,] = est.tmp$precisions.3
        res$recalls.3[i,] = est.tmp$recalls.3
        res$specificities.3[i,] =  est.tmp$specificities.3
        res$lambda1.3[i] = est.tmp$opt.lambda1.3
        res$lambda2.3[i] = est.tmp$opt.lambda2.3
        res$opt.sparsities.4[i,] = est.tmp$opt.sparsities.4 
        res$precisions.4[i,] = est.tmp$precisions.4
        res$recalls.4[i,] = est.tmp$recalls.4
        res$specificities.4[i,] =  est.tmp$specificities.4
        res$lambda1.4[i] = est.tmp$opt.lambda1.4
        res$lambda2.4[i] = est.tmp$opt.lambda2.4
      }
    }
    # Results from jointGHS
    if(include.jointGHS){
      if(n.thresh>1){
        res$opt.sparsities.jointghs[i,] = est.tmp.2$opt.sparsities.jointghs
        res$precisions.jointghs[i,] = est.tmp.2$precisions.jointghs
        res$recalls.jointghs[i,] = est.tmp.2$recalls.jointghs
        res$specificities.jointghs[i,] =  est.tmp.2$specificities.jointghs
      }
      else{
        res$opt.sparsities.jointghs[i,] = est.tmp$opt.sparsities.jointghs
        res$precisions.jointghs[i,] = est.tmp$precisions.jointghs
        res$recalls.jointghs[i,] = est.tmp$recalls.jointghs
        res$specificities.jointghs[i,] =  est.tmp$specificities.jointghs
      }
    }
    # Results from jgl
    if(include.JGL){
      res$opt.sparsities.jgl[i,] = est.tmp$opt.sparsities.jgl
      res$precisions.jgl[i,] = est.tmp$precisions.jgl
      res$recalls.jgl[i,] = est.tmp$recalls.jgl
      res$specificities.jgl[i,] =  est.tmp$specificities.jgl 
      res$lambda1.jgl[i] = est.tmp$opt.lambda1.jgl
      res$lambda2.jgl[i] = est.tmp$opt.lambda2.jgl
    }
    # Results from ggl
    if(include.GGL){
      res$opt.sparsities.ggl[i,] = est.tmp$opt.sparsities.ggl
      res$precisions.ggl[i,] = est.tmp$precisions.ggl
      res$recalls.ggl[i,] = est.tmp$recalls.ggl
      res$specificities.ggl[i,] =  est.tmp$specificities.ggl 
      res$lambda1.ggl[i] = est.tmp$opt.lambda1.ggl
      res$lambda2.ggl[i] = est.tmp$opt.lambda2.ggl
    }
    # Results from ssjgl
    if(include.SSJGL){
      res$opt.sparsities.ssjgl[i,] = est.tmp$opt.sparsities.ssjgl
      res$precisions.ssjgl[i,] = est.tmp$precisions.ssjgl
      res$recalls.ssjgl[i,] = est.tmp$recalls.ssjgl
      res$specificities.ssjgl[i,] =  est.tmp$specificities.ssjgl 
    }
    # Results from glasso
    if(include.glasso){
      res$opt.sparsities.glasso[i,] = est.tmp$opt.sparsities.glasso
      res$precisions.glasso[i,] = est.tmp$precisions.glasso
      res$recalls.glasso[i,] = est.tmp$recalls.glasso
      res$specificities.glasso[i,] =  est.tmp$specificities.glasso
      res$lambda.glasso[i,] = est.tmp$lambda.glasso
    }
  }
  # Mean results from stabJGL
  if(include.stabJGL){
    res$mean.opt.sparsities= colMeans(res$opt.sparsities,na.rm=T)
    res$mean.precisions= colMeans(res$precisions,na.rm=T)
    res$mean.recalls = colMeans(res$recalls,na.rm=T)
    res$mean.specificities =  colMeans(res$specificities,na.rm=T)
    res$mean.lambda1 = mean(res$lambda1,na.rm=T)
    res$mean.lambda2 = mean(res$lambda2,na.rm=T)
    if(n.thresh>1){
      res$mean.opt.sparsities.2= colMeans(res$opt.sparsities.2,na.rm=T)
      res$mean.precisions.2= colMeans(res$precisions.2,na.rm=T)
      res$mean.recalls.2 = colMeans(res$recalls.2,na.rm=T)
      res$mean.specificities.2 =  colMeans(res$specificities.2,na.rm=T)
      res$mean.lambda1.2 = mean(res$lambda1.2,na.rm=T)
      res$mean.lambda2.2 = mean(res$lambda2.2,na.rm=T)
      res$mean.opt.sparsities.3= colMeans(res$opt.sparsities.3,na.rm=T)
      res$mean.precisions.3= colMeans(res$precisions.3,na.rm=T)
      res$mean.recalls.3 = colMeans(res$recalls.3,na.rm=T)
      res$mean.specificities.3 =  colMeans(res$specificities.3,na.rm=T)
      res$mean.lambda1.3 = mean(res$lambda1.3,na.rm=T)
      res$mean.lambda2.3 = mean(res$lambda2.3,na.rm=T)
      res$mean.opt.sparsities.4= colMeans(res$opt.sparsities.4,na.rm=T)
      res$mean.precisions.4= colMeans(res$precisions.4,na.rm=T)
      res$mean.recalls.4 = colMeans(res$recalls.4,na.rm=T)
      res$mean.specificities.4 =  colMeans(res$specificities.4,na.rm=T)
      res$mean.lambda1.4 = mean(res$lambda1.4,na.rm=T)
      res$mean.lambda2.4 = mean(res$lambda2.4,na.rm=T)
    }
  }
  # Mean results from jointGHS
  if(include.jointGHS){
      res$mean.opt.sparsities.jointghs= colMeans(res$opt.sparsities.jointghs)
      res$mean.precisions.jointghs= colMeans(res$precisions.jointghs)
      res$mean.recalls.jointghs = colMeans(res$recalls.jointghs)
      res$mean.specificities.jointghs =  colMeans(res$specificities.jointghs)
  }
  # Mean results from JGL
  if(include.JGL){
    res$mean.opt.sparsities.jgl = colMeans(res$opt.sparsities.jgl)
    res$mean.precisions.jgl = colMeans(res$precisions.jgl)
    res$mean.recalls.jgl = colMeans(res$recalls.jgl)
    res$mean.specificities.jgl =  colMeans(res$specificities.jgl)
    res$mean.lambda1.jgl = mean(res$lambda1.jgl)
    res$mean.lambda2.jgl = mean(res$lambda2.jgl)
  }
  # Mean results from GGL
  if(include.GGL){
    res$mean.opt.sparsities.ggl = colMeans(res$opt.sparsities.ggl)
    res$mean.precisions.ggl = colMeans(res$precisions.ggl)
    res$mean.recalls.ggl = colMeans(res$recalls.ggl)
    res$mean.specificities.ggl =  colMeans(res$specificities.ggl)
    res$mean.lambda1.ggl = mean(res$lambda1.ggl)
    res$mean.lambda2.ggl = mean(res$lambda2.ggl)
  }
  # Mean results from SSJGL
  if(include.SSJGL){
    res$mean.opt.sparsities.ssjgl = colMeans(res$opt.sparsities.ssjgl)
    res$mean.precisions.ssjgl = colMeans(res$precisions.ssjgl)
    res$mean.recalls.ssjgl = colMeans(res$recalls.ssjgl)
    res$mean.specificities.ssjgl =  colMeans(res$specificities.ssjgl)
  }
  # Mean results from glasso
  if(include.glasso){
    res$mean.opt.sparsities.glasso = colMeans(res$opt.sparsities.glasso)
    res$mean.precisions.glasso = colMeans(res$precisions.glasso)
    res$mean.recalls.glasso = colMeans(res$recalls.glasso)
    res$mean.specificities.glasso =  colMeans(res$specificities.glasso)
    res$mean.lambda.glasso = colMeans(res$lambda.glasso)
  }
  res$true.prec.matrices = prec.matrices
  res$true.sparsity = spars.init
  return(res)
}

# Function for performing one iteration -----------------------------------------

stabJGL_simulation_one_iteration = function(n.vals,cov.matrices,prec.matrices,scale,include.stabJGL, include.jointGHS, include.glasso, include.SSJGL,include.JGL, 
                                            include.GGL, penalize.diagonal,seed,singleVSjoint,stars.thresh,nlambda1,lambda1.min,lambda1.max, 
                                            nlambda2,lambda2.min,lambda2.max, lambda2.init,ebic.gamma, nCores) {
  y = list()
  n.thresh=length(stars.thresh)
  K=length(n.vals)
  p=ncol(prec.matrices[[1]])
  glasso.res = list()
  lambdas.glasso = rep(0,K)
  set.seed(seed)
  # Generate data. 
  for(k in 1:K){
    y[[k]] = mvtnorm::rmvnorm(n.vals[k], mean=rep(0,p), cov.matrices[[k]])
    if (scale) y[[k]] = scale(y[[k]])
    # Use graphical lasso
    if(include.glasso & !singleVSjoint){
      glasso.tmp = huge::huge(y[[k]],method='glasso',verbose = F)
      glasso.res[[k]] = huge::huge.select(glasso.tmp,criterion='stars', stars.thresh = stars.thresh, verbose = F)
      lambdas.glasso[k] = glasso.res[[k]]$opt.lambda
    }
  }
  # Perform joint methods
  if(include.stabJGL){
    if(n.thresh>1){
      res.full.tmp = tryCatch({stabJGL::stabJGL(Y=y,var.thresh = stars.thresh[1],subsample.ratio = NULL,rep.num = 20, nlambda1=nlambda1,scale=T,
                                                  lambda1.min=lambda1.min,lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init,
                                                  ebic.gamma=ebic.gamma,verbose=F,penalize.diagonal=FALSE,parallelize = T, nCores=nCores)},
                                error=function(cond) { return(NULL)})
      registerDoSEQ()
      if(length(res.full.tmp)==0) res.full.tmp = NULL
      res.tmp = res.full.tmp$opt.fit
      joint.spars = res.full.tmp$opt.sparsities
      res.full.tmp.2 = tryCatch({stabJGL::stabJGL(Y=y,var.thresh = stars.thresh[2],subsample.ratio = NULL,rep.num = 20, nlambda1=nlambda1,scale=T,
                                                lambda1.min=lambda1.min,lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init,
                                                ebic.gamma=ebic.gamma,verbose=F,penalize.diagonal=FALSE,parallelize = T, nCores=nCores)},
                              error=function(cond) { return(NULL)})
      registerDoSEQ()
      if(length(res.full.tmp.2)==0) res.full.tmp.2 = NULL
      res.tmp.2 = res.full.tmp.2$opt.fit
      joint.spars.2 = res.full.tmp.2$opt.sparsities
      if(n.thresh>2){
        res.full.tmp.3 = tryCatch({stabJGL::stabJGL(Y=y,var.thresh = stars.thresh[3],subsample.ratio = NULL,rep.num = 20, nlambda1=nlambda1,scale=T,
                                                    lambda1.min=lambda1.min,lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init,
                                                    ebic.gamma=ebic.gamma,verbose=F,penalize.diagonal=FALSE,parallelize = T, nCores=nCores)},
                                  error=function(cond) { return(NULL)})
        registerDoSEQ()
        if(length(res.full.tmp.3)==0) res.full.tmp.3 = NULL
        res.tmp.3 = res.full.tmp.3$opt.fit
        joint.spars.3 = res.full.tmp.3$opt.sparsities
      }
      if(n.thresh>3){
        res.full.tmp.4 = tryCatch({stabJGL::stabJGL(Y=y,var.thresh = stars.thresh[4],subsample.ratio = NULL,rep.num = 20, nlambda1=nlambda1,scale=T,
                                                    lambda1.min=lambda1.min,lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init,
                                                    ebic.gamma=ebic.gamma,verbose=F,penalize.diagonal=FALSE,parallelize = T, nCores=nCores)},
                                  error=function(cond) { return(NULL)})
        registerDoSEQ()
        if(length(res.full.tmp.4)==0) res.full.tmp.4 = NULL
        res.tmp.4 = res.full.tmp.4$opt.fit
        joint.spars.4 = res.full.tmp.4$opt.sparsities
      }
    }
    else {
      res.full.tmp = tryCatch({stabJGL::stabJGL(Y=y,var.thresh = stars.thresh,subsample.ratio = NULL,rep.num = 20, nlambda1=nlambda1,scale=T,
                                    lambda1.min=lambda1.min,lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init,
                                    ebic.gamma=ebic.gamma,verbose=F,penalize.diagonal=FALSE,parallelize = F)},
                            error=function(cond) { return(NULL)})
      res.tmp = res.full.tmp$opt.fit
      joint.spars = res.full.tmp$opt.sparsities
    }
    # if comparing glasso to stabJGL, force glasso to the same sparsity
    if(singleVSjoint){
      for(k in 1:K){
        glasso.tmp = huge::huge(y[[k]],nlambda=20,method='glasso',verbose = F)
        ind.glasso = which.min(abs(joint.spars[[k]]-glasso.tmp$sparsity))
        glasso.res[[k]] = round(glasso.tmp$icov[[ind.glasso]], 5)
        lambdas.glasso[k] = glasso.tmp$lambda[ind.glasso]
      }
    }
  }
  if(include.jointGHS){
    res.full.tmp.jointghs = jointGHS::jointGHS(X=y, AIC_selection = T, AIC_eps = 0.1, epsilon=1e-3, verbose = F,nCores=1,scale=T, stop_overflow = T)
    res.tmp.jointghs = res.full.tmp.jointghs$theta
    joint.spars.jointghs = unlist(lapply(res.tmp.jointghs, FUN= function(m) tailoredGlasso::sparsity(abs(cov2cor(m))>1e-5)))
    #joint.spars.jointghs = unlist(lapply(res.tmp.jointghs, FUN= function(m) tailoredGlasso::sparsity(abs(cov2cor(m))>1e-5)))
  }
  if(include.SSJGL){
    lam1 = 1
    lam2 = 1
    v1 = 1
    lam.eff = lam1 + c(1:10) * 5
    v0s = lam1/lam.eff
    ssjgl.tmp = SSJGL(Y=y,penalty='fused',lambda0=1, lambda1=lam1,lambda2=lam2, v1 = v1, v0s = v0s, tol.em=1e-4, a=1, b=p, doubly=TRUE, normalize=TRUE)
    ssjgl.tmp = ssjgl.tmp$thetalist[[10]]
  }
  if(include.JGL){
    jgl.tmp = JGL_select_AIC(Y=y,penalty='fused',nlambda1=nlambda1,lambda1.min=lambda1.min,lambda1.max=lambda1.max,nlambda2=nlambda2,lambda2.min=lambda2.min,
                             lambda2.max=lambda2.max,lambda2.init = lambda2.init,penalize.diagonal=penalize.diagonal)
  }
  if(include.GGL){
    ggl.tmp = JGL_select_AIC(Y=y,penalty='group',nlambda1=nlambda1,lambda1.min=lambda1.min,lambda1.max=lambda1.max,nlambda2=nlambda2,lambda2.min=lambda2.min,
                             lambda2.max=lambda2.max,lambda2.init = lambda2.init,penalize.diagonal=penalize.diagonal)
  }
  est=list()
  
  # Results from stabJGL
  if(include.stabJGL){ 
    res.tmp = lapply(res.tmp, cov2cor)
    if(length(res.tmp)==0) {
      est$opt.sparsities  = NA
      est$precisions =  NA
      est$recalls = NA
      est$specificities =  NA
      est$opt.lambda1 = NA
      est$opt.lambda2 = NA
    }
    else{
      est$opt.sparsities  = joint.spars
      est$precisions =  sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, res.tmp[[k]]!=0))
      est$recalls =  sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, res.tmp[[k]]!=0))
      est$specificities =  sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, res.tmp[[k]]!=0 ))
      est$opt.lambda1 = res.full.tmp$opt.lambda1
      est$opt.lambda2 = res.full.tmp$opt.lambda2
    }
    if(length(stars.thresh)>1){
      res.tmp.2 = lapply(res.tmp.2, cov2cor)
      if(length(res.tmp.2)==0) {
        est$opt.sparsities.2  = NA
        est$precisions.2 =  NA
        est$recalls.2 = NA
        est$specificities.2 =  NA
        est$opt.lambda1.2 = NA
        est$opt.lambda2.2 = NA
      }
      else{
        est$opt.sparsities.2  = joint.spars.2
        est$precisions.2 =  sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, res.tmp.2[[k]]!=0))
        est$recalls.2 =  sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, res.tmp.2[[k]]!=0))
        est$specificities.2 =  sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, res.tmp.2[[k]]!=0 ))
        est$opt.lambda1.2 = res.full.tmp.2$opt.lambda1
        est$opt.lambda2.2 = res.full.tmp.2$opt.lambda2
      }
    }
    if(length(stars.thresh)>2){
      res.tmp.3 = lapply(res.tmp.3, cov2cor)
      if(length(res.tmp.3)==0) {
        est$opt.sparsities.3  = NA
        est$precisions.3 =  NA
        est$recalls.3 = NA
        est$specificities.3 =  NA
        est$opt.lambda1.3 = NA
        est$opt.lambda2.3 = NA
      }
      else{
        est$opt.sparsities.3  = joint.spars.3
        est$precisions.3 =  sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, res.tmp.3[[k]]!=0))
        est$recalls.3 =  sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, res.tmp.3[[k]]!=0))
        est$specificities.3 =  sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, res.tmp.3[[k]]!=0 ))
        est$opt.lambda1.3 = res.full.tmp.3$opt.lambda1
        est$opt.lambda2.3 = res.full.tmp.3$opt.lambda2
      }
    }
    if(length(stars.thresh)>3){
      res.tmp.4 = lapply(res.tmp.4, cov2cor)
      if(length(res.tmp.4)==0) {
        est$opt.sparsities.4  = NA
        est$precisions.4 =  NA
        est$recalls.4 = NA
        est$specificities.4 =  NA
        est$opt.lambda1.4 = NA
        est$opt.lambda2.4 = NA
      }
      else{
        est$opt.sparsities.4  = joint.spars.4
        est$precisions.4 =  sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, res.tmp.4[[k]]!=0))
        est$recalls.4 =  sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, res.tmp.4[[k]]!=0))
        est$specificities.4 =  sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, res.tmp.4[[k]]!=0 ))
        est$opt.lambda1.4 = res.full.tmp.4$opt.lambda1
        est$opt.lambda2.4 = res.full.tmp.4$opt.lambda2
      }
    }
  }
  # Results from jointGHS
  if(include.jointGHS){
    res.tmp.jointghs = lapply(res.tmp.jointghs, cov2cor)
    est$opt.sparsities.jointghs  = joint.spars.jointghs
    est$precisions.jointghs =  sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, abs(res.tmp.jointghs[[k]])>1e-5))
    est$recalls.jointghs =  sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, abs(res.tmp.jointghs[[k]])>1e-5 ))
    est$specificities.jointghs =  sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, abs(res.tmp.jointghs[[k]])>1e-5 ))
  }
  # Results from SSJGL
  if(include.SSJGL){
    ssjgl.tmp = lapply(ssjgl.tmp, cov2cor)
    est$opt.sparsities.ssjgl  = unlist(lapply(ssjgl.tmp, FUN= function(m) tailoredGlasso::sparsity(abs(m)>1e-5)))
    est$precisions.ssjgl = sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, ssjgl.tmp[[k]]!=0))
    est$recalls.ssjgl = sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, ssjgl.tmp[[k]]!=0))
    est$specificities.ssjgl = sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, ssjgl.tmp[[k]]!=0))
  }
  # Results from JGL
  if(include.JGL){
    est$opt.sparsities.jgl = jgl.tmp$opt.sparsities
    est$precisions.jgl = sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, jgl.tmp$opt.fit[[k]]!=0))
    est$recalls.jgl = sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, jgl.tmp$opt.fit[[k]]!=0))
    est$specificities.jgl = sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, jgl.tmp$opt.fit[[k]]!=0))
    est$opt.lambda1.jgl = jgl.tmp$opt.lambda1
    est$opt.lambda2.jgl = jgl.tmp$opt.lambda2 
  }
  # Results from GGL
  if(include.GGL){
    est$opt.sparsities.ggl = ggl.tmp$opt.sparsities
    est$precisions.ggl = sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, ggl.tmp$opt.fit[[k]]!=0))
    est$recalls.ggl = sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, ggl.tmp$opt.fit[[k]]!=0))
    est$specificities.ggl = sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, ggl.tmp$opt.fit[[k]]!=0))
    est$opt.lambda1.ggl = ggl.tmp$opt.lambda1
    est$opt.lambda2.ggl = ggl.tmp$opt.lambda2 
  }
  # Results from glasso
  if(include.glasso){
    est$opt.sparsities.glasso = unlist(lapply(glasso.res, FUN= function(m) tailoredGlasso::sparsity(abs(m$opt.icov)>1e-5)))
    est$precisions.glasso = sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, abs(glasso.res[[k]]$opt.icov)>1e-5))
    est$recalls.glasso = sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, abs(glasso.res[[k]]$opt.icov)>1e-5))
    est$specificities.glasso =  sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, abs(glasso.res[[k]]$opt.icov)>1e-5))
    est$lambda.glasso = lambdas.glasso
  }
  return(est)
}


#' Perform jointGHS simulations
#' 
#' This function performs simulations for jointGHS, averaging over the results. 
#' 
#' @param K number of data sets/networks
#' @param n.vals the number of observations in each data set. A vector
#' @param p the number of nodes
#' @param N the number of simulations to perform
#' @param seeds seeds to use in each of the \eqn{N} simulations. A vector of length \eqn{N}. 
#' @param nCores how many cores should be used
#' @param frac.disagreement the fraction of edges that the networks should disagree on
#' @param method how should the similarity between the prec matrices be? Symmetric by default, meaning all are equally different. If not, one will stand out as completely unrelated.
#' @param include.stabJGL should stabJGL be performed?
#' @param include.jointGHS should the jointGHS be performed?
#' @param include.glasso should single-network graphical lasso be performed?
#' @param include.SSJGL should we perform SSJGL?
#' @param include.JGL should the fused joint graphical lasso be included?
#' @param include.GGL should the group joint graphical lasso be included?
#' @param penalize.diagonal should the diagonal be penalized in the graphical lasso-based methods?
#' @param verbose logical indicator of printing information at each iteration
#' @param scale should the data be scaled?
#' @param singleVSjoint is the simulation comparing single and joint GHS?
#' @param JGL.eBIC should the eBCIC be used for JGL instead of AIC?
#' @return simulation results, including sparsity, precision, recall and specificity
perform_stabJGL_simulation_orig = function(K, n.vals, p, N=100, seeds=sample(1:1000,N), nCores = 3, frac.disagreement = 0, method='symmetric', include.stabJGL=TRUE,
                                      include.jointGHS=TRUE, include.glasso=TRUE, include.SSJGL=TRUE, include.JGL = TRUE, include.GGL = TRUE, penalize.diagonal=FALSE ,
                                      verbose=TRUE, scale=TRUE, singleVSjoint=FALSE,stars.thresh = 0.05, nlambda1=20,lambda1.min=0.01,
                                      lambda1.max=1,nlambda2=20,lambda2.min=0,lambda2.max=0.1,lambda2.init=0.01,ebic.gamma=0, JGL.eBIC=F, 
                                      u=NULL, v=NULL, retune.lambda1=F, add.noise=FALSE, noise.sd=0.1){
  
  res=list()
  # stabJGL results
  res$opt.sparsities = matrix(0,N,K)
  res$precisions =  matrix(0,N,K)
  res$specificities =  matrix(0,N,K)
  res$recalls =  matrix(0,N,K)
  res$lambda1 = rep(0,N)
  res$lambda2 = rep(0,N)
  # JointGHS results
  res$opt.sparsities.jointghs = matrix(0,N,K)
  res$precisions.jointghs =  matrix(0,N,K)
  res$specificities.jointghs =  matrix(0,N,K)
  res$recalls.jointghs =  matrix(0,N,K)
  # SSJGL results 
  res$opt.sparsities.ssjgl = matrix(0,N,K)
  res$precisions.ssjgl =  matrix(0,N,K)
  res$specificities.ssjgl =  matrix(0,N,K)
  res$recalls.ssjgl =  matrix(0,N,K)
  # JGL results (fused, tuned by AIC)
  res$opt.sparsities.jgl = matrix(0,N,K)
  res$precisions.jgl =  matrix(0,N,K)
  res$specificities.jgl =  matrix(0,N,K)
  res$recalls.jgl =  matrix(0,N,K)
  res$lambda1.jgl = rep(0,N)
  res$lambda2.jgl = rep(0,N)
  # GGL results (group JGL, tuned by AIC)
  res$opt.sparsities.ggl = matrix(0,N,K)
  res$precisions.ggl =  matrix(0,N,K)
  res$specificities.ggl =  matrix(0,N,K)
  res$recalls.ggl =  matrix(0,N,K)
  res$lambda1.ggl = rep(0,N)
  res$lambda2.ggl = rep(0,N)
  # Glasso results
  res$opt.sparsities.glasso = matrix(0,N,K)
  res$precisions.glasso =  matrix(0,N,K)
  res$specificities.glasso =  matrix(0,N,K)
  res$recalls.glasso =  matrix(0,N,K)
  res$lambda.glasso = matrix(0,N,K)
  
  # Start by generating the precision matrices 
  cov.matrices = list()
  prec.matrices = list()
  
  # Start by generating the first prec matrix
  #huge.init = huge::huge.generator(n.vals[1],p,graph='scale-free',verbose = F,v=1,u=0.01) # REMOVED THIS NOW
  if(!is.null(u) & (! is.null(v))){
    huge.init = huge::huge.generator(n.vals[1],p,graph='scale-free',verbose = F,v=v, u=u)
  }
  else{
    huge.init = huge::huge.generator(n.vals[1],p,graph='scale-free',verbose = F) 
  }
  theta.init = huge.init$omega
  theta.init[which(abs(theta.init)<1e-5,arr.ind=T)] = 0
  spars.init = huge.init$sparsity
  cov.matrices[[1]] = huge.init$sigma
  prec.matrices[[1]] = theta.init
  
  # Added this 
  if(scale) cov.matrices[[1]] = cov2cor(cov.matrices[[1]])
  if(scale) prec.matrices[[1]] = cov2cor(prec.matrices[[1]])
  
  # Avoid rounding errors leading to matrices not being symmetric
  if(!matrixcalc::is.symmetric.matrix(cov.matrices[[1]])){
    cov.matrices[[1]] = round(cov.matrices[[1]],8)
  }
  if(method=='symmetric'){
    for(k in 2:K){
      valid=F
      while(!valid){ # Ensure valid precision matrices
        if(!is.null(u) & (!is.null(v)) & frac.disagreement==1){
          huge.tmp = list()
          graph.new = huge.generator(nrow(huge.init$data),ncol(huge.init$omega),graph='scale-free',verbose = F, u=u, v=v)
          if(scale) huge.tmp$cov.mat = cov2cor(graph.new$sigma)
          else huge.tmp$cov.mat = graph.new$sigma
          if(scale) huge.tmp$prec.mat = cov2cor(graph.new$omega) 
          else huge.tmp$prec.mat = graph.new$omega
          huge.tmp$prec.mat[which(abs(huge.tmp$prec.mat)<10^(-4),arr.ind=T)]=0
        }
        else if(frac.disagreement==0){
          huge.tmp = list(cov.mat=cov.matrices[[1]], prec.mat = prec.matrices[[1]])
        }
        else{
          huge.tmp = mutate.graph_orig(huge.init,frac.disagreement,scale, generate.data = F, larger.partialcor = F)
        }
        cov.matrices[[k]] = huge.tmp$cov.mat
        prec.matrices[[k]] = huge.tmp$prec.mat
        # Avoid rounding errors leading to matrices not being symmetric
        if(!matrixcalc::is.symmetric.matrix(cov.matrices[[k]])){
          cov.matrices[[k]] = round(cov.matrices[[k]],8)
        }
        valid = matrixcalc::is.positive.definite(cov.matrices[[k]])
      }
    }
  }
  else{ # First K-1 graphs are similar
    for(k in 2:(K-1)){
      huge.tmp = mutate.graph_orig(huge.init,frac.disagreement,scale)
      cov.matrices[[k]] = huge.tmp$cov.mat
      prec.matrices[[k]] = huge.tmp$prec.mat
      # Avoid rounding errors leading to matrices not being symmetric
      if(!matrixcalc::is.symmetric.matrix(cov.matrices[[k]])){
        cov.matrices[[k]] = round(cov.matrices[[k]],8)
      }
    }
    # Last graph is completely different
    huge.tmp = mutate.graph_orig(huge.init,fraction = 1, scale)
    cov.matrices[[K]] = huge.tmp$cov.mat
    prec.matrices[[K]] = huge.tmp$prec.mat
    # Avoid rounding errors leading to matrices not being symmetric
    if(!matrixcalc::is.symmetric.matrix(cov.matrices[[K]])){
      cov.matrices[[K]] = round(cov.matrices[[K]],8)
    }
  }
  registerDoParallel(nCores)
  res.list = foreach (i=1:N) %dopar% {
    stabJGL_simulation_one_iteration_orig(n.vals=n.vals,cov.matrices=cov.matrices,prec.matrices=prec.matrices,scale=scale,
                                     include.stabJGL = include.stabJGL, include.jointGHS=include.jointGHS, include.glasso=include.glasso, include.SSJGL=include.SSJGL, 
                                     include.JGL=include.JGL,include.GGL=include.GGL, penalize.diagonal=penalize.diagonal,seed=seeds[i], 
                                     singleVSjoint,stars.thresh = stars.thresh,nlambda1=nlambda1,lambda1.min=lambda1.min,
                                     lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init, 
                                     ebic.gamma=ebic.gamma,JGL.eBIC=JGL.eBIC,retune.lambda1= retune.lambda1,add.noise=add.noise, noise.sd=noise.sd);
  }
  registerDoSEQ()
  
  # Save results from each replicate
  for(i in 1:N){
    est.tmp = res.list[[i]]
    
    # Results from stabJGL
    if(include.stabJGL){
      res$opt.sparsities[i,] = est.tmp$opt.sparsities 
      res$precisions[i,] = est.tmp$precisions
      res$recalls[i,] = est.tmp$recalls
      res$specificities[i,] =  est.tmp$specificities
      res$lambda1[i] = est.tmp$opt.lambda1
      res$lambda2[i] = est.tmp$opt.lambda2
    }
    # Results from jointGHS
    if(include.jointGHS){
      res$opt.sparsities.jointghs[i,] = est.tmp$opt.sparsities.jointghs
      res$precisions.jointghs[i,] = est.tmp$precisions.jointghs
      res$recalls.jointghs[i,] = est.tmp$recalls.jointghs
      res$specificities.jointghs[i,] =  est.tmp$specificities.jointghs
    }
    # Results from jgl
    if(include.JGL){
      res$opt.sparsities.jgl[i,] = est.tmp$opt.sparsities.jgl
      res$precisions.jgl[i,] = est.tmp$precisions.jgl
      res$recalls.jgl[i,] = est.tmp$recalls.jgl
      res$specificities.jgl[i,] =  est.tmp$specificities.jgl 
      res$lambda1.jgl[i] = est.tmp$opt.lambda1.jgl
      res$lambda2.jgl[i] = est.tmp$opt.lambda2.jgl
    }
    # Results from ggl
    if(include.GGL){
      res$opt.sparsities.ggl[i,] = est.tmp$opt.sparsities.ggl
      res$precisions.ggl[i,] = est.tmp$precisions.ggl
      res$recalls.ggl[i,] = est.tmp$recalls.ggl
      res$specificities.ggl[i,] =  est.tmp$specificities.ggl 
      res$lambda1.ggl[i] = est.tmp$opt.lambda1.ggl
      res$lambda2.ggl[i] = est.tmp$opt.lambda2.ggl
    }
    # Results from ssjgl
    if(include.SSJGL){
      res$opt.sparsities.ssjgl[i,] = est.tmp$opt.sparsities.ssjgl
      res$precisions.ssjgl[i,] = est.tmp$precisions.ssjgl
      res$recalls.ssjgl[i,] = est.tmp$recalls.ssjgl
      res$specificities.ssjgl[i,] =  est.tmp$specificities.ssjgl 
    }
    # Results from glasso
    if(include.glasso){
      res$opt.sparsities.glasso[i,] = est.tmp$opt.sparsities.glasso
      res$precisions.glasso[i,] = est.tmp$precisions.glasso
      res$recalls.glasso[i,] = est.tmp$recalls.glasso
      res$specificities.glasso[i,] =  est.tmp$specificities.glasso
      res$lambda.glasso[i,] = est.tmp$lambda.glasso
    }
  }
  # Mean results from stabJGL
  if(include.stabJGL){
    res$mean.opt.sparsities= colMeans(res$opt.sparsities)
    res$mean.precisions= colMeans(res$precisions)
    res$mean.recalls = colMeans(res$recalls)
    res$mean.specificities =  colMeans(res$specificities)
    res$mean.lambda1 = mean(res$lambda1)
    res$mean.lambda2 = mean(res$lambda2)
  }
  # Mean results from jointGHS
  if(include.jointGHS){
    res$mean.opt.sparsities.jointghs= colMeans(res$opt.sparsities.jointghs)
    res$mean.precisions.jointghs= colMeans(res$precisions.jointghs)
    res$mean.recalls.jointghs = colMeans(res$recalls.jointghs)
    res$mean.specificities.jointghs =  colMeans(res$specificities.jointghs)
  }
  # Mean results from JGL
  if(include.JGL){
    res$mean.opt.sparsities.jgl = colMeans(res$opt.sparsities.jgl)
    res$mean.precisions.jgl = colMeans(res$precisions.jgl)
    res$mean.recalls.jgl = colMeans(res$recalls.jgl)
    res$mean.specificities.jgl =  colMeans(res$specificities.jgl)
    res$mean.lambda1.jgl = mean(res$lambda1.jgl)
    res$mean.lambda2.jgl = mean(res$lambda2.jgl)
  }
  # Mean results from GGL
  if(include.GGL){
    res$mean.opt.sparsities.ggl = colMeans(res$opt.sparsities.ggl)
    res$mean.precisions.ggl = colMeans(res$precisions.ggl)
    res$mean.recalls.ggl = colMeans(res$recalls.ggl)
    res$mean.specificities.ggl =  colMeans(res$specificities.ggl)
    res$mean.lambda1.ggl = mean(res$lambda1.ggl)
    res$mean.lambda2.ggl = mean(res$lambda2.ggl)
  }
  # Mean results from SSJGL
  if(include.SSJGL){
    res$mean.opt.sparsities.ssjgl = colMeans(res$opt.sparsities.ssjgl)
    res$mean.precisions.ssjgl = colMeans(res$precisions.ssjgl)
    res$mean.recalls.ssjgl = colMeans(res$recalls.ssjgl)
    res$mean.specificities.ssjgl =  colMeans(res$specificities.ssjgl)
  }
  # Mean results from glasso
  if(include.glasso){
    res$mean.opt.sparsities.glasso = colMeans(res$opt.sparsities.glasso)
    res$mean.precisions.glasso = colMeans(res$precisions.glasso)
    res$mean.recalls.glasso = colMeans(res$recalls.glasso)
    res$mean.specificities.glasso =  colMeans(res$specificities.glasso)
    res$mean.lambda.glasso = colMeans(res$lambda.glasso)
  }
  res$true.prec.matrices = prec.matrices
  res$true.sparsity = spars.init
  return(res)
}

# Function for performing one iteration -----------------------------------------

stabJGL_simulation_one_iteration_orig = function(n.vals,cov.matrices,prec.matrices,scale,include.stabJGL, include.jointGHS, include.glasso, include.SSJGL,include.JGL, 
                                            include.GGL, penalize.diagonal,seed,singleVSjoint,stars.thresh,nlambda1,lambda1.min,lambda1.max, 
                                            nlambda2,lambda2.min,lambda2.max, lambda2.init,ebic.gamma,JGL.eBIC, retune.lambda1,add.noise, noise.sd) {
  y = list()
  K=length(n.vals)
  p=ncol(prec.matrices[[1]])
  glasso.res = list()
  lambdas.glasso = rep(0,K)
  set.seed(seed)
  # Generate data. 
  for(k in 1:K){
    y[[k]] = mvtnorm::rmvnorm(n.vals[k], mean=rep(0,p), cov.matrices[[k]])
    if(add.noise) {
      y[[k]] = y[[k]] + mvtnorm::rmvnorm(n.vals[k], mean=rep(0,p), diag(noise.sd^2,p))
    }
    if (scale) y[[k]] = scale(y[[k]])
    # Use graphical lasso
    if(include.glasso & !singleVSjoint){
      glasso.tmp = huge::huge(y[[k]],method='glasso',verbose = F)
      glasso.res[[k]] = huge::huge.select(glasso.tmp,criterion='stars', stars.thresh = stars.thresh, verbose = F)
      lambdas.glasso[k] = glasso.res[[k]]$opt.lambda
    }
  }
  # Perform joint methods
  if(include.stabJGL){
    res.full.tmp = stabJGL::stabJGL(Y=y,var.thresh = stars.thresh,subsample.ratio = NULL,rep.num = 20, nlambda1=nlambda1,scale=F,
                                    lambda1.min=lambda1.min,lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init,
                                    ebic.gamma=ebic.gamma,verbose=F,penalize.diagonal=FALSE,parallelize = F,retune.lambda1=retune.lambda1)
    res.tmp = res.full.tmp$opt.fit
    joint.spars = res.full.tmp$opt.sparsities
    # if comparing glasso to stabJGL, force glasso to the same sparsity
    if(singleVSjoint){
      for(k in 1:K){
        glasso.tmp = huge::huge(y[[k]],nlambda=20,method='glasso',verbose = F)
        ind.glasso = which.min(abs(joint.spars[[k]]-glasso.tmp$sparsity))
        glasso.res[[k]] = round(glasso.tmp$icov[[ind.glasso]], 5)
        lambdas.glasso[k] = glasso.tmp$lambda[ind.glasso]
      }
    }
  }
  if(include.jointGHS){
    res.full.tmp.jointghs = jointGHS::jointGHS(X=y, AIC_selection = T, AIC_eps = 0.1, epsilon=1e-3, verbose = F,nCores=1)
    res.tmp.jointghs = res.full.tmp.jointghs$theta
    joint.spars.jointghs = unlist(lapply(res.tmp.jointghs, FUN= function(m) tailoredGlasso::sparsity(abs(cov2cor(m))>1e-5)))
  }
  if(include.SSJGL){
    lam1 = 1
    lam2 = 1
    v1 = 1
    lam.eff = lam1 + c(1:10) * 5
    v0s = lam1/lam.eff
    ssjgl.tmp = SSJGL(Y=y,penalty='fused',lambda0=1, lambda1=lam1,lambda2=lam2, v1 = v1, v0s = v0s, tol.em=1e-4, a=1, b=p, doubly=TRUE, normalize=TRUE)
    ssjgl.tmp = ssjgl.tmp$thetalist[[10]]
  }
  if(include.JGL){
    if(JGL.eBIC){
      jgl.tmp = JGL_select_eBIC(Y=y,penalty='fused',nlambda1=nlambda1,lambda1.min=lambda1.min,lambda1.max=lambda1.max,nlambda2=nlambda2,lambda2.min=lambda2.min,
                                lambda2.max=lambda2.max,lambda2.init = lambda2.init,penalize.diagonal=penalize.diagonal, ebic.gamma=ebic.gamma)
    }
    else{
      jgl.tmp = JGL_select_AIC(Y=y,penalty='fused',nlambda1=nlambda1,lambda1.min=lambda1.min,lambda1.max=lambda1.max,nlambda2=nlambda2,lambda2.min=lambda2.min,
                               lambda2.max=lambda2.max,lambda2.init = lambda2.init,penalize.diagonal=penalize.diagonal)
    }
  }
  if(include.GGL){
    ggl.tmp = JGL_select_AIC(Y=y,penalty='group',nlambda1=nlambda1,lambda1.min=lambda1.min,lambda1.max=lambda1.max,nlambda2=nlambda2,lambda2.min=lambda2.min,
                             lambda2.max=lambda2.max,lambda2.init = lambda2.init,penalize.diagonal=penalize.diagonal)
  }
  est=list()
  
  # Results from stabJGL
  if(include.stabJGL){ 
    res.tmp = lapply(res.tmp, cov2cor)
    est$opt.sparsities  = joint.spars
    est$precisions =  sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, res.tmp[[k]]!=0))
    est$recalls =  sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, res.tmp[[k]]!=0))
    est$specificities =  sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, res.tmp[[k]]!=0 ))
    est$opt.lambda1 = res.full.tmp$opt.lambda1
    est$opt.lambda2 = res.full.tmp$opt.lambda2 
    
  }
  # Results from jointGHS
  if(include.jointGHS){
    res.tmp.jointghs = lapply(res.tmp.jointghs, cov2cor)
    est$opt.sparsities.jointghs  = joint.spars.jointghs
    est$precisions.jointghs =  sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, abs(res.tmp.jointghs[[k]])>1e-5))
    est$recalls.jointghs =  sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, abs(res.tmp.jointghs[[k]])>1e-5 ))
    est$specificities.jointghs =  sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, abs(res.tmp.jointghs[[k]])>1e-5 ))
  }
  # Results from SSJGL
  if(include.SSJGL){
    ssjgl.tmp = lapply(ssjgl.tmp, cov2cor)
    est$opt.sparsities.ssjgl  = unlist(lapply(ssjgl.tmp, FUN= function(m) tailoredGlasso::sparsity(abs(m)>1e-5)))
    est$precisions.ssjgl = sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, ssjgl.tmp[[k]]!=0))
    est$recalls.ssjgl = sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, ssjgl.tmp[[k]]!=0))
    est$specificities.ssjgl = sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, ssjgl.tmp[[k]]!=0))
  }
  # Results from JGL
  if(include.JGL){
    est$opt.sparsities.jgl = jgl.tmp$opt.sparsities
    est$precisions.jgl = sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, jgl.tmp$opt.fit[[k]]!=0))
    est$recalls.jgl = sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, jgl.tmp$opt.fit[[k]]!=0))
    est$specificities.jgl = sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, jgl.tmp$opt.fit[[k]]!=0))
    est$opt.lambda1.jgl = jgl.tmp$opt.lambda1
    est$opt.lambda2.jgl = jgl.tmp$opt.lambda2 
  }
  # Results from GGL
  if(include.GGL){
    est$opt.sparsities.ggl = ggl.tmp$opt.sparsities
    est$precisions.ggl = sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, ggl.tmp$opt.fit[[k]]!=0))
    est$recalls.ggl = sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, ggl.tmp$opt.fit[[k]]!=0))
    est$specificities.ggl = sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, ggl.tmp$opt.fit[[k]]!=0))
    est$opt.lambda1.ggl = ggl.tmp$opt.lambda1
    est$opt.lambda2.ggl = ggl.tmp$opt.lambda2 
  }
  # Results from glasso
  if(include.glasso){
    est$opt.sparsities.glasso = unlist(lapply(glasso.res, FUN= function(m) tailoredGlasso::sparsity(abs(m$opt.icov)>1e-5)))
    est$precisions.glasso = sapply(1:K,FUN=function(k) precision(prec.matrices[[k]]!=0, abs(glasso.res[[k]]$opt.icov)>1e-5))
    est$recalls.glasso = sapply(1:K,FUN=function(k) recall(prec.matrices[[k]]!=0, abs(glasso.res[[k]]$opt.icov)>1e-5))
    est$specificities.glasso =  sapply(1:K,FUN=function(k) specificity(prec.matrices[[k]]!=0, abs(glasso.res[[k]]$opt.icov)>1e-5))
    est$lambda.glasso = lambdas.glasso
  }
  return(est)
}

perform_stabJGL_simulation_orig_seed = function(K,n.vals, p, N, frac.disagreement, 
                                                      nCores,stars.thresh, include.stabJGL,
                                                      include.jointGHS, include.glasso, include.SSJGL, include.JGL, include.GGL,
                                                      ebic.gamma,v=NULL,u=NULL, seed){
  set.seed(seed)
  return(perform_stabJGL_simulation_orig(K=K,n.vals=n.vals, p=p, N=N, frac.disagreement=frac.disagreement, 
                                         nCores=nCores,stars.thresh=stars.thresh, include.stabJGL=include.stabJGL,
                                         include.jointGHS=include.jointGHS, include.glasso=include.glasso, include.SSJGL=include.SSJGL, 
                                         include.JGL=include.JGL, include.GGL=include.GGL,
                                         ebic.gamma=ebic.gamma,v=v,u=u))
}


