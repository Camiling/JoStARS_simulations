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
library(JoStARS)
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
#' @param include.JoStARS should JoStARS be performed?
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
perform_JoStARS_simulation = function(K, n.vals, p, N=100, seeds=sample(1:1000,N), nCores = 3, frac.disagreement = 0, method='symmetric', include.JoStARS=TRUE,
                                       include.jointGHS=TRUE, include.glasso=TRUE, include.SSJGL=TRUE, include.JGL = TRUE, include.GGL = TRUE, penalize.diagonal=FALSE ,
                                       verbose=TRUE, scale=TRUE, singleVSjoint=FALSE,stars.thresh = 0.05, nlambda1=20,lambda1.min=0.01,
                                       lambda1.max=1,nlambda2=20,lambda2.min=0,lambda2.max=0.1,lambda2.init=0.01,ebic.gamma=0){
 
  res=list()
  # JoStARS results
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
  huge.init = huge::huge.generator(n.vals[1],p,graph='scale-free',verbose = F)
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
        huge.tmp = mutate.graph(huge.init,frac.disagreement,scale, generate.data = F, larger.partialcor = F)
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
  registerDoParallel(nCores)
  res.list = foreach (i=1:N) %dopar% {
    JoStARS_simulation_one_iteration(n.vals=n.vals,cov.matrices=cov.matrices,prec.matrices=prec.matrices,scale=scale,
                                     include.JoStARS = include.JoStARS, include.jointGHS=include.jointGHS, include.glasso=include.glasso, include.SSJGL=include.SSJGL, 
                                     include.JGL=include.JGL,include.GGL=include.GGL, penalize.diagonal=penalize.diagonal,seed=seeds[i], 
                                     singleVSjoint,stars.thresh = stars.thresh,nlambda1=nlambda1,lambda1.min=lambda1.min,
                                     lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init, 
                                     ebic.gamma=ebic.gamma);
  }
  registerDoSEQ()
  
  # Save results from each replicate
  for(i in 1:N){
    est.tmp = res.list[[i]]
    
    # Results from JoStARS
    if(include.JoStARS){
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
  # Mean results from JoStARS
  if(include.JoStARS){
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

JoStARS_simulation_one_iteration = function(n.vals,cov.matrices,prec.matrices,scale,include.JoStARS, include.jointGHS, include.glasso, include.SSJGL,include.JGL, 
                                            include.GGL, penalize.diagonal,seed,singleVSjoint,stars.thresh,nlambda1,lambda1.min,lambda1.max, 
                                            nlambda2,lambda2.min,lambda2.max, lambda2.init,ebic.gamma) {
  y = list()
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
  if(include.JoStARS){
    res.full.tmp = JoStARS::JoStARS(Y=y,var.thresh = stars.thresh,subsample.ratio = NULL,rep.num = 20, nlambda1=nlambda1,scale=F,
                                    lambda1.min=lambda1.min,lambda1.max=lambda1.max, nlambda2=nlambda2,lambda2.min=lambda2.min,lambda2.max = lambda2.max, lambda2.init = lambda2.init,
                                    ebic.gamma=ebic.gamma,verbose=F,penalize.diagonal=FALSE,parallelize = F)
    res.tmp = res.full.tmp$opt.fit
    joint.spars = res.full.tmp$opt.sparsities
    # if comparing glasso to JoStARS, force glasso to the same sparsity
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
    jgl.tmp = JGL_select_AIC(Y=y,penalty='fused',nlambda1=nlambda1,lambda1.min=lambda1.min,lambda1.max=lambda1.max,nlambda2=nlambda2,lambda2.min=lambda2.min,
                             lambda2.max=lambda2.max,lambda2.init = lambda2.init,penalize.diagonal=penalize.diagonal)
  }
  if(include.GGL){
    ggl.tmp = JGL_select_AIC(Y=y,penalty='group',nlambda1=nlambda1,lambda1.min=lambda1.min,lambda1.max=lambda1.max,nlambda2=nlambda2,lambda2.min=lambda2.min,
                             lambda2.max=lambda2.max,lambda2.init = lambda2.init,penalize.diagonal=penalize.diagonal)
  }
  est=list()
  
  # Results from JoStARS
  if(include.JoStARS){ 
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




