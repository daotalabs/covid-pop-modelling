# Viet Dao
# Last updated: May 2, 2022

# Model 1 with a & b for lab test & hosp. histories
library(dplyr)
library(nimble)
library(Rlab)
library(basicMCMCplots) # for traceplots and density plots

# makeHistory((N, M, k, pa, pb, theta1, theta2) simulates capture histories h[i,j]
# Inputs:
# N - population size
# M - augmented population size
# k - number of recapture occassions
# pa - capture prob. of lab test
# pb - capture prob. of hospitalization
# theta1 - prob. of being available for lab test
# theta2 - prob. of being available for hospitalization

makeHistory <- function(N, M, k, pa, pb, theta1, theta2) {
  set.seed(5)
  
  a <- matrix(NA, nrow = N, ncol = k)
  b <- matrix(NA, nrow = N, ncol = k)
  h <- matrix(NA, nrow = N, ncol = k)
  for (i in 1:N) {
    for (j in 1:k) {
      if (j==1) {
        # must populate b matrix first: if caught twice in the same time period 
        # then only count b
        b[i,j] <- rbern(1, theta2) 
        a[i,j] <- if (b[i,j]==1) 0 else rbern(1, theta1)
      } else {
        b[i,j] <- rbern(1, prod(1-b[i,], na.rm=T) * theta2)
        a[i,j] <- rbern(1, prod(1-a[i,], na.rm=T) * prod(1-b[i,], na.rm=T) * theta1)
      }
      h[i,j] <- rbern(1, a[i,j] * pa + b[i,j] * pb)
    }
  }
  
  result <- NULL
  result$N <- N
  result$M <- M
  result$k <- k
  result$pa <- pa
  result$pb <- pb
  result$theta1 <- theta1
  result$theta2 <- theta2
  result$h <- h # h may include all-zeros; realistically it doesn't
  result$a <- a
  result$b <- b
  result$h_aug <- rbind(h, matrix(0, nrow = M-N, ncol = k))
  result$a_aug <- rbind(a, matrix(0, nrow = M-N, ncol = k))
  result$b_aug <- rbind(b, matrix(0, nrow = M-N, ncol = k))
  return(result)
}

# covidCode specifies the nimble model
covidCode <- nimbleCode({
  # priors
  psi ~ dunif(0,1) 
  pa ~ dunif(0,1) 
  pb ~ dunif(0,1) 
  theta1 ~ dunif(0,1)
  theta2 ~ dunif(0,1)

  # model
  for (i in 1:M) {
    z[i] ~ dbern(psi)
    
    # j=1
    qb[i,1] <- z[i] * theta2
    b[i,1] ~ dbern(qb[i,1])
    qa[i,1] <- (1-b[i,1]) * z[i] * theta1
    a[i,1] ~ dbern(qa[i,1])
    qh[i,1] <- z[i] * a[i,1] * pa + z[i] * b[i,1] * pb
    h[i,1] ~ dbern(qh[i,1])
    
    # j>=2
    for (j in 2:k) {
      qb[i,j] <- z[i] * prod(1-b[i,1:(j-1)]) * theta2
      b[i,j] ~ dbern(qb[i,j])
      qa[i,j] <- z[i] * prod(1-a[i,1:(j-1)]) * prod(1-b[i,1:j]) * theta1
      a[i,j] ~ dbern(qa[i,j])
      qh[i,j] <- z[i] * a[i,j] * pa + z[i] * b[i,j] * pb
      h[i,j] ~ dbern(qh[i,j])
    }
  }
})

# buildModel(sim) runs the MCMC with the simulated history from makeHistory as input 
buildModel <- function(sim) {
  covidConsts <- list(M = sim$M,
                      k = sim$k)
  
  covidData <- list(h = sim$h_aug,
                    a = sim$a_aug,
                    b = sim$b_aug)
  
  covidInits <- list(psi = runif(1,0,1), 
                     pa = runif(1,0,1),
                     pb = runif(1,0,1),
                     theta1 = runif(1,0,1),
                     theta2 = runif(1,0,1),
                     z = rep(0, covidConsts$M))
  
  mcmc.out <- nimbleMCMC(code = covidCode, 
                         constants = covidConsts,
                         data = covidData, 
                         inits = covidInits,
                         nchains = 1, 
                         niter = 7500,
                         nburnin = 2000,
                         summary = TRUE, 
                         WAIC = TRUE,
                         monitors = c('z','psi','pa','pb','theta1','theta2'
                                      # ,'qa','qb','qh'
                                      ))
  return(mcmc.out)
}

### EXPERIMENTS ###
# theta1=theta2=0.3
sim1 <- makeHistory(N=1000, M=2000, k=10, pa=0.95, pb=0.85, theta1=0.3, theta2=0.3)
mcmc.out1 <- buildModel(sim1)
# MCMC result for one chain
mcmc.out1$summary$chain1[c('pa'
                          ,'pb'
                          ,'psi'
                          ,'theta1'
                          ,'theta2'
                        ),]
# plot posterior densities
samplesPlot(mcmc.out1$samples$chain1[,c('pa'
                                ,'pb'
                                ,'psi'
                                ,'theta1'
                                ,'theta2'
                                )]
            ,traceplot=TRUE)

# theta1=0.4, theta2=0.6 (sum=1)
sim2 <- makeHistory(N=1000, M=2000, k=10, pa=0.95, pb=0.85, theta1=0.4, theta2=0.6)
mcmc.out2 <- buildModel(sim2)
# MCMC result for one chain
mcmc.out2$summary$chain1[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out2$samples$chain1[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# low theta1=theta2=0.2
sim3 <- makeHistory(N=1000, M=2000, k=10, pa=0.95, pb=0.85, theta1=0.2, theta2=0.2)
mcmc.out3 <- buildModel(sim3)
# MCMC result for one chain
mcmc.out3$summary$chain1[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out3$samples$chain1[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# high theta1=theta2=0.8
sim4 <- makeHistory(N=1000, M=2000, k=10, pa=0.95, pb=0.85, theta1=0.8, theta2=0.8)
mcmc.out4 <- buildModel(sim4)
# MCMC result for one chain
mcmc.out4$summary$chain1[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out4$samples$chain1[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# keep it simple pa=pb=1, theta1=theta2=0.2
sim5 <- makeHistory(N=1000, M=2000, k=10, pa=1, pb=1, theta1=0.2, theta2=0.2)
mcmc.out5 <- buildModel(sim5)
# MCMC result for one chain
mcmc.out5$summary$chain1[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]


# theta1=0.4, theta2=0.6
sim6 <- makeHistory(N=1000, M=2000, k=10, pa=1, pb=1, theta1=0.4, theta2=0.6)
mcmc.out6 <- buildModel(sim6)
# MCMC result for one chain
mcmc.out6$summary[c('pa'
                   ,'pb'
                   ,'psi'
                   ,'theta1'
                   ,'theta2'
),]
samplesPlot(mcmc.out6$samples[,c('pa'
                                ,'pb'
                                ,'psi'
                                ,'theta1'
                                ,'theta2'
)]
,traceplot=TRUE)

# theta1=0.4, theta2=0.4
sim7 <- makeHistory(N=1000, M=2000, k=10, pa=1, pb=1, theta1=0.4, theta2=0.4)
mcmc.out7 <- buildModel(sim7)
# MCMC result for one chain
mcmc.out7$summary[c('pa'
                    ,'pb'
                    ,'psi'
                    ,'theta1'
                    ,'theta2'
),]
samplesPlot(mcmc.out7$samples[,c('pa'
                                 ,'pb'
                                 ,'psi'
                                 ,'theta1'
                                 ,'theta2'
)]
,traceplot=TRUE)