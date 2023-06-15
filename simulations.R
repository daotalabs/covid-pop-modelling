# title: "Covid model simulation"
# author: "Viet Dao"

library(here)
library(dplyr)
library(nimble) # for nimble model
library(Rlab) # for Bernoulli distributions
library(basicMCMCplots) # for trace plots and density plots

source(here::here("R", "make_history.R"))
source(here::here("R", "populate_row_obs_ab.R"))

### Nimble model
# covidCode specifies the nimble model.

# build_model() runs the MCMC with the simulated history from make_history() as input.
build_model <- function(sim, chains, iter) {
  covidCode <- nimbleCode({
    # uniform priors
    psi ~ dunif(min = 0, max = 1) 
    pa ~ dunif(min = 0, max = 1) 
    pb ~ dunif(min = 0, max = 1) 
    theta1 ~ dunif(min = 0, max = 1)
    theta2 ~ dunif(min = 0, max = 1)
    
    # model
    for (i in 1:M) {
      z[i] ~ Rlab::dbern(prob = psi)
      
      # j=1
      # force b=0 for j=1
      qb[i,1] <- 0
      b[i,1] ~ Rlab::dbern(prob = qb[i,1]) # = 0
      qa[i,1] <- z[i] * theta1
      a[i,1] ~ Rlab::dbern(prob = qa[i,1])
      qh[i,1] <- z[i] * a[i,1] * pa + z[i] * b[i,1] * pb # = 0
      h[i,1] ~ Rlab::dbern(prob = qh[i,1])
      
      # j>=2
      for (j in 2:k) {
        qb[i,j] <- z[i] * prod(1-b[i,1:(j-1)]) * theta2
        b[i,j] ~ Rlab::dbern(prob = qb[i,j])
        qa[i,j] <- z[i] * prod(1-a[i,1:(j-1)]) * prod(1-b[i,1:j]) * theta1
        a[i,j] ~ Rlab::dbern(prob = qa[i,j])
        qh[i,j] <- z[i] * a[i,j] * pa + z[i] * b[i,j] * pb
        h[i,j] ~ Rlab::dbern(prob = qh[i,j]) 
      }
    }
  })
  
  # nimble doesn't like snake_case in object names for some reason
  covidConsts <- list(M = sim$M,
                      k = sim$k)
  
  covidData <- list(h = sim$h_aug,
                    a = sim$obs_a_aug,
                    b = sim$obs_b_aug)
  
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
                         nchains = chains,
                         niter = iter,
                         nburnin = 2000,
                         summary = TRUE, 
                         WAIC = TRUE,
                         monitors = c("psi", "pa", "pb", "theta1", "theta2", "a", "b"))
  return(mcmc.out)
}

### Experiments
sim0622 <- make_history(N=1000, M=2000, k=5, pa=0.15, pb=0.05, theta1=0.7, theta2=0.2)

# see how the result looks
# head(sim0622$obs_a, 15)
# head(sim0622$obs_b, 15)
# head(sim0622$h, 15)

# see how much time it takes to run
start_time <- Sys.time()
mcmc.out0622 <- build_model(sim0622, chains=1, iter=10000)
end_time <- Sys.time()
end_time - start_time

# MCMC result for one chain
mcmc.out0622$summary[c("pa", "pb", "psi", "theta1", "theta2"),]
# Mean    Median    St.Dev. 95%CI_low 95%CI_upp
# pa     0.9697692 0.9708393 0.01439722 0.9391133 0.9941982
# pb     0.8606580 0.8584738 0.04794272 0.7747451 0.9621275
# psi    0.4963615 0.4962926 0.01250491 0.4718958 0.5207061
# theta1 0.6840863 0.6842855 0.01755357 0.6486825 0.7175415
# theta2 0.3018353 0.3013564 0.02394127 0.2570327 0.3485501

# show distribution plots
basicMCMCplots::samplesPlot(mcmc.out$samples[,c("pa", "pb", "psi", "theta1", "theta2")], traceplot=TRUE)