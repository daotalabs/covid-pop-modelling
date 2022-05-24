# Viet Dao
# Last updated: May 23, 2022

# Model 1 with a & b for lab test & hosp. histories
library(dplyr)
library(nimble)
library(Rlab)
library(basicMCMCplots) # for trace plots and density plots

# makeHistory(N, M, k, pa, pb, theta1, theta2) simulates capture histories h[i,j]
# Inputs:
# N - population size
# M - augmented population size
# k - number of recapture occasions
# a - latent variable indicating whether individual is available for capture at lab test
# b - latent variable indicating whether individual is available for capture at hospital
# pa - capture prob. of lab test
# pb - capture prob. of hospitalization
# theta1 - prob. of being available for lab test
# theta2 - prob. of being available for hospitalization

makeHistory <- function(N, M, k, pa, pb, theta1, theta2) {
  set.seed(5)
  
  a <- matrix(NA, nrow = N, ncol = k)
  b <- matrix(NA, nrow = N, ncol = k)
  h <- matrix(0, nrow = N, ncol = k)

  for (i in 1:N) {
    for (j in 1:k) {
      if (j==1) {
        # must populate b matrix first: if caught twice in the same time period 
        # then only count b
        b[i,j] <- 0
        a[i,j] <- rbern(1, theta1)
      } else {
        b[i,j] <- rbern(1, prod(1-b[i,], na.rm=T) * theta2)
        a[i,j] <- rbern(1, prod(1-a[i,], na.rm=T) * prod(1-b[i,], na.rm=T) * theta1)
      }
      
      # indicate capture by 1 for a and recapture by 2 for b
      if (rbern(1, a[i,j] * pa) == 1) { 
        h[i,j] <- 1 
      }
      if (rbern(1, b[i,j] * pb) == 1) { 
        h[i,j] <- 2 
      } 
    }
  }
  
  # populate obs_a and obs_b as much as possible 
  obs_ab <- h %>% apply(1, populateRowObs_ab) %>% t()
  obs_a <- obs_ab[,1:k]
  obs_b <- obs_ab[,(k+1):(2*k)]
  
  result <- NULL
  result$N <- N
  result$M <- M
  result$k <- k
  result$pa <- pa
  result$pb <- pb
  result$theta1 <- theta1
  result$theta2 <- theta2
  result$h <- h 
  result$a <- a
  result$b <- b
  result$obs_a <- obs_a
  result$obs_b <- obs_b
  result$h_aug <- rbind(h, matrix(0, nrow = M-N, ncol = k))
  result$a_aug <- rbind(a, matrix(0, nrow = M-N, ncol = k))
  result$b_aug <- rbind(b, matrix(0, nrow = M-N, ncol = k))
  return(result)
}

# populateRowObs_ab(h_row) creates observed/real obs_a and obs_b from each row in history h
# the presence and positions of 1 and 2 in h determine what's in obs_a and obs_b
populateRowObs_ab <- function(h_row) {
  k <- length(h_row)
  # for h that looks like
  # [1,]    0    1    2    0    0
  # make other positions 0s for obs_a and obs_b
  if (sum(h_row) == 3) {
    h_row_1_idx <- which(h_row == 1)
    h_row_2_idx <- which(h_row == 2)
    
    obs_a_row <- rep(0, k)
    obs_a_row[h_row_1_idx] <- 1
    
    obs_b_row <- rep(0, k)
    obs_b_row[h_row_2_idx] <- 1
    }
  # for h that looks like
  # [2,]    0    0    1    0    0
  # make other positions 0s for obs_a and make prior positions 0s for obs_b
  else if (sum(h_row) == 1) {
    h_row_1_idx <- which(h_row == 1)
    
    obs_a_row <- rep(0, k)
    obs_a_row[h_row_1_idx] <- 1
    
    obs_b_row <- rep(NA, k)
    obs_b_row[1:h_row_1_idx] <- 0
    }
  # for h that looks like
  # [3,]    0    0    2    0    0
  # make subsequent positions 0s for obs_a and make other positions 0s for obs_b
  else if (sum(h_row) == 2) {
    h_row_2_idx <- which(h_row == 2)
    
    obs_a_row <- rep(NA, k)
    obs_a_row[h_row_2_idx:k] <- 0
    
    obs_b_row <- rep(0, k)
    obs_b_row[h_row_2_idx] <- 1
    }
  else {
    obs_a_row <- rep(NA, k)
    obs_b_row <- rep(NA, k)
  }
  
  obs_ab <- cbind(obs_a_row, obs_b_row)
  return(obs_ab)
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
    # force b=0 for j=1
    qb[i,1] <- 0
    b[i,1] ~ dbern(qb[i,1])
    qa[i,1] <- z[i] * theta1
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
      h[i,j] ~ dbern(qh[i,j]) # update this
    }
  }
})

# buildModel(sim) runs the MCMC with the simulated history from makeHistory as input 
buildModel <- function(sim, chains, iter) {
  covidConsts <- list(M = sim$M
                      ,k = sim$k)
  
  covidData <- list(h = sim$h_aug
                    # new a
                    # new b
                    )
  
  covidInits <- list(psi = runif(1,0,1)
                     ,pa = runif(1,0,1)
                     ,pb = runif(1,0,1)
                     ,theta1 = runif(1,0,1)
                     ,theta2 = runif(1,0,1)
                     ,z = rep(0, covidConsts$M)
                     ,a = sim$a_aug
                     ,b = sim$b_aug
                     )
  
  mcmc.out <- nimbleMCMC(code = covidCode
                         ,constants = covidConsts
                         ,data = covidData
                         ,inits = covidInits
                         ,nchains = chains 
                         ,niter = iter
                         ,nburnin = 2000
                         ,summary = TRUE 
                         ,WAIC = TRUE
                         ,monitors = c('z'
                                       ,'psi'
                                       ,'pa'
                                       ,'pb'
                                       ,'theta1'
                                       ,'theta2'
                                       ,'a' # new a instead
                                       ,'b' # new b instead
                                      # ,'qa','qb','qh'
                                      ))
  return(mcmc.out)
}

# extract estimates for a_aug and b_aug from MCMC poterior samples
extractAb <- function(mcmc.out, M, k) {
  sep_idx <- M*k
  summary_df <- as.data.frame.matrix(mcmc.out$summary)
  a_aug_vec <- summary_df[1:sep_idx,]$Median # get Medians
  b_aug_vec <- summary_df[(sep_idx+1):(2*sep_idx),]$Median
  a_aug <- matrix(a_aug_vec, nrow=M, ncol=k)
  b_aug <- matrix(b_aug_vec, nrow=M, ncol=k)
  
  result <- NULL
  result$a_aug <- a_aug
  result$b_aug <- b_aug
  return(result)
} 
