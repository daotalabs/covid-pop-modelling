# runs a MCMC on a data list or the simulated history from make_history()
build_nimble_model <- function(data, chains, iter, burnin) {
  covidCode <- nimbleCode({
    # uniform priors
    psi ~ dunif(0, 1)
    pa ~ dunif(0, 1)
    pb ~ dunif(0, 1)
    theta1 ~ dunif(0, 1)
    theta2 ~ dunif(0, 1)

    # model
    for (i in 1:M) {
      z[i] ~ dbern(psi)

      # j=1
      # force b=0 for j=1
      b[i,1] ~ dbern(0)
      a[i,1] ~ dbern(z[i] * theta1)
      h[i,1] ~ dbern(z[i] * a[i,1] * pa + z[i] * b[i,1] * pb)

      # j>=2
      for (j in 2:k) {
        b[i,j] ~ dbern(z[i] * prod(1-b[i,1:(j-1)]) * theta2)
        a[i,j] ~ dbern(z[i] * prod(1-a[i,1:(j-1)]) * prod(1-b[i,1:j]) * theta1)
        h[i,j] ~ dbern(z[i] * a[i,j] * pa + z[i] * b[i,j] * pb)
      }
    }

    N <- sum(z[1:M])
  })

  covidConsts <- list(M = data$M,
                      k = data$k)

  covidData <- list(h = data$h_aug,
                    a = data$obs_a_aug,
                    b = data$obs_b_aug)

  covidInits <- list(psi = runif(1,0,1),
                     theta1 = runif(1,0,1),
                     pa = runif(1,0,1),
                     theta2 = runif(1,0,1),
                     pb = runif(1,0,1),
                     z = data$z_init)

  mcmc_out <- nimbleMCMC(code = covidCode,
                         constants = covidConsts,
                         data = covidData,
                         inits = covidInits,
                         nchains = chains,
                         niter = iter,
                         nburnin = burnin,
                         summary = TRUE,
                         WAIC = TRUE,
                         monitors = c("N", "psi", "theta1", "pa", "theta2", "pb"))
  
  return(mcmc_out)
}
