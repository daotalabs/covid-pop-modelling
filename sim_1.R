library(Rlab)
library(dplyr)
library(nimble)
library(basicMCMCplots)

sim_1 <- make_history(N=500, M=1000, k=7, theta1=0.2, pa=0.75, theta2=0.15, pb=0.9)
mcmc_1 <- build_nimble_model(sim_1, chains=1, burnin = 5000, iter=10000)

# Uncomment the following to view results..
mcmc_1$mcmc$summary[c("N", "psi", "theta1", "pa", "theta2", "pb"),]
samplesPlot(mcmc_1$mcmc$samples[, c("psi", "theta1", "pa", "theta2", "pb")], traceplot=T)
hist(mcmc_1$mcmc$samples[, "N"])

sum(rowSums(sim_1$h) %in% c(1,2))
