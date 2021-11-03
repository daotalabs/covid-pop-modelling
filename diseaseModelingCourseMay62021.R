## Install and access deSolve package 
install.packages("deSolve") 
library(deSolve)

## Create an SIR function
# time = observation times
# state = initial conditions (t=0)
SIR = function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS= (-beta*S*I)
    dI= (beta*S*I)-(gamma*I) 
    dR= gamma*I
    return(list(c(dS, dI, dR)))
  })
}

### Initialization
init = c(S = 0.999, I = 0.001, R = 0.0) 
parameters = c(beta = 1, gamma = 0.2)
times = seq(0, 40, by = 1)

## Solve using ode (essentially produce output)
out = ode(y = init, times = times, func = SIR, parms = parameters)

## plot data
out = as.data.frame(out)
out$time = NULL #remove as I don’t want this in plot
matplot(x = times, y = out, type = "l",
        xlab = "Time", ylab = "Susceptible and Recovered",
        main = "SIR Model", lwd = 1, lty = 1, bty = "l", col = c(4,2,3))
legend(20, 0.7, c("Susceptible", "Infected", "Recovered"), pch = 1, col = c(4,2,3), bty = "n")

# session 2
install.packages("EpiILMCT") 
library("EpiILMCT")
net <- contactnet(type = 'random', num.id = 500, beta = 1)
#Note: beta above is the prob. of an edge between nodes in contact # network; not transmission rate:
epi <- datagen(type = 'SIR', kerneltype = 'network', kernelmatrix = net, suspar = 0.01,
               delta = c(1, 1))
# transmission rate (beta) is 0.01
# infectious periods follow a gamma (1,1) distribution
plot(epi, plottype = "history")


susccov = list(NULL)
susccov[[1]] = list(0.03, c("gamma", 1, 1, 0.001) ) 
susccov[[2]] = rep(1,500)
#above: gamma(1,1) prior on beta;
# 0.001 is the proposal variance for the MCMC update
# the rep(1,500) refers to there being no individual-level
#      susceptibility covariates
mcmc1 <- epictmcmc(object = epi, datatype = "known epidemic", nsim = 2000, control.sus = susccov)
#above: runs MCMC for 2000 iterations
plot(mcmc1, plottype = "parameter", start = 1, thin = 1, density = FALSE, ylab="transmission rate", main="MCMC Trace Plot")
