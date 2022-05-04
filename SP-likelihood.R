# these values are preset from simulation test case #1
set.seed(22)
sim_result <- simulateSPAS(Ns=c(15000, 10000, 15000), s=3, t=3, tag_prob=c(0.15, 0.15, 0.2), rec_prob = c(0.1, 0.15, 0.1))
SPAS.print.model(sim_result$model)
s <- 3 # t = s
t <- 3
m <- sim_result$m_table  # matrix of i rows and j columns
n <- sim_result$Ns # vector of length i
u <- sim_result$untagged_no # vector of length j

# likelihood function
lossFn <- function(param) {
  # set up the likelihood
  U <- param[1:3]
  cat("\n U: ", U)
  theta <- param[4:6]
  cat("\n theta: ", theta)
  p <- param[7:9]
  cat("\n p: ", p)
  logp_marked_captured <- 0
  for (i in 1:s) {
    cat("\n i: ", i)
    for (j in i:t) {
      cat("\n j: ", j)
      cat("\n theta[j]: ", theta[j])
      cat("\n p[j]: ", p[j])
      logp_marked_captured <- logp_marked_captured + 
                              m[i,j]*log(theta[i]*p[j]) + 
                              (n[i] - sum(m[i,])*log(1-theta[i]*p[j]))
      # cat("\n marked captured: ", logp_marked_captured)
    }
  }
  
  logp_unmarked_captured <- 0
  for (j in 1:t) {
    logp_unmarked_captured <- logp_unmarked_captured + 
                              lfactorial(U[j]) - lfactorial(u[j]) - lfactorial(U[j]+u[j])
                              u[j]*log(p[j]) + 
                              (U[j]-u[j])*log(1-p[j])
    # cat('\n unmarked captured: ', logp_unmarked_captured)
  }
  
  log_likelihood <- logp_marked_captured + logp_unmarked_captured
  return(-log_likelihood)
}

# initialize some params
U <- c(10000, 10000, 10000) 
theta <- c(0.05, 0.05, 0.05) # mirrors tag_prob
p <- c(0.1, 0.1, 0.1) # mirrors rec_prob 
lossFn(c(U, p, theta))

# TODO: put value constraints on the probabilities
fit <- optim(c(U, theta, p), lossFn, hessian = F)

fit <- optim(c(U, theta, p), lossFn, lower=c(c(0,0,0), c(0,0,0), c(0,0,0)), upper=c(c(15000,15000,15000), c(1,1,1), c(1,1,1)), method="L-BFGS-B") 


