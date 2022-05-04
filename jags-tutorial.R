library(coda)
library(R2jags)
library(rjags)

cat(
model {
  #### Priors and Constraints ####
  # Prior for Time-dependent removal-entry
  for (j in 1:(n.occasions-1)) {
    gamma[j] ~ dunif(0,1)
  }
  # Prior for Time-dependent adult survival
  for (j in 1:(n.occasions-1)) {
    phi[j] ~ dunif(0,1)
  }
  # Prior for Time-dependent adult capture
  for (j in 1:n.occasions) {
    p[j] ~ dunif(0,1)
  }
  
  #### Define State-transitions and Observation matrices ####
  for (i in 1:M) {
    # Define probabilities of S(j+1) given S(j)
    # Dimension 1: state of departure
    # Dimension 2: state of arrival
    # Dimension 3: individual
    # Dimension 4: time
    for(j in 1:(n.occasions-1)) {
      pstate[1,1,i,j] <- 1-gamma[j]
      pstate[1,2,i,j] <- gamma[j]
      pstate[1,3,i,j] <- 0
      pstate[2,1,i,j] <- 0
      pstate[2,2,i,j] <- phi[j]
      pstate[2,3,i,j] <- (1-phi[j])
      pstate[3,1,i,j] <- 0
      pstate[3,2,i,j] <- 0
      pstate[3,3,i,j] <- 1
    }
    
    # Define probabilities of individual i being observed at time j given S(j)
    for(j in 1:n.occasions) {
      pobs[1,i,j] <- 0
      pobs[2,i,j] <- p[j]
      pobs[3,i,j] <- 0 
    }
  }

  #### Likelihood ####
  for(i in 1:M) {
    # Dummy occasion 1 where everyone is in state 1 (not yet in population)
    state[i,1] <- 1
    
    for(j in 2:n.occasions) {
      # state of individual i at time j given previous state
      state[i,j] ~ dcat(pstate[state[i,j-1],,i,j-1])
      # observation of individual i at time j given current state
      X[i,j] ~ dbern(pobs[state[i,j],i,j])
    } 
  }
  
  #### Derived Parameters ####
  for (i in 1:M) {
    for (j in 2:n.occassions) {
      whenAlive[i, j-1] <- equals(state[i,j],2)
    }
    alive[i] <- sum(whenAlive[i,])
  }
  
  for (j in 1:(n.occasions-1)) {
    N[j] <- sum(whenAlive[,j]) # Population size at each occasion
  }
  
  for (i in 1:M) {
    z[i] <- 1-equals(alive[i],0) # if individual i was never alive, z[i]=0
  }
  N_super <- sum(z)
}
, file="JSWithJAGS.txt") 
  
# NOTE (Viet):
# ch is observation history of each individual i over observing occasions j

#### Determine Known States ####
# For adult CH, we assume they are adults when they are first observed
# thus between the first and last observation, the state is 5, before that
# the state is unknown so NA
known.states.js <- function(ch){
  state <- matrix(NA,nrow=nrow(ch),ncol=ncol(ch))
  for (i in 1:nrow(ch)) {
    if (sum(ch[i,])==0) {
      next               # if never observed, skip to next individual
    } else {
      fst <- min(which(ch[i,]==1))
      lst <- max(which(ch[i,]==1))
      state[i,fst:lst] <- 2
    } 
  }
  return(state)
}

# Add dummy occasion to Ad_CH
Xobs.du.js <- as.matrix(cbind(rep(0,nrow(CH.js)),CH.js))
known_state.js <- known.states.js(Xobs.du.js)
      
#### Initial States for the latent values of the state matrix
# Put NA wherever the state is known and 1’s before the known
# states. After the known states, put 3’s
initial.states.js <- function(ch) {
  init_st <- matrix(NA,nrow=nrow(ch),ncol=ncol(ch))
  for(i in 1:nrow(ch)) {
    if(sum(ch[i,])==0) {
      init_st[i,] <- rep(1,ncol(ch))
    } else {
      fst <- min(which(ch[i,]==1))
      lst <- max(which(ch[i,]==1))
      init_st[i,1:fst] <- 1
      init_st[i,fst] <- NA
      init_st[i,lst:ncol(ch)] <- 3
      init_st[i,lst] <- NA
    } 
  }
  return(init_st)
}

init_state.js <- initial.states.js(Xobs.du.js)

# Augment the data
n.aug.js <- 3000
X_aug.js <- rbind(Xobs.du.js, matrix(0,
                                     ncol=ncol(Xobs.du.js),nrow=n.aug.js))
# Augment Initial state matrix and known state matrix
known_states_aug.js <- rbind(known_state.js,
                             matrix(NA,nrow=n.aug.js,
                                    ncol=ncol(known_state.js)))
init_states_aug.js <- rbind(init_state.js,
                            matrix(1,nrow=n.aug.js,
                                   ncol=ncol(init_state.js)))
# set the first column of init_states_aug and known_states_aug = NA
init_states_aug.js[,1] <- NA
known_states_aug.js[,1] <- NA
# Bundle Data
JS_data <- list(X = X_aug.js, M=nrow(X_aug.js),n.occasions = ncol(X_aug.js),
                state = known_states_aug.js)
n.occasions.js <- ncol(X_aug.js)

#### INITIAL VALUES
# Bundle Initial Values
JS_inits <- function() {
  list(phi=runif(n.occasions.js-1,0,1), p = runif(n.occasions.js,0,1), state = init_states_aug.js)
}

# Parameters to be monitored
JS_params <- c("phi","p","N","N_super","gamma")

# MCMC settings
ni.js <- 20000 # iterations..
nt.js <- 1 # thinning..
nb.js <- 5000 # number of iterations in the burn-in phase
nc.js <- 3 # number of MCMC chains

# Run JAGS model
JS_jags.fit <- jags(model.file="JSWithJAGS.txt",data = JS_data,
                    inits = JS_inits, parameters.to.save = JS_params,
                    n.iter = ni.js,n.chains = nc.js,
                    n.burnin = nb.js,n.thin = nt.js)
      