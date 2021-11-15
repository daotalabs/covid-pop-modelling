library(tictoc)
library(parallel)
library(foreach)
library(doParallel)

tic("sleeping")
spas_pop_sim(ns, s, t, cap_prob)
toc()

# example of interval tree
range_mins <- c(0, c(0.2, 0.3, 0.55, 0.6)) # 0 is the first min
range_maxes <- c(c(0.2, 0.3, 0.55, 0.6), 1) # 1 is the final max
ranges <- data.frame(1:(4+1), range_mins, range_maxes) # (s+1) to account for the additional range representing probability of unrecovery
tree <- IntervalTree(data=ranges, root=list())
overlapQuery(tree, 0.19)


ns <- c(5, 10, 15)
ch <- vector("list", length = s)

ch <- foreach (i=1:s) %dopar% {
  data.frame(matrix(data = 0, nrow = ns[i], ncol = t))
}


ch <- foreach (i=1:s) %dopar% {
  foreach(a=1:ns[i], .combine=rbind) %dopar% ifelse(runif(1) > 0.5, 0, 1)
}

ch <- lapply(1:s, function(i) data.frame(matrix(data = 0, nrow = ns[i], ncol = t+1)))
for (i in 1:s) {
  for (a in 1:ns[i]) {
    ch[[i]][a, 1] <- ifelse(runif(1) > cap_prob[i], 0, 1)
    cap_p <- runif(1)
    # after first capture attempt a fish can go to only 1 the recovery stratum or never recovered,
    # do this by comparing cap_p against prob_tree here to determine which stratum j a fish goes to, or goes unrecovered
    interval_no <- as.integer(overlapQuery(prob_tree, cap_p)[[1]][1]) # can't throw out of range error because cap_prob is always within range [0,1]
    if (interval_no <= s) {
      ch[[i]][a, interval_no + 1] <- 1 # fish is caught in stratum j = interval_no + 1
    }
  }
  
  
}

ch[[3]][1, 1] <- 1


prob_tree <- buildIntervalTree(cap_prob)

CH <- list()
tic("running..")
CH <- sapply(1:s, function(i) {
  # CHs matrices to track all fish sampled in i=x in s
  CH[[i]] <- data.frame(matrix(data = 0, nrow = ns[i], ncol = t + 1))
  CH[[i]] <- sapply(1:ns[i], function(a) {
    if (runif(1) > cap_prob[i]) {
      CH[[i]][a, 1] <- 0 # fish not caught/untagged
    } else {
      CH[[i]][a, 1] <- 1 # fish tagged
    }
    
    cap_p <- runif(1)
    # after first capture attempt a fish can go to only 1 the recovery stratum or never recovered,
    # do this by comparing cap_p against prob_tree here to determine which stratum j a fish goes to, or goes unrecovered
    interval_no <- as.integer(overlapQuery(prob_tree, cap_p)[[1]][1]) # can't throw out of range error because cap_prob is always within range [0,1]
    if (interval_no <= s) {
      CH[[i]][a, interval_no + 1] <- 1 # fish is caught in stratum j = interval_no + 1
    }
  })
})
toc()


