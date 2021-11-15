library(dplyr)
library(SPAS)
library(rIntervalTree)

# spas_pop_sim is a function to simulate data for modelling using SPAS
# Input:
# s - number of tagging strata;
# t - number of recovery strata (t >= s);
# Ns - vector of length s contains the fish population available in each tagging stratum;
# cap_prob - vector of length s contains the capture probabilities in each recovery stratum.
#
# Output:
# SPAS.print.model output

# NOTE: generally in this function, 
# i indicates tagging stratum such as in CH[[i]];
# j indicates recovery stratum such as in CH[[i]][, j].

# TODO: currently not working for s < t
spas_pop_sim <- function(Ns, s, t, cap_prob) {
  # input validation
  validateInput(Ns, s, t, cap_prob)
  
  # build interval tree of capture probabilities, will use later for allocation of recovered fish
  prob_tree <- buildIntervalTree(cap_prob)
  
  # structures to store numbers for SPAS table
  tagged_no <- 1:s
  m_table <- data.frame(matrix(data = 0, nrow = s, ncol = t))
  not_rec <- data.frame(matrix(data = 0, nrow = s, ncol = 1))
  
  # CH[[i]] matrices to track all fish sampled in i=x in s
  CH <- list()
  CH <- lapply(1:s, function(i) data.frame(matrix(data = 0, nrow = Ns[i], ncol = t+1)))
  
  # TODO: do these loops in parallel?
  for (i in 1:s) {
    # iterate through rows in each CH[[i]]
    for (a in 1:Ns[i]) {
      # first column is whether a fish is tagged
      CH[[i]][a, 1] <- ifelse(runif(1) > cap_prob[i], 0, 1)

      # after first capture/tagging attempt, a fish can go to only 1 subsequent (j >= i) recovery stratum or never recovered,
      # simulate this allocation by comparing cap_p against prob_tree here to determine which stratum j a fish goes to, or goes unrecovered
      # need to account for intevals change when j > i (DONE?)
      interval_no <- as.integer(overlapQuery(prob_tree, runif(1))[[1]][1]) # in the rare chance runif(1) falls at the border of two intervals, just choose the first interval
                                                                           # can't throw out of range error because cap_prob is always in range [0,1]
      if (interval_no <= s && interval_no >= i) { # <= s because > s is unrecovered, >= i because fish can't be recovered in j < i
        CH[[i]][a, interval_no + 1] <- 1 # fish is caught in stratum j = interval_no + 1
      }
    }
    
    # total number of fish tagged in i=x
    tagged_no[i] <- sum(CH[[i]]$X1)
    
    # calculate m's: number of tagged fish recovered in j
    for (j in 2:(t+1)) {
      m_table[i, j-1] <- sum(CH[[i]] %>% filter(X1 == 1) %>% select(j)) 
    }

    # number of fish tagged but never recovered
    not_rec[i, 1] <- tagged_no[i] - sum(m_table[i, ])
  }
  
  # calculate total number untagged and recovered in j
  # by generalizing this:
  # untagged[1] <- sum(CH[[1]][, 1]) - m_table[1, 1]
  # untagged[2] <- sum(CH[[1]][, 2]) - m_table[1, 2] + sum(CH[[2]][, 2]) - m_table[2, 2]
  # untagged[3] <- sum(CH[[1]][, 3]) - m_table[1, 3] + sum(CH[[2]][, 3]) - m_table[2, 3] + sum(CH[[3]][, 3]) - m_table[3, 3]
  untagged_no <- rep(0, t)
  # TODO: do these loops in parallel?
  for (j in 2:(t+1)) {
    untagged_no[j-1] <- 0
    for (i in 1:min(j-1, s)) {
      untagged_no[j-1] <- untagged_no[j-1] + sum(CH[[i]][, j]) - m_table[i, j-1]
    }
  }
  
  # construct data table for SPAS 
  
  result <- NULL
  result$tagged_no <- tagged_no
  result$m_table <- m_table
  result$untagged_no <- untagged_no
  result$not_rec <- not_rec
  result$CH <- CH
  untagged_no <- c(untagged_no, NA)
  final_table <- cbind(m_table, not_rec)
  colnames(final_table)[4] <- 'X4'
  final_table <- rbind(final_table, untagged_no)
  result$final_table <- final_table
  
  return(result)
  
  # run SPAS
  # printModel <- fitSPAS(s, t, final_table)
  # return(printModel)
}

# fit SPAS model no pooling
fitSPAS <- function(s, t, data_table) {
  mod <- SPAS::SPAS.fit.model(
    as.matrix(data_table),
    model.id = "Stochastic simulation no pooling",
    row.pool.in = 1:s,
    col.pool.in = 1:t
  )
  return(SPAS.print.model(mod))
}


# TODO: make sure inputs are correct
validateInput <- function(Ns, s, t, cap_prob) {
  print('Validated.')
  # length(Ns) == s
  # s =< t
  # length(cap_prob) == s
}

# build interval tree of probabilities
buildIntervalTree <- function(cap_prob) {
  print('Building interval tree..')
  range_mins <- c(0, cumsum(cap_prob)) # 0 is the first min
  range_maxes <- c(cumsum(cap_prob), 1) # 1 is the final max
  ranges <- data.frame(1:(s+1), range_mins, range_maxes) # (s+1) to account for the additional range representing probability of unrecovery
                                                        # the intervals overlap at the range values but will handle this at overlapQuery
  tree <- IntervalTree(data=ranges, root=list())
  return(tree)
}
