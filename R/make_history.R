#' Simulate histories a, b, h, from which deduce partial availability histories obs_a and obs_b
#'
#' @param N population size
#' @param M augmented population size
#' @param k number of capture occasions
#' @param theta1 probability of being available for lab test
#' @param pa capture probability of lab test
#' @param theta2 probability of being available for hospitalization
#' @param pb capture probability of hospitalization
#'
#' @other `a` latent indicator of whether individual is available for capture at lab test
#' @other `b` latent indicator of whether individual is available for capture at hospital
#'
#' @return A `result` list containing:
#' Function input parameters,
#' `h` capture history with 0s and 1s indicators,
#' `h_2` capture history with 0s, 1s and 2s indicators,
#' `a` lab test availability history,
#' `b` hospital availability history,
#' `obs_a` observed lab test availability history,
#' `obs_b` observed hospital availability history,
#' augmented versions of the histories.

make_history <- function(N, M, k, theta1, pa, theta2, pb) {

  a <- matrix(NA, nrow = N, ncol = k)
  b <- matrix(NA, nrow = N, ncol = k)
  h <- matrix(0, nrow = N, ncol = k) 
  h_2 <- matrix(0, nrow = N, ncol = k) # to contain 2s for recaptures

  #' first, simulate latent `a` and `b` and use them to create `h` and `h_2`
  for (i in 1:N) {
    for (j in 1:k) {
      if (j == 1) {
        #' must populate `b` first: if available for both lab test and hospital in the same time period then only count `b` (Assumption)
        b[i,j] <- 0
        a[i,j] <- rbern(1, theta1)
      } else {
        b[i,j] <- rbern(1, prod(1-b[i,], na.rm=T) * theta2)
        a[i,j] <- rbern(1, prod(1-a[i,], na.rm=T) * prod(1-b[i,], na.rm=T) * theta1)
      }

      #' capture histories
      if (rbern(1, a[i,j] * pa) == 1) {
        h_2[i,j] <- 1
        h[i,j] <- 1
      }
      if (rbern(1, b[i,j] * pb) == 1) {
        h_2[i,j] <- 2
        h[i,j] <- 1
      }
    }
  }

  #' second, use `h_2` to reconstruct availability histories as completely as possible (`obs_a` and `obs_b`)
  #' for example, a `1` in column 2 in `obs_a` indicates that a person is definitely around for lab testing during stratum 2
  #' see `populate_row_obs_ab()` for details
  obs_ab <- h_2 %>% apply(1, populate_obs_ab_by_row) %>% t()
  obs_a <- obs_ab[,1:k]
  obs_b <- obs_ab[,(k+1):(2*k)]
  n <- sum(rowSums(h, na.rm = T) > 0) # number captured
  n_2 <- sum(h_2 == 2, na.rm = T) # number recaptured

  #' augmented histories contain more NA rows for obs_a, obs_b and 0s for h
  obs_a_aug <- rbind(obs_a, matrix(NA, nrow = M - N, ncol = k))
  obs_b_aug <- rbind(obs_b, matrix(NA, nrow = M - N, ncol = k))
  h_aug <- rbind(h, matrix(0, nrow = M - N, ncol = k))
  
  z_init <- rowSums(h_aug)
  z_init[z_init == 0] <- NA
  z_init[z_init > 0] <- 1
  
  result <- NULL
  result$N <- N
  result$n <- n
  result$n_2 <- n_2
  result$M <- M
  result$k <- k
  result$pa <- pa
  result$pb <- pb
  result$theta1 <- theta1
  result$theta2 <- theta2
  result$h <- h
  result$h_2 <- h_2
  result$a <- a
  result$b <- b
  result$obs_a <- obs_a
  result$obs_b <- obs_b
  result$obs_a_aug <- obs_a_aug
  result$obs_b_aug <- obs_b_aug
  result$h_aug <- h_aug
  result$z_init <- z_init

  return(result)
}
