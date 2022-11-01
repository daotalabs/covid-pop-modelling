#' Simulate histories a, b, h, from which deduce partial availability histories obs_a and obs_b
#'
#' @param N population size
#' @param M augmented population size
#' @param k number of capture occasions
#' @param pa capture/(detection?) probability of lab test
#' @param pb capture probability of hospitalization
#' @param theta1 probability of being available for lab test
#' @param theta2 probability of being available for hospitalization
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

make_history <- function(N, M, k, pa, pb, theta1, theta2) {
  set.seed(5) # for random number generation
  
  a <- matrix(NA, nrow = N, ncol = k)
  b <- matrix(NA, nrow = N, ncol = k)
  h <- matrix(0, nrow = N, ncol = k) # contains 0s 1s indicating captures
  h_2 <- matrix(0, nrow = N, ncol = k) # contains 0s 1s 2s indicating specific capture occasions
  
  #' first, simulate latent `a` and `b` and use them to create `h` and `h_2`
  for (i in 1:N) {
    for (j in 1:k) {
      if (j==1) {
        #' must populate `b` first: if available for both lab test and hospital in the same time period then only count `b` (Assumption) 
        b[i,j] <- 0
        a[i,j] <- rbern(1, theta1)
      } else {
        b[i,j] <- rbern(1, prod(1-b[i,], na.rm=T) * theta2)
        a[i,j] <- rbern(1, prod(1-a[i,], na.rm=T) * prod(1-b[i,], na.rm=T) * theta1)
      }
      
      #' for `h_2`, indicate captures by `1`s and recaptures by `2`s 
      #' in `h`, both are `1`s
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
  
  #' second, use `h_2` to reconstruct certain/observed availability histories `obs_a` and `obs_b` as completely as possible 
  #' for example, a `1` in position #2 in `obs_a` indicates that a person is definitely around for lab testing during time period #2
  #' see `populate_row_obs_ab()` for more examples
  obs_ab <- h_2 %>% apply(1, populate_row_obs_ab) %>% t()
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
  result$h_2 <- h_2
  result$a <- a
  result$b <- b
  result$obs_a <- obs_a
  #' augmented histories contain more NA rows
  result$obs_a_aug <- rbind(obs_a, matrix(NA, nrow = M-N, ncol = k))
  result$obs_b <- obs_b
  result$obs_b_aug <- rbind(obs_b, matrix(NA, nrow = M-N, ncol = k))
  result$h_aug <- rbind(h, matrix(0, nrow = M-N, ncol = k))
  result$h_2_aug <- rbind(h_2, matrix(0, nrow = M-N, ncol = k))
  result$a_aug <- rbind(a, matrix(0, nrow = M-N, ncol = k))
  result$b_aug <- rbind(b, matrix(0, nrow = M-N, ncol = k))
  return(result)
}