#' Create observed obs_a and obs_b from each row in history h_2. The presence and positions of 1 and 2 in h_2 determine what's in obs_a and obs_b.
#'
#' @param h_row a row in h_2 (capture history of a person)
#'
#' @return combined obs_a and obs_b deduced from h_2

populate_obs_ab_by_row <- function(h_row) {
  
  k <- length(h_row)
  
  #' for `h_2` that looks like (a `1` always precedes a `2`)
  #' [1,]    0 1 0 2 0
  #' make other positions `0`s for `obs_a`
  #' make other positions `0`s `obs_b`
  #' obs_a = 0 1 0 0 0
  #' obs_b = 0 0 0 1 0
  if (sum(h_row, na.rm = T) == 3) {
    h_row_1_idx <- which(h_row == 1)
    h_row_2_idx <- which(h_row == 2)
    
    obs_a_row <- rep(0, k)
    obs_a_row[h_row_1_idx] <- 1
    
    obs_b_row <- rep(0, k)
    obs_b_row[h_row_2_idx] <- 1
  }
  
  #' for `h_2` that looks like
  #' [2,]    0 0 1 0 0
  #' make other positions `0`s for `obs_a`,
  #' make prior positions `0`s for `obs_b`
  #' obs_a = 0 0 1  0  0
  #' obs_b = 0 0 0 NA NA
  else if (sum(h_row, na.rm = T) == 1) {
    h_row_1_idx <- which(h_row == 1)
    
    obs_a_row <- rep(0, k)
    obs_a_row[h_row_1_idx] <- 1
    
    obs_b_row <- rep(NA, k)
    obs_b_row[1:h_row_1_idx] <- 0
  }
  
  #' for `h_2` that looks like
  #' [3,]    0    0    2    0    0
  #' make subsequent positions `0`s for `obs_a`,
  #' make other positions `0`s for `obs_b`
  #' obs_a = NA NA 0 0 0
  #' obs_b =  0  0 1 0 0
  else if (sum(h_row, na.rm = T) == 2) {
    h_row_2_idx <- which(h_row == 2)
    
    obs_a_row <- rep(NA, k)
    obs_a_row[h_row_2_idx:k] <- 0
    
    obs_b_row <- rep(0, k)
    obs_b_row[h_row_2_idx] <- 1
  }
  
  #' for `h_2` with no captures
  #' [4,]    0  0  0  0  0
  else {
    obs_a_row <- rep(NA, k)
    obs_b_row <- rep(NA, k)
  }
  
  obs_ab <- cbind(obs_a_row, obs_b_row)
  return(obs_ab)
}