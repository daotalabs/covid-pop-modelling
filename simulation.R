n <- 100 # number of fish marked and released on day i at 1st trap
U <- 200 # total unmarked fish captured at 2nd trap on day j
s <- 10 # capture days at 1st trap
t <- 10 # capture days at 2nd trap
p <- .4 # probability of recapture on day j
theta <-
  1 # probability of marked fish released at 1st trap on day i passing 2nd trap on day j
u <-
  vector(length = 10) # number of unmarked fish captured at 2nd trap on day j
m <-
  matrix(0, nrow = s, ncol = t) # number of fish among n_i recaptured on day j

# want to find total marked fish never recaptured from each stratum i
# make a 3D array to track each fish
CH <- array(NA, c(s, n, t))


for (i in 1:(s - 1)) {
  for (k in 1:n) {
    for (j in (i + 1):t) {
      uni1 <- runif(1, 0, 1)
      uni2 <- runif(1, 0, 1)
      # 1 if a marked fish is recaptured, 0 if not recaptured
      CH[i, k, j] <- rbinom(1, 1, p * theta)
      # if (uni1 <= p) {
      #   CH[i, k, j] <- 1
      # } else {
      #   CH[i, k, j] <- 0
      # }
    }
  }
}

# sum the columns in CH to populate m[i,j]
for (i in 1:(s - 1)) {
  for (j in (i + 1):t) {
    m[i, j] <- sum(CH[i, , j])
  }
}

# calculate u[j] = n - sum(m[,j])
for (j in 2:t) {
  u[j] <- 200
}

# z[i] - number of marked fish on day i never recaptured
z <- array(dim = 10)
for (i in 1:s) {
  temp <- c()
  for (k in 1:n) {
    temp <- c(temp, sum(CH[i, k, ], na.rm = T))
  }
  z[i] <- sum(temp == 0)
}
# for (i in 1:s) {
#   z[i] <- n - sum(m[i,])
# }

# final data matrix
data <- cbind(m, z)
data <- rbind(data, c(u, NA))
data <- data[-10,-1] # make it look like in SPAS examples

# fit stratified Petersen model SPAS
library(SPAS)
mod1 <- SPAS.fit.model(
  data,
  model.id = "Trial",
  row.pool.in = 1:(s - 1),
  col.pool.in = 1:(t - 1)
) # no pooling
SPAS.print.model(mod1) # psi is z's, cap.prob is p*theta


# conne.data.csv <- textConnection("
#  149,  126,    0,    0,    0,    0,      1561
#    0,  308,   65,    0,    0,    0,      1235
#    0,    0,  161,   77,    0,    0,       884
#    0,    0,    0,   67,    7,    0,       215
#    0,    0,    0,    0,   17,    3,        71
#    0,    0,    0,    0,    0,   13,        16
# 1895, 8503, 2184,  525,  155,  118,         0")
# conne.data <- as.matrix(read.csv(conne.data.csv, header=FALSE))
# mod2 <- SPAS.fit.model(conne.data,
#                        model.id="Trial 2",
#                        row.pool.in=1:6, col.pool.in=1:6) # no pooling
# SPAS.print.model(mod2) # psi is z's, cap.prob is p*theta
