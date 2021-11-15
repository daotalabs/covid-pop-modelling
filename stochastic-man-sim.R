library(dplyr)
library(SPAS)

################
# Stochastic 1 - this works!
# s = t = 3
# n's = 10,000
# psi_1 = 0.2, psi_2 = 0.1, psi_3 = 0.05 (capture prob.'s for stratum 1, 2, 3)
n <- 10000
s <- 3
t <- 3
psi1 <- 0.2
psi2 <- 0.1
psi3 <- 0.05

###########
# Stochastic 3 - this also works! a bit off for psi3, may be due to small numbers of recovery
# s = t = 3
# n1 = 5000, n2 = 10000, n3 = 15000
# psi1 = 0.4, psi2 = 0.5, psi3 = 0.05
# n1 <- 5000
# n2 <- 10000
# n3 <- 15000
# psi1 <- 0.4
# psi2 <- 0.5
# psi3 <- 0.05


# matrix CH1 to capture/tag and track each fish in i=1
# nrow is the number of fish available for sampling in i=1
# ncol = t+1
# first column is whether a fish is tagged
# columns 2 to 4 is whether a fish, tagged or untagged, is recovered in j=2,j=3, j=4
# the sum of each column is the total number of fish caught in j's
# the number of tagged fish recovered is the total number of caught fish - those tagged in column 1
CH1 <- data.frame(matrix(nrow = n1, ncol = 4))
colnames(CH1) <- c('tagged', 'j1', 'j2', 'j3')
for (a in 1:n1) {
  if (runif(1) > psi1) {
    CH1[a, 1] <- 0 # first col: a fish isn't tagged
  } else {
    CH1[a, 1] <- 1 # first col: a fish is tagged
  }
  cap_p <-
    runif(1) # from now a fish can go to only 1 recovery stratum: 1 or 2 or 3 or never recovered
  if (cap_p < psi3) {
    CH1[a, 4] <- 1 # caught in j=3
  } else if (cap_p < psi2 + psi3) {
    CH1[a, 3] <- 1 # caught in j=2
  } else if (cap_p < psi1 + psi2 + psi3) {
    CH1[a, 2] <- 1 # caught in j=1
  }
}

# matrix CH2 to capture/tag and track each animal in sample 2
CH2 <- data.frame(matrix(nrow = n2, ncol = 4))
colnames(CH2) <- c('tagged', 'j1', 'j2', 'j3')
for (a in 1:n2) {
  if (runif(1) > psi2) {
    CH2[a, 1] <- 0
  } else {
    CH2[a, 1] <- 1
  }
  cap_p <- runif(1)
  if (cap_p < psi3) {
    CH2[a, 4] <- 1
  } else if (cap_p < psi2 + psi3) {
    CH2[a, 3] <- 1
  }
}

# matrix CH3 to capture/tag and track each animal in sample 3
CH3 <- data.frame(matrix(nrow = n3, ncol = 4))
colnames(CH3) <- c('tagged', 'j1', 'j2', 'j3')
for (a in 1:n3) {
  if (runif(1) > psi3) {
    CH3[a, 1] <- 0
  } else {
    CH3[a, 1] <- 1
  }
  cap_p <- runif(1)
  if (cap_p < psi3) {
    CH3[a, 4] <- 1
  }
}

# sum up matrix columns for Table 3 (Schwartz & Taylor) values
tagged1 <- sum(CH1[, 1], na.rm = T)
tagged2 <- sum(CH2[, 1], na.rm = T)
tagged3 <- sum(CH3[, 1], na.rm = T)

m11 <- sum(CH1 %>% filter(tagged == 1) %>% select(j1), na.rm = T)
m12 <- sum(CH1 %>% filter(tagged == 1) %>% select(j2), na.rm = T)
m13 <- sum(CH1 %>% filter(tagged == 1) %>% select(j3), na.rm = T)

m22 <- sum(CH2 %>% filter(tagged == 1) %>% select(j2), na.rm = T)
m23 <- sum(CH2 %>% filter(tagged == 1) %>% select(j3), na.rm = T)

m33 <- sum(CH3 %>% filter(tagged == 1) %>% select(j3), na.rm = T)

# fish marked but never recovered
not.rec1 <- tagged1 - sum(m11, m12, m13)
not.rec2 <- tagged2 - sum(m22, m23)
not.rec3 <- tagged3 - m33

# need to figure out the numbers of recovered, untagged fish in each stratum
# untagged1 is number of untagged fish caught in i=1
# untagged1 is sampling from i=1 with psi1 - number of tagged fish in j=1
# untagged2 is sampling from i=1 with psi1 + sampling from i=2 with psi2 - tagged in j=1 - tagged in j=2
# untagged3 is sampling from i=1 with psi1 + sampling from i=2 with psi2 + sampling from i=3 with psi3 - tagged in j=1 - tagged in j=2 - tagged in j=3
untagged1 <- sum(CH1 %>% select(j1), na.rm = T) - m11
untagged2 <-
  sum(CH1 %>% select(j2), na.rm = T) + sum(CH2 %>% select(j2), na.rm = T) - m12 - m22
untagged3 <-
  sum(CH1 %>% select(j3), na.rm = T) + sum(CH2 %>% select(j3), na.rm = T) + sum(CH3 %>% select(j3), na.rm = T) - m13 - m23 - m33

stoc.data1 <- c(
  m11,
  m12,
  m13,
  not.rec1,
  0,
  m22,
  m23,
  not.rec2,
  0,
  0,
  m33,
  not.rec3,
  untagged1,
  untagged2,
  untagged3,
  0
)
stoc.data1 <- matrix(stoc.data1,
                     nrow = 4,
                     ncol = 4,
                     byrow = T)

stoc.mod1 <- SPAS::SPAS.fit.model(
  stoc.data1,
  model.id = "Stochastic simulation no pooling 1",
  row.pool.in = 1:3,
  col.pool.in = 1:3
)
SPAS.print.model(stoc.mod1)
