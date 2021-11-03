library(SPAS)

#################
# Naive 1
# s = t = 3
# n = 10,000 (number of animals available for sampling in each of 3 strata)
# psi = 0.05 (capture prob. in each of 3 strata)
naive.data.csv <- textConnection("
25 ,  25  ,  25  ,  425
0  ,  25  ,  25  ,  450
0  ,   0  ,  25  ,  475
475, 950  ,1425  ,    0")
naive.data <- as.matrix(read.csv(naive.data.csv, header = FALSE))

naive.mod <- SPAS::SPAS.fit.model(
  naive.data,
  model.id = "Naive data no pooling",
  row.pool.in = 1:3,
  col.pool.in = 1:3
)
SPAS.print.model(naive.mod)

#################
# Naive 2
# s = t = 3
# n = 10,000 (number of animals available for sampling in each of 3 strata)
# psi1 = 0.2, psi2 = 0.1, psi3 = 0.05 (capture prob. in each of 3 strata)
naive.data2.csv <- textConnection("
400,  200,  100,  1300
0  ,  100,   50,   850
0  ,    0,   25,   475
1600, 1700, 1325,    0")
naive.data2 <- as.matrix(read.csv(naive.data2.csv, header = FALSE))

naive.mod2 <- SPAS::SPAS.fit.model(
  naive.data2,
  model.id = "Naive data 2 no pooling",
  row.pool.in = 1:3,
  col.pool.in = 1:3
)
SPAS.print.model(naive.mod2)

#################
# Naive 3
# s = t = 3
# n1 = 5,000, n2 = 10,000, n3 = 15,000 (number of animals available for sampling in each of 3 strata)
# psi1 = 0.4, psi2 = 0.5, psi3 = 0.05 (capture prob. in each of 3 strata)
naive.data3.csv <- textConnection("
800,  1000,  100,  100
0  ,  2500,  250, 2250
0  ,     0,   37,  713
1200, 4000, 1112,    0")
naive.data3 <- as.matrix(read.csv(naive.data3.csv, header = FALSE))

naive.mod3 <- SPAS::SPAS.fit.model(
  naive.data3,
  model.id = "Naive data 3 no pooling",
  row.pool.in = 1:3,
  col.pool.in = 1:3
)
SPAS.print.model(naive.mod3)

#################
