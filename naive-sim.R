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
# INCORRECT DATA TABLE
# Naive 4
# s = 2, t = 3
# n's = 10,000 (number of animals available for sampling in each of 3 strata)
# psi1 = 0.2, psi2 = psi3 = 0.1 (capture prob. in each of 3 strata)
naive.data4.csv <- textConnection("
400,   200,  200, 1200
0  ,   100,  100,  800
1600, 1700, 1700,    0")
naive.data4 <- as.matrix(read.csv(naive.data4.csv, header = FALSE))

naive.mod4 <- SPAS::SPAS.fit.model(
  naive.data4,
  model.id = "Naive data 4 no pooling",
  row.pool.in = 1:2,
  col.pool.in = 1:3
)
SPAS.print.model(naive.mod4)

#################
# Naive 5
# s = 3, t = 5
# n1 = 10,000; n2=7500; n3=15,000 (number of animals available for sampling in each of 3 strata)
# psi1 = 0.1; psi2 = 0.2; psi3 = 0.1 (tagging capture prob. in each of s=3)
# p1 = 0.2; p2=0.15; p3=0.05; p4=0.3; p5=0.075 (recovery capture prob. in each of t=5)
#
# NOTE: result estimates only 3 cap. prob's and they don't match any psi or p
naive.data5.csv <- textConnection("
 200,  150,   50,  300,   75,  225
   0,  225,   75,  450,  112,  638
   0,    0,  225, 1350,  337, 2588
1800, 2250, 5025, 7650, 1912,    0")
naive.data5 <- as.matrix(read.csv(naive.data5.csv, header = FALSE))

naive.mod5 <- SPAS::SPAS.fit.model(
  naive.data5,
  model.id = "Naive data 5 no pooling",
  row.pool.in = 1:3,
  col.pool.in = 1:5
)
SPAS.print.model(naive.mod5)

