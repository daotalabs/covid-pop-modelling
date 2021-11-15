# naive 1
s <- 3
t <- 3
Ns <- c(10000, 10000, 10000) # length of ns = s
cap_prob <-
  c(0.05, 0.05, 0.05) # length of cap_prob = s, the rest of the prob's = last prob

result1 <- spas_pop_sim(Ns, s, t, cap_prob)

# naive 2
s <- 3
t <- 3
Ns <- c(10000, 10000, 10000) # length of ns = s
cap_prob <-
  c(0.2, 0.1, 0.05) # length of cap_prob = s, the rest of the prob's = last prob

result2 <- spas_pop_sim(Ns, s, t, cap_prob)
result2$final_table

# naive 3
s <- 3
t <- 3
Ns <- c(5000, 10000, 15000) # length of ns = s
cap_prob <-
  c(0.4, 0.5, 0.05) # length of cap_prob = s, the rest of the prob's = last prob

result3 <- spas_pop_sim(Ns, s, t, cap_prob)
result3$final_table

# t > s
s <- 3
t <- 5
Ns <- c(1500, 500, 500) # length of ns = s
cap_prob <-
  c(0.05, 0.05, 0.05) # length of cap_prob = s, the rest of the prob's = last prob

spas_pop_sim(Ns, s, t, cap_prob)