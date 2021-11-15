# naive 1
spas_pop_sim(Ns=c(10000, 10000, 10000), s=3, t=3, cap_prob=c(0.05, 0.05, 0.05))

# naive 2
spas_pop_sim(Ns=c(10000, 10000, 10000), s=3, t=3, cap_prob=c(0.2, 0.1, 0.05))

# naive 3
spas_pop_sim(Ns=c(5000, 10000, 15000), s=3, t=3, cap_prob=c(0.4, 0.5, 0.05))

# larger s=t=8
spas_pop_sim(Ns=c(5000, 10000, 15000, 7500, 20000, 25000, 12000, 30000), s=8, t=8, cap_prob=c(0.1, 0.18, 0.075, 0.2, 0.1, 0.05, 0.15, 0.125))
# pop est comes out very small if cap_prob is very small (pool 5: p=0.005, pop=112)
spas_pop_sim(Ns=c(5000, 10000, 15000, 7500, 20000, 25000, 12000, 30000), s=8, t=8, cap_prob=c(0.1, 0.18, 0.075, 0.2, 0.005, 0.05, 0.15, 0.125))

# t > s
# this isn't working properly yet for t > s, see TODO in stochastic-sim.R
spas_pop_sim(Ns=c(5000, 1000, 15000), s=3, t=5, cap_prob=c(0.075, 0.1, 0.2))
