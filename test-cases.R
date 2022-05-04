# Test Case 1: s = t = 3
mod1 <- simulateSPAS(Ns=c(30000, 15000, 20000), s=3, t=3, tag_prob=c(0.15, 0.175, 0.1), rec_prob = c(0.1, 0.2, 0.35))
SPAS.print.mod(mod1$model)

# Test Case 2: larger s=t=8
simulateSPAS(Ns=c(5000, 10000, 15000, 7500, 20000, 25000, 12000, 30000), s=8, t=8, tag_prob=c(0.1, 0.18, 0.075, 0.2, 0.1, 0.05, 0.15, 0.125), rec_prob = rep(0.1, 8))

# pop est comes out very small if cap_prob and/or rec_prob is very small (pool 5: p=0.005, pop=112), need more effort to recover, or drop or pool strata
simulateSPAS(Ns=c(5000, 10000, 15000, 7500, 20000, 25000, 12000, 30000), s=8, t=8, tag_prob=c(0.1, 0.18, 0.075, 0.2, 0.005, 0.05, 0.15, 0.125), rec_prob = rep(0.1, 8))

# s < t
# this isn't working properly yet for s < t, see TODO in stochastic-sim.R
simulateSPAS(Ns=c(25000, 10000, 15000), s=3, t=5, tag_prob=c(0.075, 0.1, 0.2), rec_prob = rep(0.1, 5))
