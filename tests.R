# Viet Dao
# Last updated: May 3, 2022

### EXPERIMENTS ###
# estimate augmented a and b
sim31 <- makeHistory(N=1000, M=2000, k=5, pa=0.95, pb=0.85, theta1=0.7, theta2=0.3)
mcmc.out31 <- buildModel(sim31, chains=1, iter=20000)
# MCMC result for one chain
mcmc.out31$summary[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]
# Mean    Median    St.Dev. 95%CI_low 95%CI_upp
# pa     0.9702949 0.9713662 0.01446874 0.9396934 0.9955436
# pb     0.8527429 0.8472915 0.04830201 0.7726856 0.9704270
# psi    0.4970839 0.4971540 0.01227544 0.4733617 0.5210088
# theta1 0.6823180 0.6824081 0.01810593 0.6465289 0.7180792
# theta2 0.3062909 0.3069196 0.02483829 0.2523140 0.3524897

# plot posterior densities
samplesPlot(mcmc.out31$samples[,c('pa'
                                  ,'pb'
                                  ,'psi'
                                  ,'theta1'
                                  ,'theta2'
)]
,traceplot=TRUE)

sim31.a_aug <- extractAb(mcmc.out31, M=2000, k=5)$a_aug
sim31.b_aug <- extractAb(mcmc.out31, M=2000, k=5)$b_aug
# compare estimates with simulation
# NOTE: initial values are currently simulated a & b 
head(sim31$a_aug, 20)
head(sim31.a_aug, 20)
head(sim31$b_aug, 20)
head(sim31.b_aug, 20)

# last iter=12000 doesnt look converge yet
sim32 <- makeHistory(N=1000, M=2000, k=5, pa=0.95, pb=0.85, theta1=0.3, theta2=0.3)
mcmc.out32 <- buildModel(sim32, chains=1, iter=12000)
# MCMC result for one chain
mcmc.out32$summary[c('pa'
                     ,'pb'
                     ,'psi'
                     ,'theta1'
                     ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out32$samples[,c('pa'
                                  ,'pb'
                                  ,'psi'
                                  ,'theta1'
                                  ,'theta2'
)]
,traceplot=TRUE)

sim32.a_aug <- extractAb(mcmc.out32, M=2000, k=5)$a_aug
sim32.b_aug <- extractAb(mcmc.out32, M=2000, k=5)$b_aug
# compare estimates with simulation
# NOTE: initial values are currently simulated a & b 
head(sim32$a_aug, 20)
head(sim32.a_aug, 20)
head(sim32$b_aug, 20)
head(sim32.b_aug, 20)


# theta1=theta2=0.3
sim1 <- makeHistory(N=1000, M=2000, k=10, pa=0.95, pb=0.85, theta1=0.3, theta2=0.3)
mcmc.out1 <- buildModel(sim1)
# MCMC result for one chain
mcmc.out1$summary$chain1[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out1$samples$chain1[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# theta1=0.4, theta2=0.6 (sum=1)
sim2 <- makeHistory(N=1000, M=2000, k=10, pa=0.95, pb=0.85, theta1=0.4, theta2=0.6)
mcmc.out2 <- buildModel(sim2)
# MCMC result for one chain
mcmc.out2$summary$chain1[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out2$samples$chain1[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# low theta1=theta2=0.2
sim3 <- makeHistory(N=1000, M=2000, k=10, pa=0.95, pb=0.85, theta1=0.2, theta2=0.2)
mcmc.out3 <- buildModel(sim3)
# MCMC result for one chain
mcmc.out3$summary$chain1[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out3$samples$chain1[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# high theta1=theta2=0.8
sim4 <- makeHistory(N=1000, M=2000, k=10, pa=0.95, pb=0.85, theta1=0.8, theta2=0.8)
mcmc.out4 <- buildModel(sim4)
# MCMC result for one chain
mcmc.out4$summary$chain1[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out4$samples$chain1[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# keep it simple pa=pb=1, theta1=theta2=0.2
sim5 <- makeHistory(N=1000, M=2000, k=10, pa=1, pb=1, theta1=0.2, theta2=0.2)
mcmc.out5 <- buildModel(sim5)
# MCMC result for one chain
mcmc.out5$summary$chain1[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]


# theta1=0.4, theta2=0.6
sim6 <- makeHistory(N=1000, M=2000, k=10, pa=1, pb=1, theta1=0.4, theta2=0.6)
mcmc.out6 <- buildModel(sim6)
# MCMC result for one chain
mcmc.out6$summary[c('pa'
                    ,'pb'
                    ,'psi'
                    ,'theta1'
                    ,'theta2'
),]
samplesPlot(mcmc.out6$samples[,c('pa'
                                 ,'pb'
                                 ,'psi'
                                 ,'theta1'
                                 ,'theta2'
)]
,traceplot=TRUE)

# theta1=0.4, theta2=0.4
sim7 <- makeHistory(N=1000, M=2000, k=10, pa=1, pb=1, theta1=0.4, theta2=0.4)
mcmc.out7 <- buildModel(sim7)
# MCMC result for one chain
mcmc.out7$summary[c('pa'
                    ,'pb'
                    ,'psi'
                    ,'theta1'
                    ,'theta2'
),]
samplesPlot(mcmc.out7$samples[,c('pa'
                                 ,'pb'
                                 ,'psi'
                                 ,'theta1'
                                 ,'theta2'
)]
,traceplot=TRUE)

# theta1=theta2=0.3
sim21 <- makeHistory2(N=1000, M=2000, k=10, pa=0.95, pb=0.85, theta1=0.3, theta2=0.3)
mcmc.out21 <- buildModel2(sim21)
# MCMC result for one chain
mcmc.out21$summary[c('pa'
                     ,'pb'
                     ,'psi'
                     ,'theta1'
                     ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out21$samples[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# theta1=0.4, theta2=0.6 (sum=1)
sim22 <- makeHistory2(N=1000, M=2000, k=5, pa=0.95, pb=0.85, theta1=0.4, theta2=0.6)
mcmc.out22 <- buildModel2(sim22)
# MCMC result for one chain
mcmc.out22$summary[c('pa'
                     ,'pb'
                     ,'psi'
                     ,'theta1'
                     ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out22$samples$chain1[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# low theta1=theta2=0.2, pa=1
sim23 <- makeHistory2(N=1000, M=2000, k=5, pa=1, pb=0.8, theta1=0.2, theta2=0.2)
mcmc.out23 <- buildModel2(sim23)
# MCMC result for one chain
mcmc.out23$summary[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out23$samples[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# high theta1=theta2=0.8
sim24 <- makeHistory2(N=1000, M=2000, k=5, pa=0.75, pb=0.85, theta1=0.8, theta2=0.8)
mcmc.out24 <- buildModel2(sim24)
# MCMC result for one chain
mcmc.out24$summary[c('pa'
                     ,'pb'
                     ,'psi'
                     ,'theta1'
                     ,'theta2'
),]
# plot posterior densities
samplesPlot(mcmc.out24$samples[,c('pa'
                                        ,'pb'
                                        ,'psi'
                                        ,'theta1'
                                        ,'theta2'
)]
,traceplot=TRUE)

# simple theta1=0.4, theta2=0.6, pb=1
sim25 <- makeHistory2(N=1000, M=2000, k=5, pa=.8, pb=1, theta1=0.4, theta2=0.6)
mcmc.out25 <- buildModel2(sim25)
# MCMC result for one chain
mcmc.out25$summary[c('pa'
                           ,'pb'
                           ,'psi'
                           ,'theta1'
                           ,'theta2'
),]

# simple theta1=0.3, theta2=0.6, k=5
sim26 <- makeHistory2(N=1000, M=2000, k=5, pa=1, pb=1, theta1=0.3, theta2=0.6)
mcmc.out26 <- buildModel2(sim26)
# MCMC result for one chain
mcmc.out26$summary[c('pa'
                    ,'pb'
                    ,'psi'
                    ,'theta1'
                    ,'theta2'
),]
samplesPlot(mcmc.out26$samples[,c('pa'
                                 ,'pb'
                                 ,'psi'
                                 ,'theta1'
                                 ,'theta2'
)]
,traceplot=TRUE)
