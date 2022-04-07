###############   ETHIAS    ###############

library(ChainLadder) 

n = 10 # Number of years

Claims <- data.frame(originf = factor(rep(2009:2018, n:1)), 
                     dev = sequence(n:1),
                     inc.paid = c(1161121, 1125654, 1091957, 1058073, 1020267, 937696, 843480, 719725, 588080, 381185,
                                  1174899, 1134537, 1091823, 1051063, 1006067, 898096, 775651, 604239, 408841,
                                  1148312, 1100098, 1044866, 997535, 942122, 804683, 630961, 395553,
                                  1106182, 1045508, 977346, 902025, 829149, 652680, 424653,
                                  960814, 891250, 809244, 715510, 621242, 397164,
                                  828395, 747095, 652016, 529082, 392400,
                                  687415, 590706, 483331, 317918,
                                  583283, 466909, 338794,
                                  466862, 310154,
                                  365399)) # Run-Off Triangle 

inc.triangle <- with(Claims, {
  M <- matrix(nrow=n, ncol=n,
  dimnames=list(origin=levels(originf), dev=1:n))
  M[cbind(originf, dev)] <- inc.paid
  M
}) # This operation allows us to calculate the cumulative triangle


cum.triangle <- incr2cum(inc.triangle) # this function converts from incremental to cumulative triangle
cum.triangle

latest.paid <- cum.triangle[row(cum.triangle) == n - col(cum.triangle) + 1]
Claims$cum.paid <- cum.triangle[with(Claims, cbind(originf, dev))]

### Plot of Incremental and Cumulative Claims Development
op <- par(fig=c(0,0.5,0,1), cex=0.8, oma=c(0,0,0,0))

with(Claims, {
  interaction.plot(x.factor=dev, trace.factor=originf, response=inc.paid,
                   fun=sum, type="b", bty='n', legend=FALSE); axis(1, at=1:n)
  par(fig=c(0.45,1,0,1), new=TRUE, cex=0.8, oma=c(0,0,0,0))
  interaction.plot(x.factor=dev, trace.factor=originf, response=cum.paid,
                   fun=sum, type="b", bty='n'); axis(1,at=1:n)
})

mtext("Incremental and cumulative claims development",
      side=3, outer=TRUE, line=-3, cex = 1.1, font=2)

par(op)

### Plot of Cumulative Claims Development
library(lattice)
xyplot(cum.paid ~ dev | originf, data=Claims, t="b", layout=c(5,5),
       as.table=TRUE, main="Cumulative claims development")


# # # Mack Method # # #
mack <- MackChainLadder(cum.triangle, est.sigma="Mack")
mack
mack$f
mack$FullTriangle
plot(mack)                      # General Mack Chian Ladder Results 
plot(mack, lattice=TRUE)        # Analysis of Standard Error  
MackChainLadder(cum.triangle, weights=1, est.sigma = "Mack")

# # # Bootstrap Method # # #
Boot <- BootChainLadder(cum.triangle, R=1000, process.distr="od.pois")
Boot
plot(Boot)                     # General Bootstrap Chian - Ladder Results 
quantile(Boot, c(0.75,0.95,0.99, 0.995))
library(MASS)
plot(ecdf(Boot$IBNR.Totals))   # Empirical Cumulative Distribution Function
fit <- fitdistr(Boot$IBNR.Totals[Boot$IBNR.Totals>0], "lognormal")
fit                 
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]),
      col="red", add=TRUE) 

