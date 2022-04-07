###############   BALOISE    ###############

library(ChainLadder) 

n = 10 # Number of years

Claims <- data.frame(originf = factor(rep(2010:2019, n:1)), 
                     dev = sequence(n:1),
                     inc.paid = c(682.3, 684.0, 592.3, 526.6, 511.4, 499.8, 493.3, 404.1, 444.4, 395.2, 
                                  643.8, 580.8, 527.2, 493.3, 491.9, 486.4, 395.2, 431.5, 384.7, 	
                                  495.0, 483.9, 470.5, 478.9, 479.1, 388.2, 417.5, 374.4,		
                                  446.8, 478.9, 480.7, 483.4, 394.4, 416.9, 387.1, 			
                                  470.3, 476.0, 488.7, 396.7, 410.7, 388.6,				
                                  459.9, 494.3, 398.0, 412.9, 392.5,					
                                  483.7, 402.5, 421.9, 387.9, 						
                                  403.6, 426.5, 392.2,							
                                  412.4, 395.1, 								
                                  308.7)) # Run-Off Triangle 

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

