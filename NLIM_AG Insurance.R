###############   AG INSURANCE    ###############

library(ChainLadder) 

n = 10 # Number of years

Claims <- data.frame(originf = factor(rep(2010:2019, n:1)), 
                     dev = sequence(n:1),
                     inc.paid = c(889.6, 530.9, 117.2,	89.6,	50.6,	50.5,	17.7,	15.9,	10.3,	9.3,
                                  887.7, 516.8,	121.5,	88.7,	61.4,	30.6,	21.4,	9.9,	8.7,	
                                  867.9, 518.4,	133.9,	81.7,	59.1,	40.3,	18.0,	11.3,		
                                  860.1, 518.6,	130.1,	74.8,	54.4,	27.8,	25.9,			
                                  861.3, 508.4,	114.4,	89.4,	50.3,	46.4,				
                                  858.2, 499.7,	119.4,	71.2,	55.9,					
                                  857.3, 489.9,	134.1,	87.4,						
                                  848.9, 512.8, 122,							
                                  826.6, 603.9,								
                                  775.4)) # Run-Off Triangle 

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

