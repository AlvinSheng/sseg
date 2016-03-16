# compare Dinhom to spatstat
# uses graph now

library(devtools)
load_all(".")


library(spatstat)
x <- rThomas(20, 0.04, 50)
lambda <- intensity_optimal(x, seq(0.01, 1, length=10), verb=T)$intensity

t0<-system.time(g0 <- Ginhom(x, lambda = lambda))
t1<-system.time(g1 <- Dinhom(x, g0$r, lambda))
t2 <- system.time(g2 <- Dinhom_cross(setmarks(x,factor(1)), g0$r, lambda, i=1,j=1)) # graph version

print(rbind(t0,t1,t2))

plot(g1$r, g1$Dinhom, lty=3, col=6, lwd=3, "l")
lines(g0$r, g0$bord, col=1, lty=2)
lines(g2$r, g2$Dinhom, lty=3, col=7, lwd=3)

print(all.equal(g2$Dinhom, g1$Dinhom))
