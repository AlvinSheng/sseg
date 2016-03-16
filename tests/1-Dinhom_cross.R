# compare Dinhom_cross to spatstat (in homog. case of course)
library(spatstat)
library(devtools)
load_all(".")

# test data
if(!exists("l1")){
  #source("tests/load-data.R")
  x <- lansing
  sx <- split(x)
  intensity <- lapply(sx, intensity)
  # constant intensities
  l0 <- rep(intensity(x), table(x$marks))
  # construct separate intensities
  l1 <- intensity_optimal(x, bw_vector=0.1, verb = TRUE)
  # fields
  ims <- lapply(1:length(sx), function(i) density(sx[[i]], l1$bandwidth[i]))
}

r <- seq(0, 10, length=150)
i<-1
j <- 3

# check inputs
g <- Dinhom_cross(x, intensity=l1$int, i=i, j=j)
g2 <- Dinhom_cross(x, intensity=l1$int, i=i, j=j+1)
par(mfrow=c(2,2))
plot(ims[[i]]); points(sx[[i]])
plot(ims[[j]]); points(sx[[j]])
plot(g)
lines(g2)

# comparison to spatstat
if(0){
t0 <- system.time( g0 <- Gcross(x, r=r, correction = "rs", i = i, j=j) )
t1 <- system.time( g1 <- Dinhom_cross(x, r=r, intensity = l0, i = i, j=j, v2=F) )
t2 <- system.time( g2 <- Dinhom_cross(x, r=r, intensity = l0, i = i, j=j, v2=T) )
t3 <- system.time( g3 <- Dinhom_cross(x, r=r, intensity = l1$int, i=i, j=j)  )

print(rbind(t0,t1,t2,t3))

plot(r, g1$Dinhom, lty=1, col=4, "l", lwd=1)
lines(r, g0$rs, lty=2, col=2)
lines(r, g2$Dinhom, lty=2, col=3, lwd=2)
lines(r, g3$Dinhom, lty=2, col=3, lwd=3)
}
