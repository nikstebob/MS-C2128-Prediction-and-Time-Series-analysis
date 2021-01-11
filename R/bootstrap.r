emis = read.table("emissions.txt", header=T, sep="\t")
set.seed(123)

hist(emis[,2])
cor(emis)
fit1 = lm(NOx~Humidity+Temp+Pressure, data=emis)
summary(fit1)

tmp <- as.matrix(emis[,c(3,4)])
n <- nrow(emis)
Intercept <- rep(1,n)
emis2 <- cbind(Intercept,tmp)
p <- ncol(emis2)

res <- fit1$res
s2 <- sum(res^2)/(n-p)
stdev <- sqrt(diag( s2*solve(t(emis2)%*%emis2)))

confinterval <- confint(fit1, level=0.95) # g)

coef <- fit1$coef
up <- coef + stdev * qt(0.975,n-p)
down <- coef - stdev * qt(0.975,n-p)


# bootstrapping

k <- 2000
bootmat <- matrix(NA,nrow=k,ncol=3)
y <- emis$NOx
X <- emis2
set.seed(123)
for(i in 1:(k-1)){
  ind <- sample(1:n,replace = TRUE)
  Xtmp <- X[ind,]
  ytmp <- y[ind]
  btmp <- solve(t(Xtmp)%*%Xtmp)%*%t(Xtmp)%*%ytmp
  bootmat[i,] <- t(btmp)
}
boriginal <- solve(t(X)%*%X)%*%t(X)%*%y
bootmat[k,] <- t(boriginal)
boot1 <- sort(bootmat[,1])
boot2 <- sort(bootmat[,2])
boot3 <- sort(bootmat[,3])

qconst <- quantile(boot1, probs = c(0.025,0.975))
qhum <- quantile(boot2, probs = c(0.025,0.975))
qtemp <- quantile(boot3, probs = c(0.025,0.975))

hist(bootmat[,1])
hist(bootmat[,2])
hist(bootmat[,3])


cor.test(emis$NOx, emis$Pressure)
cor.test(emis$NOx, emis$Temp)
cor.test(emis$NOx, emis$Humidity)
 emis2
