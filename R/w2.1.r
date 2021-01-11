smoking <- read.table("tobacco.txt",header=T,sep="\t")
model <- lm(ILL~CONSUMPTION,data=smoking)
countries <- c("Iceland","Norway","Sweden","Canada","Denmark","Austria","USA","Netherlands","Switzerland","Finland","England")

plot(smoking$CONSUMPTION,smoking$ILL, ylab="Cases in 1950",
xlab="CONSUMPTION in 1930", pch=16,
main="CONSUMPTION/ILL per 100 000 individuals")
abline(model,col="red")
text(smoking$CONSUMPTION, smoking$ILL, labels=countries, cex= 0.8, pos=3)

FIT <- model$fit
RES <- model$res

plot(smoking$ILL,FIT, ylab="Fits",xlab="Sick",pch=16)
text(smoking$ILL,FIT,  labels = ifelse(rownames(smoking)=="7", countries, NA),pos=2)
plot(FIT,RES, xlab="Fits", ylab="Residuals", pch=16)
text(FIT,RES,  labels = ifelse(rownames(smoking) == "7", countries, NA),pos=3)

cooksd <- cooks.distance(model)
x <-plot(cooksd,xaxt="n",xlab=" ",ylab="Cook’s distances")
axis(side=1,at=1:11, labels=countries,las=2 )
