# CONSUMPTION = column 5
# ILL = column 14
tob = read.table("tobacco.txt", header=T, sep="\t")
set.seed(123)

fit = lm(tob[,14]~ tob[,5], data=tob)
summary(fit)

plot(tob[,5], tob[,14], xlab="CONSUMPTION", ylab="ILL")
abline(fit)
