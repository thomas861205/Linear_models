gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw5/E6.10.txt", header=T)

# The variable x gives travel times which were computed from bus timetables augmented 
# by walk times from zone centroids to bus-stops (assuming a walking speed of 3 m.p.h.) 
# and expected waiting times for the bus (which were set at half the headway, i.e., the 
# time between successive buses).
# 
# The variable y was the average of travel times as reported to the U.S. Census Bureau by n travelers.

plot(gala$x, gala$y)

n <- gala[,2]
x <- gala[,3]
y <- gala[,4]

fit1 <- lm(y ~ x, weights=n)
summary(fit1)
# plot(x, y)
# abline(fit1)

fit2 <- lm(y ~ x)
summary(fit2)

plot(n, fit1$residuals, pch=1)
points(n, fit2$residuals, pch=4)
legend("bottomleft", legend = c("WLS", "OLS"), pch = c(1, 4), lty = c(1, 1))
abline(a=0, b=0)