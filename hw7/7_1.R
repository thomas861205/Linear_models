gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw7/climate.txt", header=T)
plot(gala$year, gala$temp)

# ii
dyear <- diff(gala$year)
dyear[dyear > 1] # gap in the missing years
which(dyear > 1)
fala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw7/climate_fix.txt", header=T)


# iii backward elinimation
fit10 <- lm(temp ~ poly(year, 10), data=gala)
fit9 <- lm(temp ~ poly(year, 9), data=gala)
fit8 <- lm(temp ~ poly(year, 8), data=gala)
fit7 <- lm(temp ~ poly(year, 7), data=gala)
fit6 <- lm(temp ~ poly(year, 6), data=gala)
fit5 <- lm(temp ~ poly(year, 5), data=gala)

x <- c(1854:2020)
xx <- data.frame(year=x)
pred <- predict(fit5, xx)
plot(x, pred, type="l", xlab="year", ylab="temp", col=2)
points(gala$year, gala$temp, pch=18)



# iv piecewise-linear
g1 <- lm(temp ~ 1, data=gala, subset=(year<=1930))
g2 <- lm(temp ~ year, data=gala, subset=(year>1930))
plot(gala$year, gala$temp, xlab="year", ylab="temp")
segments(1854, g1$coef[1], 1930, g1$coef[1])
abline(v=1930, lty=5)
segments(2000, g2$coef[1]+g2$coef[2]*2000, 1930, g2$coef[1]+g2$coef[2]*1930)

# B-spline
library(splines)
knots <- c(1854, 1854, 1854, 1854, 1902.5, 1951.5, 2000, 2000, 2000, 2000)
year_spline <- splineDesign(knots, gala$year)
matplot(gala$year,year_spline,type="l",main="B-spline basis functions")

B_fit <- lm(gala$temp ~ year_spline)
summary(B_fit)

matplot(gala$year,cbind(gala$temp,B_fit$fit),type="pll",ylab="y",pch=18,lty=1,main="Spline fit")