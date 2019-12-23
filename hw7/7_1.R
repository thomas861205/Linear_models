gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw7/climate.txt", header=T)

fit1 <- lm(temp ~ year, data=gala)
fit10 <- lm(temp ~ poly(year, 10), data=gala)



# B-spline
library(splines)
knots <- c(1854, 1854, 1854, 1854, 1902.5, 1951.5, 2000, 2000, 2000, 2000)
year_spline <- splineDesign(knots,gala$year)
matplot(gala$year,year_spline,type="l",main="B-spline basis functions")
B_fit <- lm(gala$temp ~ year_spline)
summary(B_fit)

matplot(gala$year,cbind(gala$temp,B_fit$fit),type="pll",ylab="y",pch=18,lty=1,main="Spline fit")