gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw7/soil.txt", header=T)
# outlier
# 44 Meeker  31  1047    1548
gala <- gala[-44,]
county <- gala[,1]
P <- gala[,2]
y1981 <- gala[,3]
y1982 <- gala[,4]


fit_82 <- lm(y1982 ~ P)

r1 <- lm(rate ~ 1, subset=(y1982 < 1500 & rate < 0.12))
r2 <- lm(rate ~ 1, subset=(y1982 >= 1500 & y1982 < 1800 & rate >= 0.12 & rate < 0.18))
r3 <- lm(rate ~ 1, subset=(y1982 >= 1800 & rate >= 0.18))
segments(800, r1$coef[1], 1500, r1$coef[1])
abline(v=1500, lty=5)
segments(1500, r2$coef[1], 1800, r2$coef[1])
abline(v=1800, lty=5)
segments(1800, r3$coef[1], 2200, r3$coef[1])