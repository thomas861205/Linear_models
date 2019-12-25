gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw7/car.txt", header=T)

AO <- gala[,2]
GNP <- gala[,3]
CP <- gala[,4]
OP <- gala[,5]

DAO <- diff(AO)
DGNP <- diff(GNP)
DCP <- diff(CP)
DOP <- diff(OP)

AO <- AO[1:length(AO)-1]
GNP <- GNP[1:length(GNP)-1]
CP <- CP[1:length(CP)-1]
OP <- OP[1:length(OP)-1]

# DGNP, DCP and DOP as the independent variables
g <- lm(DAO ~ DGNP + DCP + DOP)
# coefficients(g)
 #  (Intercept)          DGNP           DCP           DOP 
 # 1.264734e-04  8.097440e-06 -6.066330e-07 -2.772627e-06 
summary(g)

library(MASS)
# boxcox(g, plotit=T)
boxcox(g, plotit=T, lambda=seq(-1,1,by=0.1))

log_DAO <- log(DAO)
g2 <- lm(log_DAO ~ DGNP + DCP + DOP)
summary(g2)

opt_DAO <- (DAO^(-0.25)-1)/-0.25
g3 <- lm(opt_DAO ~ DGNP + DCP + DOP)
summary(g3)