gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw6/acc.txt", header=T)
# ACC: acceleration of different vehicles.
# WHP: weight-to-horsepower ratio.
# SP: the speed at which they were traveling. 
# G: the grade, G=0 implies the road was horizontal.

ACC <- gala[,1]
WHP <- gala[,2]
SP <- gala[,3]
G <- gala[,4]


fit1 <- lm(ACC ~ WHP + SP + G, data=gala)
prplot <- function(g,i)
{
# Partial residuals plot for predictor i
  xl<-attributes(g$terms)$term.labels[i]
  yl<-paste("beta*",xl,"+res",sep="")
  x<-model.matrix(g)[,i+1]
  plot(x,g$coeff[i+1]*x+g$res,xlab=xl,ylab=yl)
  abline(0,g$coeff[i+1])
  invisible()
}

prplot(fit1, 1)
prplot(fit1, 2)
prplot(fit1, 3)
plot(fit1$fit, fit1$residuals)
sum(fit1$residuals^2)

ACCsq <- ACC^2
ACCrec <- 1/ACC
ACCrecsqrt <- ACCrec^0.5
WHPsq <- WHP^2
WHPrec <- 1/WHP
SPadj <- SP^(2/3)

fit2 <- lm(ACC ~ WHP + WHPsq + SP + G) # error^2: 37

# fit3 <- lm(ACCrec ~ WHP + SP + G)

# fit4 <- lm(ACC ~ WHPrec + SP + G)

# fit5 <- lm(ACC ~ WHP + SP + SPadj + G)

fit3 <- lm(ACCrec ~ WHP + WHPsq + SP + G) # error^2: 31


# fit4 <- lm(ACCsq ~ WHP + WHPsq + SP + G) # trash
# fit4 <- lm(ACCrecsq ~ WHP + WHPsq + SP + G) # trash
fitop <- lm(ACCrecsqrt ~ WHP + WHPsq + SP + G) # error^2: 2.930332
prplot(fitop, 1)
prplot(fitop, 2)
prplot(fitop, 3)
prplot(fitop, 4)
plot(fitop$fit, fitop$residuals)