gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw6/E3.7.txt", header=T)

# Variable meaning
# y   log (oxygen demand, mg oxygen per minute)
# x1   biological oxygen demand, mg/liter
# x2   total Kjeldahl nitrogen, mg/liter
# x3   total solids, mg/liter
# x4   total volatile solids, a component of x4, mg/liter
# x5   chemical oxygen demand, mg/liter

y <- gala[,7]
x1 <- gala[,2]
x2 <- gala[,3]
x3 <- gala[,4]
x4 <- gala[,5]
x5 <- gala[,6]
name <- c(1:20)

# (a)

fit1 <- lm(y ~ x1 + x2 + x3 + x4 + x5)
# summary(fit1)

# par(mfrow=c(2,2))  # split the graphic window into 2x2 subwindows

# draw the normal probability plot for raw residuals
qqnorm(fit1$res, ylab="raw residuals")
# add a straight line to guide the examination
qqline(fit1$res)
hist(fit1$residuals, breaks=20)





plot(fit1$res, ylab="Residual", main="index plot of residuals")
identify(1:20,fit1$res, name)

x1 <- model.matrix(fit1)
lev1 <- hat(x1)
# sum(lev1)
plot(lev1, ylab="Leverages",main="index plot of leverages")
abline(h=2*6/20)
identify(1:20,fit1$lev1, name)
names(lev1) <- name
lev1[lev1 > 2*6/20]

cv1 <- qt(0.05/2, 20-6-1)
cvBF1 <- qt(0.05/(20*2), 20-6-1)
stud1 <- rstandard(fit1)
jack1 <- rstudent(fit1)
par(mfrow=c(1,2))
plot(stud1, ylab="Studentized residual", main="index plot of residuals");
abline(cv1, 0, lty=1);abline(-cv1, 0, lty=1);
abline(cvBF1, 0, lty=2);abline(-cvBF1, 0, lty=2);
legend(1, 2, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:20,stud1, name)

plot(jack1, ylab="Jacknife residual", main="index plot of residuals");
abline(cv1, 0, lty=1);abline(-cv1, 0, lty=1);
abline(cvBF1, 0, lty=2);abline(-cvBF1, 0, lty=2);
legend(1, 3, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:20,jack1, name)

# jack1[abs(jack1)==max(abs(jack1))] # -3.626886

par(mfrow=c(1,1))
cook1 <- cooks.distance(fit1)
plot(cook1, ylab="Cooks distance")
identify(1:20,cook1,name)

inf1 <- lm.influence(fit1)
# names(inf1)
# inf1
par(mfrow=c(1,2))
plot(inf1$coefficients[,2]) # too small
abline(0, 0)
identify(1:20, inf1$coefficients[,2], name)

plot(inf1$coefficients[,3]) # ditto
abline(0, 0)
identify(1:20, inf1$coefficients[,3], name)

plot(inf1$coefficients[,4]) # ditto
abline(0, 0)
identify(1:20, inf1$coefficients[,4], name)

plot(inf1$coefficients[,5])
abline(0, 0)
identify(1:20, inf1$coefficients[,5], name)

plot(inf1$coefficients[,6])
abline(0, 0)
identify(1:20, inf1$coefficients[,6], name)

plot(inf1$sigma)
identify(1:20, inf1$sigma, name)
# par(mfrow=c(1,1))
























# (b)

fit2 <- lm(y ~ x3 + x5)
# summary(fit2)
qqnorm(fit2$res, ylab="raw residuals")
qqline(fit2$res)
hist(fit2$residuals, breaks=20)


plot(fit2$res, ylab="Residual", main="index plot of residuals")
# identify(1:20,fit2$res, name)

x2 <- model.matrix(fit2)
lev2 <- hat(x2)
sum(lev2)
plot(lev2, ylab="Leverages",main="index plot of leverages")
abline(h=2*3/20)
identify(1:20,lev2, name)
names(lev2) <- name
lev2[lev2 > 2*3/20]

cv2 <- qt(0.05/2, 20-3-1)
cvBF2 <- qt(0.05/(20*2), 20-3-1)
stud2 <- rstandard(fit2)
jack2 <- rstudent(fit2)
par(mfrow=c(1,2))
plot(stud2, ylab="Studentized residual", main="index plot of residuals");
abline(cv2, 0, lty=1);abline(-cv2, 0, lty=1);
abline(cvBF2, 0, lty=2);abline(-cvBF2, 0, lty=2);
legend(1, 2.5, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:20,stud2, name)

plot(jack2, ylab="Jacknife residual", main="index plot of residuals");
abline(cv2, 0, lty=1);abline(-cv2, 0, lty=1);
abline(cvBF2, 0, lty=2);abline(-cvBF2, 0, lty=2);
legend(1, 3, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:20,jack2, name)

# jack2[abs(jack2)==max(abs(jack2))] # -3.626886 

par(mfrow=c(1,1))
cook2 <- cooks.distance(fit2)
plot(cook2, ylab="Cooks distance")
identify(1:20,cook2,name)

inf2 <- lm.influence(fit2)
# names(inf2)
# inf2
par(mfrow=c(1,2))
plot(inf2$coefficients[,2])
abline(0, 0)
identify(1:20, inf2$coefficients[,2], name)

plot(inf2$coefficients[,3])
abline(0, 0)
identify(1:20, inf2$coefficients[,3], name)

par(mfrow=c(1,1))
plot(inf2$sigma)
identify(1:20, inf2$sigma, name)
par(mfrow=c(1,1))