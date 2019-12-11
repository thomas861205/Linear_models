gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw6/crime.txt", header=T)


name <- gala[,1]
violent <- gala[,2]
property <- gala[,3]
population <- gala[,4]

fit1 <- lm(property ~ population)
# summary(fit1)
plot(fit1$res, ylab="Residual", main="index plot of residuals")
identify(1:23,fit1$res, name)

x1 <- model.matrix(fit1)
lev1 <- hat(x1)
# sum(lev1)
plot(lev1, ylab="Leverages",main="index plot of leverages")
abline(h=2*2/23)
names(lev1) <- name
lev1[lev1 > 2*2/23]

cv1 <- qt(0.05/2, 23-2-1)
cvBF1 <- qt(0.05/(23*2), 23-2-1)
stud1 <- rstandard(fit1)
jack1 <- rstudent(fit1)
par(mfrow=c(1,2))
plot(stud1, ylab="Studentized residual", main="index plot of residuals");
abline(cv1, 0, lty=1);abline(-cv1, 0, lty=1);
abline(cvBF1, 0, lty=2);abline(-cvBF1, 0, lty=2);
legend(1, -2, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:23,stud1, name)

plot(jack1, ylab="Jacknife residual", main="index plot of residuals");
abline(cv1, 0, lty=1);abline(-cv1, 0, lty=1);
abline(cvBF1, 0, lty=2);abline(-cvBF1, 0, lty=2);
legend(1, -2, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:23,jack1, name)

# jack1[abs(jack1)==max(abs(jack1))] # -3.626886

par(mfrow=c(1,1))
cook1 <- cooks.distance(fit1)
plot(cook1, ylab="Cooks distance")
identify(1:23,cook1,name)

inf1 <- lm.influence(fit1)
# names(inf1)
# inf1
par(mfrow=c(1,2))
plot(inf1$coefficients[,2])
abline(0, 0)
identify(1:23, inf1$coefficients[,2], name)
plot(inf1$sigma)
identify(1:23, inf1$sigma, name)
# par(mfrow=c(1,1))










par(mfrow=c(1,1))
fit2 <- lm(violent ~ property)
# summary(fit2)
plot(fit2$res, ylab="Residual", main="index plot of residuals")
identify(1:23,fit2$res, name)

x2 <- model.matrix(fit2)
lev2 <- hat(x2)
sum(lev2)
plot(lev2, ylab="Leverages",main="index plot of leverages")
abline(h=2*2/23)
names(lev2) <- name
lev2[lev2 > 2*2/23]

cv2 <- qt(0.05/2, 23-2-1)
cvBF2 <- qt(0.05/(23*2), 23-2-1)
stud2 <- rstandard(fit2)
jack2 <- rstudent(fit2)
par(mfrow=c(1,2))
plot(stud2, ylab="Studentized residual", main="index plot of residuals");
abline(cv2, 0, lty=1);abline(-cv2, 0, lty=1);
abline(cvBF2, 0, lty=2);abline(-cvBF2, 0, lty=2);
legend(1, 3, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:23,stud2, name)

plot(jack2, ylab="Jacknife residual", main="index plot of residuals");
abline(cv2, 0, lty=1);abline(-cv2, 0, lty=1);
abline(cvBF2, 0, lty=2);abline(-cvBF2, 0, lty=2);
legend(1, 3, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:23,jack2, name)

# jack2[abs(jack2)==max(abs(jack2))] # -3.626886 

par(mfrow=c(1,1))
cook2 <- cooks.distance(fit2)
plot(cook2, ylab="Cooks distance")
identify(1:23,cook2,name)

inf2 <- lm.influence(fit2)
# names(inf2)
# inf2
par(mfrow=c(1,2))
plot(inf2$coefficients[,2])
abline(0, 0)
identify(1:23, inf2$coefficients[,2], name)

plot(inf2$sigma)
identify(1:23, inf2$sigma, name)
par(mfrow=c(1,1))









fit3 <- lm(violent ~ property + population)
# summary(fit3)
plot(fit3$res, ylab="Residual", main="index plot of residuals")
identify(1:23,fit3$res, name)

x3 <- model.matrix(fit3)
lev3 <- hat(x3)
# sum(lev3) # 3 
plot(lev3, ylab="Leverages",main="index plot of leverages")
abline(h=2*3/23)
identify(1:23, lev3, name)
names(lev3) <- name
lev3[lev3 > 2*3/23]

cv3 <- qt(0.05/2, 23-3-1)
cvBF3 <- qt(0.05/(23*2), 23-3-1)
stud3 <- rstandard(fit3)
jack3 <- rstudent(fit3)
par(mfrow=c(1,2))
plot(stud3, ylab="Studentized residual", main="index plot of residuals");
abline(cv3, 0, lty=1);abline(-cv3, 0, lty=1);
abline(cvBF3, 0, lty=2);abline(-cvBF3, 0, lty=2);
legend(1, -1.5, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:23,stud3, name)

plot(jack3, ylab="Jacknife residual", main="index plot of residuals");
abline(cv3, 0, lty=1);abline(-cv3, 0, lty=1);
abline(cvBF3, 0, lty=2);abline(-cvBF3, 0, lty=2);
legend(1, -1.3, legend=c("raw critical value", "Bonferroni critical value"), lty=1:2, cex=0.8)
identify(1:23,jack3, name)

# jack3[abs(jack3)==max(abs(jack3))] # -3.626886

par(mfrow=c(1,1))
cook3 <- cooks.distance(fit3)
plot(cook3, ylab="Cooks distance")
identify(1:23,cook3,name)

inf3 <- lm.influence(fit3)
# names(inf3)
# inf3
par(mfrow=c(1,2))

plot(inf3$coef[,2],inf3$coef[,3],xlab="change in property coef", ylab="change in population coef")
identify(inf3$coef[,2],inf3$coef[,3],name) 
# plot(inf3$coefficients[,2]) # property
# abline(0, 0)
# identify(1:23, inf3$coefficients[,2], name)

# plot(inf3$coefficients[,3])
# abline(0, 0)
# identify(1:23, inf3$coefficients[,3], name) # population

plot(inf3$sigma)
identify(1:23, inf3$sigma, name)
# par(mfrow=c(1,1))


