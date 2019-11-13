gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw4/set.txt", header=T)

# PRICE:	selling price of house in thousands of dollars
# BDR:	number of bedrooms
# FLR:	floor space in sq. ft. (computed from dimensions of each room and then augmented by 10%)
# FP:	number of fireplaces
# RMS:	number of rooms
# ST:	storm windows (1 if present, 0 if absent)
# LOT:	front footage of lot in feet
# TAX:	annual taxes
# BTH:	number of bathrooms
# CON:	construction (0 if frame, 1 if brick)
# GAR:	garage size (0=no garage, 1=one-car garage, etc.)
# CDN:	condition (1="needs work", 0 otherwise)
# L1:	location (L1=1 if property is in zone A, L1=0 otherwise)
# L2:	location (L2=1 if property is in zone B, L2=0 otherwise)

PRICE <- gala[,1]
BDR <- gala[,2]
FLR <- gala[,3]
FP  <- gala[,4]
RMS <- gala[,5]
ST  <- gala[,6]
LOT <- gala[,7]
TAX <- gala[,8]
BTH <- gala[,9]
CON <- gala[,10]
GAR <- gala[,11]
CDN <- gala[,12]
L1  <- gala[,13]
L2  <- gala[,14]

# fit <- lm(PRICE ~ BDR + FLR + FP + RMS + ST + LOT + TAX + BTH + CON + GAR + CDN + L1 + L2)
fit <- lm(PRICE ~ BDR + FLR + FP + RMS + ST + LOT + BTH + GAR)

summary(fit)

# Call:
# lm(formula = PRICE ~ BDR + FLR + FP + RMS + ST + LOT + BTH + 
#     GAR)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -10.3058  -2.8417  -0.1511   3.2882   7.9518 

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 18.637664   5.240957   3.556 0.002429 ** 
# BDR         -7.697444   1.829426  -4.208 0.000592 ***
# FLR          0.017570   0.003235   5.431 4.49e-05 ***
# FP           6.909765   3.083583   2.241 0.038680 *  
# RMS          3.904374   1.615617   2.417 0.027194 *  
# ST          10.818663   2.300203   4.703 0.000205 ***
# LOT          0.263522   0.135109   1.950 0.067808 .  
# BTH          2.374591   2.557865   0.928 0.366221    
# GAR          1.770861   1.404310   1.261 0.224334    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 4.717 on 17 degrees of freedom
# Multiple R-squared:  0.9044,	Adjusted R-squared:  0.8595 
# F-statistic: 20.11 on 8 and 17 DF,  p-value: 3.147e-07

# ----- New Prediction -----
# intercept
# 2 bedrooms
# 750 square feet of space
# 1 fireplace 
# 5 rooms
# a 25 front-foot lot
# storm windows
# 1.5 baths
# a 1-car garage

# Method 1.
x0 <- c(1, 2, 750, 1, 5, 1, 25, 1.5, 1)
y0 <- sum(x0 * fit$coef)
# [1] 65.59152
cv <- qt(0.975, fit$df)
x <- model.matrix(fit) # the model matrix X
xtxi <- solve(t(x)%*%x) # (XTX)-1, %*%: inner product, *: element-wise product
bm <- sqrt(t(x0)%*%xtxi%*%x0) * summary(fit)$sigma  # this is the standard error of the estimate of mean response.
c(y0 - cv * bm, y0 + cv * bm)
# 57.69899 73.48406

# Method 2.
predict(fit, data.frame(BDR=2, FLR=750, FP=1, RMS=5, ST=1, LOT=25, BTH=1.5, GAR=1), se=T, interval="confidence")
# $fit
#        fit      lwr      upr
# 1 65.59152 57.69899 73.48406

# $se.fit
# [1] 3.740865

# $df
# [1] 17

# $residual.scale
# [1] 4.716756


predict(fit, data.frame(BDR=2, FLR=750, FP=1, RMS=5, ST=1, LOT=25, BTH=1.5, GAR=1), se=T, interval="prediction")
# $fit
#        fit      lwr      upr
# 1 65.59152 57.69899 73.48406

# $se.fit
# [1] 3.740865

# $df
# [1] 17

# $residual.scale
# [1] 4.716756