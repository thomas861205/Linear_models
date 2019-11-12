gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw4/E3.7.txt", header=T)

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

fit <- lm(y ~ x1 + x2 + x3 + x4 + x5)
summary(fit)

# Call:
# lm(formula = y ~ x1 + x2 + x3 + x4 + x5)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.39447 -0.11847  0.00053  0.08313  0.56232 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -2.156e+00  9.135e-01  -2.360   0.0333 *
# x1          -9.012e-06  5.184e-04  -0.017   0.9864  
# x2           1.316e-03  1.263e-03   1.041   0.3153  
# x3           1.278e-04  7.690e-05   1.662   0.1188  
# x4           7.899e-03  1.400e-02   0.564   0.5815  
# x5           1.417e-04  7.375e-05   1.921   0.0754 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2618 on 14 degrees of freedom
# Multiple R-squared:  0.8107,	Adjusted R-squared:  0.743 
# F-statistic: 11.99 on 5 and 14 DF,  p-value: 0.0001184

# qt : inverse of CDF of Student-t distribution
cv <- qt(0.975, fit$df) # the 95% critical value, dfW=n-p=14
# [1] 2.144787
# equivalent to using
# qt(c(.025, .975), 14)
# [1] -2.144787  2.144787



# (a) find a 95% C.I. for β1.
c(-9.012e-06 - cv * 5.184e-04, -9.012e-06 +  cv * 5.184e-04) # confidence interval = estimate ± (critical value)*(s.e. of estimate). 
# [1] -0.001120869  0.001102845
# Q:	The interval contains zero, what does it mean?
# A:	Notice that CIs have a duality with 2-sided hypothesis tests. Because the interval contains zero,
# 		it indicates that the null hypothesis H0: β1=0 would not be rejected at the 5% significance level.
# 		we can see from the summary of 'fit' that the p-value is 0.9864 --- greater than 5% --- confirming this point. 

# The following is a convenient way to obtain all the uni-variate intervals:
confint(fit) # the "confint" command computes confidence intervals for parameters in a fitted model
#                     2.5 %        97.5 %
# (Intercept) -4.115384e+00 -0.1969076588
# x1          -1.120765e-03  0.0011027419
# x2          -1.394016e-03  0.0040257880
# x3          -3.713929e-05  0.0002927368
# x4          -2.212779e-02  0.0379255006
# x5          -1.652198e-05  0.0002998305




# (b) find a 95% C.I. for β3 + 2β5.
A <- t(c(0, 0, 0, 1, 0, 2))
y0 <- sum(A * fit$coef)
# 0.0004111073
x <- model.matrix(fit) # the model matrix X
xtxi <- solve(t(x)%*%x) # (XTX)-1, %*%: inner product, *: element-wise product
bm <- sqrt(A%*%xtxi%*%t(A)) * summary(fit)$sigma 
bm
# 0.0001641751
cv <- qt(0.975, fit$df)
c(y0 - cv * bm, y0  +  cv * bm)
# 5.898666e-05 7.632279e-04