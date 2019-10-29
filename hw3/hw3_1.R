gala <- read.table("C:/Users/Thomas/Downloads/Linear model/hw3/E3.7.txt", header=T)

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

# (a)

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


# (b)

fit2 <- lm(y ~ x3 + x5)
summary(fit2)

# Call:
# lm(formula = y ~ x3 + x5)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.37768 -0.09357 -0.04241  0.06230  0.59623 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.371e+00  1.963e-01  -6.988 2.19e-06 ***
# x3           1.492e-04  5.473e-05   2.726   0.0144 *  
# x5           1.419e-04  5.302e-05   2.676   0.0160 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2519 on 17 degrees of freedom
# Multiple R-squared:  0.7872,	Adjusted R-squared:  0.7621 
# F-statistic: 31.44 on 2 and 17 DF,  p-value: 1.942e-06


# (c)
# Test H_0: beta_1 = beta_2 = beta_4 = 0
# Omega: fit  = beta_x0 + beta_x1 * x1 + beta_2 * x2 + beta_x3 * x3 + beta_x4 * x4 + beta_x5 * x5
# omega: fit2 = beta_x0                              + beta_x3 * x3                + beta_x5 * x5
# Method 1.
ip <- function(x,y) sum(x*y) # define 'inner product'
RSS_O <- ip(fit$res, fit$res) # RSS of Omega
RSS_o <- ip(fit2$res, fit2$res) # RSS of omega
df_O <- fit$df.residual # n - p = 20 - 6 = 14
df_o <- fit2$df.residual # n - q = 20 - 3 = 17
F_stat <- ((RSS_o - RSS_O) / (df_o - df_O)) / (RSS_O / df_O)
p_val <- 1 - pf(F_stat, df_o - df_O, df_O) # the p-value of the overall F-test
# > F_stat
# [1] 0.5790073
# > p_val
# [1] 0.6383458


# Method 2.
# anova(fit2, fit)
# Analysis of Variance Table

# Model 1: y ~ x3 + x5
# Model 2: y ~ x1 + x2 + x3 + x4 + x5
#   Res.Df     RSS Df Sum of Sq     F Pr(>F)
# 1     17 1.07859                          
# 2     14 0.95953  3   0.11905 0.579 0.6383 -> Same result

sim <- function(x,y) sum(x*y) / sqrt(sum(x*x)) / sqrt(sum(y*y))