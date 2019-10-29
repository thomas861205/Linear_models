# Ref
# https://www.guru99.com/r-simple-multiple-linear-regression.html
# https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/
# https://stackoverflow.com/questions/24192428/what-does-the-capital-letter-i-in-r-linear-regression-formula-mean/24192745#24192745
# https://www.econometrics-with-r.org/8-3-interactions-between-independent-variables.html


library(dplyr) # import %>% and select()

gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw2/2_1.txt", header=T)
# Output: per capita output in Chinese yuan
# SI: number (SI) of workers in the factory
# SP: land area (SP) of the factory in square meters per worker
# I: investment (I) in yuans perworker


# i.
# Method 1.
# X <- select(gala, -Output) %>% as.matrix()
# fit <- lm(gala$Output~X)

# Method 2.
fit1 <- lm(Output ~ SI + SP + I, gala)

# print(summary(fit1))
# Call:
# lm(formula = model, data = gala)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6638.7 -3578.0  -558.5  4011.6  9637.4 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 6026.061   5245.659   1.149   0.2713  
# SI             1.742      3.777   0.461   0.6523  
# SP             5.302      2.188   2.423   0.0307 *
# I           -255.506    333.194  -0.767   0.4569  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 5262 on 13 degrees of freedom
# Multiple R-squared:  0.4174,	Adjusted R-squared:  0.2829 
# F-statistic: 3.104 on 3 and 13 DF,  p-value: 0.06371



# ii.
# Method 1.
aug_gala <- gala
aug_gala["SIxSI"] <- gala$SI^2
aug_gala["SPxI"] <- gala$SI*gala$I
fita <- lm(Output ~ SI + SP + I + SIxSI + SPxI, aug_gala)
coefficients(fita)
# Call:
# lm(formula = aug_gala$Output ~ aug_X)

# # Residuals:
# #     Min      1Q  Median      3Q     Max 
# # -6151.2 -2764.6  -885.6  3432.9  7522.1 

# # Coefficients:
# #               Estimate Std. Error t value Pr(>|t|)
# # (Intercept) -4.582e+03  1.363e+04  -0.336    0.743
# # aug_X1       2.335e+01  1.396e+01   1.673    0.123
# # aug_X2       2.579e+00  2.717e+00   0.949    0.363
# # aug_X3       8.245e+02  1.346e+03   0.613    0.553
# # aug_X4      -4.202e-03  9.980e-03  -0.421    0.682
# # aug_X5      -8.625e-01  1.092e+00  -0.790    0.446

# # Residual standard error: 5109 on 11 degrees of freedom
# # Multiple R-squared:  0.5352,	Adjusted R-squared:  0.3239 
# # F-statistic: 2.533 on 5 and 11 DF,  p-value: 0.09239

# Method 2.
# Note that the SP * I term is shorthand for SP + I + SP:I in our model.
# I() to isolate
fit2 <- lm(Output ~ SI + SP + I + I(SI^2) + SP:I, gala)
coefficients(fit2)
# summary(fit2)
# Call:
# lm(formula = Output ~ SI + SP + I + I(SI^2) + SP:I, data = gala)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -4821.1 -1766.4  -316.4  1032.9  5638.4 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  5.240e+04  1.354e+04   3.869  0.00261 **
# SI           3.513e+01  1.029e+01   3.414  0.00579 **
# SP          -1.372e+01  4.978e+00  -2.755  0.01871 * 
# I           -3.716e+03  1.001e+03  -3.710  0.00344 **
# I(SI^2)     -1.454e-02  4.894e-03  -2.971  0.01273 * 
# SP:I         1.022e+00  2.841e-01   3.597  0.00419 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 3560 on 11 degrees of freedom
# Multiple R-squared:  0.7743,	Adjusted R-squared:  0.6717 
# F-statistic: 7.548 on 5 and 11 DF,  p-value: 0.002667
