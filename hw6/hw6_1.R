gala <- read.table("C:/Users/Thomas/Downloads/Linear_models/hw6/crime.txt", header=T)


name <- gala[,1]
violent <- gala[,2]
property <- gala[,3]
population <- gala[,4]

fit1 <- lm(property ~ population)
summary(fit1)

