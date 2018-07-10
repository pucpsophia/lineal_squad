install.packages("VIF")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("GGally")


library(learnr)
library(MASS)
library(car)

library(corrplot)
library(ggplot2)
library(ggpubr)
library(GGally)


data(swiss)
colnames(swiss)

str(swiss)
summary(swiss)

lm1 <- lm(formula =  Fertility ~ Agriculture + Examination  + Education + Catholic + Infant.Mortality, data = swiss)
summary(lm1)
lm1$coefficients

vif(lm1)

plot(lm1$fitted.values, swiss$Fertility)
abline(0, 1, col = 2)

lm1 <- lm(formula = Fertility ~ Fertility ~ Agriculture  + Education + Catholic + Infant.Mortality, data = swiss)
plot(lm1$fitted.values, lm1$residuals)
abline(0, 0, col = 2)

library("PerformanceAnalytics")
chart.Correlation(swiss, histogram=FALSE, pch=19)


plot(cooks.distance(lm1))


indices.cook <- which(cooks.distance(lm1) >= 4/nrow(swiss))
swiss2 <-swiss[-indices.cook, ]
lm2 <- lm(formula = Fertility ~ ., data = swiss2)
summary(lm2)

plot(lm2$fitted.values, swiss2$Fertility)
abline(0, 1, col = 2)

plot(lm2$fitted.values, lm2$residuals)
abline(0, 0, col = 2)

step<-stepAIC(lm2, direction = "both")
summary(step)

lm3 <- lm(formula = Fertility ~ Agriculture  + Education + Catholic + Infant.Mortality, data = swiss2) #+ Examination
summary(lm3)

anova(lm2, lm3)


mat.cor <- cor(swiss[, !colnames(swiss) %in% ("Fertility")], method = "pearson")


library(corrplot)
corrplot(mat.cor, method = "square", order = "AOE")

ggpairs(swiss) # cor(data_scaled)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(swiss, histogram=FALSE, pch=19)

# variation inflation factor
# https://data.library.virginia.edu/diagnostic-plots/
# http://www.ugr.es/~montero/matematicas/regresion_lineal.pdf 
# https://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/
# There are five major assumptions of linear regression:
  
# That the errors (residuals) are normally distributed with a mean of zero

  # usar grafico 2 QQ

qqnorm(lm2$residuals)
qqline(lm2$residuals)
shapiro.test(lm2$residuals) # h0 es normal  > 0.05

# That the error terms are uncorrelated

  # usar grafico residuals vs fitted








car::vif(lm2)

par(mfrow=c(2,2))
plot(lm2)

