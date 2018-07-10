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

library("PerformanceAnalytics")
chart.Correlation(swiss, histogram=FALSE, pch=19)

summary(lm1) # 0.671 

x = model.matrix(Fertility~., swiss)[, -c(1)] # matriz de covariables
y = swiss$Fertility              # vector respuesta 


set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)  # escoge 50% de los datos para entrenamiento
test = (-train) 
y.test = y[test]



grid = 10 ^ seq(10, -2, length=100) #grid de 100 valores para lambda (de 10^10 a 10^{-2})
ridge.mod = glmnet(x,y,alpha=0,lambda=grid) #alpha= 0 => regresion ridge
ridge.mod

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=0,nfolds=5)
plot(cv.out,main="Ridge regression") # Traza la curva de validacion cruzada y las curvas de desviacion estandar superior e inferior, en función de los valores de lambda utilizados.
bestlambda = cv.out$lambda.min #escoge el mejor valor de lambda (el que alcanza el menor ECM - error cuadratico medio -  en la validacion cruzada)
bestlambda
#ECM del mejor modelo en el conjunto de prueba
ridge.pred = predict(ridge.mod, s=bestlambda, newx=x[test,])
mean((ridge.pred-y.test)^2)  #igual a 44932.78

#reajusta el modelo usando todos los dados y el mejor lambda
out=glmnet( x , y , alpha=0)
predict(out, type="coefficients", s=bestlambda) #muestra los coeficientes del modelo final


# Validation 

lm2 = lm(Fertility~., swiss[train,])
summary(lm2)







tt =  as.numeric( predict(lm1, as.data.frame(x[test,])))
tt_test = as.numeric(y.test)
length(tt)
length(y.test)

mean((tt -tt_test)^2)  


# variation inflation factor
# https://data.library.virginia.edu/diagnostic-plots/
# http://www.ugr.es/~montero/matematicas/regresion_lineal.pdf 
# https://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/
# There are five major assumptions of linear regression:
