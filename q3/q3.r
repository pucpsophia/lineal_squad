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

# Descriptivo
data(swiss)
colnames(swiss)
str(swiss)
summary(swiss)

# Descriptivos adicionales de la variable (ejem: Catholic)
summary(swiss$Catholic)
mean(swiss$Catholic, na.rm = T)
quantile(swiss$Catholic)
quantile(swiss$Catholic, probs = seq(0, 1, by = 0.1))
sd(swiss$Catholic)

##Primer Modelo
lm1 <- lm(formula =  Fertility ~ Agriculture + Examination  + Education + Catholic + Infant.Mortality, data = swiss)
summary(lm1)
lm1$coefficients

#Valores reales vs. los Valores estimados
plot(lm1$fitted.values, swiss$Fertility) 
abline(0, 1, col = 2)

#Residuales vs. los Valores estimados
plot(lm1$fitted.values, lm1$residuals) 
abline(0, 0, col = 2)

###Colinealidad
vif(lm1)
library("PerformanceAnalytics")
chart.Correlation(swiss, histogram=FALSE, pch=19)
summary(lm1) # R2 = 0.671 

##2da opcion para ver la Colinealidad
library(car)
cor(swiss[,-1]) #matriz
library(psych)  #De manera gráfica
pairs.panels(swiss[c(1,2,3,4,5,6)])

# Corrigiendo el problema de multicolinealidad en clase se realizó lo mismo con la variable pop
Fer_Edu <- swiss$Fertility/swiss$Education
Agri_Edu <- swiss$Agriculture/swiss$Education
Exam_Edu <- swiss$Examination/swiss$Education
Catho_Edu <- swiss$Catholic/swiss$Education
Infant_Edu <- swiss$Infant.Mortality/swiss$Education

summary(lm(Fer_Edu~Agri_Edu+Exam_Edu+Catho_Edu+Infant_Edu))
summary(aov(lm(Fer_Edu~Agri_Edu+Exam_Edu+Catho_Edu+Infant_Edu)))

# sospechamos correlacion y que esto afecta la prediccion de la regresion.
# para validar esto realizaremos una validacion entre una data de entrenamiento y una de prueba
# como podemos ver R2 se incrementara con respecto al modelo base LM utilizando Ridge y Lasso
#https://en.wikipedia.org/wiki/Coefficient_of_determination



# generacion de data de entrenamiento y validacion
set.seed(1)

x = model.matrix(Fertility~., swiss)[, -c(1)] # matriz de covariables
y = swiss$Fertility                           # vector respuesta 

train = sample( 1 : nrow( x ) , nrow( x ) / 2 )  # escoge 50% de los datos para entrenamiento
test = (-train) 
y.test = y[test]


# metrica R square   1- sse/sst
value_y = swiss[test,1]
sst <- sum((value_y - mean(value_y))^2)

# baselline LM

lm2 = lm(Fertility~., swiss[train,])
summary(lm2)

lm_predicted = as.numeric(predict(lm2, swiss[test,-1]))
sse_lm <- sum((lm_predicted - value_y)^2)

rsq_lm = 1 - sse_lm / sst
rsq_lm


grid_ridge = 10 ^ seq ( 10, -2, length=100) #grid de 100 valores para lambda (de 10^10 a 10^{-2})
ridge.mod = glmnet( x , y , alpha=0, lambda=grid_ridge) #alpha= 0 => regresion ridge
ridge.mod
#vamos a escoger lambda usando validación cruzada (5-fold cross validation) para la regresión lasso 
cv.out_ridge = cv.glmnet( x[ train , ], y [ train ], alpha=0, nfolds=5)
plot(cv.out_ridge, main="Ridge regression") # Traza la curva de validacion cruzada y las curvas de desviacion estandar superior e inferior, en función de los valores de lambda utilizados.
best_ridge_lambda = cv.out_ridge$lambda.min #escoge el mejor valor de lambda (el que alcanza el menor ECM - error cuadratico medio -  en la validacion cruzada)
best_ridge_lambda
#ECM del mejor modelo en el conjunto de prueba
ridge.pred = predict(ridge.mod, s=best_ridge_lambda, newx=x[test,])
mean(( ridge.pred - y.test ) ^ 2)  

# reajusta el modelo usando todos los dados y el mejor lambda
out_ridge = glmnet( x , y , alpha=0)
predict(out_ridge, type="coefficients", s = best_ridge_lambda) #muestra los coeficientes del modelo final

ridge_predicted = predict(out_ridge, as.matrix(swiss[test,-1]), s=best_ridge_lambda) 
ridge_predicted = as.numeric(ridge_predicted)
ridge_sse <- sum((ridge_predicted - value_y)^2)
rsq_ridge <- 1 - ridge_sse / sst
rsq_ridge

# Regresión Lasso  
grid_lasso = 10 ^ seq ( 10, -2, length=100) #grid de 100 valores para lambda (de 10^10 a 10^{-2})
lasso.mod = glmnet( x[ train, ], y[ train ], alpha=1, lambda=grid_lasso)
#vamos a escoger lambda usando validación cruzada (5-fold cross validation) para la regresión lasso 
cv.out_lasso = cv.glmnet(x[train,],y[train],alpha=1,nfolds=5)
plot(cv.out_lasso,main="Lasso regression")
best_lasso_lambda = cv.out_lasso$lambda.min #igual a 20.12811
best_lasso_lambda

#ECM del mejor modelo en el conjunto de prueba
lasso.pred=predict(lasso.mod, s=best_ridge_lambda, newx=x[test,])
mean((lasso.pred - y.test) ^ 2) #igual a 70365.12

#reajusta el modelo usando todos los dados y el mejor lambda
out_lasso = glmnet( x , y , alpha=1, lambda=grid_lasso)
lasso.coef=predict(out_lasso, type="coefficients",s=best_lasso_lambda)
lasso.coef  ##muestra los coeficientes del modelo final
lasso.coef[lasso.coef!=0] ##muestra los coeficientes no nulos

lasso_predicted = predict(out_lasso, as.matrix(swiss[test,-1]), s=best_lasso_lambda) 
lasso_predicted = as.numeric(lasso_predicted)
lasso_sse <- sum((lasso_predicted - value_y) ^ 2)
rsq_lasso <- 1 - lasso_sse / sst
rsq_lasso

c(rsq_lm, rsq_ridge, rsq_lasso)


# variation inflation factor
# https://data.library.virginia.edu/diagnostic-plots/
# http://www.ugr.es/~montero/matematicas/regresion_lineal.pdf 
# https://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/
# There are five major assumptions of linear regression:
