#Tarea1 CD AM-ML
par(mar = rep(2, 4))
library(ggplot2)
library(reshape)

datos <- data.frame("Y" = c(11,13,14,15,17,18,19,21,22,26,28,31,32,34,35.5,36,38,40,40.6,41,42,43),
                    "X1" = c(81.4508687,181.272242,270.426407,403.428793,897.847292,1339.43076,1998.1959,4447.06675,6634.24401,32859.6257,73130.4418,242801.617,362217.45,806129.759,1468864.19,1794074.77,3992786.84,8886110.52,11296460.4,13256519.1,19776402.7,29502925.9),
                    "X2" = c(59.29,82.81,96.04,110.25,141.61,158.76,176.89,216.09,237.16,331.24,384.16,470.89,501.76,566.44,617.5225,635.04,707.56,784,807.6964,823.69,864.36,906.01))
names(datos)

#Gráfica Y vs X1
datos1 <- datos[,c("Y", "X1")]
ggplot(data = datos1)+geom_line(mapping = aes(x= X1, y = Y), color = "red")+
  ggtitle('Y vs X1') + labs(x = "Y", y = "X1")

#Gráfica Y vs X2
datos2 <- datos[,c("Y", "X2")]
ggplot(data = datos2)+geom_line(mapping = aes(x= X2, y = Y), color = "blue")+
  ggtitle('Y vs X2') + labs(x = "Y", y = "X2")

#Gráfica X1 vs X2
datos3 <- datos[,c("X1", "X2")]
ggplot(data = datos3)+geom_line(mapping = aes(x= X1, y = X2), color = "green")+
  ggtitle('X1 vs X2') + labs(x = "X1", y = "X2")


Estimadores <- function(X){
  n <- nrow(X)
  X_mean <- mean(X[,c(2)])
  Y_mean <-  mean(X[,c(1)])
  X_sum <- sum(X[,c(2)])
  Y_sum <- sum(X[,c(1)])
  X_sum_2 <- sum('^'(X[,c(2)],2))
  XY_sum <- sum(X[,c(2)] * X[,c(1)])
  
  Beta <- (n*X_mean*Y_mean - XY_sum)/(n*(X_mean^2)- X_sum_2)
  Alpha <- Y_mean - Beta*X_mean
  estimadores <- list("Alpha" = Alpha, "Beta" = Beta)
  return(estimadores)
}

#Estimadores de modelo: Y = B_1 + B_2X_1 + e
Estimadores(X=datos1)
lm(formula = datos1$Y  ~ datos1$X1)
#Estimadores de modelo: Y = B_1 + B_2X_2 + e
Estimadores(X=datos2)
lm(formula = datos2$Y  ~ datos2$X2)

#Estimadores de modelo: Y = B_1 + B_2X_1 B_3X_2+ e
#install.packages("matlib")
library(matlib)

X <- matrix(c(rep(1, nrow(datos)), datos$X1, datos$X2), ncol=3)

Y <- matrix(datos$Y)
Beta_GLM = solve(t(X) %*% X) %*% t(X) %*% Y
Beta_GLM

lm(formula = Y ~ X1+X2, data=datos)

#Comparacion Y y Y estimada, Y vs X1
Y_hat <- Estimadores(X=datos1)[["Alpha"]] + Estimadores(X=datos1)[["Beta"]]*datos[,c("X1")]
Comp <- data.frame("X" = 1:length(datos$Y - Y_hat),
                   "Y-Y_hat" = datos$Y - Y_hat)

ggplot(Comp, aes(x=X, y=Y-Y_hat)) + 
  geom_point(size=2, color = "black", alpha=0.5) +
  ggtitle("Y-Y_hat (Y vs X1)")

#Comparacion Y y Y estimada, Y vs X2
Y_hat <- Estimadores(X=datos2)[["Alpha"]] + Estimadores(X=datos1)[["Beta"]]*datos[,c("X2")]
Comp <- data.frame("X" = 1:length(datos$Y - Y_hat),
                   "Y-Y_hat" = datos$Y - Y_hat)

ggplot(Comp, aes(x=X, y=Y-Y_hat)) + 
  geom_point(size=2, color = "black", alpha=0.5) +
  ggtitle("Y-Y_hat (Y vs X2)")

#Comparacion Y y Y estimada, Y vs X2
Y_hat <- Estimadores(X=datos2)[["Alpha"]] + Estimadores(X=datos1)[["Beta"]]*datos[,c("X2")]
Comp <- data.frame("X" = 1:length(datos$Y - Y_hat),
                   "Y-Y_hat" = datos$Y - Y_hat)

ggplot(Comp, aes(x=X, y=Y-Y_hat)) + 
  geom_point(size=2, color = "black", alpha=0.5) +
  ggtitle("Y-Y_hat (Y vs X2)")

#Comparacion Y y Y estimada, Y vs X2,X1
Y_hat <- Beta_GLM[1] + Beta_GLM[2]*datos[,c("X1")] +Beta_GLM[3]*datos[,c("X2")]
Comp <- data.frame("X" = 1:length(datos$Y - Y_hat),
                   "Y-Y_hat" = datos$Y - Y_hat)

ggplot(Comp, aes(x=X, y=Y-Y_hat)) + 
  geom_point(size=2, color = "black", alpha=0.5) +
  ggtitle("Y-Y_hat (Y vs X2,X1)")



## Transformación de los datos

library(ggplot2)
library(reshape)

#install.packages("writexl")
library(writexl)
library(matlib)
library(MASS)

datos <- data.frame("Y" = c(11,13,14,15,17,18,19,21,22,26,28,31,32,34,35.5,36,38,40,40.6,41,42,43),
                    "X1" = c(81.4508687,181.272242,270.426407,403.428793,897.847292,1339.43076,1998.1959,4447.06675,6634.24401,32859.6257,73130.4418,242801.617,362217.45,806129.759,1468864.19,1794074.77,3992786.84,8886110.52,11296460.4,13256519.1,19776402.7,29502925.9),
                    "X2" = c(59.29,82.81,96.04,110.25,141.61,158.76,176.89,216.09,237.16,331.24,384.16,470.89,501.76,566.44,617.5225,635.04,707.56,784,807.6964,823.69,864.36,906.01))

# Cambios a variables X1 y X2
datos_transformados <- data.frame("Y" = datos$Y,
                                  "X1" = log(datos$X1),
                                  "X2" = datos$X2^(1/2))

write_xlsx(datos_transformados, "Datos_transformados.xlsx")

Estimadores <- function(X){
  n <- nrow(X)
  X_mean <- mean(X[,c(2)])
  Y_mean <-  mean(X[,c(1)])
  X_sum <- sum(X[,c(2)])
  Y_sum <- sum(X[,c(1)])
  X_sum_2 <- sum('^'(X[,c(2)],2))
  XY_sum <- sum(X[,c(2)] * X[,c(1)])
  
  Beta <- (n*X_mean*Y_mean - XY_sum)/(n*(X_mean^2)- X_sum_2)
  Alpha <- Y_mean - Beta*X_mean
  estimadores <- list("Alpha" = Alpha, "Beta" = Beta)
  return(estimadores)
}

#Estimadores de model: Y = B_1 + B_2X_1 + e
datos_transformados1 <- datos_transformados[,c("Y", "X1")]
Estimadores(X=datos_transformados1)
lm(formula = datos_transformados1$Y  ~ datos_transformados1$X1)

#Estimadores de model: Y = B_1 + B_2X_2 + e
datos_transformados2 <- datos_transformados[,c("Y", "X2")]
Estimadores(X=datos_transformados2)
lm(formula = Y  ~ X2, data = datos_transformados2)


datos_transformados3 <- datos_transformados[,c("X1", "X2")]

X <- datos_transformados3
X$Unos <- rep(1, nrow(X))
X <- X[,c("Unos", "X1", "X2")]
X <- data.matrix(X)

Y <- matrix(datos_transformados$Y)
Beta_GLM1 = ginv(t(X) %*% X) %*% t(X) %*% Y
Beta_GLM1

#Con  SVD para sacar la inversa de t(X) %*% X
SVD <- svd(t(X) %*% X)
inversa <- SVD$v %*% ginv(diag(SVD$d)) %*% t(SVD$u) #Error

Beta_GLM2 <- inversa %*% t(X) %*% Y
Beta_GLM2

lm(Y ~ X2+X1,data=datos_transformados)


#Comparacion Y y Y estimada, Y vs X1
Y_hat <- Estimadores(X=datos_transformados1)[["Alpha"]] + Estimadores(X=datos_transformados1)[["Beta"]]*datos_transformados1[,c("X1")]
Comp <- data.frame("X" = 1:length(datos_transformados1$Y - Y_hat),
                   "Y-Y_hat" = datos_transformados1$Y - Y_hat)

ggplot(Comp, aes(x=X, y=Y-Y_hat)) + 
  geom_point(size=2, color = "black", alpha=0.5) +
  ggtitle("Y-Y_hat (Y vs X1)")

#Comparacion Y y Y estimada, Y vs X2
Y_hat <- Estimadores(X=datos_transformados2)[["Alpha"]] + Estimadores(X=datos_transformados2)[["Beta"]]*datos_transformados2[,c("X2")]
Comp <- data.frame("X" = 1:length(datos_transformados2$Y - Y_hat),
                   "Y-Y_hat" = datos_transformados2$Y - Y_hat)

ggplot(Comp, aes(x=X, y=Y-Y_hat)) + 
  geom_point(size=2, color = "black", alpha=0.5) +
  ggtitle("Y-Y_hat (Y vs X2)")


#Comparacion Y y Y estimada, Y vs X2,X1
Y_hat <- Beta_GLM2[1] + Beta_GLM2[2]*datos_transformados3[,c("X1")] +Beta_GLM2[3]*datos_transformados3[,c("X2")]
Comp <- data.frame("X" = 1:length(datos_transformados3$Y - Y_hat),
                   "Y-Y_hat" = datos_transformados3$Y - Y_hat)

ggplot(Comp, aes(x=X, y=Y-Y_hat)) + 
  geom_point(size=2, color = "black", alpha=0.5) +
  ggtitle("Y-Y_hat (Y vs X2,X1)")
