
#pólizas vendidas entre enero de 2017 hasta abril de 2018.  
#La base de datos tiene 20192 pólizas con una antigüedad de tres meses o más. 

#Estado: Corresponde al estado de la póliza cancelada (1) o vigente (0). 

#Variables numéricas
#Prima: monto mensual de la prima en colones.
#Antigüedad: La antigüedad de la póliza en meses
#Edad: Edad del cliente.
#Número de hijos: Cantidad de hijos que tiene el cliente.

#Variables categóricas:
#Producto: Tipo de póliza (por confidencialidad se clasificaron en Producto A, B, C, D, E)
#Moneda: Tipo de moneda de la póliza (colones o dólares)  
#Plan:  Tipo de pago de la prima (mensual o anual)
#Medio de pago por emisor: corresponde al tipo de emisor de la tarjeta con que se realiza el pago de la póliza (por confidencialidad se clasificaron en Emisor A y Emisor B) 
#Medio de pago por plan: corresponde al tipo de plan de la tarjeta con que se realiza el pago de la póliza (por confidencialidad se clasificaron en Plan A, B, C, D, E) 
#Sexo: Sexo del cliente. (0 hombre 1 mujer)


datos=read.csv("BASE SEGUROS.csv",sep = ";",dec=",",stringsAsFactors = F)

datos$Producto<-as.factor(datos$Producto)
datos$Moneda<-as.factor(datos$Moneda)
datos$Plan<-as.factor(datos$Plan)
datos$MedioPagoEmisor<-as.factor(datos$MedioPagoEmisor)
datos$MedioPagoPlan<-as.factor(datos$MedioPagoPlan)
datos$sexo<-as.factor(datos$sexo)
datos$Estado<-as.numeric(datos$Estado)
datos$Numero_Hijos<-as.numeric(datos$Numero_Hijos)

#1. Convergencia
n<-nrow(datos)
n
x<-table(datos$Estado)[2]
x
# La previa de Jeffreys es Beta(0.5,0.5)
posterior <- MCMCpack::MCbinomialbeta(x, n, alpha=0.5, beta=0.5, mc=4000)

summary(posterior)
plot(posterior)

grid <- seq(0,1,0.01)
plot(grid, dbeta(grid, 0.5, 0.5), type="l", col="red",ylim=c(0,100), lwd=2,xlab="pi", ylab="density")
lines(density(posterior), col="blue", lwd=2)
legend("topright", c("prior", "posterior"), lwd=2, col=c("red", "blue"))
grid()


#convergencia

# Longitud de la cadena
coda::raftery.diag(posterior) 

#1.Geweke
#Alcanza convergencia ya que en el gráfico se observa que los puntos estan dentro de los límites.
coda::geweke.plot(posterior) 

#2.Gráfico de autocorrelación
#Si hay convergencia ya que el primer rezago es alto y luego las barras de los otros rezagos disminuyen.
coda::autocorr.plot(posterior) 

#3.Heidelberger y Welch
#Los valores muestreados para cada variable forman un proceso estacionario, ya que no se rechaza
#la hipótesis nula. Por lo que se puede decir que se alcanza convergencia.
coda::heidel.diag(posterior)



#2. Modelo logistico
library(coda)
library(MASS)
library(MCMCpack)


model1 <- MCMClogit(Estado~Producto+Moneda+Plan+MedioPagoEmisor+MedioPagoPlan+Antiguedad+edad+sexo+
                      Numero_Hijos,b0=0, B0=.001, data=datos,mcmc = 4000,marginal.likelihood ="Laplace")

summary(model1)

#Quitar sexo
model2 <- MCMClogit(Estado~Producto+Moneda+Plan+MedioPagoEmisor+MedioPagoPlan+Antiguedad+edad+
                      Numero_Hijos, b0=0, B0=.001, data=datos,mcmc = 4000,
                    marginal.likelihood ="Laplace")

#Modelo más probable es el modelo 2 (sin sexo)
BF<-BayesFactor(model1,model2)
mod.probs<- PostProbMod(BF)
mod.probs

summary(model2)

#Quitar Numero hijos
model3 <- MCMClogit(Estado~Producto+Moneda+Plan+MedioPagoEmisor+MedioPagoPlan+Antiguedad+edad, 
                    b0=0, B0=.001, data=datos,mcmc = 4000,marginal.likelihood ="Laplace")


#Modelo más probable es el modelo3  (sin numero hijos)
BF<-BayesFactor(model2,model3)
mod.probs<- PostProbMod(BF)
mod.probs

summary(model3)


#Quitar Moneda
model4 <- MCMClogit(Estado~Producto+Plan+MedioPagoEmisor+MedioPagoPlan+Antiguedad+edad, 
                    b0=0, B0=.001, data=datos,mcmc = 4000,marginal.likelihood ="Laplace")


#Modelo más probable es el modelo4  (sin moneda)
BF<-BayesFactor(model3,model4)
mod.probs<- PostProbMod(BF)
mod.probs

summary(model4)


#Quitar Plan
model5 <- MCMClogit(Estado~Producto+MedioPagoEmisor+MedioPagoPlan+Antiguedad+edad, 
                    b0=0, B0=.001, data=datos,mcmc = 4000,marginal.likelihood ="Laplace")


#Modelo más probable es el modelo5  (sin plan)
BF<-BayesFactor(model4,model5)
mod.probs<- PostProbMod(BF)
mod.probs

summary(model5)

#Quitar medio pago emisor
model6 <- MCMClogit(Estado~Producto+MedioPagoPlan+Antiguedad+edad, 
                    b0=0, B0=.001, data=datos,mcmc = 4000,marginal.likelihood ="Laplace")


#Modelo más probable es el modelo6  (sin medio de pago emisor)
BF<-BayesFactor(model5,model6)
mod.probs<- PostProbMod(BF)
mod.probs

summary(model6)



