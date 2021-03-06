
#p�lizas vendidas entre enero de 2017 hasta abril de 2018.  
#La base de datos tiene 20192 p�lizas con una antig�edad de tres meses o m�s. 

#Estado: Corresponde al estado de la p�liza cancelada (1) o vigente (0). 

#Variables num�ricas
#Prima: monto mensual de la prima en colones.
#Antig�edad: La antig�edad de la p�liza en meses
#Edad: Edad del cliente.
#N�mero de hijos: Cantidad de hijos que tiene el cliente.

#Variables categ�ricas:
#Producto: Tipo de p�liza (por confidencialidad se clasificaron en Producto A, B, C, D, E)
#Moneda: Tipo de moneda de la p�liza (colones o d�lares)  
#Plan:  Tipo de pago de la prima (mensual o anual)
#Medio de pago por emisor: corresponde al tipo de emisor de la tarjeta con que se realiza el pago de la p�liza (por confidencialidad se clasificaron en Emisor A y Emisor B) 
#Medio de pago por plan: corresponde al tipo de plan de la tarjeta con que se realiza el pago de la p�liza (por confidencialidad se clasificaron en Plan A, B, C, D, E) 
#Sexo: Sexo del cliente. (0 hombre 1 mujer)

# Datos
datos=read.csv("BASE SEGUROS.csv",sep = ";",dec=",",stringsAsFactors = F)

datos$Producto<-as.factor(datos$Producto)
datos$Moneda<-as.factor(datos$Moneda)
datos$Plan<-as.factor(datos$Plan)
datos$MedioPagoEmisor<-as.factor(datos$MedioPagoEmisor)
datos$MedioPagoPlan<-as.factor(datos$MedioPagoPlan)
datos$sexo<-as.factor(datos$sexo)
datos$Estado<-as.numeric(datos$Estado)
datos$Numero_Hijos<-as.numeric(datos$Numero_Hijos)

# Seleccion de variables

library(coda)
library(MASS)
library(MCMCpack)

##### Modelo1
model1 <- MCMClogit(Estado~Producto+Moneda+Plan+MedioPagoEmisor+MedioPagoPlan+Antiguedad+edad+sexo+Numero_Hijos,
                    b0=0, B0=.001, data=datos,mcmc = 10000,marginal.likelihood ="Laplace",
                    burnin = 1000,thin = 10)
#convergencia

#1.Geweke
#Alcanza convergencia ya que en el gr�fico se observa que los puntos estan dentro de los l�mites.
coda::geweke.plot(model1) 

#2.Gr�fico de autocorrelaci�n
#Si hay convergencia ya que el primer rezago es alto y luego las barras de los otros rezagos disminuyen.
coda::autocorr.plot(model1) 

#3.Heidelberger y Welch
#Los valores muestreados para cada variable forman un proceso estacionario, ya que no se rechaza
#la hip�tesis nula. Por lo que se puede decir que se alcanza convergencia.
coda::heidel.diag(model1)

summary(model1)

##### Modelo2
#Quitar sexo,Numero hijos
model2 <- MCMClogit(Estado~Producto+Moneda+Plan+MedioPagoEmisor+MedioPagoPlan+Antiguedad+edad,
                    b0=0, B0=.001, data=datos,mcmc = 30000,
                    marginal.likelihood ="Laplace",burnin = 1000,thin = 10)

#convergencia

#1.Geweke
#Alcanza convergencia ya que en el gr�fico se observa que los puntos estan dentro de los l�mites.
coda::geweke.plot(model2) 

#2.Gr�fico de autocorrelaci�n
#Si hay convergencia ya que el primer rezago es alto y luego las barras de los otros rezagos disminuyen.
coda::autocorr.plot(model2) 

#3.Heidelberger y Welch
#Los valores muestreados para cada variable forman un proceso estacionario, ya que no se rechaza
#la hip�tesis nula. Por lo que se puede decir que se alcanza convergencia.
coda::heidel.diag(model2)

summary(model2)

##### Modelo3
#Quitar Moneda y plan
model3 <- MCMClogit(Estado~Producto+MedioPagoEmisor+MedioPagoPlan+Antiguedad+edad, 
                    b0=0, B0=.001, data=datos,mcmc = 30000,marginal.likelihood ="Laplace",
                    burnin = 1000,thin = 10)
#convergencia

#1.Geweke
#Alcanza convergencia ya que en el gr�fico se observa que los puntos estan dentro de los l�mites.
coda::geweke.plot(model3) 

#2.Gr�fico de autocorrelaci�n
#Si hay convergencia ya que el primer rezago es alto y luego las barras de los otros rezagos disminuyen.
coda::autocorr.plot(model3) 

#3.Heidelberger y Welch
#Los valores muestreados para cada variable forman un proceso estacionario, ya que no se rechaza
#la hip�tesis nula. Por lo que se puede decir que se alcanza convergencia.
coda::heidel.diag(model3)

summary(model3)

##### Modelo4
#Quitar medio pago emisor
model4 <- MCMClogit(Estado~Producto+MedioPagoPlan+Antiguedad+edad, 
                    b0=0, B0=.001, data=datos,mcmc = 30000,marginal.likelihood ="Laplace",
                    burnin = 1000,thin = 10)
#convergencia

#1.Geweke
#Alcanza convergencia ya que en el gr�fico se observa que los puntos estan dentro de los l�mites.
coda::geweke.plot(model4) 

#2.Gr�fico de autocorrelaci�n
#Si hay convergencia ya que el primer rezago es alto y luego las barras de los otros rezagos disminuyen.
coda::autocorr.plot(model4) 

#3.Heidelberger y Welch
#Los valores muestreados para cada variable forman un proceso estacionario, ya que no se rechaza
#la hip�tesis nula. Por lo que se puede decir que se alcanza convergencia.
coda::heidel.diag(model4)

summary(model4)



#Modelo m�s probable es el modelo4 
BF<-BayesFactor(model1,model2,model3,model4)
mod.probs<- PostProbMod(BF)
mod.probs






