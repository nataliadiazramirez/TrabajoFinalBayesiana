
#Context
#This dataset is created for prediction of Graduate Admissions from an Indian perspective.

#Content
#The dataset contains several parameters which are considered important during the application 
#for Masters Programs.

#The parameters included are :

#GRE Scores ( out of 340 )
#TOEFL Scores ( out of 120 )
#University Rating ( out of 5 )
#Statement of Purpose and Letter of Recommendation Strength ( out of 5 )
#Undergraduate GPA ( out of 10 )
#Research Experience ( either 0 or 1 )
#Chance of Admit ( ranging from 0 to 1 )


datos=read.csv("datasets_14872_228180_Admission_Predict_Ver1.1.csv",sep = ",",dec=".",stringsAsFactors = F)

datos<-datos[,-1]
datos$GRE.Score<-as.numeric(datos$GRE.Score)
datos$TOEFL.Score<-as.numeric(datos$TOEFL.Score)
datos$University.Rating<-as.numeric(datos$University.Rating)
datos$Research<-as.numeric(datos$Research)


#1. Convergencia
n<-nrow(datos)
n
x<-table(datos$Research)[2]
x
# La previa de Jeffreys es Beta(0.5,0.5)
posterior <- MCMCpack::MCbinomialbeta(x, n, alpha=0.5, beta=0.5, mc=3750)

summary(posterior)
plot(posterior)

grid <- seq(0,1,0.01)
plot(grid, dbeta(grid, 0.5, 0.5), type="l", col="red",ylim=c(0,20), lwd=2,xlab="pi", ylab="density")
lines(density(posterior), col="blue", lwd=2)
legend("topright", c("prior", "posterior"), lwd=2, col=c("red", "blue"))
grid()


#convergencia

# Longitud de la cadena
coda::raftery.diag(posterior) 

#1.Geweke
#Alcanza convergencia ya que en el gr?fico se observa que los puntos estan dentro de los l?mites.
coda::geweke.plot(posterior) 

#2.Gr?fico de autocorrelaci?n
#Si hay convergencia ya que el primer rezago es alto y luego las barras de los otros rezagos disminuyen.
coda::autocorr.plot(posterior) 

#3.Heidelberger y Welch
#Los valores muestreados para cada variable forman un proceso estacionario, ya que no se rechaza
#la hip?tesis nula. Por lo que se puede decir que se alcanza convergencia.
coda::heidel.diag(posterior)



#2. Modelo logistico
library(coda)
library(MASS)
library(MCMCpack)


model1 <- MCMClogit(Research~GRE.Score+TOEFL.Score+University.Rating+SOP+LOR+CGPA+Chance.of.Admit,
                    b0=0, B0=.001, data=datos,mcmc = 3750,marginal.likelihood ="Laplace")

summary(model1)

#Quitar TOEFL.Score 
model2 <-MCMClogit(Research~GRE.Score+Chance.of.Admit,
                   b0=0, B0=.001, data=datos,mcmc = 3750,marginal.likelihood ="Laplace")

#Modelo m?s probable es el modelo 2 (sin las 6 variables )
BF<-BayesFactor(model1,model2)
mod.probs<- PostProbMod(BF)
mod.probs

summary(model2)
