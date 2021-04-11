# Libreria
library(forecast)
library(TSA)
library(fANCOVA)

# Carga de los datos

Datos20=read.table("anex-EMMET-dic2019-Fabricacion de otros productos quimicos (1).csv",header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos20=ts(Datos20,freq=12,start=c(2001,1))

# Tendencia de la serie
Tt=decompose(Datos20)$trend

# Gráfico de la serie y la tendencia

par(mfrow=c(1,2))
plot(Datos20, sub="(a)", lwd=1.5, xlab="Tiempo")
grid()
plot(Tt,ylim=c(min(Datos20),max(Datos20)), sub="(b)", xlab= "Tiempo", lwd=1.5, ylab=expression(T[t]))
grid()

# Boxplots y periodogramas para mirar estacionalidad

par(mfrow=c(2,2))
boxplot(Datos20~cycle(Datos20), names=month.abb, ylab="Producción nominal", xlab = "Meses del año", sub="(a)")
grid()
periodogram(diff(Datos20),lwd=4, sub="(b)") #periodograma sobre los logaritmos diferenciados
abline(v=c(1:6)/12,col=2,lty=2)
grid()

# Ajuste de los modelos 

m <- 12 # numero de periodos a pronosticar dentro de la muestra
n <-length(Datos20)-m # tamaño de la muestra para el ajuste
t <- 1:n #Indice de tiempo en los periodos de ajuste

# Datos para el ajuste 
yt <- ts(Datos20[t], frequency = 12, start=c(2001, 1))


# Creación de las variables indicadoras para los datos de muestra

mes <- seasonaldummy(yt) #Matriz con las 11 primeras variables Indicadoras mes

I1 <- mes[,1]
I2 <- mes[,2]
I3 <- mes[,3]
I4 <- mes[,4]
I5 <- mes[,5]
I6 <- mes[,6]
I7 <- mes[,7]
I8 <- mes[,8]
I9 <- mes[,9]
I10 <- mes[,10]
I11 <- mes[,11]



tnuevo <- (n+1):length(Datos20)
ytnuevo <- ts(Datos20[tnuevo], frequency = 12, start = c(2019, 1))

# Creación de las variables indicadoras para los datos de validación cruzada

mesnuevo <- seasonaldummy(yt, h=12)
#Separando una a una las 11 indicadoras para los tiempos de pron?stico
I1n=mesnuevo[,1]
I2n=mesnuevo[,2]
I3n=mesnuevo[,3]
I4n=mesnuevo[,4]
I5n=mesnuevo[,5]
I6n=mesnuevo[,6]
I7n=mesnuevo[,7]
I8n=mesnuevo[,8]
I9n=mesnuevo[,9]
I10n=mesnuevo[,10]
I11n=mesnuevo[,11]

## Ajuste del modelo 1 Modelo cuadrático estacional con indicadoras

mod1 <- lm(yt~t+I(t^2)+I1+I2+I3+I4+I5+I6+I7+I8+I9+I10+I11)
summary(mod1)

## Ajuste del modelo 2 Modelo cúbico estacional con indicadoras

mod2 <- lm(yt~t+I(t^2)+I(t^3)+I1+I2+I3+I4+I5+I6+I7+I8+I9+I10+I11)
summary(mod2)


# Funciones a utilizar para el ajuste del modelo DLC

#Creando función para extraer correctamente estimaciones de los efectos estacionales 𝜹𝒊 por filtro de descomposición
factoresdeltai=function(descom,s,estacionini){
  if(estacionini==1){
    deltasi=descom$figure
  }
  if(estacionini!=1){
    j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
  }
  deltasi
}

## Ajuste modelo 3 Descomposición aditiva & Loess cuadrático


#Descomposición aditiva de la serie recortada
descom=decompose(yt,type="additive")

s=12 #Longitud del periodo estacional


#Componente estacional estimada de la descomposición de la serie recortada
St=descom$seasonal

#Obteniendo los s factores estacionales estimados
deltas_i=factoresdeltai(descom=descom,s=12,estacionini=1)

#el período es s=12 y la serie arranca en estación 1
data.frame(deltas_i)

#identificando la estación correspondiente a los m=12 períodos de pronósticos
i=c(1,2,3,4,5,6,7,8,9,10,11,12) 

Stnuevo=deltas_i[i] #Asignando el valor de St a los periodos a pronosticar
Stnuevo=ts(Stnuevo,frequency=12,start=c(2019,1)) #convirtiendo en serie de tiempo al pronóstico de St
Stnuevo


#Desestacionalizando o ajustando estacionalmente a la serie recortada, según modelo aditivo
ytd=yt-St


# Ajuste modeloo con LOESS cuadrático (AICC) sobre serie desestacionalizada
mod3=loess.as(t,ytd,degree=2,criterion="aicc",family="gaussian",plot=F)
summary(mod3)
alfa.optim=mod3$pars$span #guardando el valor óptimo del parámetro alfa

# Ajuste modelo 4 con método Suavizamiento exponencial Holt-Winters aditivo

mod4=HoltWinters(yt,seasonal="additive") #Suavizamiento con valores Optimos en parametros
mod4


# Valores ajustados de los modelos

#Modelo 1
mod1_ajust <- ts(fitted(mod1), start  = c(2001,1), frequency = 12)

#Modelo 2
mod2_ajust <- ts(fitted(mod2), start  = c(2001,1), frequency = 12)

#Modelo 3
mod3_Tt <- ts(fitted(mod3), start  = c(2001,1), frequency = 12)
mod3_ajust <- mod3_Tt + St # Ajuste D&LC(AICC)

#Modelo 4
mod4_ajust <- fitted(mod4)[,1]

# Gráfica de los reales vs los ajustes con todos los modelos 

#par(mfrow=c(2,2))
plot(Datos20)
lines(mod1_ajust, col=2, lwd=2)
grid()
legend("topleft", legend = c("Original", "Ajuste del modelo1"), lty=1, col=c(1,2), cex=0.7)

plot(Datos20)
lines(mod2_ajust, col=2, lwd=2)
grid()
legend("topleft", legend = c("Original", "Ajuste del modelo2"), lty=1, col=c(1,2), cex=0.7)

plot(Datos20)
lines(mod3_ajust, col=2, lwd=2)
grid()
legend("topleft", legend = c("Original", "Ajuste D&LC(AICC)"), lty=1, col=c(1,2), cex=0.7)

plot(Datos20)
lines(mod4_ajust, col=2, lwd=2)
grid()
legend("topleft", legend=c("Original","Ajuste H-W"), col=c(1,2), lty=1, cex=0.7)



#Creando funcion usuario crit.inf.resid() para calcular C^*_n(p)
crit.inf.resid <- function(residuales,n.par,AIC="TRUE"){
  if(AIC=="TRUE"){
    #Calcula AIC
    CI=log(mean(residuales^2))+2*n.par/length(residuales)
  }
  if(AIC=="FALSE"){
    #Calcula BIC
    CI=log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales)
  }
  CI
}

# Calculo de los AIC y BIC en cada uno de los modelos

#modelo 1
resmod1.orig <- residuals(mod1) #seudo-residuos en la escala original. Usados solo para calcular AIC y BIC
npar1 <- length(coef(mod1)[coef(mod1)!=0]) #numero parametros modelo 1
AIC1 <- exp(crit.inf.resid(resmod1.orig,n.par=npar1))
BIC1 <- exp(crit.inf.resid(resmod1.orig ,n.par=npar1, AIC="FALSE"))


# modelo 2

resmod2.orig <- residuals(mod2) #seudo-residuos en la escala original. Usados s?lo para calcular AIC y BIC
npar2 <- length(coef(mod2)[coef(mod2)!=0]) #n?mero par?metros modelo 1
AIC2 <- exp(crit.inf.resid(resmod1.orig,n.par=npar2))
BIC2 <- exp(crit.inf.resid(resmod1.orig ,n.par=npar2, AIC="FALSE"))

#modelo 3
et3 <- yt - mod3_ajust
p3 <- round(mod3$enp)+s-1
AIC3 <- exp(crit.inf.resid(residuales=et3,n.par=p3))
BIC3 <- exp(crit.inf.resid(residuales=et3,n.par=p3,AIC="FALSE"))


#modelo 4
p4 <- 2+s-1 #Aprox. del n?mero de par?metros del suavizamiento
AIC4 <- exp(crit.inf.resid(residuales=residuals(mod4),n.par=p4))
BIC4 <- exp(crit.inf.resid(residuales=residuals(mod4),n.par=p4,AIC="FALSE"))


# Gráfico de la estacionalidad modelos globales

p1 <- 2 # grado del polinomio modelo 1
p2 <- 3 # grado del polinomio modelo 2

efectosestac1 <- ts(c(coef(mod1)[(p1+2):npar1],0),freq=1,start=1)
efectosestac2 <- ts(c(coef(mod2)[(p2+2):npar2],0),freq=1,start=1)
plot(efectosestac1,lwd=4,xlab="Periodo del año", ylab="deltas", col="blue")
lines(efectosestac2,lty=2,col="cyan4",lwd=4)
grid()
legend("bottomright",legend=c("Modelo 1","Modelo 2"),col=c("blue", "cyan4"),lty=1:2,lwd=2)


# Gráfico de la estacionalidad modelos locales 

deltasiHW=ts(mod4$coef[c(3:14)],freq=1,start=1)#Estimaciones de los efectos estacionales segun filtro de descomposicióon

deltasDescomp=ts(deltas_i,freq=1,start=1)
deltasilocales=data.frame(rbind(deltasiHW,deltasDescomp),row.names=c("HW","Descomp&loess"))
names(deltasilocales)=c(1:12)#Gráfico de los efectos estacionales estimados


plot(deltasiHW,lwd=4,ylim=c(min(deltasiHW,deltasDescomp),max(deltasiHW,deltasDescomp)+0.05),ylab="",xlab="Mes del año")
lines(deltasDescomp,lty=2,lwd=4,col=2)
grid()
legend("topleft",legend=c("Efectos estacionales H-W en t=216","Efectos estacionales Filtro de descomposición"),col=1:2,lty=1:2,lwd=3, cex=0.7)


# Gráfico de la estacionalidad en todos los modelos

plot(efectosestac1,lwd=3, col="blue", xlab="Periodo del año", ylim =c(-10,23))
lines(efectosestac2,lty=2,col="cyan4",lwd=4)
lines(deltasiHW,lwd=3, col=3)
lines(deltasDescomp,lty=2,lwd=4,col="purple")
grid()
legend("topleft",legend=c("Polinomial cuadrático","Polinomial cúbico", "SEHW","DLC"),col=c("blue", "cyan4", 3, "purple"),lwd=3, cex=0.8)

# Serie desestacionalizada

plot(ytd, ylab="")
lines(mod3_Tt, col=2, lwd = 2)
grid()
legend("topleft", legend=c("Serie ajustada estacionalmente", "Tendencia LOESS cuadrático (AICC)"), col=c(1,2), lty=1)


# Pronósticos de los modelos 

# Modelo 1
ytpron1 <- predict(mod1, newdata=data.frame(t=tnuevo, I1=I1n, I2=I2n, I3=I3n, I4=I4n, I5=I5n, I6=I6n, I7=I7n, I8=I8n, I9=I9n, I10=I10n, I11=I11n), interval="prediction")
ytpron1 <- ts(ytpron1,freq=12,start=c(2019,1))


# Modelo 2

ytpron2 <- predict(mod2, newdata=data.frame(t=tnuevo, I1=I1n, I2=I2n, I3=I3n, I4=I4n, I5=I5n, I6=I6n, I7=I7n, I8=I8n, I9=I9n, I10=I10n, I11=I11n), interval="prediction")
ytpron2 <- ts(ytpron2,freq=12,start=c(2019,1))
ytpron2

# Modelo 3

#Pronósticos de la tendencia por loess cuadrático óptimo (AICC)
Ttnuevo3 <- predict(loess(ytd~t,span=alfa.optim2,degree=2,control=loess.control(surface="direct")),
                    data.frame(t=tnuevo),se=FALSE)
Ttnuevo3 <- ts(Ttnuevo3,freq=12,start=c(2019,1)) #convirtiendo en serie de tiempo al pronóstico de Tt, modelo 3
ytpron3 <- Ttnuevo3 + Stnuevo #Pronóstico puntual Modelo 2
#Tabla con pronósticos de las componentes y de la serie, Modelo 2
tablapron3 <- cbind(Pron_Tt=Ttnuevo3,Pron_St=Stnuevo,Pron_serie=ytpron3)
tablapron3

# Modelo 4

pronos4 <- predict(mod4, n.ahead=12,prediction=T,level=0.95)
ytpron4 <- pronos4[,1] #sólo los pronósticos puntuales del suavizamiento


# Gráfica de los residuales vs el tiempo 

# Residuales vs tiempo mod1
plot.ts(residuals(mod1))
abline(h=c(-2*summary(mod1)$sigma, 0, 2*summary(mod1)$sigma), col=2)
legend("topleft", legend=c("Modelo1"), lty=1, col = 1, lwd=2)


# Residuales vs tiempo mod2
plot.ts(residuals(mod2))
abline(h=c(-2*summary(mod2)$sigma, 0, 2*summary(mod2)$sigma), col=2)
legend("topleft", legend=c("Modelo2"), lty=1, col = 1, lwd=2)


# Residuales vs tiempo mod3
df=n-(round(mod3$enp)+s-1) #Grados de libertad aproximados del ajuste total
MSE3=sum(et3^2)/df #MSE aproximado del ajuste total del modelo 3
plot(et3,ylim=c(min(-2*sqrt(MSE3),et3),max(2*sqrt(MSE3),et3)), ylab="residuales(mod3)")
abline(h=c(-2*sqrt(MSE3),0,2*sqrt(MSE3)),col=2)
legend("topleft", legend=c("Modelo3"), lty=1, col = 1, lwd=2)


# Residuales vs tiempo mod4

et4=residuals(mod4)
df4=n-2*s-((s-1)+2)
MSE4=mod4$SSE/df4 #MSE aproximado del ajuste total del Suavizamiento

plot(et4,ylim=c(min(-2*sqrt(MSE4),et4),max(2*sqrt(MSE4),et4)), ylab="residuales(mod4)")
abline(h=c(-2*sqrt(MSE4),0,2*sqrt(MSE4)),col=2)
legend("topleft", legend=c("Modelo4"), lty=1, col = 1, lwd=2)



# Gráfica de residuales vs valores ajustados

# Residuales vs valores ajustados mod1

plot(fitted(mod1), residuals(mod1), pch=19)
abline(h=c(-2*summary(mod1)$sigma, 0, 2*summary(mod1)$sigma), col=2)
legend("topleft", legend=c("Modelo1"), lty=1, col = 1, lwd=2,cex = 0.8)


# Residuales vs valores ajustados mod2

plot(fitted(mod2), residuals(mod2), pch=19)
abline(h=c(-2*summary(mod2)$sigma, 0, 2*summary(mod2)$sigma), col=2)
legend("topleft", legend=c("Modelo2"), lty=1, col = 1, lwd=2,cex = 0.8)

# Residuales vs valores ajustados mod3
plot(as.numeric(mod3_ajust),et3,ylim=c(min(-2*sqrt(MSE3),et3),max(2*sqrt(MSE3),et3)), pch=19)
abline(h=c(-2*sqrt(MSE3),0,2*sqrt(MSE3)),col=2)
legend("topleft", legend=c("Modelo3"), lty=1, col = 1, lwd=2,cex = 0.8)

# Residuales vs valores ajustados mod4

plot(as.numeric(mod4_ajust),et4,ylim=c(min(-2*sqrt(MSE4),et4),max(2*sqrt(MSE4),et4)), pch=19)
abline(h=c(-2*sqrt(MSE4),0,2*sqrt(MSE4)),col=2)
legend("topleft", legend=c("Modelo4"), lty=1, col = 1, lwd=2,cex = 0.8)

# Medidas de pronosticos

accuracy(ytpron1,ytnuevo) #Modelo 1
accuracy(ytpron2,ytnuevo) #Modelo 2
accuracy(ytpron3,ytnuevo) #Modelo 3
accuracy(ytpron4,ytnuevo) #Modelo 4

# Función que calcula la amplitud y cobertura

#Creando función usuario amplitud() para calcular la amplitud promedio de los I.P en pronósticos ex – post
amplitud=function(LIP,LSP){
  a=LSP-LIP
  am=mean(a)
  am
}
#Creando función usuario cobertura() para calcular la cobertura de los I.P en pronósticos ex – post
cobertura=function(real,LIP,LSP){
  I=ifelse(real>=LIP & real<=LSP,1,0)
  p=mean(I)
  p
}

# Modelo 1:

amplitud(LIP=ytpron1[,2],LSP=ytpron1[,3])
cobertura(real=ytnuevo,LIP=ytpron1[,2],LSP=ytpron1[,3])


# Modelo 2:

amplitud(LIP=ytpron2[,2],LSP=ytpron2[,3])
cobertura(real=ytnuevo,LIP=ytpron2[,2],LSP=ytpron2[,3])


#Modelo 4:
  
  #Precisión pronósticos por I.P Holt-Winters
  
  amplitud(LIP=pronos4[,3],LSP=pronos4[,2])
cobertura(real=ytnuevo,LIP=pronos4[,3],LSP=pronos4[,2])


# Gráfico de pronosticos

plot(ytnuevo,lwd=2, col=1, type="b", pch=1, xlab="Periodo del año", ylab="")
lines(ytpron1[,1],lwd=2, col=2, type="b", pch=2)
lines(ytpron2[,1],lwd=2, col=3, type="b", pch=3)
lines(ytpron3,lwd=2,col=4, type="b", pch=4)
lines(ytpron4,lwd=2,col=5, type="b", pch=5)
grid()
legend("bottomright",legend=c("Real","Pronostico modelo1", 
                              "Pronostico modelo2", "Pronostico modelo3", "Pronostico modelo4"),
       col=1:5,lwd=2, pch=1:5, cex=0.8)


