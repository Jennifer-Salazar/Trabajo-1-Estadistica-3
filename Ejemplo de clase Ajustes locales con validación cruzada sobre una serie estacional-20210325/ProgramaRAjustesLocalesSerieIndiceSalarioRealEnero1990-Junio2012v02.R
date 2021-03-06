library(fANCOVA)
library(forecast)

#Creando funci�n para extraer correctamente los valores deltai
factoresdeltai=function(descom,s,estacionini){
if(estacionini==1){
deltasi=descom$figure
}
if(estacionini!=1){
j=estacionini;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
}
deltasi
}

#Creando funci�n usuario crit.inf.resid() para calcular C_n^*(p)
crit.inf.resid=function(residuales,n.par,AIC="TRUE"){
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

#Funci�n para calcular la amplitud de los I.P
amplitud=function(LIP,LSP){
a=LSP-LIP
am=mean(a)
am
}

#Funci�n para calcular la cobertura de los I.P 
cobertura=function(real,LIP,LSP){
I=ifelse(real>=LIP & real<=LSP,1,0)
p=mean(I)
p
}

#para leer desde DATOSSALARIOREALENER1990-JUNIO2012.txt
salario1990_2012=read.table(file.choose(),header=T)
salario1990_2012=ts(salario1990_2012,freq=12,start=c(1990,1))

#Gr�ficos descriptivos
plot(salario1990_2012,ylab="Salario real 1990-enero a 2012-junio")
plot(decompose(salario1990_2012,type="additive")$trend,ylim=c(min(salario1990_2012),max(salario1990_2012)))
boxplot(salario1990_2012~cycle(salario1990_2012),names=month.abb)

#Definiendo periodos para ajuste y pron�stico
m=13
n=length(salario1990_2012)-m
t=1:n

tnuevo=(n+1):length(salario1990_2012)
#Serie para ajustes
yt=ts(salario1990_2012[t],freq=12,start=c(1990,1))

#Valores de la serie en per�odos de pron�sticos
ytf=ts(salario1990_2012[tnuevo],freq=12,start=c(2011,6))

#Descomposici�n aditiva de la serie
descom=decompose(yt,type="additive")

s=12 #Longitud del periodo estacional

#Componente estacional de la descomposici�n
St=descom$seasonal

plot(St)

deltas_i=factoresdeltai(descom=descom,s=12,estacionini=1) #Obteniendo valor de los s factores estacionales estimados
                                                      #el per�odo es s=12 y la serie arranca en estaci�n 1
data.frame(deltas_i)

#Pron�sticos para la componente estacional
i=c(6,7,8,9,10,11,12,1,2,3,4,5,6) #identificando la estaci�n correspondiente a los m=13 per�odos de pron�stico

Stnuevo=deltas_i[i] #Asignando el valor de St a los per�odos a pronosticar
Stnuevo=ts(Stnuevo,frequency=12,start=c(2011,6)) #convirtiendo en serie de tiempo al pron�stico de St
Stnuevo

#Desestacionalizando o ajustando estacionalmente a la serie
ytd=yt-St

#LOESS lineal �ptimo (AICC) sobre serie desestacionalizada
ajusteLoess1=loess.as(t,ytd,degree=1,criterion="aicc",family="gaussian",plot=F)
summary(ajusteLoess1)
alfa.optim1=ajusteLoess1$pars$span #guardando el valor �ptimo del par�metro alfa

#LOESS lineal �ptimo (GCV) sobre serie desestacionalizada
ajusteLoess1b=loess.as(t,ytd,degree=1,criterion="gcv",family="gaussian",plot=F)
summary(ajusteLoess1b)
alfa.optim1b=ajusteLoess1b$pars$span #guardando el valor �ptimo del par�metro alfa

#LOESS cuadr�tico (AICC) sobre serie desestacionalizada
ajusteLoess2=loess.as(t,ytd,degree=2,criterion="aicc",family="gaussian",plot=F)
summary(ajusteLoess2)
alfa.optim2=ajusteLoess2$pars$span #guardando el valor �ptimo del par�metro alfa

#LOESS cuadr�tico (GCV) sobre serie desestacionalizada
ajusteLoess2b=loess.as(t,ytd,degree=2,criterion="gcv",family="gaussian",plot=F)
summary(ajusteLoess2b)
alfa.optim2b=ajusteLoess2b$pars$span #guardando el valor �ptimo del par�metro alfa

#Series de tiempo de los valores de tendencia ajustada
Tt1=ts(fitted(ajusteLoess1),frequency=12,start=c(1990,1)) #Tendencia ajustada por Modelo 1
Tt1b=ts(fitted(ajusteLoess1b),frequency=12,start=c(1990,1)) #Tendencia ajustada por Modelo 1b
Tt2=ts(fitted(ajusteLoess2),frequency=12,start=c(1990,1)) #Tendencia ajustada por Modelo 2
Tt2b=ts(fitted(ajusteLoess2b),frequency=12,start=c(1990,1)) #Tendencia ajustada por Modelo 2b

#GR�FICOS DE LA SERIE DESESTACIONALIZADA Y SUS AJUSTES LOESS
plot(ytd)
lines(Tt1,col=2)
legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS lineal Criterio AICC"),col=c(1,2),lty=1)

plot(ytd)
lines(Tt1b,col=2)
legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS lineal, criterio GCV"),col=c(1,2),lty=1)

plot(ytd)
lines(Tt2,col=2)
legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS cuadr�tico (AICC)"),col=c(1,2),lty=1)

plot(ytd)
lines(Tt2b,col=2)
legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS cuadr�tico (GCV)"),col=c(1,2),lty=1)

#AJUSTES DE LA SERIE POR DESCOMPOSICI�N ADITIVA & LOESS
ythat1=Tt1+St #Ajuste en Modelo 1
ythat1b=Tt1b+St #Ajuste en Modelo 1b
ythat2=Tt2+St #Ajuste en Modelo 2
ythat2b=Tt2b+St #Ajuste en Modelo 2b

#GRAFICANDO LA SERIE Y SUS AJUSTES POR DESCOMPOSICI�N & LOESS
plot(salario1990_2012)
lines(ythat1,col=2)
legend("topleft",legend=c("Original","Ajuste por D&LL (AICC)"),col=c(1,2),lty=1)

plot(salario1990_2012)
lines(ythat1b,col=2)
legend("topleft",legend=c("Original","Ajuste D&LL(GCV)"),col=c(1,2),lty=1)

plot(salario1990_2012)
lines(ythat2,col=2)
legend("topleft",legend=c("Original","Ajuste D&LC(AICC)"),col=c(1,2),lty=1)

plot(salario1990_2012)
lines(ythat2b,col=2)
legend("topleft",legend=c("Original","Ajuste D&LC(GCV)"),col=c(1,2),lty=1)


#Residuales de ajuste del modelo 1
et1=yt-ythat1

df1=n-(round(ajusteLoess1$enp)+s-1) #Grados de libertad aproximados del ajuste modelo 1
MSE1=sum(et1^2)/df1 #MSE aproximado del ajuste total del modelo 1
MSE1

#Residuales de ajuste del modelo 1b
et1b=yt-ythat1b

df1b=n-(round(ajusteLoess1b$enp)+s-1) #Grados de libertad aproximados del ajuste modelo 1b
MSE1b=sum(et1b^2)/df1b #MSE aproximado del ajuste total del modelo 1b
MSE1b

#Residuales de ajuste del modelo 2
et2=yt-ythat2

df2=n-(round(ajusteLoess2$enp)+s-1) #Grados de libertad aproximados del ajuste modelo 2
MSE2=sum(et2^2)/df2 #MSE aproximado del ajuste total del modelo 2
MSE2

#Residuales de ajuste del modelo 2b
et2b=yt-ythat2b

df2b=n-(round(ajusteLoess2b$enp)+s-1) #Grados de libertad aproximados del ajuste modelo 2b
MSE2b=sum(et2b^2)/df2b #MSE aproximado del ajuste total del modelo 2b
MSE2b

#C�lculo AIC y BIC aproximados versi�n exp(Cn*(p))
p1=round(ajusteLoess1$enp)+s-1 #n�mero aproximado de par�metros en ajuste Modelo 1
p1b=round(ajusteLoess1b$enp)+s-1 #n�mero aproximado de par�metros en ajuste Modelo 1b
p2=round(ajusteLoess2$enp)+s-1 #n�mero aproximado de par�metros en ajuste Modelo 2
p2b=round(ajusteLoess2b$enp)+s-1 #n�mero aproximado de par�metros en ajuste Modelo 2b

AIC1=exp(crit.inf.resid(residuales=et1,n.par=p1));AIC1
AIC1b=exp(crit.inf.resid(residuales=et1b,n.par=p1b));AIC1b
AIC2=exp(crit.inf.resid(residuales=et2,n.par=p2));AIC2
AIC2b=exp(crit.inf.resid(residuales=et2b,n.par=p2b));AIC2b

BIC1=exp(crit.inf.resid(residuales=et1,n.par=p1,AIC="FALSE"));BIC1
BIC1b=exp(crit.inf.resid(residuales=et1b,n.par=p1b,AIC="FALSE"));BIC1b
BIC2=exp(crit.inf.resid(residuales=et2,n.par=p2,AIC="FALSE"));BIC2
BIC2b=exp(crit.inf.resid(residuales=et2b,n.par=p2b,AIC="FALSE"));BIC2b

#GR�FICOS DE RESIDUALES MODELOS POR DESCOMPOSICI�N & LOESS
#Residuos vs. tiempo
plot(et1,ylim=c(min(-2*sqrt(MSE1),et1),max(2*sqrt(MSE1),et1)))
abline(h=c(-2*sqrt(MSE1),0,2*sqrt(MSE1)),col=2)
legend("topleft",legend="Modelo 1")

plot(et1b,ylim=c(min(-2*sqrt(MSE1b),et1b),max(2*sqrt(MSE1b),et1b)))
abline(h=c(-2*sqrt(MSE1b),0,2*sqrt(MSE1b)),col=2)
legend("topleft",legend="Modelo 1b")

plot(et2,ylim=c(min(-2*sqrt(MSE2),et2),max(2*sqrt(MSE2),et2)))
abline(h=c(-2*sqrt(MSE2),0,2*sqrt(MSE2)),col=2)
legend("topleft",legend="Modelo 2")

plot(et2b,ylim=c(min(-2*sqrt(MSE2b),et2b),max(2*sqrt(MSE2b),et2b)))
abline(h=c(-2*sqrt(MSE2b),0,2*sqrt(MSE2b)),col=2)
legend("topleft",legend="Modelo 2b")

#Residuos vs. valores ajustados
plot(as.numeric(ythat1),et1,ylim=c(min(-2*sqrt(MSE1),et1),max(2*sqrt(MSE1),et1)))
abline(h=c(-2*sqrt(MSE1),0,2*sqrt(MSE1)),col=2)
legend("topleft",legend="Modelo 1")

plot(as.numeric(ythat1b),et1b,ylim=c(min(-2*sqrt(MSE1b),et1b),max(2*sqrt(MSE1b),et1b)))
abline(h=c(-2*sqrt(MSE1b),0,2*sqrt(MSE1b)),col=2)
legend("topleft",legend="Modelo 1b")

plot(as.numeric(ythat2),et2,ylim=c(min(-2*sqrt(MSE2),et2),max(2*sqrt(MSE2),et2)))
abline(h=c(-2*sqrt(MSE2),0,2*sqrt(MSE2)),col=2)
legend("topleft",legend="Modelo 2")

plot(as.numeric(ythat2b),et2b,ylim=c(min(-2*sqrt(MSE2b),et2b),max(2*sqrt(MSE2b),et2b)))
abline(h=c(-2*sqrt(MSE2b),0,2*sqrt(MSE2b)),col=2)
legend("topleft",legend="Modelo 2b")

#PRON�STICOS LOESS DE LA TENDENCIA
#Pron�sticos de tendencia por loess lineal �ptimo (AICC)
Ttnuevo1=predict(loess(ytd~t,span=alfa.optim1,degree=1,control=loess.control(surface="direct")),data.frame(t=tnuevo),se=FALSE)
Ttnuevo1=ts(Ttnuevo1,freq=12,start=c(2011,6))#convirtiendo en serie de tiempo al pron�stico de Tt, modelo 1
Ttnuevo1

#Pron�sticos de tendencia por loess lineal �ptimo (GCV)
Ttnuevo1b=predict(loess(ytd~t,span=alfa.optim1b,degree=1,control=loess.control(surface="direct")),data.frame(t=tnuevo),se=FALSE)
Ttnuevo1b=ts(Ttnuevo1b,freq=12,start=c(2011,6))#convirtiendo en serie de tiempo al pron�stico de Tt, modelo 1b
Ttnuevo1b

#Pron�sticos de la tendencia por loess cuadr�tico �ptimo (AICC)
Ttnuevo2=predict(loess(ytd~t,span=alfa.optim2,degree=2,control=loess.control(surface="direct")),data.frame(t=tnuevo),se=FALSE)
Ttnuevo2=ts(Ttnuevo2,freq=12,start=c(2011,6))#convirtiendo en serie de tiempo al pron�stico de Tt, modelo 2
Ttnuevo2

#Pron�sticos de la tendencia por loess cuadr�tico �ptimo (GCV)
Ttnuevo2b=predict(loess(ytd~t,span=alfa.optim2b,degree=2,control=loess.control(surface="direct")),data.frame(t=tnuevo),se=FALSE)
Ttnuevo2b=ts(Ttnuevo2b,freq=12,start=c(2011,6))#convirtiendo en serie de tiempo al pron�stico de Tt, modelo 2b
Ttnuevo2b

#PRON�STICOS DE LA SERIE POR DESCOMPOSICI�N & LOESS
ytpron1=Ttnuevo1+Stnuevo #Pron�stico puntual Modelo 1
ytpron1

#Tabla con pron�sticos de las componentes y de la serie, Modelo 1
tablapron1=cbind(Pron_Tt=Ttnuevo1,Pron_St=Stnuevo,Pron_serie=ytpron1)
tablapron1

ytpron1b=Ttnuevo1b+Stnuevo #Pron�stico puntual Modelo 1b
ytpron1b

#Tabla con pron�sticos de las componentes y de la serie, Modelo 1b
tablapron1b=cbind(Pron_Tt=Ttnuevo1b,Pron_St=Stnuevo,Pron_serie=ytpron1b)
tablapron1b

ytpron2=Ttnuevo2+Stnuevo #Pron�stico puntual Modelo 2

#Tabla con pron�sticos de las componentes y de la serie, Modelo 2
tablapron2=cbind(Pron_Tt=Ttnuevo2,Pron_St=Stnuevo,Pron_serie=ytpron2)
tablapron2

ytpron2b=Ttnuevo2b+Stnuevo #Pron�stico puntual Modelo 2b

#Tabla con pron�sticos de las componentes y de la serie, Modelo 2b
tablapron2b=cbind(Pron_Tt=Ttnuevo2b,Pron_St=Stnuevo,Pron_serie=ytpron2b)
tablapron2b

#Calculando medidas de precisi�n de pron�sticos del modelo 1
accuracy(ytpron1,ytf)

#Calculando medidas de precisi�n de pron�sticos del modelo 1b
accuracy(ytpron1b,ytf)

#Calculando medidas de precisi�n de pron�sticos del modelo 2
accuracy(ytpron2,ytf)

#Calculando medidas de precisi�n de pron�sticos del modelo 2b
accuracy(ytpron2b,ytf)

#Suavizamiento exponencial Holt-Winters aditivo
suav=HoltWinters(yt,seasonal="additive")
suav

plot(salario1990_2012)
lines(fitted(suav)[,1],col=2)
legend("topleft",legend=c("Original","Ajuste H-W"),col=c(1,2),lty=1)

#C�lculo de AIC y BIC aproximados con exp(Cn*(p))
p3=(s-1)+2 #Aprox. del n�mero de par�metros del suavizamiento
AIC3=exp(crit.inf.resid(residuales=residuals(suav),n.par=p3)); AIC3
BIC3=exp(crit.inf.resid(residuales=residuals(suav),n.par=p3,AIC="FALSE")); BIC3

df3=n-2*s-((s-1)+2)
MSE3=suav$SSE/df3 #MSE aproximado del ajuste total del Suavizamiento
MSE3

#GR�FICOS DE RESIDUALES MODELO 3
plot(residuals(suav),ylim=c(min(-2*sqrt(MSE3),residuals(suav)),max(2*sqrt(MSE3),residuals(suav))))
abline(h=c(-2*sqrt(MSE3),0,2*sqrt(MSE3)),col=2)
legend("topleft",legend="Modelo 3")

plot(as.numeric(fitted(suav)[,1]),residuals(suav),ylim=c(min(-2*sqrt(MSE3),residuals(suav)),max(2*sqrt(MSE3),residuals(suav))))
abline(h=c(-2*sqrt(MSE3),0,2*sqrt(MSE3)),col=2)
legend("topleft",legend="Modelo 3")

#Pron�sticos e I.P del 95% del suavizamiento
pronos3=predict(suav, n.ahead=13,prediction=T,level=0.95)
pronos3

ytpron3=pronos3[,1] #s�lo los pron�sticos puntuales del suavizamiento

accuracy(ytpron3,ytf) #Precisi�n pron�sticos puntuales

#Precisi�n pron�sticos por I.P.
#el Lim. inferior de pron�stico en el pron�stico HoltWinters es la tercera columna de pronos3
#el Lim. superior de pron�stico en el pron�stico HoltWinters es la segunda columna de pronos3

amplitud(LIP=pronos3[,3],LSP=pronos3[,2])
cobertura(real=ytf,LIP=pronos3[,3],LSP=pronos3[,2])

#COMPARACI�N DE EFECTOS ESTACIONALES ESTIMADOS
#extracci�n de las estimaciones en t=257 de los efectos estacionales seg�n Holt-Winters
#en el vector de coeficientes (que es de longitud s+2=14 e inicia con los valores de nivel y pendiente), 
#los efectos de enero-mayo est�n en este ejemplo, en las posiciones 10 a 14, 
#mientras que los correspondientes a junio-diciembre est�n en las posiciones 3-9 
#Ver Tabla 2 en documento del ejemplo

deltasiHW=ts(suav$coef[c(10:14,3:9)],freq=1,start=1)

#Estimaciones de los efectos estacionales seg�n filtro de descomposici�n
deltasDescomp=ts(deltas_i,freq=1,start=1)

#Gr�fico de los efectos estacionales estimados
plot(deltasiHW,lwd=3,ylim=c(min(deltasiHW,deltasDescomp),max(deltasiHW,deltasDescomp)+2),ylab="",xlab="Mes del a�o")
lines(deltasDescomp,lty=2,lwd=3,col=2)
legend("topleft",legend=c("Efectos estacionales H-W en t=257","Efectos estacionales Filtro de descomposici�n"),col=1:2,lty=1:2,lwd=3)

#Tabla n�mero de par�metros y criterios de informaci�n
tablacriter=cbind(p=c(p1,p1b,p2,p2b,p3),AIC=c(AIC1,AIC1b,AIC2,AIC2b,AIC3),BIC=c(BIC1,BIC1b,BIC2,BIC2b,BIC3))
rownames(tablacriter)=c("Modelo 1","Modelo 1b","Modelo 2","Modelo 2b","Modelo 3")
tablacriter

#Tabla con medidas de precisi�n de pron�sticos puntuales
tablaprecistodos=rbind(DLLAICC=accuracy(ytpron1,ytf),DLLGCV=accuracy(ytpron1b,ytf),DLCAICC=accuracy(ytpron2,ytf),DLCGCV=accuracy(ytpron2b,ytf),HW=accuracy(ytpron3,ytf))[,c(2,3,5)]
rownames(tablaprecistodos)=c("Modelo 1","Modelo 1b","Modelo 2","Modelo 2b","Modelo 3")
tablaprecistodos

#Comparando los pron�sticos de los cuatro ajustes locales por Descomposici�n & Loess y Holt-Winters aditivo
plot(ytf,type="b",pch=19,lty=1,col=1,lwd=2,ylab="Salario real 2011-junio a 2012-junio",ylim=c(min(ytf,ytpron1,ytpron1b,ytpron2,ytpron2b,ytpron3),max(ytf,ytpron1,ytpron1b,ytpron2,ytpron2b,ytpron3)),xaxt="n")
lines(ytpron1,col=2,pch=2,lty=2,type="b",lwd=2) 
lines(ytpron1b,col=3,pch=3,lty=3,type="b",lwd=2)
lines(ytpron2,col=4,pch=4,lty=4,type="b",lwd=2) 
lines(ytpron2b,col=5,pch=5,lty=5,type="b",lwd=2) 
lines(ytpron3,col=6,pch=6,lty=6,type="b",lwd=2) 


#Colocando leyenda en esquina superior izquierda para identificar las series graficadas
#debe usarse los mismos formatos de color (argumento col), tipo de s�mbolo gr�fico (argumento pch)y tipo de 
#l�neas (argumento lty) usados para cada serie previamente graficada
#las etiquetas "Real", "D&LL(AICC)", "D&LL(GCV)", "D&LC(AICC)", "D&LC(GCV)","HW", corresponden en su orden a los objetos R ytf, ytpron1, ytpron1b, ytpron2, ytpron2b, ytpron3. 

legend("topleft",legend=c("Real","D&LL(AICC)","D&LL(GCV)","D&LC(AICC)","D&LC(GCV)","HW"),bty="n",col=1:6,pch=c(19,2:6),lty=1:6,lwd=2)

#Etiquetando valores en eje x con las fechas de los m=13 pron�sticos
axis(1,at=time(ytf),labels=c("jun-11","jul-11","ago-11","sep-11","oct-11","nov-11","dic-11","ene-12","feb-12","mar-12","abr-12","may-12","jun-12"))
