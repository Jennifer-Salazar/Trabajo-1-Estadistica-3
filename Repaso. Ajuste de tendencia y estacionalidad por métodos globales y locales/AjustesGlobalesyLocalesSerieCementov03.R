#CARGANDO LIBRERÍAS USADAS
library(TSA)
library(forecast)
library(fANCOVA)

#Creando función usuario crit.inf.resid() para calcular C*n(p)
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

#Función para obtener las estimaciones de los factores estacionales deltai 
#en la descomposición clásica
#el argumento descom es un objeto que supone guardó el resultado de la función decompose()
#el argumento s es la longitud del periodo estacional
#el argumento estacionini es el número de la estación que corresponde a t=1
factoresdeltai=function(descom,s,estacionini){
if(estacionini==1){
deltasi=descom$figure
}
if(estacionini!=1){
j=estacionini
deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
}
deltasi
}

amplitud=function(LIP,LSP){
a=LSP-LIP
am=mean(a)
am
}

cobertura=function(real,LIP,LSP){
I=ifelse(real>=LIP & real<=LSP,1,0)
p=mean(I)
p
}

#INGRESANDO LOS DATOS 
yt=scan()
465 532 561 570 529 604 603 582 554 620 646
637 573 673 690 681 621 698 753 728 688 737
782 692 637 757 783 757 674 734 835 838 797
904 949 975 902 974 969 967 849 961 966 922
836 998 1025 971 892 973 1047 1017 948 1032
1190 1136 1049 1134 1229 1188 1058 1209 1199
1253 1070 1282 1303 1281 1148 1305 1342 1452
1184 1352 1316 1353 1121 1297 1318 1281 1109
1299 1341 1290 1101 1284 1321 1317 1122 1261
1312 1298 1202 1302 1377 1359 1232 1386 1440
1439 1282 1573 1533 1651 1347 1575 1475 1357
1086 1158 1279 1313 1166 1373 1456 1496 1251
1456 1631 1554 1347 1516 1546 1564 1333 1458
1499 1613 1416 1625 1770 1791 1622 1719 1972
1893 1575 1644 1658 1668 1343 1441 1444 1497
1267 1501 1538 1569 1450 1569 1648 1777 1468
1732 1962

#CREANDO SERIE DE TIEMPO Y GRAFICANDO SERIE ORIGINAL Y SU LOGARITMO NATURAL
yt=ts(yt,frequency=4,start=c(1956,1)) #serie con todas las observaciones
plot(yt,ylab="Miles de toneladas")
plot(log(yt),ylab="Log de Miles de toneladas")

plot(decompose(log(yt))$trend,ylim=c(min(log(yt)),max(log(yt))))

boxplot(log(yt)~cycle(log(yt)),names=c("Q1","Q2","Q3","Q4")) #boxplots según trimestres, para el logaritmo natural

periodogram(diff(log(yt)),lwd=4) #periodograma sobre los logaritmos diferenciados
abline(v=c(0.25,0.5),col=2,lty=2)

#DEFINIENDO HORIZONTE DE PRONÓSTICO Y TAMAÑO DE MUESTRA PARA AJUSTE Y
#VALIDACIÓN CRUZADA. SE DEFINE TAMBIÉN LONGITUD PERIODO ESTACIONAL
m=4 #Número de períodos a pronosticar dentro de la muestra
n=length(yt)-m #tamaño de la muestra para el ajuste
s=4

#DEFINIENDO VARIABLES PARA EL AJUSTE 
#USANDO SÓLO LOS n PRIMEROS DATOS
t=1:n #índice de tiempo

yt2=ts(yt[t],frequency=4,start=c(1956,1)) #serie con las primeras n observaciones

#DEFINIENDO INDICADORAS PARA ESTACIONALIDAD
#pueden usarse en modelos de regresión lineal y en
#modelos de regresión no lineal
trim=seasonaldummy(yt2) #observe que se usa función seasonaldummy()
Q1=trim[,1]
Q2=trim[,2]
Q3=trim[,3]


#DEFINIENDO VALORES EN LOS PERÍODOS DE PRONÓSTICO PARA 
#EL ÍNDICE DE TIEMPO, LAS VARIABLES RELACIONADAS CON St Y DE LA SERIE 

tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
trimnuevo=seasonaldummy(yt2,h=m) #valores de las variables indicadoras en los pronósticos
Q1nuevo=trimnuevo[,1]
Q2nuevo=trimnuevo[,2]
Q3nuevo=trimnuevo[,3]

ytf=ts(yt[tnuevo],frequency=4,start=c(1993,4)) #Tomando de la serie completa los últimos m valores que son pronosticados

#####AJUSTE POR REGRESIÓN CON TENDENCIA CUADRÁTICA Y ESTACIONALIDAD TRIMESTRAL########
#####MODELO LOG-CUADRÁTICO ESTACIONAL#################################################
lnyt2=log(yt2)

modelo1=lm(lnyt2~t+I(t^2)+Q1+Q2+Q3) 
summary(modelo1)

#Serie de tiempo de valores ajustados en escala original
ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(1956,1))

#GRAFICANDO LA SERIE, SUS AJUSTES MODELO 1
plot(yt)
lines(ythat1,col=2)
legend("topleft",legend=c("Original","Ajustada"),col=c(1,2),lty=1)

#Calculando AIC y BIC usando exp(C*n(p))
#Primero calculamos los seudoresiduos del modelo log
resmod1.orig=yt2-ythat1
npar1=length(coef(modelo1)[coef(modelo1)!=0]); npar1 #número parámetros modelo 1
aic1=exp(crit.inf.resid(resmod1.orig,n.par=npar1))
aic1
bic1=exp(crit.inf.resid(resmod1.orig,n.par=npar1,AIC="FALSE"))
bic1

#GRÁFICO DE RESIDUALES EN ESCALA LOG
plot.ts(residuals(modelo1),ylim=c(min(residuals(modelo1),-2*summary(modelo1)$sigma),max(residuals(modelo1),2*summary(modelo1)$sigma)))
abline(h=c(-2*summary(modelo1)$sigma,0,2*summary(modelo1)$sigma),col=2)

plot(fitted(modelo1),residuals(modelo1),ylim=c(min(residuals(modelo1),-2*summary(modelo1)$sigma),max(residuals(modelo1),2*summary(modelo1)$sigma)))
abline(h=c(-2*summary(modelo1)$sigma,0,2*summary(modelo1)$sigma),col=2)

#PRONOSTICANDO PERÍODOS t=152 a 155 EN ESCALA ORIGINAL
predicciones1=exp(predict(modelo1,data.frame(t=tnuevo,Q1=Q1nuevo,Q2=Q2nuevo,Q3=Q3nuevo),interval="prediction"))*exp(summary(modelo1)$sigma^2/2)
predicciones1=ts(predicciones1,frequency=4,start=c(1993,4))
predicciones1

#Convirtiendo en serie de tiempo los pronósticos
ytpron1=ts(predicciones1[,1],frequency=4,start=c(1993,4)) #los pronósticos comienzan desde 1993-Q4

accuracy(ytpron1,ytf) #Calculando exactitud de los pronósticos puntuales

#Amplitud media y cobertura I.P
Amplmodelo1=amplitud(LIP=predicciones1[,2],LSP=predicciones1[,3])
Amplmodelo1

Cobmodelo1=cobertura(real=ytf,LIP=predicciones1[,2],LSP=predicciones1[,3])
Cobmodelo1

#MODELO LOG CÚBICO ESTACIONAL
modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+Q1+Q2+Q3) 
summary(modelo1b)

#Serie de tiempo de valores ajustados en escala original
ythat1b=ts(exp(fitted(modelo1b))*exp(summary(modelo1b)$sigma^2/2),frequency=4,start=c(1956,1))

#GRAFICANDO LA SERIE, SUS AJUSTES MODELO1b 
plot(yt)
lines(ythat1b,col=2)
legend("topleft",legend=c("Original","Ajustada"),col=c(1,2),lty=1)

#Calculando AIC y BIC usando exp(C*n(p))
#Primero calculamos los seudoresiduos del modelo log
resmod1b.orig=yt2-ythat1b 
npar1b=length(coef(modelo1b)[coef(modelo1b)!=0]); npar1b #número parámetros modelo 1b
aic1b=exp(crit.inf.resid(resmod1b.orig,n.par=npar1b))
aic1b

bic1b=exp(crit.inf.resid(resmod1b.orig,n.par=npar1b,AIC="FALSE"))
bic1b

#GRÁFICO DE RESIDUALES EN ESCALA LOG
plot.ts(residuals(modelo1b),ylim=c(min(residuals(modelo1b),-2*summary(modelo1b)$sigma),max(residuals(modelo1b),2*summary(modelo1b)$sigma)))
abline(h=c(-2*summary(modelo1b)$sigma,0,2*summary(modelo1b)$sigma),col=2)

plot(fitted(modelo1b),residuals(modelo1b),ylim=c(min(residuals(modelo1b),-2*summary(modelo1b)$sigma),max(residuals(modelo1b),2*summary(modelo1b)$sigma)))
abline(h=c(-2*summary(modelo1b)$sigma,0,2*summary(modelo1b)$sigma),col=2)

#PRONOSTICANDO PERÍODOS t=152 a 155 EN ESCALA ORIGINAL
predicciones1b=exp(predict(modelo1b,data.frame(t=tnuevo,Q1=Q1nuevo,Q2=Q2nuevo,Q3=Q3nuevo),interval="prediction"))*exp(summary(modelo1b)$sigma^2/2)
predicciones1b

#Convirtiendo en serie de tiempo los pronósticos
ytpron1b=ts(predicciones1b[,1],frequency=4,start=c(1993,4)) #los pronósticos comienzan desde 1993-Q4

accuracy(ytpron1b,ytf) #Calculando exactitud de los pronósticos

#Amplitud media y cobertura de los I.P
Amplmodelo1b=amplitud(LIP=predicciones1b[,2],LSP=predicciones1b[,3])
Amplmodelo1b

Cobmodelo1b=cobertura(real=ytf,LIP=predicciones1b[,2],LSP=predicciones1b[,3])
Cobmodelo1b

###AJUSTE Y PRONÓSTICO POR MODELO EXPONENCIAL ESTACIONAL################

#AJUSTANDO MODELO EXPONENCIAL CUADRÁTICO ESTACIONAL USANDO COMO VALORES INICIALES
#PARÁMETROS AJUSTADOS DEL MODELO 1, SON SEIS EN TOTAL CON beta0
coef0=coef(modelo1) #extrae coeficientes ajustados del modelo1

modelo2=nls(yt2~exp(beta0+beta1*t+beta2*I(t^2)+delta1*Q1+delta2*Q2+delta3*Q3),
start=list(beta0=coef0[[1]],beta1=coef0[[2]],beta2=coef0[[3]],delta1=coef0[[4]],delta2=coef0[[5]],delta3=coef0[[6]]))
summary(modelo2)

#Serie de tiempo de valores ajustados
ythat2=ts(fitted(modelo2),frequency=4,start=c(1956,1))

#GRAFICANDO LA SERIE, SUS AJUSTES MODELO 2
plot(yt)
lines(ythat2,col=2)
legend("topleft",legend=c("Original","Ajustada"),col=c(1,2),lty=1)

#Calculando AIC y BIC usando exp(C*n(p))
npar2=length(coef(modelo2)[coef(modelo2)!=0]); npar2 #número parámetros modelo 2
aic2=exp(crit.inf.resid(residuals(modelo2),n.par=npar2))
aic2

bic2=exp(crit.inf.resid(residuals(modelo2),n.par=npar2,AIC="FALSE"))
bic2

#GRÁFICOS DE RESIDUALES
plot.ts(residuals(modelo2),ylim=c(min(residuals(modelo2),-2*summary(modelo2)$sigma),max(residuals(modelo2),2*summary(modelo2)$sigma)))
abline(h=c(-2*summary(modelo2)$sigma,0,2*summary(modelo2)$sigma),col=2)

plot(fitted(modelo2),residuals(modelo2),ylim=c(min(residuals(modelo2),-2*summary(modelo2)$sigma),max(residuals(modelo2),2*summary(modelo2)$sigma)))
abline(h=c(-2*summary(modelo2)$sigma,0,2*summary(modelo2)$sigma),col=2)

#PRONÓSTICOS DEL MODELO EXPONENCIAL CUADRÁTICO ESTACIONAL
predicciones2=predict(modelo2,newdata=data.frame(t=tnuevo,Q1=Q1nuevo,Q2=Q2nuevo,Q3=Q3nuevo),interval="prediction")
predicciones2

#Convirtiendo en serie de tiempo las predicciones
ytpron2=ts(predicciones2,frequency=4,start=c(1993,4))

accuracy(ytpron2,ytf) #Calculando exactitud de los pronósticos

#AJUSTANDO MODELO EXPONENCIAL CÚBICO ESTACIONAL USANDO COMO VALORES INICIALES
#PARÁMETROS AJUSTADOS DEL MODELO 1b, SON SIETE EN TOTAL CON beta0
coef0b=coef(modelo1b) #extrae coeficientes ajustados del modelo1b

modelo2b=nls(yt2~exp(beta0+beta1*t+beta2*I(t^2)+beta3*I(t^3)+delta1*Q1+delta2*Q2+delta3*Q3),
start=list(beta0=coef0b[[1]],beta1=coef0b[[2]],beta2=coef0b[[3]],beta3=coef0b[[4]],delta1=coef0b[[5]],delta2=coef0b[[6]],delta3=coef0b[[7]]))
summary(modelo2b)

#Serie de tiempo de valores ajustados
ythat2b=ts(fitted(modelo2b),frequency=4,start=c(1956,1))

#GRAFICANDO LA SERIE, SUS AJUSTES Y PRONÓSTICOS 
plot(yt)
lines(ythat2b,col=2)
legend("topleft",legend=c("Original","Ajustada"),col=c(1,2),lty=1)


#Calculando AIC y BIC usando exp(C*n(p))
npar2b=length(coef(modelo2b)[coef(modelo2b)!=0]); npar2b #número parámetros modelo 2b
aic2b=exp(crit.inf.resid(residuals(modelo2b),n.par=npar2b))
aic2b

bic2b=exp(crit.inf.resid(residuals(modelo2b),n.par=npar2b,AIC="FALSE"))
bic2b

#GRÁFICOS DE RESIDUALES
plot.ts(residuals(modelo2b),ylim=c(min(residuals(modelo2b),-2*summary(modelo2b)$sigma),max(residuals(modelo2b),2*summary(modelo2b)$sigma)))
abline(h=c(-2*summary(modelo2b)$sigma,0,2*summary(modelo2b)$sigma),col=2)

plot(fitted(modelo2b),residuals(modelo2b),ylim=c(min(residuals(modelo2b),-2*summary(modelo2b)$sigma),max(residuals(modelo2b),2*summary(modelo2b)$sigma)))
abline(h=c(-2*summary(modelo2b)$sigma,0,2*summary(modelo2b)$sigma),col=2)

#PRONÓSTICOS DEL MODELO EXPONENCIAL CÚBICO ESTACIONAL
predicciones2b=predict(modelo2b,newdata=data.frame(t=tnuevo,Q1=Q1nuevo,Q2=Q2nuevo,Q3=Q3nuevo),interval="prediction")
predicciones2b

#Convirtiendo en serie de tiempo las predicciones
ytpron2b=ts(predicciones2b,frequency=4,start=c(1993,4))
ytpron2b

accuracy(ytpron2b,ytf) #Calculando exactitud de los pronósticos


#Calculando exp(delta_i estimado)*100 en modelos globales
efectosestac1=ts(c(exp(coef(modelo1)[4:6])*100,100),freq=1,start=1) #parámetros 4 a 6 son los deltas_i
efectosestac1b=ts(c(exp(coef(modelo1b)[5:7])*100,100),freq=1,start=1) #parámetros 5 a 7 son los deltas_i
efectosestac2=ts(c(exp(coef(modelo2)[4:6])*100,100),freq=1,start=1) #parámetros 4 a 6 son los deltas_i
efectosestac2b=ts(c(exp(coef(modelo2b)[5:7])*100,100),freq=1,start=1) #parámetros 5 a 7 son los deltas_i

par(mar=c(5,5,4,2))
plot(efectosestac1,lwd=2,las=2,type="b",pch=19,main="Porcentaje estimado de la media\n de la serie en trimestre i con relación al trimeste 4",ylim=c(86,102),xaxt="n",xlab="Trimestre",ylab=expression(paste(exp(hat(delta)[i]),sep="","*",sep="","100%")))
lines(efectosestac1b,type="b",pch=2,lty=2,col=2,lwd=2)
lines(efectosestac2,type="b",pch=3,lty=3,col=3,lwd=2)
lines(efectosestac2b,type="b",pch=4,lty=4,col=4,lwd=2)
axis(1,at=1:4,labels=c("Q1","Q2","Q3","Q4"))
legend("bottomright",legend=c("Modelo 1","Modelo 1b","Modelo 2","Modelo 2b"),pch=c(19,2:4),col=1:4,lty=1:4,lwd=2)


###AJUSTES Y PRONÓSTICOS POR HOLTWINTERS MULTIPLICATIVO###########
suaviza=HoltWinters(yt2,seasonal="multiplicative")
suaviza

ythat3=fitted(suaviza)[,1] #valores ajustados. Ya tienen formato de serie de tiempo

#GRAFICANDO LA SERIE, SUS AJUSTES Y PRONÓSTICOS
plot(yt)
lines(ythat3,col=2)
legend("topleft",legend=c("Original","Ajustada"),col=c(1,2),lty=1)

et3=residuals(suaviza) #residuales. Ya tienen formato de serie de tiempo

#Calculando AIC y BIC usando exp(C*n(p))
npar3=(s-1)+2;npar3 #Aprox. del número de parámetros del suavizamiento
aic3=exp(crit.inf.resid(residuals(suaviza),n.par=npar3)) #número de parámetros es (s-1)+2=5
aic3

bic3=exp(crit.inf.resid(residuals(suaviza),n.par=npar3,AIC="FALSE"))
bic3

df3=n-2*s-((s-1)+2)
MSE3=suaviza$SSE/df3 #MSE aproximado del ajuste total del Suavizamiento
MSE3

#GRÁFICOS DE RESIDUALES MODELO 3
plot(et3,ylim=c(min(-2*sqrt(MSE3),et3),max(2*sqrt(MSE3),et3)))
abline(h=c(-2*sqrt(MSE3),0,2*sqrt(MSE3)),col=2)

plot(as.numeric(ythat3),et3,ylim=c(min(-2*sqrt(MSE3),et3),max(2*sqrt(MSE3),et3)))
abline(h=c(-2*sqrt(MSE3),0,2*sqrt(MSE3)),col=2)

#PREDICCIONES Y INTERVALOS DE PRONÓSTICO
predicciones3=predict(suaviza,n.ahead=4,prediction.interval=TRUE)
predicciones3

ytpron3=predicciones3[,1] #Separando los pronosticos puntuales
ytpron3

accuracy(ytpron3,ytf) #Calculando exactitud de los pronósticos

#Amplitud media y cobertura de los I.P
Amplmodelo3=amplitud(LIP=predicciones3[,3],LSP=predicciones3[,2])
Amplmodelo3

Cobmodelo3=cobertura(real=ytf,LIP=predicciones3[,3],LSP=predicciones3[,2])
Cobmodelo3


#####AJUSTES Y PRONÓSTICOS POR DESCOMPOSICIÓN & LOESS CUADRÁTICO#########
#DESCOMPOSICIÓN CLÁSICA POR MEDIAS MÓVILES
des=decompose(yt2,type="multiplicative") #Descomposición multiplicativa

St=des$seasonal #Extrayendo la serie de componente estacional
plot(St)

deltas_i=factoresdeltai(descom=des,s=4,estacionini=1) #Obteniendo valor de los s factores estacionales estimados
                                                      #el período es s=4 y la serie arranca en estación
data.frame(deltas_i)

#Pronósticos para la componente estacional                                                       
i=c(4,1,2,3) #identificando la estación correspondiente a los m=4 períodos de pronóstico
Stnuevo=deltas_i[i]  #Asignando el valor de St a los períodos a pronosticar
Stnuevo=ts(Stnuevo,frequency=4,start=c(1993,4)) #convirtiendo en serie de tiempo al pronóstico de St
Stnuevo

#AJUSTES Y PRONÓSTICOS LOESS ÓPTIMO GRADO 2 DE LA TENDENCIA, USANDO AICC EN LA SELECCIÓN de alpha
yt3=yt2/St  #Serie desestacionalizada en forma multiplicativa
ajusteLoess=loess.as(t,yt3,degree=2,criterion="aicc",family="gaussian",plot=F)
 
summary(ajusteLoess)
Tt=ts(fitted(ajusteLoess),frequency=4,start=c(1956,1)) #tendencia estimada por loess

alfa.optim=ajusteLoess$pars$span #guardando el valor óptimo del parámetro alfa
alfa.optim

plot(yt3) 
lines(Tt,col=2)
legend("topleft",legend=c("Serie ajustada estacionalmente","Ajuste LOESS cuadrático (AICC)"),col=c(1,2),lty=1)

#Pronósticos de sólo tendencia por loess cúadrático óptimo
#sobre la serie desestacionalizada
Ttnuevo=predict(loess(yt3~t,span=alfa.optim,degree=2,control=loess.control(surface="direct")),data.frame(t=tnuevo),se=FALSE)
Ttnuevo=ts(Ttnuevo,freq=4,start=c(1993,4))#convirtiendo en serie de tiempo al pronóstico de Tt
Ttnuevo

#AJUSTE DE LA SERIE COMBINANDO DE FORMA MULTIPLICATIVA
#LOS AJUSTES DE LAS COMPONENTES
ythat4=Tt*St  

#GRAFICANDO LA SERIE, SUS AJUSTES Y PRONÓSTICOS
plot(yt)
lines(ythat4,col=2)
legend("topleft",legend=c("Original","Ajustada"),col=c(1,2),lty=1)

#RESIDUALES
et4=yt2-ythat4

#Calculando AIC y BIC usando exp(C*n(p))
npar4=round(ajusteLoess$enp)+s-1;npar4 #número aproximado de parámetros en ajuste descompos. & loess 
aic4=exp(crit.inf.resid(et4,n.par=npar4)) 
aic4

bic4=exp(crit.inf.resid(et4,n.par=npar4,AIC="FALSE"))
bic4

#GRÁFICOS DE RESIDUALES
df=n-(round(ajusteLoess$enp)+s-1) #Grados de libertad aproximados del ajuste total
MSE4=sum(et4^2)/df #MSE aproximado del ajuste total del modelo 1
MSE4

plot(et4,ylim=c(min(-2*sqrt(MSE4),et4),max(2*sqrt(MSE4),et4)))
abline(h=c(-2*sqrt(MSE4),0,2*sqrt(MSE4)),col=2)

plot(as.numeric(ythat4),et4,ylim=c(min(-2*sqrt(MSE4),et4),max(2*sqrt(MSE4),et4)))
abline(h=c(-2*sqrt(MSE4),0,2*sqrt(MSE4)),col=2)

#Pronóstico de la serie combinando de forma multiplicativa 
#los pronósticos de las componentes
ytpron4=Ttnuevo*Stnuevo
ytpron4

#tabla resumen de los pronósticos de las compponentes y de la serie
tablapron4=cbind(Pron_Tt=Ttnuevo,Pron_St=Stnuevo,Pron_serie=ytpron4)
tablapron4

#Calculando medidas de precisión de pronósticos
accuracy(ytpron4,ytf)

#Extrayendo y comparando efectos estacionales en los dos modelos locales
deltasiHW=ts(suaviza$coef[c(4:6,3)],freq=1,start=1) #coeficientes 4 a 6 son delta _i, para i=1, 2, 3 y coeficiente 3 es delta_4	
deltasDL=ts(deltas_i,freq=1,start=1) #Los delta_i según el filtro de descomposición
win.graph()
par(mar=c(5,5,4,2))
plot(deltasiHW,lwd=2,las=2,ylab=expression(hat(delta[i])),ylim=c(0.88,1.05),type="b",pch=19,main="Efectos estacionales estimados\n Holt Winters (en t=151) y Descomp. & Loess",xlab="Trimestre del año",xaxt="n")
lines(deltasDL,lty=2,type="b",pch=2,lwd=2,col=2)
axis(1,at=1:4,labels=c("Q1","Q2","Q3","Q4"))
legend("topleft",legend=c("H-W","D&LC"),col=1:2,lty=c(1,2),lwd=3,pch=c(19,2))


##COMPARACIÓN GRÁFICA DE LOS SEIS PRONÓSTICOS PRODUCIDOS Y VALORES REALES###
plot(ytf,ylim=c(min(ytf,ytpron1,ytpron1b,ytpron2,ytpron2b,ytpron3,ytpron4),max(ytf,ytpron1,ytpron1b,ytpron2,ytpron2b,ytpron3,ytpron4)),type="b",pch=19,lwd=2,xaxt="n")
axis(1,at=time(ytf),labels=c("1993Q4","1994Q1","1994Q2","1994Q3"))
lines(ytpron1,lty=2,col="red",type="b",pch=2,lwd=2)
lines(ytpron1b,lty=3,col="blue",type="b",pch=3,lwd=2)
lines(ytpron2,lty=4,col="orange",type="b",pch=4,lwd=2)
lines(ytpron2b,lty=5,col="brown",type="b",pch=5,lwd=2)
lines(ytpron3,lty=6,col="darkgreen",type="b",pch=6,lwd=2)
lines(ytpron4,lty=7,col="darkred",type="b",pch=7,lwd=2)
legend("topleft",legend=c("Real","Log-cuadrático estacional","Log-cúbico estacional","Exponencial cuadrático estacional","Exponencial cúbico estacional","Holt-Winters","Descomposición & LOESS"),
lty=c(1:7),pch=c(19,2:7),col=c("black","red","blue","orange","brown","darkgreen","darkred"),lwd=2)

