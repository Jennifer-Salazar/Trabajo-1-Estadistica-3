#Para leer ?anex-EMMET-dic2019-Fabricacion de otros productos qu?micos.csv?, Columna 5: ?ndice de producci?n nominal
Datos20=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos20=ts(Datos20,freq=12,start=c(2001,1))
plot(Datos20)
