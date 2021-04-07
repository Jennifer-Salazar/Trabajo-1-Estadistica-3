#Para leer “anex-EMMET-dic2019-Confeccion prendas de vestir.csv”, Columna 5: índice de producción nominal
Datos1=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos1=ts(Datos1,freq=12,start=c(2001,1))
plot(Datos1)

#Para leer “anex-EMMET-dic2019-Confeccion prendas de vestir.csv”, Columna 7: índice de ventas nominales
Datos2=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos2=ts(Datos2,freq=12,start=c(2001,1))
plot(Datos2)

#Para leer “anex-EMMET-dic2019-Total industria.csv”, Columna 5: índice de producción nominal
Datos3=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos3=ts(Datos3,freq=12,start=c(2001,1))
plot(Datos3)

#Para leer “anex-EMMET-dic2019-Total industria.csv”, Columna 6: índice de producción real
Datos4=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos4=ts(Datos4,freq=12,start=c(2001,1))
plot(Datos4)

#Para leer “anex-EMMET-dic2019-Total industria.csv”, Columna 7: índice de ventas nominales
Datos5=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos5=ts(Datos5,freq=12,start=c(2001,1))
plot(Datos5)

#Para leer “anex-EMMET-dic2019-Total industria.csv”, Columna 8: índice de ventas reales
Datos6=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos6=ts(Datos6,freq=12,start=c(2001,1))
plot(Datos6)

#Para leer “anex-EMMET-dic2019-Elaboracion de azucar y panela.csv”, Columna 5: índice de producción nominal
Datos7=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos7=ts(Datos7,freq=12,start=c(2001,1))
plot(Datos7)

#Para leer “anex-EMMET-dic2019-Elaboracion de azucar y panela.csv”, Columna 6: índice de producción real
Datos8=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos8=ts(Datos8,freq=12,start=c(2001,1))
plot(Datos8)

#Para leer “anex-EMMET-dic2019-Elaboracion de bebidas.csv”, Columna 5: índice de producción nominal
Datos9=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos9=ts(Datos9,freq=12,start=c(2001,1))
plot(Datos9)

#Para leer “anex-EMMET-dic2019-Elaboracion de bebidas.csv”, Columna 6: índice de producción real
Datos10=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos10=ts(Datos10,freq=12,start=c(2001,1))
plot(Datos10)

#Para leer “anex-EMMET-dic2019-Elaboracion de bebidas.csv”, Columna 7: índice de ventas nominales
Datos11=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos11=ts(Datos11,freq=12,start=c(2001,1))
plot(Datos11)

#Para leer “anex-EMMET-dic2019-Elaboracion de bebidas.csv”, Columna 8: índice de ventas reales
Datos12=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos12=ts(Datos12,freq=12,start=c(2001,1))
plot(Datos12)

#Para leer “anex-EMMET-dic2019-Elaboracion de productos de molinería.csv”, Columna 5: índice de producción nominal
Datos13=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos13=ts(Datos13,freq=12,start=c(2001,1))
plot(Datos13)

#Para leer "anex-EMMET-dic2019-Fabricacion de papel carton y sus productos.csv”, Columna 7: índice de ventas nominales
Datos14=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos14=ts(Datos14,freq=12,start=c(2001,1))
plot(Datos14)

#Para leer “anex-EMMET-dic2019-Elaboracion de productos de molinería.csv”, Columna 7: índice de ventas nominales
Datos15=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos15=ts(Datos15,freq=12,start=c(2001,1))
plot(Datos15)

#Para leer “anex-EMMET-dic2019-Elaboracion de productos lácteos.csv”, Columna 5: índice de producción nominal
Datos16=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos16=ts(Datos16,freq=12,start=c(2001,1))
plot(Datos16)

 
#Para leer “anex-EMMET-dic2019-Elaboracion de productos lácteos.csv”, Columna 6: índice de producción real
Datos17=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos17=ts(Datos17,freq=12,start=c(2001,1))
plot(Datos17)

#Para leer “anex-EMMET-dic2019-Elaboracion de productos lácteos.csv”, Columna 7: índice de ventas nominales
Datos18=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos18=ts(Datos18,freq=12,start=c(2001,1))
plot(Datos18)

#Para leer “anex-EMMET-dic2019-Elaboracion de productos lácteos.csv”, Columna 8: índice de ventas reales
Datos19=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos19=ts(Datos19,freq=12,start=c(2001,1))
plot(Datos19)

#Para leer “anex-EMMET-dic2019-Fabricacion de otros productos químicos.csv”, Columna 5: índice de producción nominal
Datos20=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos20=ts(Datos20,freq=12,start=c(2001,1))
plot(Datos20)


#Para leer “anex-EMMET-dic2019-Fabricacion de otros productos químicos.csv”, Columna 6: índice de producción real
Datos21=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos21=ts(Datos21,freq=12,start=c(2001,1))
plot(Datos21)

#Para leer “anex-EMMET-dic2019-Fabricacion de otros productos químicos.csv”, Columna 7: índice de ventas nominales
Datos22=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos22=ts(Datos22,freq=12,start=c(2001,1))
plot(Datos22)

#Para leer “anex-EMMET-dic2019-Fabricacion de otros productos químicos.csv”, Columna 8: índice de ventas reales
Datos23=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos23=ts(Datos23,freq=12,start=c(2001,1))
plot(Datos23)

#Para leer "anex-EMMET-dic2019-Fabricacion de papel carton y sus productos.csv”, Columna 5: índice de producción nominal
Datos24=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos24=ts(Datos24,freq=12,start=c(2001,1))
plot(Datos24)

#Para leer "anex-EMMET-dic2019-Fabricacion de papel carton y sus productos.csv”, Columna 8: índice de ventas reales
Datos25=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos25=ts(Datos25,freq=12,start=c(2001,1))
plot(Datos25)

#Para leer “anex-EMMET-dic2019-Elaboracion de productos de panaderia macarrones fideos y sus productos”, Columna 5: índice de producción nominal
Datos26=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos26=ts(Datos26,freq=12,start=c(2001,1))
plot(Datos26)

#Para leer “anex-EMMET-dic2019-Elaboracion de productos de panaderia macarrones fideos y sus productos”, Columna 6: índice de producción real
Datos27=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos27=ts(Datos27,freq=12,start=c(2001,1))
plot(Datos27)

#Para leer “anex-EMMET-dic2019-Elaboracion de productos de panaderia macarrones fideos y sus productos”, Columna 7: índice de ventas nominales
Datos28=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos28=ts(Datos28,freq=12,start=c(2001,1))
plot(Datos28)

#Para leer “anex-EMMET-dic2019-Elaboracion de productos de panaderia macarrones fideos y sus productos”, Columna 8: índice de ventas reales
Datos29=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos29=ts(Datos29,freq=12,start=c(2001,1))
plot(Datos29)

#Para leer “anex-EMMET-dic2019-Procesamiento y conservacion de carne pescado y sus productos.csv, Columna 5: índice de producción nominal
Datos30=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos30=ts(Datos30,freq=12,start=c(2001,1))
plot(Datos30)

#Para leer “anex-EMMET-dic2019-Procesamiento y conservacion de carne pescado y sus productos.csv, Columna 6: índice de producción real
Datos31=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos31=ts(Datos31,freq=12,start=c(2001,1))
plot(Datos31)

#Para leer “anex-EMMET-dic2019-Procesamiento y conservacion de carne pescado y sus productos.csv, Columna 7: índice de ventas nominales
Datos32=read.table(file.choose(),header=T,sep=";",skip=14,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos32=ts(Datos32,freq=12,start=c(2001,1))
plot(Datos32)









