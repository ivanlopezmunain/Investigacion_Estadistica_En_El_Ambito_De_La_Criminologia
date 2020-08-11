

#Ivan Lopez de Munain Quintana

#================= Procesamiento de datos- Serie temporal =============================

library("xlsx")

setwd('C:/Users/Iván/Desktop/INDAT/5º/Segundo cuatrimestre/TFG-Estadistica/Código/SerieTemporal')

#años 2018-2017
c1.18.17<-read.xlsx('1cuatri-1817.xlsx', sheetIndex = 1)
c2.18.17<-read.xlsx('2cuatri-1817.xlsx', sheetIndex = 1)
c3.18.17<-read.xlsx('3cuatri-1817.xlsx', sheetIndex = 1)
c4.18.17<-read.xlsx('4cuatri-1817.xlsx', sheetIndex = 1)

ind<- which(c1.18.17[,2]!="<NA>")

#cuatrimestres
primerCuatri17<-as.numeric(as.vector(c1.18.17[ind[2:length(ind)],2]))
primerCuatri18<-as.numeric(as.vector(c1.18.17[ind[2:length(ind)],3]))
segundoCuatriAux17<-as.numeric(as.vector(c2.18.17[ind[2:length(ind)],2]))
segundoCuatriAux18<-as.numeric(as.vector(c2.18.17[ind[2:length(ind)],3]))

segundoCuatri17<-segundoCuatriAux17-primerCuatri17
segundoCuatri18<-segundoCuatriAux18-primerCuatri18

tercerCuatriAux17<-as.numeric(as.vector(c3.18.17[ind[2:length(ind)],2]))
tercerCuatriAux18<-as.numeric(as.vector(c3.18.17[ind[2:length(ind)],3]))

tercerCuatri17<-tercerCuatriAux17-segundoCuatriAux17
tercerCuatri18<-tercerCuatriAux18-segundoCuatriAux18


cuartoCuatriAux17<-as.numeric(as.vector(c4.18.17[ind[2:length(ind)],2]))
cuartoCuatriAux18<-as.numeric(as.vector(c4.18.17[ind[2:length(ind)],3]))

cuartoCuatri17<-cuartoCuatriAux17-tercerCuatriAux17
cuartoCuatri18<-cuartoCuatriAux18-tercerCuatriAux18

años1718<-c(primerCuatri17,segundoCuatri17,tercerCuatri17,cuartoCuatri17, primerCuatri18, segundoCuatri18,tercerCuatri18,cuartoCuatri18)


#años 2016-2015
c1.16.15<-read.xlsx('1cuatri-1615.xlsx', sheetIndex = 1)
c2.16.15<-read.xlsx('2cuatri-1615.xlsx', sheetIndex = 1)
c3.16.15<-read.xlsx('3cuatri-1615.xlsx', sheetIndex = 1)
c4.16.15<-read.xlsx('4cuatri-1615.xlsx', sheetIndex = 1)

ind<- which(c1.16.15[,2]!="<NA>")

#cuatrimestres
ident<-c(rep(c(1:19),each=8))
primerCuatri15<-data.frame(inf=as.numeric(as.vector(c1.16.15[ind[2:length(ind)],2])),ident=ident)
primerCuatri15<- tapply(primerCuatri15$inf,primerCuatri15$ident,sum)

primerCuatri16<-data.frame(inf=as.numeric(as.vector(c1.16.15[ind[2:length(ind)],3])), ident=ident)
primerCuatri16<- tapply(primerCuatri16$inf,primerCuatri16$ident,sum)

segundoCuatriAux15<-data.frame(inf=as.numeric(as.vector(c2.16.15[ind[2:length(ind)],2])), ident=ident)
segundoCuatriAux15<- tapply(segundoCuatriAux15$inf,segundoCuatriAux15$ident,sum)

segundoCuatriAux16<-data.frame(inf=as.numeric(as.vector(c2.16.15[ind[2:length(ind)],3])), ident=ident)
segundoCuatriAux16<- tapply(segundoCuatriAux16$inf,segundoCuatriAux16$ident,sum)

segundoCuatri15<-segundoCuatriAux15-primerCuatri15
segundoCuatri16<-segundoCuatriAux16-primerCuatri16

#tercero
tercerCuatriAux15<-data.frame(inf=as.numeric(as.vector(c3.16.15[ind[2:length(ind)],2])), ident=ident)
tercerCuatriAux15<- tapply(tercerCuatriAux15$inf,tercerCuatriAux15$ident,sum)

tercerCuatriAux16<-data.frame(inf=as.numeric(as.vector(c3.16.15[ind[2:length(ind)],3])), ident=ident)
tercerCuatriAux16<- tapply(tercerCuatriAux16$inf,tercerCuatriAux16$ident,sum)

tercerCuatri15<-tercerCuatriAux15-segundoCuatriAux15
tercerCuatri16<-tercerCuatriAux16-segundoCuatriAux16

#cuarto
cuartoCuatriAux15<-data.frame(inf=as.numeric(as.vector(c4.16.15[ind[2:length(ind)],2])), ident=ident)
cuartoCuatriAux15<- tapply(cuartoCuatriAux15$inf,cuartoCuatriAux15$ident,sum)

cuartoCuatriAux16<-data.frame(inf=as.numeric(as.vector(c4.16.15[ind[2:length(ind)],3])), ident=ident)
cuartoCuatriAux16<- tapply(cuartoCuatriAux16$inf,cuartoCuatriAux16$ident,sum)

cuartoCuatri15<-cuartoCuatriAux15-tercerCuatriAux15
cuartoCuatri16<-cuartoCuatriAux16-tercerCuatriAux16

años1516<-c(primerCuatri15,segundoCuatri15,tercerCuatri15,cuartoCuatri15, primerCuatri16, segundoCuatri16,tercerCuatri16,cuartoCuatri16)

años15161718<-c(años1516,años1718)

datosFinales<-data.frame(Infracciones=años15161718, Cuatrimestre= rep(rep(c(1:4),each=19),4), Año=rep(c(2015:2018),each=4*19))
dim(datosFinales)

#================ Datos de prueba: Año 2019 ===========================

c1.19<-read.xlsx('1cuatri-19.xlsx', sheetIndex = 1)
c2.19<-read.xlsx('2cuatri-19.xlsx', sheetIndex = 1)
c3.19<-read.xlsx('3cuatri-19.xlsx', sheetIndex = 1) 



ind<- which(c1.19[,2]!="<NA>")

#cuatrimestres
primerCuatri19<-as.numeric(as.vector(c1.19[ind[2:length(ind)],2]))
segundoCuatriAux19<-as.numeric(as.vector(c2.19[ind[2:length(ind)],2]))

segundoCuatri19<-segundoCuatriAux19-primerCuatri19

tercerCuatriAux19<-as.numeric(as.vector(c3.19[ind[2:length(ind)],2]))

tercerCuatri19<-tercerCuatriAux19-segundoCuatriAux19

test19<-data.frame(Infracciones=c(primerCuatri19, segundoCuatri19, tercerCuatri19), Cuatrimestre=rep(c(1:3),each=19), Año=rep(2019,57))              

conjuntoEntero<-as.data.frame(rbind(datosFinales,test19))
dim(conjuntoEntero)
#============== Exportar datos =====================

file<-paste(getwd(), "/test2019.xlsx",sep="")
write.xlsx(test19, file)

file<-paste(getwd(), "/entrenamiento15-18.xlsx",sep="")
write.xlsx(datosFinales, file)

file<-paste(getwd(), "/datos15-19.xlsx",sep="")
write.xlsx(conjuntoEntero, file)
