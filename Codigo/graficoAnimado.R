

library(xlsx)
library(RColorBrewer)


#=========poblacion reclusa=========================

setwd('C:/Users/Iván/Desktop/INDAT/5º/Segundo cuatrimestre/TFG-Estadistica/Código/')

datos<-read.xlsx('Estadistica diciembre 2018.xlsx', sheetIndex = 3)
a18<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2017.xlsx', sheetIndex = 3)
a17<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2016.xlsx', sheetIndex = 3)
a16<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2015.xlsx', sheetIndex = 3)
a15<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2014.xlsx', sheetIndex = 3)
a14<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2013.xlsx', sheetIndex = 3)
a13<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2012.xlsx', sheetIndex = 3)
a12<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2011.xlsx', sheetIndex = 3)
a11<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2010.xlsx', sheetIndex = 3)
a10<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2009.xlsx', sheetIndex = 3)
a09<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2008.xlsx', sheetIndex = 3)
a08<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2007.xlsx', sheetIndex = 3)
a07<-as.numeric(as.vector(datos[3:21,4]))

datos<-read.xlsx('Estadistica diciembre 2006.xlsx', sheetIndex = 3)
a06<-as.numeric(as.vector(datos[3:21,4]))


poblacionReclusa<- data.frame(Año18= a18, Año17=a17, Año16=a16,
                              Año15=a15, Año14=a14, Año13=a13,
                              Año12=a12, Año11=a11, Año10=a10,
                              Año09=a09, Año08=a08,Año07=a07,
                              Año06=a06)

pRec<-as.vector(t(poblacionReclusa))

#======poblacion comunidad

datos<-read.xlsx('2915.xlsx', sheetIndex = 1)

poblacionTotal<-as.numeric(as.vector(t(datos[8:26,2:14])))



#======= PIB per capita ===========================

datos<-read.xlsx('pr_cre.xlsx', sheetIndex = 3)

#which(datos[4,]==2006)
#which(datos[4,]=="2018 (A)")
secuencias<- seq(from=25, to=73, by=4)
pib<-as.numeric(as.vector(t(datos[c(6,15,19,20,21,24,25,35,41,46,50,53,58,59,60,61,65,66,67),secuencias])))


#=====datos finales

comunidad<-c("Andalucía" ,                 "Aragón"   , "Principado de Asturias", "Islas Baleares", "Islas Canarias",                 
               "Cantabria"    ,  "Castilla y León" ,            "Castilla-La Mancha"  ,      
               "Cataluña"  , "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Región de Murcia","Comunidad Foral de Navarra", "País Vasco" , "La Rioja" , "Ceuta" , "Melilla"           
)


datosFin<-data.frame(Reclusos=pRec,
                     Poblacion=poblacionTotal,
                    Año=rep(c(2018:2006),19),
                    Comunidad=rep(comunidad, each=13)
)

datosFin2<-data.frame(PIB=pib,
                      Año=rep(c(2006:2018),19),
                      Comunidad=rep(comunidad, each=13))

datosFinales<-merge(datosFin, datosFin2)

n <- 19
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colores<-sample(col_vector, n)

datosFinales <- datosFinales %>% mutate(Color=rep(colores,13))



p <- datosFinales %>%
  plot_ly(
    x = ~PIB, 
    y = ~Reclusos, 
    size = ~Poblacion, 
    color=~Comunidad,
    frame = ~Año, 
    text = paste("Comunidad:", datosFinales$Comunidad,"<br>Población:", datosFinales$Poblacion,
                 "<br>Población reclusa:", datosFinales$Reclusos,"<br>PIB:",datosFinales$PIB), 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    ),
    title="Evolución económica y delictiva por comunidad"
  )

p %>%  animation_opts(
  1000, easing = "elastic", redraw = FALSE
)



