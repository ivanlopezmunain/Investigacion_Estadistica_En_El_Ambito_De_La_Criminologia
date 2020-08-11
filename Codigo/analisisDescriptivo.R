
#Ivan Lopez de Munain Quintana

#TFG - Investigación estadística en la criminología

# =============== Creación de mapas ==================== 

install.packages("sp")
install.packages("xlsx")

library(sp)
library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(networkD3)
library(dplyr)
library(plotly)

setwd('C:/Users/Iván/Desktop/INDAT/5º/Segundo cuatrimestre/TFG-Estadistica/Código/')


#----------------------Mapa por provincias: valores absolutos -----------------------------

mapaProv<-readRDS('gadm36_ESP_2_sp.rds')

#mapaProv <- mapaProv[mapaProv$NAME_2!="Santa Cruz de Tenerife",]
#mapaProv <- mapaProv[mapaProv$NAME_2!="Las Palmas",]

datosProv <- read.xlsx('total-infrac-prov.xlsx', sheetIndex = 1)

infracProv <- which(datosProv[,2]!="<NA>")

#quitamos el total y el extranjero
infracProv <- infracProv[3:(length(infracProv)-1)]

#tabla de autoridad
provDefinit<-c("Álava", "Albacete", "Alicante"  , "Almería"               
                ,"Ávila", "Badajoz"     , "Baleares"              
                , "Barcelona"   ,"Burgos"     ,"Cáceres"               
                , "Cádiz", "Castellón","Ciudad Real" , "Córdoba" , "A Coruña"             
                , "Cuenca","Girona"     ,"Granada"               
                , "Guadalajara" ,"Guipúzcoa"   ,"Huelva"                
                , "Huesca","Jaén" ,   "León"     ,  "Lleida","La Rioja" ,"Lugo","Madrid"    ,             "Málaga"                
                 ,"Murcia"    , "Navarra"               
                 , "Ourense"    ,   "Asturias", "Palencia", "Las Palmas",
                 "Pontevedra", "Salamanca"  ,            "Santa Cruz de Tenerife",
                 "Cantabria",  "Segovia", "Sevilla"    ,"Soria" ,"Tarragona"             
                 , "Teruel"     ,"Toledo"    ,"Valencia"              
                 , "Valladolid" ,"Vizcaya"   , "Zamora"                
                  , "Zaragoza" , "Ceuta", "Melilla" )



#which(provDefinit=="Santa Cruz de Tenerife")
#which(provDefinit=="Las Palmas")
datosProv <- as.numeric(as.vector(datosProv[infracProv[c(1:length(infracProv))],2]))

datosAbsProv<-data.frame(NAME_2=provDefinit, Infracciones=datosProv)

merged<- merge(mapaProv@data, datosAbsProv)



correct.ordering <- match(mapaProv$NAME_2, merged$NAME_2)
mapaProv@data <- merged[correct.ordering,]

my.palette <- colorRampPalette(brewer.pal(9, "Reds"))(max(mapaProv@data$Infracciones))
spplot(mapaProv, "Infracciones",  col.regions=my.palette, main="Infracciones por provincia año 2018")
  
#----------------------Mapa por provincias: tasa de criminalidad -----------------------------

poblacionProv<-read.xlsx('poblacionProvincias18.xlsx', sheetIndex = 1)

#tabla de autoridad
provPobl<-c("Albacete", "Alicante", "Almería", "Álava","Asturias", "Ávila","Badajoz", "Baleares", "Barcelona", "Vizcaya",
            "Burgos", "Cáceres", "Cádiz", "Cantabria","Castellón", "Ciudad Real", "Córdoba","A Coruña","Cuenca","Guipúzcoa","Girona",
            "Granada", "Guadalajara","Huelva","Huesca","Jaén","León","Lleida","Lugo","Madrid","Málaga","Murcia","Navarra","Ourense",
            "Palencia", "Las Palmas",
            "Pontevedra","La Rioja","Salamanca","Santa Cruz de Tenerife",
            "Segovia","Sevilla","Soria","Tarragona",
            "Teruel","Toledo","Valencia","Valladolid","Zamora","Zaragoza","Ceuta" , "Melilla")

#which(provPobl=="Santa Cruz de Tenerife")
#which(provPobl=="Las Palmas")
aux <- which(poblacionProv[,2]!="<NA>")
aux <- aux[3:(length(aux))]
pobProv18 <- as.numeric(as.vector(poblacionProv[aux,2]))

dataPoblProv <- data.frame(NAME_2=provPobl, poblacion=pobProv18)

datosUnion<-merge(datosAbsProv,dataPoblProv)

#variable tasa por mil
tasaProv<-1000*datosUnion$Infracciones/datosUnion$poblacion

tasaDatosProv<-data.frame(NAME_2=datosUnion$NAME_2, Tasa=tasaProv)

merged<- merge(mapaProv@data, tasaDatosProv)

correct.ordering <- match(mapaProv$NAME_2, merged$NAME_2)
mapaProv@data <- merged[correct.ordering,]

my.palette <- colorRampPalette(brewer.pal(9, "Reds"))(max(mapaProv$Tasa))
spplot(mapaProv, "Tasa", col.regions=my.palette, main="Número de infracciones por cada 1000 habitantes por provincia en el año 2018")


#================= POR COMUNIDADES =================================

#tabla de autoridad por comunidades

comunidades<-c("Andalucía" , "Aragón"   , "Principado de Asturias", "Islas Baleares", "Islas Canarias",                 
               "Cantabria"    ,  "Castilla y León" , "Castilla-La Mancha"  ,      
               "Cataluña"  , "Comunidad Valenciana", "Extremadura", "Galicia",
               "Comunidad de Madrid", "Región de Murcia","Comunidad Foral de Navarra",
               "País Vasco" , "La Rioja" , "Ceuta y Melilla"           
)

#----------poblacion por comunidades---------------------

poblacion<-read.xlsx('poblacionComunidades.xlsx', sheetIndex = 1)


aux <- which(poblacion[,2]!="<NA>")
aux <- aux[3:(length(aux))]


pob18 <- as.numeric(as.vector(poblacion[aux,2]))
pob17 <- as.numeric(as.vector(poblacion[aux,3]))
pob16 <- as.numeric(as.vector(poblacion[aux,4]))
pob <- data.frame(com=poblacion[aux,1], pob16=pob16, pob17=pob17, pob18=pob18)
pobFinal<-data.frame(com=pob$com,PobMedia=apply(pob[,c(2,3,4)],1,mean))

#obtener de menor a mayor las poblaciones
pobFinal$com[order(pobFinal$PobMedia)]

#-------------------------------------------------------------------


#------------------- Mapa por comunidades: valores absolutos ------------

mapa<-readRDS('gadm36_ESP_1_sp.rds')
#mapa <- mapa[mapa$NAME_1!="Islas Canarias",]

data <- read.xlsx('total-infrac-com.xlsx', sheetIndex = 1)

infrac <- which(data[,2]!="<NA>")
infrac <- infrac[3:(length(infrac)-1)]

datos <- as.numeric(as.vector(data[infrac[c(1:5,6:(length(infrac)-1))],2]))
datos[length(datos)]<-datos[length(datos)]+as.numeric(as.vector(data[infrac[length(infrac)],2]))
datosFinales<-data.frame(NAME_1=comunidades, Infracciones=datos)

merged<- merge(mapa@data, datosFinales)

correct.ordering <- match(mapa$NAME_1, merged$NAME_1)
mapa@data <- merged[correct.ordering,]

my.palette <- colorRampPalette(brewer.pal(8, "Reds"))(max(mapa$Infracciones))
spplot(mapa, "Infracciones", col.regions=my.palette, main="Infracciones por comunidad año 2018")

#------------------- Mapa por comunidades: tasa de criminalidad ------------

#para juntar la poblacion de ceuta y melilla pues en el mapa se consideran juntas
pob18[length(pob18)-1]<-pob18[length(pob18)-1]+pob18[length(pob18)]

#ademas no coger el total de poblacion en España
pob18<-pob18[2:(length(pob18)-1)]

#variable tasa por mil
tasa<-1000*datosFinales$Infracciones/pob18

tasaDatos<-data.frame(NAME_1=comunidades, Tasa=tasa)

merged<- merge(mapa@data, tasaDatos)

correct.ordering <- match(mapa$NAME_1, merged$NAME_1)
mapa@data <- merged[correct.ordering,]

my.palette <- colorRampPalette(brewer.pal(8, "Reds"))(max(mapa$Tasa))
spplot(mapa, "Tasa", col.regions=my.palette, main="Número de infracciones por cada 1000 habitantes por comunidad en el año 2018")

#========================Tabla valores medios infracciones 2016-2018=======================

d18 <- read.xlsx('total-infrac-com.xlsx', sheetIndex = 1)
d1617 <- read.xlsx('total-infrac-com-1617.xlsx', sheetIndex = 1)

infrac18 <- which(d18[,2]!="<NA>")
infrac18 <- infrac18[2:(length(infrac18)-1)]

aux18 <- as.numeric(as.vector(d18[infrac18,2]))

dfin18<-data.frame(com=d18[infrac18-1,1], Infracciones18=aux18)

infrac1617 <- which(d1617[,2]!="<NA>")
infrac1617 <- infrac1617[2:(length(infrac1617)-1)]

aux16 <- as.numeric(as.vector(d1617[infrac1617,2]))
aux17 <- as.numeric(as.vector(d1617[infrac1617,3]))

dfin1617<-data.frame(com=d1617[infrac1617-1,1], Infracciones16=aux16,Infracciones17=aux17)
    
unidas<- merge(dfin1617,dfin18)     

#medias por comunidad

unidas<-cbind(unidas,apply(unidas[,c(2,3,4)],1,sum))
colnames(unidas)<-c("com", "Infracciones16", "Infracciones17", "Infracciones18", "Total")
unidas<-cbind(unidas, unidas$Total/3)
colnames(unidas)<-c("com", "Infracciones16", "Infracciones17", "Infracciones18", "Total", "Media")
unidas



#===================== Bar Plots ==============================


#--------------Barplot: Por tipologia  ----------------

data <- read.xlsx('total-tiposInfrac-18.xlsx', sheetIndex = 1)

tipos <- which(data[,2]!="<NA>")
tipos <- tipos[2:(length(tipos))]

#-->comentados para realizarlo sin hurtos y sin resto de delitos

auxTipos <- as.numeric(as.vector(data[tipos,2]))
#auxTipos <- as.numeric(as.vector(data[tipos[c(1:10,12,13)],2]))

names <- c("A: Asesinatos consumados","B: Asesinatos en grado tentativa", "C: Delitos de lesiones", "D: Secuestro", "E: Delitos contra la libertad e indemnidad sexual", "F: Agresion sexual con penetración", "G: Resto de delitos contra la libertad", "H: Robos con violencia e intimidación","I: Robos con fuerza","J: Robos en domicilios","K: Hurtos", "L: Sustracción de vehículos","M: Tráfico de drogas","N: Resto de infracciones")
#names <- c("A: Asesinatos consumados","B: Asesinatos en grado tentativa", "C: Delitos de lesiones", "D: Secuestro", "E: Delitos contra la libertad e indemnidad sexual", "F: Agresion sexual con penetración", "G: Resto de delitos contra la libertad", "H: Robos con violencia e intimidación","I: Robos con fuerza","J: Robos en domicilios", "K: Sustracción de vehículos","L: Tráfico de drogas")
ind <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N")
#ind <- c("A","B","C","D","E","F","G","H","I","J","K","L")
finalTipos<-data.frame(Clasificación=names, NumInfr=auxTipos, Identificador=ind)

graf<-ggplot(data=finalTipos, aes(x=Identificador, y=NumInfr, fill=Clasificación)) + 
  geom_bar(stat="identity", position="dodge")

graf<-graf+ ggtitle("Distribución de los delitos según su naturaleza") +
  xlab("Tipos de infracciones") + ylab("Número de infracciones") + theme(plot.title = element_text(hjust = 0.5, face="bold"))

#con ggplotly se puede observar qué tipos quieres ver, zoom, etc
ggplotly(graf)

#-------------- barplot: distincion sexo y edad -----------------

data <- read.xlsx('total-infracSexoEdad-18.xlsx', sheetIndex = 1)

tipos <- which(data[,2]!="<NA>")
tipos <- tipos[4:(length(tipos))]

auxMasc <- as.numeric(as.vector(data[tipos,2]))
auxFem <- as.numeric(as.vector(data[tipos,3]))

edad<-c("14-17","18-30","31-40","41-64","Más de 64")
finalSex<-data.frame(Edad=edad, Masculino=auxMasc, Femenino=auxFem)

sex<-c(rep("Masculino",5),rep("Femenino",5))
infr<-c(finalSex$Masculino,finalSex$Femenino)

fin<-data.frame(Sexo=sex, NumInfr=infr, Edad=rep(finalSex$Edad,2))


grafSex<-ggplot(data=fin, aes(x=Sexo, y=NumInfr, fill=Edad)) + 
  geom_bar(stat="identity", position="stack")

grafSex<-grafSex + ggtitle("Distribución de los delitos según sexo y edad") +
  xlab("Sexo") + ylab("Número de infracciones")  +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))  

ggplotly(grafSex)


#========================== Boxplot violin -> sexo y edad ==================



data <- read.xlsx('total-infracSexoEdadCom-18.xlsx', sheetIndex = 1)

tipos <- which(data[,2]!="<NA>")
tipos <- tipos[3:(length(tipos))]

auxMasc <- as.numeric(as.vector(data[tipos,2]))
auxFem <- as.numeric(as.vector(data[tipos,3]))


comunidades<-c("Andalucía" ,                 "Aragón"   , "Principado de Asturias", "Islas Baleares", "Islas Canarias",                 
               "Cantabria"    ,  "Castilla y León" ,            "Castilla-La Mancha"  ,      
               "Cataluña"  , "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Región de Murcia","Comunidad Foral de Navarra", "País Vasco" , "La Rioja" , "Ceuta" , "Melilla"           
)
edad<-rep(c("14-17","18-30","31-40","41-64","Más de 64"),19)
sex<-c(rep("Masculino",5*19),rep("Femenino",5*19))

dfComSexoEdad<-data.frame(Edad=edad, Sexo=sex,
                          Infracciones=c(auxMasc,auxFem),
                          Comunidad=rep(comunidades,each=5),
                          Poblacion=pob$pob18[2:dim(pob)[1]]
                          )
dfComSexoEdad <- dfComSexoEdad %>% mutate(Tasa=1000*Infracciones/Poblacion)

p <- dfComSexoEdad %>%
  plot_ly(type = 'violin') %>%
  add_trace(
    x = ~Edad[dfComSexoEdad$Sexo == 'Femenino'],
    y = ~Infracciones[dfComSexoEdad$Sexo == 'Femenino'],
    legendgroup = 'Femenino',
    scalegroup = 'Femenino',
    name = 'Femenino',
    side = 'negative',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
    color = I("#8dd3c7")
  ) %>%
  add_trace(
    x = ~Edad[dfComSexoEdad$Sexo == 'Masculino'],
    y = ~Infracciones[dfComSexoEdad$Sexo == 'Masculino'],
    legendgroup = 'Masculino',
    scalegroup = 'Masculino',
    name = 'Masculino',
    side = 'positive',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
    color = I("#bebada")
  ) %>% 
  layout(
    title = "Densidad y cuartiles",
    xaxis = list(
      title = "Edad"  
    ),
    yaxis = list(
      title = "Número de infracciones",
      zeroline = F
    ),
    violingap = 0,
    violingroupgap = 0,
    violinmode = 'overlay'
  )

p





#========================= Shankey: Territorio y tipologia ==============

data <- read.xlsx('total-tiposInfracComSinResto-18.xlsx', sheetIndex = 1)

comunidades<-c("Andalucía" , "Aragón"   , "Principado de Asturias", "Islas Baleares", "Islas Canarias",                 
               "Cantabria"    ,  "Castilla y León" ,  "Castilla-La Mancha"  ,      
               "Cataluña"  , "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid",
               "Región de Murcia","Comunidad Foral de Navarra", "País Vasco" , "La Rioja" , "Ceuta" , "Melilla"           
)

nombresDelitos <- c("Asesinatos consumados","Asesinatos en grado tentativa", "Delitos de lesiones", "Secuestro", "Delitos contra la libertad e indemnidad sexual", "Agresion sexual con penetración", "Resto de delitos contra la libertad", "Robos con violencia e intimidación","Robos con fuerza","Robos en domicilios","Hurtos", "Sustracción de vehículos","Tráfico de drogas")


tipos <- which(data[,2]!="<NA>")
tipos <- tipos[2:(length(tipos))]

valoresCom_Tipo <- as.numeric(as.vector(data[tipos,2]))
tasaPorCienMil<-100000*valoresCom_Tipo/pob$pob18[2:length(pob$pob18)]
tiposDelitos<-rep(nombresDelitos,each=length(comunidades))
repComunidades<- rep(comunidades,length(nombresDelitos))

links <- data.frame(
  source=tiposDelitos, 
  target=repComunidades, 
  value=round(tasaPorCienMil,2),
  ind=0
)

write.csv(links, file="tasaTiposCom-18.csv")

#ordenar los grupos en funcion de la tasa para representarlos en orden creciente
ordenado<- aggregate(links$value,list(links$source),sum) 
ordenado$Group.1 <- ordenado$Group.1[order(ordenado$x)]
ordenado <- select(ordenado, -x) %>% mutate(ind=c(1:13))

for(i in 1:length(ordenado$Group.1)){
  links$ind[which(links$source==ordenado$Group.1[i])]<-ordenado$ind[i]
}

links$source<- links$source[order(links$ind)]
links$target<- links$target[order(links$ind)]
links$value<- links$value[order(links$ind)]
links$ind<-links$ind[order(links$ind)]

#orden de comunidades por agresion sexual
links$value[57+order(links$value[which(links$source=="Agresion sexual con penetración")])]
links$target[57+order(links$value[which(links$source=="Agresion sexual con penetración")])]

links$value[95+order(links$value[which(links$source=="Delitos contra la libertad e indemnidad sexual")])]
links$target[95+order(links$value[which(links$source=="Delitos contra la libertad e indemnidad sexual")])]


nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

nodes$group <- as.factor(c(rep("a",length(nombresDelitos)),rep("b",length(comunidades))))
links$group <- as.factor(c(rep(c("1","2","3","4","5","6","7","8","9","10","11","12","13"),each=length(comunidades))))
#brewer.pal(13, "Purples")

my_color <- 'd3.scaleOrdinal() .domain(["a", "b","1","2","3","4","5","6","7","8","9","10","11","12","13"]) 
.range(["#69b3a2", "steelblue", "#FCFBFD" ,"#EFEDF5", "#DADAEB", "#BCBDDC", "#BCBDDC" ,"#BCBDDC", "#BCBDDC", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D"])'

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value",units="delitos/100.000hab", NodeID = "name",
                   colourScale=my_color, LinkGroup = "group", NodeGroup="group",
                   iterations=0)
p



#======================= Boxplot MENAS ================================


comunidades<-c("Andalucía" ,                 "Aragón"   , "Principado de Asturias", "Islas Baleares", "Islas Canarias",                 
               "Cantabria"    ,  "Castilla y León" ,            "Castilla-La Mancha"  ,      
               "Cataluña"  , "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Región de Murcia","Comunidad Foral de Navarra", "País Vasco" , "La Rioja" , "Ceuta" , "Melilla"           
)

pobMenores<-read.xlsx('poblacionMenoresNacioCom-18.xlsx', sheetIndex = 1)

indice <- which(pobMenores[,2]!="<NA>")
indice <- indice[4:(length(indice))]

auxPobEsp<- as.numeric(as.vector(pobMenores[indice,2]))
auxPobExtr <- as.numeric(as.vector(pobMenores[indice,3]))

aux_pob<-data.frame(Españoles=auxPobEsp, Extranjeros=auxPobExtr, ident=rep(1:19, each=4))

pob_menores18<- data.frame(Españoles=tapply(aux_pob$Españoles,aux_pob$ident,sum),Extranjeros=tapply(aux_pob$Extranjeros,aux_pob$ident,sum))

data <- read.xlsx('total-infrComunExtr-18.xlsx', sheetIndex = 1)

ind <- which(data[,2]!="<NA>")
ind <- ind[4:(length(ind))]

auxEsp<- as.numeric(as.vector(data[ind,2]))
auxExtr <- as.numeric(as.vector(data[ind,3]))

delitos<-c(auxEsp,auxExtr)
nacionalidad <- c(rep("Español",19),rep("Extranjero",19))

dataExtr<- data.frame(Comunidades=comunidades, Nacionalidad=nacionalidad, Infracciones=delitos)

dataExtr<-dataExtr %>% mutate(Poblacion=c(pob_menores18$Españoles,pob_menores18$Extranjeros)) %>% mutate(Tasa=Infracciones*1000/Poblacion)


#Infracciones totales -> No es muy util
grafExtr<-ggplot(dataExtr, aes(x=Nacionalidad, y=Infracciones, color=Nacionalidad)) +
  geom_boxplot()

grafExtr + geom_jitter(shape=16, position=position_jitter(0.1)) + 
  ggtitle("Distribución de los delitos para menores según su nacionalidad") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

#Tasa infracciones por mil menores 
grafExtr<-ggplot(dataExtr[1:(dim(dataExtr)[1]-2),], aes(x=Nacionalidad, y=Tasa, color=Nacionalidad, text=paste("Tasax1000:", round(Tasa,2), "<br>Nacionalidad:",Nacionalidad,"<br>Comunidad:",Comunidades))) +
  geom_boxplot()

grafExtr<-grafExtr + geom_jitter(shape=16, position=position_jitter(0.1)) + 
  ggtitle("Distribución de la tasa de delincuencia para menores según su nacionalidad") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

ggplotly(grafExtr,tooltip = "text")

#obtencion de las respectivas poblaciones y tasas (datos cogidos directamente de INE)
pobMenoresEsp<-sum(pob_menores18$Españoles)
pobMENAS<-sum(pob_menores18$Extranjeros)
infEsp<- sum(dataExtr$Infracciones[1:19])
infExt<-sum(dataExtr$Infracciones[20:38])
tasaEsp<-1000*infEsp/pobMenoresEsp
tasaExt<-1000*infExt/pobMENAS
