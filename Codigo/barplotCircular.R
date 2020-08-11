library(tidyverse)
library(xlsx)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(plotly)

#==========datos infrac ============

comunidades<-c("Andalucía" ,                 "Aragón"   , "Principado de Asturias", "Islas Baleares", "Islas Canarias",                 
               "Cantabria"    ,  "Castilla y León" ,            "Castilla-La Mancha"  ,      
               "Cataluña"  , "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Región de Murcia","Comunidad Foral de Navarra", "País Vasco" , "La Rioja" , "Ceuta" , "Melilla"           
)

poblacion<-read.xlsx('poblacionComSexo-18.xlsx', sheetIndex = 1)

aux <- which(poblacion[,2]!="<NA>")
aux <- aux[3:(length(aux))]

pobMasc <- as.numeric(as.vector(poblacion[aux,2]))
pobFem <- as.numeric(as.vector(poblacion[aux,3]))

#----

data <- read.xlsx('total-infracSexoEdadCom-18.xlsx', sheetIndex = 1)

tipos <- which(data[,2]!="<NA>")
tipos <- tipos[3:(length(tipos))]

auxMasc <- as.numeric(as.vector(data[tipos,2]))
auxFem <- as.numeric(as.vector(data[tipos,3]))

edad<-rep(c("14-17","18-30","31-40","41-64","Más de 64"),19)
sex<-c(rep("Masculino",5*19),rep("Femenino",5*19))

dfComSexoEdad<-data.frame(Edad=edad, Sexo=sex,
                          Infracciones=c(auxMasc,auxFem),
                          Indicador=as.factor(rep(c(1:19),each=5))
)

infMasc<- dfComSexoEdad %>% filter(Sexo=="Masculino")
tasaMasc<-1000*tapply(infMasc$Infracciones, infMasc$Indicador,sum)/pobMasc

infFem<- dfComSexoEdad %>% filter(Sexo=="Femenino")
tasaFem<-1000*tapply(infFem$Infracciones, infFem$Indicador,sum)/pobFem

#========== datos paro==============

datos<-read.xlsx('tasaParoSexo-18.xlsx', sheetIndex = 1)

tipos <- which(datos[,2]!="<NA>")
tipos <- tipos[4:(length(tipos))]

auxMasc <- (as.numeric(as.vector(datos[tipos,2])) + as.numeric(as.vector(datos[tipos,3])) + 
              as.numeric(as.vector(datos[tipos,4])) + as.numeric(as.vector(datos[tipos,5])))/4
auxFem <- (as.numeric(as.vector(datos[tipos,6])) + as.numeric(as.vector(datos[tipos,7]))+
             as.numeric(as.vector(datos[tipos,8])) + as.numeric(as.vector(datos[tipos,9])))/4

#======================= Barplot circular ==============

Sexo<-rep(rep(c("Masculino","Femenino"),each=19),2)
valores<-c(tasaMasc,tasaFem, auxMasc, auxFem )

data=data.frame(
  individual=rep(comunidades,4),
  group=c( rep('A', 19), rep('B', 19), rep('D', 19), rep('C', 19)) ,
  value=valores
)
data = data %>% arrange(group, value)

# Set a number of 'empty bar' to add at the end of each group
empty_bar=3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(group)
data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

# Make the plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p


#====================Barplot normal==========================


Sexo<-rep(rep(c("Masculino","Femenino"),each=19),2)
valores<-c(tasaMasc,tasaFem, auxMasc, auxFem )

data=data.frame(
  Comunidad=rep(comunidades,4),
  Sexo=Sexo ,
  Tasa=round(valores,2),
  Clasificacion=rep(c("Tasa criminalidad", "Tasa de paro"), each=19*2)
)

p1<-ggplot(data %>% filter(Sexo=="Masculino"), aes(fill=Clasificacion, y=Tasa, x=reorder(Comunidad,-Tasa),
                                                   text=paste("Tipo:",Clasificacion,"<br>Comunidad:",Comunidad,"<br>Tasa:",Tasa," delitos/1.000hab"))) + 
  geom_bar(position="dodge", stat="identity") + coord_flip() + 
  ggtitle("Relación paro/criminalidad por sexo y comunidad")+theme(plot.title = element_text(hjust = 0.5, face="bold"))+
  labs(fill="")

aa<-c("Cataluña","País Vasco","Comunidad Foral de Navarra", "Cantabria", "Castilla y León", "Aragón", "Galicia", "La Rioja", "Principado de Asturias", "Comunidad de Madrid",
      "Castilla-La Mancha", "Región de Murcia", "Comunidad Valenciana", "Extremadura", "Islas Baleares", "Andalucía", "Islas Canarias", "Ceuta","Melilla")
aa<-aa[length(aa):1]
auxiliar<-data.frame(Comunidad=aa)
auxFem<-data %>% filter(Sexo=="Femenino")

auxFinal<-inner_join(auxiliar, auxFem)
auxFinal$Comunidad <- factor(auxFinal$Comunidad, levels = auxiliar$Comunidad)

p2<-ggplot(auxFinal,aes(fill=Clasificacion, y=Tasa, x=Comunidad,
                                                  text=paste("Tipo:",Clasificacion,"<br>Comunidad:",Comunidad,"<br>Tasa:",Tasa," delitos/1.000hab"))) + 
  geom_bar(position="dodge", stat="identity") + coord_flip()+
  labs(fill="")



pFinal<-subplot(ggplotly(p1,tooltip="text"), 
       style(ggplotly(p2,tooltip="text"),showlegend=FALSE ),
        shareY = TRUE, titleY = FALSE,heights = 0.9)


pFinal %>% layout(annotations = list(
  list(x = 0.2 , y = 1, text = "Hombre", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.8 , y =1, text = "Mujer", showarrow = F, xref='paper', yref='paper'))
)


#=================== Bubble-Plot animado =========================


paroSerie<-read.xlsx('tasaParoCom-1018.xlsx', sheetIndex = 1)

tasasParo<-(as.numeric(t(paroSerie[9:27,2:10])))

crimiSerie<-read.xlsx('tasaCrimiCom-1018.xlsx', sheetIndex = 1)

tasasCrimi<-(as.numeric(t(crimiSerie[8:26,2:10])))

df<-data.frame(Paro=tasasParo,
               Criminalidad=tasasCrimi,
               Comunidad=comunidades,
               Año=c(2018:2010))


p <- df %>%
  plot_ly(
    x = ~Criminalidad, 
    y = ~Paro, 
    #size = ~pop, 
    color = ~Comunidad, 
    frame = ~Año, 
    text = ~Comunidad, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )

p %>%  animation_opts(
  1000, easing = "elastic", redraw = FALSE
)

