

#Ivan Lopez de Munain Quintana

#================= Procesamiento de datos =============================

library(xlsx)
library(ggplot2)
library(MASS)
library(scales)
library(dplyr)
library(plotly)
library(caret)
library(FactoMineR)
library(factoextra)
library(pca3d)

datos<-read.xlsx('datosSexEdadYear-Modelos.xlsx', sheetIndex = 1)

ind<- which(datos[,2]!="<NA>")

infr <- as.numeric(as.vector(datos[ind[3:length(ind)],2]))
sexo<- as.factor(rep(c("Masculino", "Femenino"),9*5))
edad<-as.factor(rep(rep(c("14-17","18-30","31-40","41-64","M�s de 64"),each=2), 9))
anio<-(rep(c(2018,2017,2016,2015,2014,2013,2012,2011,2010), each=10))

dataProc <- data.frame(Infracciones=infr, Sexo=sexo, Edad=edad, A�o=anio)


#=======================Grafico descriptivo =======================


graf <- ggplot(dataProc, aes(A�o, Infracciones, color=Edad)) + geom_point(aes(text=paste("A�o:", A�o,"<br>Sexo:",Sexo, "<br>Edad:",Edad,"<br>N�mero de infracciones:", Infracciones)))
graf<-graf + stat_smooth(se =FALSE) + facet_wrap(~ Sexo) + 
  ggtitle("Distribuci�n de los delitos seg�n a�o, sexo y edad") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

ggplotly(graf,tooltip="text")


#======================== Modelos ===================================

attach(dataProc)


#=============normalidad =====================

library(nortest)

lillie.test(Infracciones)
ks.test(Infracciones, pnorm , mean(Infracciones), sd(Infracciones))

#graficos normalidad respuesta
grafQQ<-ggplot(dataProc, aes(sample = Infracciones)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Normalidad de la variable objetivo") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

#ggplotly(grafQQ)

#graficos normalidad respuesta por grupos sexo
grafQQsexo<-ggplot(dataProc, aes(sample = Infracciones, colour=Sexo, text=paste("Sexo:", Sexo))) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Normalidad de la variable objetivo por sexo") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

ggplotly(grafQQsexo,tooltip = "text")

#graficos normalidad respuesta por grupos edad
grafQQedad<-ggplot(dataProc, aes(sample = Infracciones, colour=Edad, text=paste("Edad:", Edad))) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Normalidad de la variable objetivo por edad") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

ggplotly(grafQQedad, tooltip = "text" )

grafHist<-ggplot(data = dataProc, aes(x = Infracciones)) +
  geom_histogram(aes(y = ..density.., fill = ..count..)) +
  scale_fill_gradient(low = "#DCDCDC", high = "#7C7C7C") +
  stat_function(fun = dnorm, colour = "firebrick",
                args = list(mean = mean(dataProc$Infracciones),
                            sd = sd(dataProc$Infracciones))) +
  ggtitle("Histograma / Curva normal te�rica") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

#ggplotly(grafHist)

#==========Modelo anova ===============

anovaInfr <- aov(Infracciones~Sexo+Edad+A�o, data=dataProc)

summary(anovaInfr)

residuales<-data.frame(Residuo=anovaInfr$residuals, Ajustado=anovaInfr$fitted.values, Sexo=dataProc$Sexo, Edad=dataProc$Edad)

grafResi<-ggplot(residuales, aes(x=Ajustado, y = Residuo)) + geom_point(aes(color=Sexo)) +
  geom_hline(yintercept = 0, lty=3) +
  ggtitle("Gr�fico de dispersi�n") +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) 

ggplotly(grafResi)


#================= GLM poisson =================================

#modelos usando covariables indivuales -> SOBREDISPERSION

modSexo<- glm (Infracciones ~ Sexo, family=poisson(link="log"))
summary(modSexo)

modA�o<- glm (Infracciones ~ A�o, family=poisson(link="log"))
summary(modA�o)

modEdad<- glm (Infracciones ~ Edad, family=poisson(link="log"))
summary(modEdad)

#modelos usando covariables indivuales -> PALIANDO LA SOBREDISPERSION

modSexoNB<-glm.nb(Infracciones ~ Sexo, link="log")
summary(modSexoNB)

modEdadNB<-glm.nb(Infracciones ~ Edad, link="log")
summary(modEdadNB)

modA�oNB<-glm.nb(Infracciones ~ A�o, link="log")
summary(modA�oNB)

#vemos que por separado son todas significativas

#a�adimos Edad
modSexoEdadNB<- glm.nb(Infracciones ~ Sexo + Edad,link="log")
summary(modSexoEdad)


#a�adimos a�o
modSexoEdadA�oNB<- glm.nb (Infracciones ~ Sexo + Edad + A�o,link="log")
summary(modSexoEdadA�oNB)

#comparacion los 3 modelos

anova(modSexoNB, modSexoEdadNB, modSexoEdadA�oNB)

#manual (ejemplo)
chis<-2*(logLik(modSexoEdadA�oNB)-logLik(modSexoEdadNB))
1-pchisq(chis[1],1)


#============== Interacciones  ==============

mod1 <- glm.nb(Infracciones ~ A�o * Edad + Sexo , link="log")
summary(mod1)

#descartado
mod2 <- glm.nb(Infracciones ~ A�o + Edad * Sexo , link="log")
summary(mod2)

mod3 <- glm.nb(Infracciones ~ Edad + A�o * Sexo , link="log")
summary(mod3)

mod4 <- glm.nb(Infracciones ~ A�o * Edad * Sexo , link="log")
summary(mod4)

#descartamos mod2, el resto nos quedamos con mod3 que es el �nico v�lido
anova(modSexoEdadA�oNB, mod1)
anova(modSexoEdadA�oNB, mod2)
anova(modSexoEdadA�oNB, mod3)
anova(modSexoEdadA�oNB, mod4)



#==================MODELO ESCOGIDO: mod3 ==================

#===== grafico coeficientes: Barplot horizontal==========

auxDf<-data.frame(Estimaci�n=coef(mod3)[order(coef(mod3))],
                  Covariable=c("Intercept","Edad +64", "A�o*SexoMasculino", "A�o","Edad41-64",
                                "Edad31-40","Edad18-30",
                                "SexoMasculino")) %>%
  mutate(Correlaci�n=ifelse(Estimaci�n>0,"Positiva","Negativa")) %>% 
  arrange(Estimaci�n) %>% mutate(Covariable=factor(Covariable, levels = Covariable))

p <- ggplot(auxDf, aes(x = Covariable, y = Estimaci�n))+
  geom_col(aes(fill = Correlaci�n), width = 0.7)
p<-p+ coord_flip() + ggtitle("Relacciones covariables-respuesta") + theme(plot.title = element_text(hjust=0.5))

ggplotly(p)

#=========predicciones y graficos correspondientes==========

predicciones <- predict(mod3,type="link" ,se.fit =T)

#grafico por a�os
auxA�o<-data.frame(pre=predicciones$fit, year=rep(c(18:10),each=10))
auxA�o<-data.frame(inf=tapply(log(dataProc$Infracciones), dataProc$A�o, sum), predichos = tapply(auxA�o$pre, auxA�o$year, sum), a�o=c(2010:2018))
auxA�o<-data.frame(Infracciones=c(auxA�o$inf,auxA�o$predichos),A�o=rep(c(2010:2018),2), ind=rep(c("Reales","Predichos"),each=9))

graf <- ggplot(auxA�o, aes(A�o, Infracciones)) + geom_line(aes(col=ind)) + geom_point(aes(col=ind)) #+ geom_smooth(aes(col=ind))
graf<-graf + ggtitle("Reales frente a predichos por a�o") + labs(colour="") + ylab("Log(Infracciones)") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

graf
#ggplotly(graf)

#========== residuales ===============

resi<-data.frame(Residuo=resid(mod1), Ajustado=fitted(mod1), Edad=Edad, Sexo=Sexo)

#distinguiendo por sexo
grafResi<-ggplot(resi, aes(x=Ajustado, y = Residuo, text=paste("Sexo:",Sexo, "<br>Edad:",Edad,"<br>Valor ajustado:", round(Ajustado,2),"<br>Residuo:",round(Residuo,2)))) + geom_point(aes(color=Sexo)) +
  geom_hline(yintercept = 0, lty=3) +
  ggtitle("Gr�fico de dispersi�n") +
  theme(plot.title = element_text(hjust = 0.5,face="bold")) 

ggplotly(grafResi,tooltip = "text")

#colour=
ggplot(resi, aes(sample = Residuo)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Normalidad de los residuales") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))

#=====Capacidad predictiva de mod3 ---- Validacion cruzada 10Fold



folds<- createFolds(dataProc$Infracciones, k=10)

tasaExito<-lapply(folds, function(x){
  
  entre<-dataProc[-x,]
  test<-dataProc[x,]
  mod<-glm.nb(Infracciones~Edad+A�o*Sexo, link="log", data=entre)
  predicc<-predict(mod, newdata=test, type="response")
  aciertos<-0
  for(i in 1:length(predicc)){
    if(predicc[i]<test$Infracciones[i]*1.15 && predicc[i]>test$Infracciones[i]*0.85){
      aciertos<-aciertos+1
    }
  }
  return(aciertos/length(test$Infracciones))
  
})

tasaMedia<-mean(as.numeric(tasaExito))

df<-data.frame(Clasificacion=c("0-Fallos","1-Aciertos"),
               Tasa=c(1-tasaMedia,tasaMedia))


graf<-ggplot(df, aes(fill = Clasificacion, ymax = cumsum(Tasa), ymin = c(0, head(cumsum(Tasa), n=-1)), xmax = 2, xmin = 1)) + geom_rect(color="white") + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2))

graf + theme_void() + scale_fill_manual(values=c("#e67070", "#00ff00")) +
  geom_text(aes(x=c(1.5,1.5), y=c(0.2,0.75),label = percent(Tasa)), size=5) +
  ggtitle("Porcentaje de acierto/error") + theme(plot.title = element_text(hjust=0.55, size=20))



#======================Analisis de correspondencias==============================

datos<-read.xlsx('datosSexCom-AC.xlsx', sheetIndex = 1)

ind<- which(datos[,2]!="<NA>")

comunidad<-c("Andalucia" ,                 "Aragon"   , "Principado de Asturias", "Islas Baleares", "Islas Canarias",                 
             "Cantabria"    ,  "Castilla y Leon" ,            "Castilla-La Mancha"  ,      
             "Catalunia"  , "Comunidad Valenciana", "Extremadura", "Galicia", "Comunidad de Madrid", "Region de Murcia","Comunidad Foral de Navarra", "Pais Vasco" , "La Rioja" , "Ceuta" , "Melilla"           
)

masc <- as.numeric(as.vector(datos[ind[4:length(ind)],2]))
fem <- as.numeric(as.vector(datos[ind[4:length(ind)],3]))

dataACsexo<- data.frame(Masculino=masc,
                        Femenino=fem)

rownames(dataACsexo)<-comunidad



#----por edad

datos<-read.xlsx('datosEdadCom-AC.xlsx', sheetIndex = 1)

ind<- which(datos[,2]!="<NA>")

prim <- as.numeric(as.vector(datos[ind[4:length(ind)],2]))
seg <- as.numeric(as.vector(datos[ind[4:length(ind)],3]))
ter <- as.numeric(as.vector(datos[ind[4:length(ind)],4]))
cuart <- as.numeric(as.vector(datos[ind[4:length(ind)],5]))
quin <- as.numeric(as.vector(datos[ind[4:length(ind)],6]))


dataACEdad<- data.frame(E14_17=prim,
                        E18_30=seg,
                        E31_40=ter,
                        E41_64=cuart,
                        E64_mas=quin)

#tabla para ver los totales
#dataACEdad$Total <- apply(dataACEdad,1,sum)
#dataACEdad<-as.data.frame(rbind(dataACEdad,apply(dataACEdad,2,sum)))
#rownames(dataACEdad)<-c(comunidad,"Total")

rownames(dataACEdad)<-comunidad

propor_contig_edad<-as.data.frame(prop.table(as.matrix(dataACEdad[1:19,1:5])))

ac<-CA(dataACEdad)
summary(ac)
get_eigenvalue(ac)

#grafico de contribucion de cada dimension
ggplotly(fviz_screeplot(ac, addlabels=TRUE) +
           geom_hline(yintercept=33.33, linetype=2, color="red") + 
           theme(plot.title = element_text(hjust = 0.5,face="bold")))


#contribuciones filas y columnas a las dimensiones 1 y 2
rdim1<-ggplotly(fviz_contrib(ac, choice = "row", axes = 1) +
                  ggtitle("Contribuci�n perfiles fila")+
                  theme(plot.title = element_text(hjust = 0.5, face="bold")))
rdim2<-ggplotly(fviz_contrib(ac, choice = "row", axes = 2)+
                  ggtitle("Contribuci�n perfiles fila")+
                  theme(plot.title = element_text(hjust = 0.5, face="bold")))

filas<-subplot(rdim1, rdim2,
                  shareY = TRUE, heights = 0.9)

filas %>% layout(annotations = list(
  list(x = 0.2 , y = 1, text = "Dimension 1", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.8 , y =1, text = "Dimension 2", showarrow = F, xref='paper', yref='paper'))
)

cdim1<-ggplotly(fviz_contrib(ac, choice = "col", axes = 1) +
                  ggtitle("Contribuci�n perfiles columna")+
                  theme(plot.title = element_text(hjust = 0.5, face="bold")))
cdim2<-ggplotly(fviz_contrib(ac, choice = "col", axes = 2) +
                  ggtitle("Contribuci�n perfiles columna")+
                  theme(plot.title = element_text(hjust = 0.5, face="bold")))

columnas<-subplot(cdim1, cdim2,
               shareY = TRUE, heights = 0.9)

columnas %>% layout(annotations = list(
  list(x = 0.2 , y = 1, text = "Dimension 1", showarrow = F, xref='paper', yref='paper'),
  list(x = 0.8 , y =1, text = "Dimension 2", showarrow = F, xref='paper', yref='paper'))
)


#representacion categorias
fviz_ca_col(ac)
fviz_ca_row(ac, repel = TRUE)
fviz_ca_biplot(ac, repel = TRUE)+
  ggtitle("Biplot analisis de correspondencias")+
  theme(plot.title = element_text(hjust = 0.25, face="bold"))


#----por tipologia-----


data <- read.xlsx('total-tiposInfracComSinResto-18.xlsx', sheetIndex = 1)

nombresDelitos <- c("Asesinatos consumados","Asesinatos en grado tentativa", "Delitos de lesiones", "Secuestro", "Delitos contra la libertad e indemnidad sexual", "Agresion sexual con penetracion", "Resto de delitos contra la libertad", "Robos con violencia e intimidacion","Robos con fuerza","Robos en domicilios","Hurtos", "Sustraccion de vehiculos","Trafico de drogas")


tipos <- which(data[,2]!="<NA>")
tipos <- tipos[2:(length(tipos))]

valoresCom_Tipo <- as.numeric(as.vector(data[tipos,2]))


final<-matrix(nrow = 19, ncol = 13 )
i<-1
z<-1
while(i < length(valoresCom_Tipo)){
  
  
  final[,z]<-valoresCom_Tipo[i:(18+i)]
  
  #final<-cbind(final,aux)
  i<-i+19
  z<-z+1
  
}

final<-as.data.frame(final)
colnames(final)<-nombresDelitos
rownames(final)<-comunidad


acTipo<-CA(final)
summary(acTipo)

fviz_ca_biplot(acTipo, repel = TRUE)+
  ggtitle("Biplot analisis de correspondencias")+
  theme(plot.title = element_text(hjust = 0.25, face="bold"))


#================ Analisis de componentes principales ================

setwd('C:/Users/Iv�n/Desktop/INDAT/5�/Segundo cuatrimestre/TFG-Estadistica/C�digo/')


datos<-read.csv("tasaTiposCom-18.csv", header=T)

finalACP<-matrix(nrow = 19, ncol = 13 )
i<-1
z<-1
while(i < length(datos$value)){
  
  
  finalACP[,z]<-datos$value[i:(18+i)]
  
  #final<-cbind(final,aux)
  i<-i+19
  z<-z+1
  
}

finalACP<-as.data.frame(finalACP)
colnames(finalACP)<-nombresDelitos
rownames(finalACP)<-comunidad

pca <- prcomp(finalACP, scale = TRUE)
summary(pca)
#pca$rotation

pca$rotation <- -pca$rotation
pca$x        <- -pca$x
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

#screeplot
ggplotly(fviz_screeplot(pca, addlabels=TRUE) +
           geom_hline(yintercept=33.33, linetype=2, color="red") + 
           theme(plot.title = element_text(hjust = 0.5,face="bold")))

#nube de variables bonito
fviz_pca_var(pca, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)



pca3d(pca,show.ellipses=TRUE,
      ellipse.ci=0.75, show.plane=TRUE, biplot=TRUE,
      fancy=TRUE, title="Representaci�n 3D de las componentes principales",
      radius = 1.25)

rgl::writeWebGL("pca3D.html")

#para obener nube de variables de form alternativa
a<-PCA(finalACP)
#summary(a)
plot(a,choix="var")


#=============== Clasificacion: KMEANS ===========

#mirar suma de cuadrado para ver el n�mero de cluster

wss <- (nrow(finalACP)-1)*sum(apply(finalACP,2,var))

for (i in 2:13){
  wss[i] <- sum(kmeans(finalACP,centers=i)$tot.withinss)
}

aux<-data.frame(Clusters=c(1:13),
                WSEE=wss)

#con el plot se ve que se cogen 3 cluster, cuando se ve el codo
ggplotly(
  ggplot(aux, aes(x=Clusters, y=WSEE)) +
    geom_point() +
    geom_line() +
    ggtitle("Total within-clusters SSE / Numero de clusters")+
    theme(plot.title = element_text(hjust = 0.5, face="bold")))


comp<- pca$x[,1:3]

k <- kmeans(comp, 3, nstart=25, iter.max=1000)

comp<-as.data.frame(comp)
comp<- comp %>% 
  mutate(size=round(apply(finalACP,1, mean),2)) %>% 
  mutate(Comunidad= as.factor(rownames(finalACP))) %>% 
  mutate(Cluster=as.factor(k$cluster))

colors <- c('#965F8A', '#FF7070', '#C61951')
fig <- 
  plot_ly(comp, x = ~PC1, y = ~PC2, z = ~PC3,
          color = ~Cluster, size = ~size, colors = colors,
               marker = list(symbol = 'circle',
                             sizemode = 'diameter'),
          sizes = c(5, 150),
          text = ~paste('Comunidad:', Comunidad,
                        '<br>Tasa media criminalidad:', size,
                        '<br>Cluster:', Cluster))

fig <- fig %>% 
  layout(title = 'Clasificaci�n comunidades K-Medias',
         scene = list(
           xaxis = list(title = 'PC1',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,ticklen = 5,
                        gridwidth = 2),
           yaxis = list(title = 'PC2',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,ticklen = 5,
                        gridwith = 2),
           zaxis = list(title = 'PC3',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,ticklen = 5,
                        gridwith = 2)),
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      plot_bgcolor = 'rgb(243, 243, 243)')

fig

