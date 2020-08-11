
#Ivan Lopez de Munain Quintana

#TFG - Investigación estadística en la criminología

# =============== Mapa interactivo Europa ==================== 


library("plotly") 
library("RColorBrewer")


datosEuropa<-read.csv("europaTasaCrimenes.csv", header=T, sep=",")

#tasa por cada 100000 habitantes
datosEuropa <- datosEuropa %>%
  mutate(Value=as.numeric(as.character(datosEuropa$Value)))

levels(datosEuropa$GEO)<-c(levels(datosEuropa$GEO),"United Kingdom")

df17<- datosEuropa %>% filter(TIME=="2017")
df17$GEO[which(df17$GEO=="England and Wales")]<-"United Kingdom"
df17<- df17[order(df17$GEO),]

df16<- datosEuropa %>% filter(TIME=="2016")
df16$GEO[which(df16$GEO=="England and Wales")]<-"United Kingdom"
df16<- df16[order(df16$GEO),]

df15<- datosEuropa %>% filter(TIME=="2015")
df15$GEO[which(df15$GEO=="England and Wales")]<-"United Kingdom"
df15<- df15[order(df15$GEO),]

aa<-levels(df17$GEO)[-10]
aa[13]<-"Germany"
aa[19]<-"Kosovo"


a17<-tapply( df17$Value[!is.na(df17$Value)] , df17$GEO[!is.na(df17$Value)],sum)
a16<-tapply( df16$Value[!is.na(df16$Value)] , df16$GEO[!is.na(df16$Value)],sum)
a15<-tapply( df15$Value[!is.na(df15$Value)] , df15$GEO[!is.na(df15$Value)],sum)

datos17<-data.frame(value=a17[-10], pais=as.factor(aa))
datos17<- datos17[order(datos17$pais),]

datos16<-data.frame(value=a16[-10], pais=as.factor(aa))
datos16<- datos16[order(datos16$pais),]

datos15<-data.frame(value=a15[-10], pais=as.factor(aa))
datos15<- datos15[order(datos15$pais),]


#====concatenacion de los textos====
aux <- with(df17, paste('<br>->',  ICCS, ":",  Value))

i<-0
conca<-NULL
while(i < length(aux)){
 
  conca[(1+i/13)]<-paste(aux[i+1], aux[i+2], aux[i+3], aux[i+4], aux[i+5], aux[i+6],
                     aux[i+7], aux[i+8], aux[i+9], aux[i+10], aux[i+11], aux[i+12], aux[i+13])
  i<-i+13 
}

datos17$texto<-conca
datos17$texto<-paste("<br>Categorization of crimes in 2017:",datos17$texto)

#2016

aux <- with(df16, paste('<br>->',  ICCS, ":",  Value))

i<-0
conca<-NULL
while(i < length(aux)){
  
  conca[(1+i/13)]<-paste(aux[i+1], aux[i+2], aux[i+3], aux[i+4], aux[i+5], aux[i+6],
                         aux[i+7], aux[i+8], aux[i+9], aux[i+10], aux[i+11], aux[i+12], aux[i+13])
  i<-i+13 
}

datos16$texto<-conca
datos16$texto<-paste("<br>Categorization of crimes in 2016:",datos16$texto)


#2015

aux <- with(df15, paste('<br>->',  ICCS, ":", Value))

i<-0
conca<-NULL
while(i < length(aux)){
  
  conca[(1+i/13)]<-paste(aux[i+1], aux[i+2], aux[i+3], aux[i+4], aux[i+5], aux[i+6],
                         aux[i+7], aux[i+8], aux[i+9], aux[i+10], aux[i+11], aux[i+12], aux[i+13])
  i<-i+13 
}

datos15$texto<-conca
datos15$texto<-paste("<br>Categorization of crimes in 2015:",datos15$texto)


updatemenus <- list(
  list(
    active = 0,
    type= 'buttons',
    showactive = F,
    buttons = list(
      list(
        label = "Año 2017",
        method = "update",
        args = list(list(visible = c(T, T, F,F)))),
      list(
        label = "Año 2016",
        method = "update",
        args = list(list(visible = c(T,F, T,F))))
      ,list(
        label = "Año 2015",
        method = "update",
        args = list(list(visible = c(T,F, F,T))))
    )))
# colors:
cols=brewer.pal(9, "Reds")


plot_geo() %>% 
  add_trace(z = ~value, data = datos17,  locations = c("x"), locationmode= "country names", color = ~value, colors = cols,visible=T,showscale=T) %>% # dummy trace for constant legend
  add_trace(z = ~value, data = datos17, text= ~texto, locations = ~pais, locationmode= "country names", color = ~value, colors = cols,visible=T,showscale=FALSE) %>%
  add_trace(z = ~value, data = datos16, text= ~texto, locations = ~pais, locationmode= "country names", color = ~value, colors =cols,visible=F,showscale=FALSE)  %>% 
  add_trace(z = ~value, data = datos15, text= ~texto, locations = ~pais, locationmode= "country names", color = ~value, colors = cols,visible=F,showscale=F)  %>% 
 layout(geo = list(scope="europe", projection=list(scale=1.6)), 
         title="Número de delitos cometidos por cada 100.000 habitantes",
         margin=list(l=0,r=0,b=0,t=30), updatemenus=updatemenus)  %>% 
  colorbar(title = "", thickness=10, which=1, yanchor="bottom", x=1, y=0.5)  

