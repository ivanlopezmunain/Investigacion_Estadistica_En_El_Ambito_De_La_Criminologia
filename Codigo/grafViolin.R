library(plotly)

df<-data.frame(Sexo=rep(c("masc","fem"),10), edad=rep(c("Joven","Anciano"), each=10), inf=runif(20,1,10), texto=c("a"))

p <- df %>%
  plot_ly(type = 'violin') %>%
  add_trace(
    x = ~edad[df$Sexo == 'fem'],
    y = ~inf[df$Sexo == 'fem'],
    legendgroup = 'Yes',
    scalegroup = 'Yes',
    name = 'Yes',
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
    x = ~edad[df$Sexo == 'masc'],
    y = ~inf[df$Sexo == 'masc'],
    legendgroup = 'No',
    scalegroup = 'No',
    name = 'No',
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
      title = "Numero de infracciones",
      zeroline = F
    ),
    violingap = 0,
    violingroupgap = 0,
    violinmode = 'overlay'
  )

p

