library(ggplot2)
library(plotly)
library(plyr)
library(flexdashboard)
library(readxl)
library(knitr)
library(DT)
library(rpivotTable)
library(dplyr)
library(openintro)
library(highcharter)
library(ggvis)
library(tidyverse)
library(ggthemes)
library(gridExtra)

######################################################################
################################### Seguridad Y Convivencia
######################################################################
{
## Read the Data  
convivencia <- read_excel("DATA/Seguridad-Convivencia.xlsx", 
                                    sheet = "Indicadores de convivencia")
seguridad <- read_excel("DATA/Seguridad-Convivencia.xlsx", 
                          sheet = "Indicadores de seguridad")
View(head(seguridad))
## Line plots
graph1 <- ggplot(data = seguridad) +
  geom_line(mapping = aes(x = t, y = hurt_personas, colour = "Hurto Personas")) + 
  geom_line(mapping = aes(x = t, y = hurt_comercio, colour = "Hurto Comercios")) +
  geom_line(mapping = aes(x = t, y = lesiones, colour = "Lesiones Personales")) + 
  geom_line(mapping = aes(x = t, y = homicidios, colour = "Homicidios")) 

theme_economist_white(base_size = 11, base_family = "sans",
                      gray_bg = TRUE, horizontal = TRUE)

graph1 <- graph1 + theme_economist_white(gray_bg = FALSE) +
  scale_colour_economist() + 
  ylab('Número de Casos')+xlab('Fecha')

ggplotly(graph1)


## Growth rate
  seguridad = seguridad %>% arrange(t) %>% 
  mutate(tasa_hurt_personas =((hurt_personas - lag(hurt_personas))/lag(hurt_personas)*100),
         tasa_hurt_comercio =((hurt_comercio - lag(hurt_comercio))/lag(hurt_comercio)*100),
         tasa_lesiones =((lesiones - lag(lesiones))/lag(lesiones)*100),
         tasa_homicidios =((homicidios - lag(homicidios))/lag(homicidios)*100)) 

ggplot(data = seguridad) +
  geom_line(mapping = aes(x = t, y = tasa_hurt_personas, colour = "Hurto Personas")) + 
  geom_line(mapping = aes(x = t, y = tasa_hurt_comercio, colour = "Hurto Comercios")) +
  geom_line(mapping = aes(x = t, y = tasa_lesiones, colour = "Lesiones Personales")) + 
  geom_line(mapping = aes(x = t, y = tasa_homicidios, colour = "Homicidios")) + 
  ylab('Número de Casos')+xlab('Fecha')

###############

crimen <- rbind("Hurto Personas", "Hurto Comercio", "Lesiones Personales", "Homicidios")
total <- rbind(sum(seguridad$hurt_personas), sum(seguridad$hurt_comercio), sum(seguridad$lesiones), sum(seguridad$homicidios))
total <- mapply(total, FUN=as.numeric)
data1 <- data.frame(crimen, total)
data1 <- data1 %>% mutate(pp=total/sum(total)*100)

fig <- data1 %>% plot_ly(type='pie', labels=crimen, values=total, 
               textinfo='percent',
               insidetextorientation='radial')
fig 
}
######################################################################
################################### Transito
######################################################################
{
#import data
transito <- read_excel("DATA/Transito.xlsx", 
                       sheet = "Transito") 

transito %>% select(-Tiempo) %>%  group_by(Categoria)


transito %>% filter(Categoria != "comparendos") %>%
  ggplot(aes(x = Tiempo, y = Numero)) +
  geom_bar(
    aes(fill = Indicador),
    stat = "identity", position = "dodge",
  ) + facet_grid(Categoria ~ .)

transito %>% filter(Categoria != "comparendos") %>%
ggplot(aes(y= Numero, x = Tiempo, group = Indicador, colour = Indicador)) +
  facet_wrap(~Categoria) +
  geom_line()
             


transito %>% select(-Tiempo) %>% group_by(Categoria, Indicador) %>% summarise_all(sum) %>% 
  mutate(is_comparendo=ifelse((Categoria=="comparendos"),1,0)) %>% 
  filter(is_comparendo != 1) %>%
ggplot() +
  geom_bar(
    aes(x = Categoria, y = Numero, fill = Indicador),
    stat = "identity", position = "dodge",
  ) 

#Accdidentes
a<- transito %>% filter(Categoria == "accidentes") %>%
ggplot(transito, mapping=aes(x=Tiempo, y=Numero, colour = Indicador)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()         # Add a loess smoothed fit curve with confidence region

#Lesiones
b<- transito %>% filter(Categoria == "lesiones") %>%
  ggplot(transito, mapping=aes(x=Tiempo, y=Numero, colour = Indicador)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()         # Add a loess smoothed fit curve with confidence region

#Muertes
c<- transito %>% filter(Categoria == "muertes") %>%
  ggplot(transito, mapping=aes(x=Tiempo, y=Numero, colour = Indicador)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth()         # Add a loess smoothed fit curve with confidence region

#Comparendos
d<- transito %>% filter(Categoria == "comparendos") %>%
  ggplot(transito, mapping=aes(x=Tiempo, y=Numero)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method = lm)  +       # Add a loess smoothed fit curve with confidence region
  facet_grid(Indicador ~ .)

prueba <- grid.arrange(a, b, c, d, nrow = 2)
prueba

### Chart (5)

```{r}
graph5 <- transito %>% filter(Categoria != "comparendos") %>%
  ggplot(aes(x = Tiempo, y = Numero)) +
  geom_bar(
    aes(fill = Indicador),
    stat = "identity", position = "dodge",
  ) + facet_grid(Categoria ~ .) + 
  ylab('Número de Casos')+xlab('Fecha')

graph5 <- graph5 + theme_economist_white(gray_bg = FALSE) +
  scale_colour_economist() 

ggplotly(graph5)
```

}

######################################################################
################################### Abastecimiento
######################################################################
{
  ############ MAPA
  {  
  ##Data
  {
    abast <- read_excel("DATA/Abastecimiento.xlsx", 
                        sheet = "Volumenes de destino")
    j <- abast %>% select(-t) %>% names()
    abast <- gather(abast, j , key="Departamento", value="Volumenes")
    rm(j)
    abast <- abast %>% arrange(Departamento)
    mean_abast <- abast %>% select(-t) %>% group_by(Departamento) %>% 
      summarise_all(mean) %>% mutate(State = toupper(substring(Departamento, 1, 2)))
  }
  ###Plot
  {
    map_col <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/co/co-all.js"))
    state= map_col%>%
      select(`hc-a2`)%>%
      arrange(`hc-a2`)
    
    mean_abast <- state %>%
      left_join(mean_abast, by = c("hc-a2" = "State")) %>% 
      select("hc-a2", "Volumenes")
    mean_abast <- as.data.frame(mean_abast)
    mean_abast$"Volumenes" <- replace_na(mean_abast$"Volumenes",0)
    names(mean_abast)= c("Departamento", "volumenes")
    rm(state)
    hcmap("https://code.highcharts.com/mapdata/countries/co/co-all.js", data = mean_abast, value = "volumenes",
          joinBy = c("hc-a2", "Departamento"), name = "Volumenes de destino",
          dataLabels = list(enabled = TRUE, format = '{point.name}'),
          borderColor = "#FAFAFA", borderWidth = 0.1,
          tooltip = list(valueDecimals = 1)) %>%
      hc_title(text = "Volumenes de Destino") %>%
      hc_subtitle(text = "Fuente: Alcadia Bucaramanga") %>%
      hc_mapNavigation(enabled = T)
  }
  }
  ############ Tendencias
  {

    theme_set(theme_bw())  
    
    abast <- abast %>% group_by(Departamento) %>% mutate(volumen_z = (Volumenes - mean(Volumenes))/sd(Volumenes), volumen_tipo = ifelse(volumen_z < 0, "Menor", "Mayor"), volumen_tipo2 = ifelse(volumen_z < 0, 0, 1))
    
    graph9 <-     abast %>%
      ggplot()+
      geom_bar(aes(x=t, y=volumen_z, fill=volumen_tipo),  stat='identity') +
      scale_fill_manual(name="Volumen de Destino", 
                        labels = c("Mayor al promedio", "Menor al promedio"), 
                        values = c("Mayor"="#00ba38", "Menor"="#f8766d"))  +
      coord_flip() +
      ylab('Volumen de Destino')+xlab('Fecha') +
      facet_grid(.~Departamento)
    
    graph9 <- graph9 + theme_bw() + theme(legend.position="top")
  }
}