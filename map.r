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

##Data
{
abast <- read_excel("DATA/Abastecimiento.xlsx", 
                    sheet = "Volumenes de destino")
j <- abast %>% select(-t) %>% names()
abast <- gather(abast, j , key="Departamento", value="Volumenes de destino")
rm(j)
abast <- abast %>% arrange(Departamento)
mean_abast <- abast %>% select(-t) %>% group_by(Departamento) %>% 
  summarise_all(mean) %>% mutate(State = toupper(substring(Departamento, 1, 2)))
}
###Mapa
{
map_col <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/co/co-all.js"))
state= map_col%>%
  select(`hc-a2`)%>%
  arrange(`hc-a2`)

mean_abast <- state %>%
  left_join(mean_abast, by = c("hc-a2" = "State")) %>% 
  select("hc-a2", "Volumenes de destino")
mean_abast <- as.data.frame(mean_abast)
mean_abast$"Volumenes de destino" <- replace_na(mean_abast$"Volumenes de destino",0)
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

###########
##Trend
abast %>% 

