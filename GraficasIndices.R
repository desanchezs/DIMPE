library(readxl)
library(dplyr)
library(zoo)

datos<-  read_excel("MMH/DatosMMH.xlsx", sheet = "IndicesHoteles")
datos[is.na(datos)] <- 0
###################
library(reshape)
f<- function(x){
  y<- stl(ts(as.vector(ts(x)), start=c(2004,7), frequency=12),
          s.window = "periodic")$time.series[,3]
  return(y)
}
dd <- datos %>% select(-Años, -Meses) %>% sapply(., f) %>% as_tibble() %>% 
  dplyr::mutate(.,Date=as.Date.yearmon(2004 + seq(6, 186)/12))


df.melted <- data.table::melt(dd, id = "Date")

library(scales)
library(ggplot2)
ggplot(data = df.melted, aes(x = Date, y = value, color= variable)) +
  geom_line() + facet_grid(variable ~ .)+
  scale_x_date(date_labels = "%b-%y",  expand =c(0,0),
               date_breaks = "4 month") +
  geom_hline(aes(yintercept=0))+
  labs(title = "COMPONENTE IRREGULAR SERIES DE ÍNDICES DE LA MUESTRA MENSUAL DE HOTELES")+
  theme(strip.text.y = element_blank(), legend.position="bottom",
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 8))

####### Tarifas
datos<-  read_excel("MMH/DatosMMH.xlsx", sheet = "Tarifas")
datos[is.na(datos)] <- 0
###################
f<- function(x){
  y<- stl(ts(as.vector(ts(x)), start=c(2005,7), frequency=12),
          s.window = "periodic")$time.series[,3]
  return(y)
}

dd <- datos %>% select(-Años, -Meses) %>% sapply(., f) %>% as_tibble() %>% 
  dplyr::mutate(.,Date=as.Date.yearmon(2005 + seq(0, 174)/12))


df.melted <- data.table::melt(dd, id = "Date")
ggplot(data = df.melted, aes(x = Date, y = value, color= variable)) +
  geom_line() + facet_grid(variable ~ .)+
  scale_x_date(date_labels = "%b-%y",  expand =c(0,0),
               date_breaks = "4 month") +
  geom_hline(aes(yintercept=0))+
  labs(title = "COMPONENTE IRREGULAR SERIES DE ÍNDICES TARIFAS PROMEDIO POR ACOMODACIÓN")+
  theme(strip.text.y = element_blank(), legend.position="bottom",
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 8))

