#Anali del mercado de valores en el segmento bancario en función 
#de los valores bancarios que se enumeran en NSE India.

#objetivo: Encontrar las tendencias (estacionales o cíclicas) 
#en las acciones bancarias.

#Paquetes a usar
##Quandl para descarga de datos
##timetk para forzar el marco de datos en xts
##tidyverse para usar tibble para agrupar y realizar una sola operación en múltiples grupos
##tidyquant para series temporales y funciones financieras para realizar el análisis
##gglot para trazar y visualizar
##gganimate para trazar los precios mensuales. 
##forcats para modificar los niveles de factor
##stringr para uso de cuerdas
##Learn to pronounce
library(Quandl)
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(timetk)
library(forcats)
library(stringr)
library(gganimate)
library(plyr)
library(stringr)
library(gridExtra)

Quandl.api_key("AfZrVk7b3Wmk8xQ7EV-3")

## descargar los datos
ICICI = Quandl("NSE/ICICIBANK",collapse="daily",start_date="2016-09-01",type="raw")
PNB= Quandl("NSE/PNB",collapse="daily",start_date="2016-09-01",type="raw")
Axis=Quandl("NSE/AXISBANK",collapse="daily",start_date="2016-09-01",type="raw")
Canara=Quandl("NSE/CANBK",collapse="daily",start_date="2016-09-01",type="raw")
BOB=Quandl("NSE/BANKBARODA",collapse="daily",start_date="2016-09-01",type="raw")
SBI=Quandl("NSE/SBIN",collapse="daily",start_date="2016-09-01",type="raw")

## Adicionar otra columna ("Stock") 
ICICI<-cbind(ICICI,Stock="")
PNB<-cbind(PNB,Stock="")
Axis<-cbind(Axis,Stock="")
SBI<-cbind(SBI,Stock="")
Canara<-cbind(Canara,Stock="")
BOB<-cbind(BOB,Stock="")

## Pegar el nombre del "stock" en la columna stock
ICICI$Stock<-paste(ICICI$Stock,"ICICI",sep="")
PNB$Stock<-paste(PNB$Stock,"PNB",sep="")
Axis$Stock<-paste(Axis$Stock,"Axis",sep="")
SBI$Stock<-paste(SBI$Stock,"SBI",sep="")
Canara$Stock<-paste(Canara$Stock,"Canara",sep="")
BOB$Stock<-paste(BOB$Stock,"BOB",sep="")

## Unir en un solo data set
Master_Data<-rbind(ICICI,PNB,Axis,SBI,Canara,BOB)

## Visuali en ggplot2
## pasa los datos a caracteres para dividir la columna en columnas "Y" "m" "dt" "
Master_Data$Date<-as.character(Master_Data$Date)

##Divide la fecha y crea una lista para el mismo
list<-strsplit(Master_Data$Date,"-")

## Convierte la lista en un dataset
library(plyr)
Master_Date1<-ldply(list)
colnames(Master_Date1)<-c("Year","Month","Day")

## Enlace de columna con el marco de datos principal
Master_Data<-cbind(Master_Data,Master_Date1)
names(Master_Data)

## Cambiar la escala para la cantidad negociada
Master_Data$`Total Trade Quantity`<-Master_Data$`Total Trade Quantity`/100000

## Convertir el Date a as.Date()
Master_Data$Date<-as.Date(Master_Data$Date)

## Visualización con "Bubble Plot"
P<- ggplot(Master_Data,aes(factor(Stock),Close,color=Stock,frame=Month)) +
  geom_jitter(aes(size = Close, colour=Stock, alpha=.02)) +
  ylim(0,1000)+
  labs(title = "Bank Stock Monthly Prices", x = "Banks", y= "Close Price") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"))+
  theme(legend.position="none")


P1<-gganimate(P,'Price_Range.gif',ani.width=600,ani.height=400,interval=1)

## Agrupar por Stock
Master_Data<-Master_Data%>%
  tibble::as.tibble()%>%
  group_by(Stock)

## Visualización para los precios diarios
Master_Data %>%
  ggplot(aes(x = Date, y = Close, color = Stock)) +
  geom_point() +
  labs(title = "Daily Close Price", x = "Month",y="Close Price") +
  facet_wrap(~ Stock, ncol = 3, scale = "free_y") +
  scale_fill_tq(fill="green4",theme="light") +
  theme_tq() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"))+
  theme(legend.position="none")

## Cantidad negociada vs precio
z<-Master_Data %>%
  ggplot(aes(x = `Total Trade Quantity`, y = Close, color = Stock,frame=Month)) +
  geom_smooth(method='loess') +
  xlim(0,400)+
  labs(title = "Monthly Traded Quantity vs Price", x = "Traded Quantity (Lacs)",y="Close Price") +
  facet_wrap(~ Stock, ncol = 3, scale = "free_y") +
  scale_fill_tq(fill="green4",theme="light") +
  theme_tq() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"),
        axis.line = element_line(colour = "black"))+
  theme(legend.position="none")

z1<-gganimate(z,'Quantity_Price.gif',ani.width=600,ani.height=400,interval=0.7)


#Distribución de densidad de la desviación del precio alto respecto precio de apertura
##1. diferencia de precio alto y de apertura
##2. diferencia de precio bajo y de apertura
##3. Calcule el promedio semanal de diferencias 

##1
Master_Data_High<-Master_Data%>%mutate(Dev_High=High-Open)
Master_Data_Low<-Master_Data%>%mutate(Dev_Low=Open-Low)

##2 Precio alto 

Master_Data_High_Week <- Master_Data_High %>%
  tq_transmute(
    select     = Dev_High,
    mutate_fun = apply.weekly, 
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "Dev_High_Mean"
  )

##Precio bajo
Master_Data_Low_Week<-Master_Data_Low%>%
  tq_transmute(
    select  = Dev_Low,
    mutate_fun = apply.weekly,
    FUN = mean,
    na.rm = TRUE,
    col_rename = "Dev_Low_Mean"
  )

## Visualización de la distribución de densidad precio alto
High<-Master_Data_High_Week%>%ggplot(aes(x=Dev_High_Mean,color=Stock))+
  geom_dotplot(binwidth=0.50,aes(fill=Stock))+
  xlim(0,10)+
  scale_fill_manual(values=c("#999999", "#E69F00","#CC9933","#99FF00","#CC3399","#FF9933"))+
  labs(title="Distribution of High Price Deviation from Open Price",x="Weekly Mean Deviation")+
  facet_wrap(~Stock,ncol=3,scale="free_y")+
  scale_color_tq(values=c("#999999"))+
  theme_tq()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=16,colour="indianred4"))+
  theme(legend.position="none")

## Visualización de la distribución de densidad precio bajo
Low<-Master_Data_Low_Week%>%ggplot(aes(x=Dev_Low_Mean,color=Stock))+
  geom_dotplot(binwidth=0.50,aes(fill=Stock))+
  xlim(0,10)+
  scale_fill_manual(values=c("#999999", "#E69F00","#CC9933","#99FF00","#CC3399","#FF9933"))+
  labs(title="Distribution of Weekly Low Price Deviation from Open Price",x="Weekly Mean Deviation")+
  facet_wrap(~Stock,ncol=3,scale="free_y")+
  scale_color_tq(values=c("#999999"))+
  theme_tq()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=16,colour="indianred4"))+
  theme(legend.position="none")
##Arreglo
grid.arrange(High,Low,ncol = 2, nrow = 1)
k <- 1:180
col_names <- paste0("lag_", k)

## Solo seleccione columnas "Date" y "Close" del hte master data frame.
Master_Data_lags<-Master_Data%>%
  tibble::as_tibble() %>%
  group_by(Stock)

Master_Data_lags<-Master_Data_lags%>%select(Date,Close)
# aplique la función lag.xts usando tq_mutate 
# Métodos para calcular retrasos y diferencias en objetos xts
Master_Data_lags<-Master_Data_lags%>%
  tq_mutate(
    select = Close,
    mutate_fun = lag.xts,
    k=1:180,
    col_rename=col_names
  )

#Calula la auntocorrelación y el corte 95%
Master_Data_AutoCorrelations<-Master_Data_lags%>%
  gather(key = "lag", value = "lag_value", -c(Stock,Date, Close))%>%
  mutate(lag = str_sub(lag, start = 5)%>%as.numeric)%>%
  group_by(Stock, lag)%>%
  summarize(
    cor = cor(x = Close, y = lag_value, use = "pairwise.complete.obs"),
    cutoff_upper = 2/(n())^0.5,
    cutoff_lower = -2/(n())^0.5
  )

## Visualización de la autocorrelación: ACF Plot
Master_Data_AutoCorrelations %>%
  ggplot(aes(x = lag, y = cor, color = Stock, group = Stock)) +
  
  # Adicionar una linea horizontal y=0
  geom_hline(yintercept = 0) +
  
  # Plot autocorrelaciones
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  
  # adicionar cortes
  geom_line(aes(y = cutoff_upper), color = "blue", linetype = 2) +
  geom_line(aes(y = cutoff_lower), color = "blue", linetype = 2) +
  
  # Agregar facetas
  facet_wrap(~ Stock, ncol = 3) +
  
  # Estética
  expand_limits(y = c(-1, 1)) +
  scale_color_tq() +
  theme_tq() +
  labs(
    title = paste0("Tidyverse ACF Plot: Lags ", rlang::expr_text(k)),
    x = "Lags"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
    plot.title = element_text(hjust = 0.5,size=18,colour="indianred4")
  )


