#Comercio con bandas de Bollinger

#OBJETIVO: identificar patrones para saber cómo se comporta una acción. 
#Predecir los precios de las acciones utilizando la metodología convencional ARIMA.

##Bandas de Bollinger: Estas bandas representan la volatilidad 
##de las acciones a medida que aumenta o  disminuye. Las bandas se colocan
##por encima  y por debajo de la línea de promedio móvil de las acciones.   
##Cuanto mayor es la brecha entre las bandas, mayor es el grado de volatilidad.

##Hay tres líneas en la Banda de Bollinger,
#La línea media con promedio móvil del período N (MA); SMA de 20 días
#Una banda superior en K multiplicada por una desviación estándar del período 
# N por encima de la media móvil; SMA + de 20 días (des están de precio 20díasx2)
#Una banda inferior en K multiplicada por una desviación estándar del período N 
# por debajo de la media móvil; SMA de 20 días: (des están de precio de 20díasx2)

####NOTA
##(La SMA es el promedio móvil simple, la desviación estándar, el período K y N 
##generalmente se establece en 20 días. Las bandas superior e inferior se colocan 
##2 unidades arriba y abajo respectivamente.)

library(Quandl)
library(tidyverse)
library(tidyquant)
library(timetk)
library(forecast)
library(gridExtra)
library(tibble)

##Please copy and paste the API key in order to 
Quandl.api_key("AfZrVk7b3Wmk8xQ7EV-3")

## descargue el data Set
ICICI = Quandl("NSE/ICICIBANK",collapse="daily",start_date="2016-09-01",type="raw")
PNB= Quandl("NSE/PNB",collapse="daily",start_date="2016-09-01",type="raw")
Axis=Quandl("NSE/AXISBANK",collapse="daily",start_date="2016-09-01",type="raw")
Canara=Quandl("NSE/CANBK",collapse="daily",start_date="2016-09-01",type="raw")
BOB=Quandl("NSE/BANKBARODA",collapse="daily",start_date="2016-09-01",type="raw")
SBI=Quandl("NSE/SBIN",collapse="daily",start_date="2016-09-01",type="raw")

## Adicione otra columna en el dataset llamada "Stock"
ICICI<-cbind(ICICI,Stock="")
PNB<-cbind(PNB,Stock="")
Axis<-cbind(Axis,Stock="")
SBI<-cbind(SBI,Stock="")
Canara<-cbind(Canara,Stock="")
BOB<-cbind(BOB,Stock="")

## Pegue el nombre de la acción en esa columna
ICICI$Stock<-paste(ICICI$Stock,"ICICI",sep="")
PNB$Stock<-paste(PNB$Stock,"PNB",sep="")
Axis$Stock<-paste(Axis$Stock,"Axis",sep="")
SBI$Stock<-paste(SBI$Stock,"SBI",sep="")
Canara$Stock<-paste(Canara$Stock,"Canara",sep="")
BOB$Stock<-paste(BOB$Stock,"BOB",sep="")

##Consolide sobre un solo data set
Master_Data<-rbind(ICICI,PNB,Axis,SBI,Canara,BOB)  

## Coaccione la variable "Date" que esta en caracter a fecha con as.Date
Master_Data$Date<-as.Date(Master_Data$Date)
end<-ymd("2017-09-01")
start<-ymd("2016-09-01")

Master_Data<-Master_Data%>% as_tibble() %>% group_by(Stock)

#Identificación de patrones

##El patrón de doble techo muestra que la demanda está superando a la oferta 
#(predominan los compradores) hasta el primer techo, lo que hace que los precios suban.
#El equilibrio entre la oferta y la demanda se invierte; la oferta supera a la 
#demanda (predominan los vendedores), lo que hace que los precios caigan. 
#Después de un valle de precios, los compradores vuelven a predominar y los 
#precios suben. Si los comerciantes ven que los precios no están superando su 
#nivel en el primer top, los vendedores pueden volver a prevalecer, bajando 
#los precios y haciendo que se forme un doble top. 

##Un doble fondo es la formación final en un mercado en declive. 
#Es idéntico al doble techo, excepto por la relación inversa en el precio. 
#El patrón está formado por dos mínimos de precios separados por un pico local
#que define la línea del cuello. La formación se completa y se confirma cuando
#el precio sube por encima de la línea del cuello, lo que indica que un aumento
#de precio adicional es inminente o altamente probable.


## Visuali of BBand 
Master_Data%>%filter(Stock=="ICICI"|Stock=="PNB")%>%ggplot(aes(x=Date,y=Close))+
  geom_line(size=0.5)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  coord_x_date(xlim = c(start, end), expand = TRUE)+
  facet_wrap(~ Stock, scales = "free_y")+
  labs(title = "Bollinger Band", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = 'lightyellow')
        ,panel.grid.minor = element_blank(),
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15)
  ) +
  theme(legend.position="none")

Master_Data%>%filter(Stock=="Axis"|Stock=="SBI")%>%ggplot(aes(x=Date,y=Close))+
  geom_line(size=1)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  coord_x_date(xlim = c(start, end), expand = TRUE)+
  facet_wrap(~ Stock, scales = "free_y")+
  labs(title = "Bollinger Band", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = 'lightyellow')
        ,panel.grid.minor = element_blank(),
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15)
  ) +
  theme(legend.position="none")

Master_Data%>%filter(Stock=="Canara"|Stock=="BOB")%>%ggplot(aes(x=Date,y=Close))+
  geom_line(size=1)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  coord_x_date(xlim = c(start, end), expand = TRUE)+
  facet_wrap(~ Stock, scales = "free_y")+
  labs(title = "Bollinger Band", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = 'lightyellow')
        ,panel.grid.minor = element_blank(),
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15)
  ) +
  theme(legend.position="none")

##Predicción de precios de acciones
#También analizaremos la parte aleatoria del movimiento del precio de las acciones,
#llamado ruido blanco e incluiremos en nuestro modelo de predicción.

#Dos metodológias que combinaremos:
#1 Enfoque Regresión
#2 ARIMA

## Descarga de datos
PNB = Quandl("NSE/ICICIBANK",collapse="monthly",start_date="2016-09-01",type="raw")
Axis=Quandl("NSE/AXISBANK",collapse="monthly",start_date="2016-09-01",type="raw")

## Convertir el conjunto de datos PNB y AXIS a df par aun modeo de regresión
PNB_df=PNB
Axis_df=Axis
colnames(PNB_df)<-c("Date","Open","High","Low","Last","Close","TTQ","Turnover")
colnames(Axis_df)<-c("Date","Open","High","Low","Last","Close","TTQ","Turnover")

## cambair la escala de la variable "Trade quantity"
PNB_df$TTQ<-PNB_df$TTQ/100000
Axis_df$TTQ<-Axis_df$TTQ/100000

##Modelos de regresión 
m1=lm(PNB_df$Close~PNB_df$High+PNB_df$Low+PNB_df$TTQ)
p1.df=as.data.frame(predict(m1,interval="predict"))
m3=lm(Axis_df$Close~Axis_df$High+Axis_df$Low+Axis_df$TTQ)
p3.df=as.data.frame(predict(m3, interval="predict"))

##Pronosticos con un ARIMA parano ignorar la estacionalidad y la parte ciclica de el precio
m2=arima(diff(PNB_df$Close),order=c(1,0,0))
m4=arima(diff(Axis_df$Close),order=c(1,0,0))
p2.df=as.data.frame(predict(m2,n.ahead=3))
p4.df=as.data.frame(predict(m4,n.ahead=3))

## Combinar el Random y Stock juntos
p1.df=p1.df[1:3,]
p1.df$fit=p1.df$fit+p2.df$pred
p3.df=p3.df[1:3,]
p3.df$fit=p3.df$fit+p4.df$pred

## Crear la fecha para los siguientes tres meses
date<-as.data.frame(as.Date(c("2019-02-28","2019-03-31","2019-04-30")))
colnames(date)=c("date")

## Modificar el conjunto de datos predicho, adicionando una variable
# "key" para PNB
p1.df<-cbind(p1.df,date)
p1.df["Key"]<-"Predicted"
p1.df<-p1.df[,c("date","fit","lwr","upr","Key")]

## para PNB
p3.df<-cbind(p3.df,date)
p3.df["Key"]<-"Predicted"
p3.df<-p3.df[,c("date","fit","lwr","upr","Key")]

## Renombrar las columnas
colnames(p1.df)<-c("Date","Close","lwr","upr","Key")
colnames(p3.df)<-c("Date","Close","lwr","upr","Key")

## Modify the PNB_df dataset
PNB_df<-PNB%>%select("Date","Close")
Axis_df<-Axis%>%select("Date","Close")

## Adicionar dos variables para el intervalo de confianza
##"lwr" and "upr"
var<-c("lwr","upr")
PNB_df[var]<-NA
Axis_df[var]<-NA

## Adicionar la variable key para los datos actuales
PNB_df["Key"]<-"Actual"
Axis_df["Key"]<-"Actual"

## adicionar las filas predichas y actuales 
PNB_com=rbind(PNB_df,p1.df)
PNB_com$Date<-as.Date(PNB_com$Date)

Axis_com=rbind(Axis_df,p3.df)
Axis_com$Date<-as.Date(Axis_com$Date)

## Visual
PNB_Plot<-ggplot(data=PNB_com,aes(x= Date, y = Close,color=Key,label=Close)) +
  # Prediction intervals
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Key), 
              fill = "khaki2", size = 0)+
  geom_line(size = 1.7) + 
  geom_point(size = 2)+
  labs(title = "Actual and Predicted Price, PNB", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = "honeydew")
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15))


Axis_Plot<- ggplot(data=Axis_com,aes(x= Date, y = Close,color=Key,label=Close)) +
  # Prediction intervals
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Key), 
              fill = "khaki2", size = 0)+
  geom_line(size = 1.7) + 
  geom_point(size = 2)+
  labs(title = "Actual and Predicted Price, Axis Bank", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = "honeydew")
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15))

grid.arrange(PNB_Plot,Axis_Plot,ncol = 1, nrow = 2)

