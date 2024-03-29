library(devtools)
library(plotly)

Sys.setenv("plotly_username"="deansase")
Sys.setenv("plotly_api_key"="Gylu9uPqoUPia0opzIp7")

library(ggplot2)
p<-ggplot(data=V, aes(x = TIPO , y = Total.IPI )) + 
  geom_bar(stat="identity", width=0.5, fill=c("greenyellow"  ,"turquoise" )) + xlab(NULL) +
  ylab("VARIACI�N (%)") + ggtitle("Anual y A�o corrido")


p1<-ggplot(data=V1, aes(x = reorder(D, -value), y =value, fill= variable)) + 
  geom_bar(stat="identity", width=0.5,  position = "dodge") + xlab(NULL) +
  ylab("VARIACI�N (%) y Contribuci�n") + ggtitle("Anual") +
  theme(axis.text.x=element_text( hjust=1))+ coord_flip()


p <- ggplotly(p)
p1 <- ggplotly(p1)

api_create(p, filename = "bar", fileopt = "overwrite")
api_create(p1, filename = "Var y Cont", fileopt = "overwrite")
