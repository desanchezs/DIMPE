library(SnowballC)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(readr)
library(cluster)
library(readtext)
library(tidyr)
library(facto)
nov_raw <- readLines("C:\\Users\\DASanchezS\\Documents\\Análisis de texto\\GEIH\\OcupadosPreguntas.txt", skip = 0, encoding = "latin1")
str(nov_raw)

nov_raw1 <- readLines("C:\\Users\\DASanchezS\\Documents\\Análisis de texto\\GEIH\\FuerzaDeTrabajoPreguntas.txt.txt", skip = 0, encoding = "latin1")
str(nov_raw1)
# Usaremos estos números para hacer grupos de diez renglones consecutivos.
# nos quedamos con un número de elementos igual al número de renglones 
# del objeto nov_raw (length(nov_raw)), para facilitar combinarlos.
# convertimos a data.frame para que las columnas estén identificadas con un nombre,
# lo cual será útil en los siguientes pasos.
#Usamos aggregate para concatenar los renglones (FUN = paste, con collapse = " " para preservar el espacio entre palabras), 
# agrupados por  diez (formula = nov_raw ~ diez)
#Como sólo necesitamos la columna con los ahora párrafos de texto, con eso nos quedamos. Aprovechamos para transformar nov_text en una 
#matrix, pues esto nos facilitará los pasos siguientes.
## en un unico paso
nov_text <- cbind(rep(0:ceiling(length(nov_raw)/1), each = 1)%>%
      .[1:length(nov_raw)], nov_raw) %>%
      data.frame %>% aggregate(nov_raw~V1,data=.,FUN=paste,collapse=" ")%>%
      select(nov_raw) %>% as.matrix
dim(nov_text)

nov_text<-nov_text[!apply(nov_text == "", 1, all),]

##### Limpieza del texto
# Quitar saltos de línea y tabulaciones
nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
# Quitar ?
nov_text <- gsub("\\?", " ", nov_text)
nov_text <- gsub("\\¿", " ", nov_text)
#Todo en minusculas
nov_text <- tolower(nov_text)
# Quitar palabras vacias (preposiciones y muletillas)
nov_text <- removeWords(nov_text, words = stopwords("spanish"))
#Quitar puntuación
nov_text <- removePunctuation(nov_text)

v<- paste("Q", substr(nov_text, 1,3))
v
# Quitar números o fechas (si se requiere)
nov_text <- removeNumbers(nov_text)
# Quitar espacios vacios grandes porducidos por los pasos anteriores
nov_text <- stripWhitespace(nov_text)
nov_text <- removeWords(nov_text, words = c( "c","b", "a","siguientes", "además", "cuanto",
                                            "usted", "pues", "pasado","cuál", "cuáles", "cuantos", "así", "dijo", "cómo", "sino", "entonces", "aunque", "cuáles"))

##### Análisis del Corpus
nov_corpus <- Corpus(VectorSource(nov_text))
nov_corpus

####### Term Document Matrix
#Mapearemos nuestro Corpus indicando que es una matriz de términos
nov_tdm <- TermDocumentMatrix(nov_corpus)
nov_tdm
###### Frecuencia de palabras
#transformaremos nuestro objeto nov_tdm en un objeto de clase matrix, 
#que de nuevo tendrá un número de renglones igual al número de palabras distintas 
#de nuestro Corpus y número de columnas igual a su número de documentos.

nov_mat <- as.matrix(nov_tdm)
dim(nov_mat)

#Obtenemos las sumas de renglones (rowSums) odenadas de mayor a menor 
#(sort con decreasing = TRUE)para conocer la frecuencia de cada palabra y 
#después transformamos los resultados a objeto de clase data.frame de dos columnas,
#palabra y frec, que nos permitirá graficar fácilmente su contenido. 
nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)

wordcloud(words = nov_mat$palabra, freq = nov_mat$frec,max.words = 5, random.order = F, colors=brewer.pal(name = "Dark2", n = 8))
#veinte palabras mas frecuentes
nov_mat[1:20, ]

##### Gráficas de frecuencia
nov_mat %>% mutate(perc = (frec/sum(frec))*100) %>% .[1:10, ] %>%
  ggplot(aes(palabra, perc)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = round(perc, 2))) + 
  coord_flip() +
  labs(title = "Diez palabras más frecuentes", x = "Palabras", y = "Porcentaje")

####### Asociaciones entre palabras
#Esta también nos pide el límite inferior de correlación (corlimit) para mostrarnos. 
#Valores cercanos a 1 indican que las palabras aparecen casi siempre asociadas 
#una con otra, valores cercanos a 0 nos indican que nunca o casi nunca lo hacen.
findAssocs(nov_tdm, terms = c("vivienda", "servicios"), corlimit = .25)

##########
dtm_DT <- DocumentTermMatrix(nov_corpus)
m  <- as.matrix(dtm_DT)
rownames(m )<-v
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="minkowski")

groups <- hclust(distMatrix,method="ward.D")
plot(groups, cex=0.6, hang=-1, labels = v)
rect.hclust(groups, k=4)


#####
#k means algorithm, 2 clusters, 100 starting configurations
k<-2
kfit <- kmeans(distMatrix, k, nstart=100)
#plot - need library cluster
library(cluster)
clusplot(as.matrix(distMatrix), kfit$cluster, color=T, shade=T, labels=2, lines=0, main="Agrupación")

library(factoextra)
library(ggrepel)
fviz_cluster(kfit, distMatrix,  pointsize = 1,
             labelsize=6, check_overlap=T,  main = "Cluster")


#Descripcion por topicos de clusters (Mas o menos que tema)
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kfit$centers[i, ], decreasing = T)
  cat(names(s)[1:3], "\n")}


d<-data.frame(nov_text)
rownames(d)<-v


for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kfit$centers[i, ], decreasing = T)
  print(d[names(s)[1:3],])}

