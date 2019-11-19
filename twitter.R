#Librerias (Las que no funcionen, por favor instalar)
library(rvest)
library(httr)
library(devtools)
library(twitteR)
library(base64enc)
library(wordcloud)
library(tm)
library(ggplot2)
library(graph)
#Para obtener estas claves deben ingresar desde la cuenta de twitter para desarrolladores y aplicaciones
consumer_key <- 's3Bc4l0PQJhOOq0bIcEbpEoZ9'
consumer_secret <- 'Su9kf3DNk8xCh5zr0PpQAnGCH2oXopamcHrrWnJB4YC56FZj0k'
access_token <- '378837725-ymnXYF9YFThYT7SS3nh7Jjs2CHSPD6qPkwZUWahh'
access_secret <- 'jlJKThGepUYavkjtAa9Ofq1MojOKEO5xuG9zA3kpiwys2'

#Autorización desde R para exportar datos de Twitter
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

# Recolección de tweets
tweets <- searchTwitter('#EURO2016',n=2000,lang="es",locale="co")
#tweets <- userTimeline("elusuaarioquequieran", 2000)

#Observen la ayuda, se puede obtener geolocalizacion, tweets recientes o 
#populares, filtrar por fechas (max 1 semana) y una serie de opciones mas
#?searchTwitter
#?userTimeline
#Vuelca la información de los tweets a un data frame
df <- twListToDF(tweets)
#Observacion de lo que se obtiene
head(df)
colnames(df)
#Obtiene el texto de los tweets
txt <- df$text


#####Limpieza de datos #####
#Remueve retweets
txtclean <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)

#Remueve @otragente
txtclean <- gsub("@\\w+", "", txtclean)

#Remueve simbolos de puntuación
txtclean <- gsub("[[:punct:]]", "", txtclean)

#Remueve números
txtclean <- gsub("[[:digit:]]", "", txtclean)

#Remueve links
txtclean <- gsub("http\\w+", "", txtclean)
#
txtclean<-iconv(txtclean, from="UTF-8", to="LATIN1")
#
txtclean = gsub("\n", "", txtclean)
#
txtclean = gsub("\t", "", txtclean)
#
txtclean = gsub("\r", "", txtclean)
##### fin limpieza de datos #####

#Construye un corpus
corpus <- Corpus(VectorSource(txtclean))

#Convierte a minúsculas
corpus <- tm_map(corpus, content_transformer(tolower))

#Remueve palabras vacías (stopwords) en español
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"),"#EURO2016","euro"))


#Carga archivo de palabras vacías personalizada y lo convierte a ASCII
#sw <- readLines("stopwords.es.txt",encoding="UTF-8")
#sw <- iconv(sw, to="ASCII//TRANSLIT")

#Remueve palabras vacías personalizada
#corpus <- tm_map(corpus, removeWords, sw)

#Remueve espacios en blanco extras
corpus <- tm_map(corpus, stripWhitespace)
#Crea una matriz de términos
tdm <- TermDocumentMatrix(corpus)
#Convierte a una matriz
m <- as.matrix(tdm)
#Conteo de palabras en orden decreciente
wf <- sort(rowSums(m),decreasing=TRUE)
#Crea un data frame con las palabras y sus frecuencias
dm <- data.frame(word = names(wf), freq=wf)
#Términos más frecuentes (Más de 15 menciones)
terminosfrec <- findFreqTerms(tdm, lowfreq = 15)
#Frecuencia términos
term.freq<-rowSums(as.matrix(wf))
#Frecuencia términos mas frecuentes
term.freq <-subset(term.freq, term.freq >= 15)
#Palabras más frecuentes y su frecuencia
df <- data.frame(word = names(term.freq), freq = term.freq)

#Gráfico de frecuencias
ggplot(df, aes(x = word, y = freq )) +  geom_bar(stat = "identity") + 
  xlab("Palabras") + ylab("Conteo") + coord_flip()

#Grafica la nube de palabras con todos 
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#Grafica la nube de palabras con los mas frecuentes
wordcloud(df$word, df$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#
library(graph)
library(Rgraphviz)

plot(tdm, term = terminosfrec, corThreshold = 0.2, weighting = T)
#A partir de acá se hace un breve análisis (a manera de ejemplo nada concluyente)

#"Coeficiente de dispersion sparse", entre más cercano a 1 más dispersos 
#estarán
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)

#Paso a matriz
m2 <- as.matrix(tdm2)

#Escalar
distMatrix <- dist(scale(m2))

#Agrupación jerárquica
fit <- hclust(distMatrix, method = "ward.D")

#Grafica con los grupos (A esta hora identifico 3 grupos)
#Cambiar k numero de grupos dependiendo la hora y el tema 
plot(fit)
rect.hclust(fit, k = 3) 


#Clustering
#Transpuesta de la matriz agrupada por el sparse
m3 <- t(m2) 

#Semilla!!
set.seed(122) 

#K means
#Cambiar k numero de grupos dependiendo la hora y el tema
k<-3 
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3)

#Descripcion por topicos de clusters (Mas o menos que tema)
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:6], "\n")}



