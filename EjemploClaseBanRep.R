library(RCurl)
library(XML)
library(rvest)
library(stringr)
library(tidyr)
library(tm)
library(wordcloud)
##Último comunicado del banco de la republica de Colombia
banrep <- read_html(iconv("http://www.banrep.gov.co/es/node/6750", to = "UTF-8"), encoding =  "utf8")
comuni <- banrep%>%html_nodes("#content.section") %>%  html_text()
#Observemos lo que leyo R
comuni
#Correción de error
comuni<-iconv(comuni, from="UTF-8", to="LATIN1")
#Veamos que paso
comuni
#####Inicio limpieza de datos #####
#Se remueven simbolos extraños
txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", comuni)
#
txtclean = gsub("@\\w+", "", txtclean)
#
txtclean = gsub("\n", "", txtclean)
#
txtclean = gsub("\t", "", txtclean)
#
txtclean = gsub("\r", "", txtclean)
# remueve simbolos de puntuación
txtclean = gsub("[[:punct:]]", "", txtclean)
# remueve números
txtclean = gsub("[[:digit:]]", "", txtclean)
# remueve links
txtclean = gsub("http\\w+", "", txtclean)
#################
#Fin de limpieza#
#################
txtclean
#Construye un corpus
corpus = Corpus(VectorSource(txtclean))
# convierte a minúsculas
corpus = tm_map(corpus, content_transformer(tolower))
# remueve palabras vacías (stopwords) en español
corpus = tm_map(corpus, removeWords, c(stopwords("spanish")))
# remueve espacios en blanco extras
corpus = tm_map(corpus, stripWhitespace)
# crea una matriz de términos
tdm <- TermDocumentMatrix(corpus)
# convierte a una matriz
m = as.matrix(tdm)
# conteo de palabras en orden decreciente
wf <- sort(rowSums(m),decreasing=TRUE)
# crea un data frame con las palabras y sus frecuencias
dm <- data.frame(word = names(wf), freq=wf)
##############
###Análisis###
##############
library(ggplot2)
#Términos mas frecuentes
terminosfrec <- findFreqTerms(tdm, lowfreq = 3)
#Frecuencia términos
term.freq<-rowSums(as.matrix(wf))
#Frecuencia términos más frecuentes
term.freq <-subset(term.freq, term.freq >= 3)
#Palabras mas frecuentes y su frecuencia
df <- data.frame(word = names(term.freq), freq = term.freq)
#Gráfico de frecuencias
ggplot(df, aes(x = word, y = freq )) +  geom_bar(stat = "identity") + 
  xlab("Palabras") + ylab("Conteo") + coord_flip()
# gráfica la nube de palabras (wordcloud)
wordcloud(df$word, df$freq, random.order=TRUE, colors=brewer.pal(8, "Dark2"))