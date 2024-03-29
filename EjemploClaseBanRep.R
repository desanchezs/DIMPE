library(RCurl)
library(XML)
library(rvest)
library(stringr)
library(tidyr)
library(tm)
library(wordcloud)
##�ltimo comunicado del banco de la republica de Colombia
banrep <- read_html(iconv("http://www.banrep.gov.co/es/node/6750", to = "UTF-8"), encoding =  "utf8")
comuni <- banrep%>%html_nodes("#content.section") %>%  html_text()
#Observemos lo que leyo R
comuni
#Correci�n de error
comuni<-iconv(comuni, from="UTF-8", to="LATIN1")
#Veamos que paso
comuni
#####Inicio limpieza de datos #####
#Se remueven simbolos extra�os
txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", comuni)
#
txtclean = gsub("@\\w+", "", txtclean)
#
txtclean = gsub("\n", "", txtclean)
#
txtclean = gsub("\t", "", txtclean)
#
txtclean = gsub("\r", "", txtclean)
# remueve simbolos de puntuaci�n
txtclean = gsub("[[:punct:]]", "", txtclean)
# remueve n�meros
txtclean = gsub("[[:digit:]]", "", txtclean)
# remueve links
txtclean = gsub("http\\w+", "", txtclean)
#################
#Fin de limpieza#
#################
txtclean
#Construye un corpus
corpus = Corpus(VectorSource(txtclean))
# convierte a min�sculas
corpus = tm_map(corpus, content_transformer(tolower))
# remueve palabras vac�as (stopwords) en espa�ol
corpus = tm_map(corpus, removeWords, c(stopwords("spanish")))
# remueve espacios en blanco extras
corpus = tm_map(corpus, stripWhitespace)
# crea una matriz de t�rminos
tdm <- TermDocumentMatrix(corpus)
# convierte a una matriz
m = as.matrix(tdm)
# conteo de palabras en orden decreciente
wf <- sort(rowSums(m),decreasing=TRUE)
# crea un data frame con las palabras y sus frecuencias
dm <- data.frame(word = names(wf), freq=wf)
##############
###An�lisis###
##############
library(ggplot2)
#T�rminos mas frecuentes
terminosfrec <- findFreqTerms(tdm, lowfreq = 3)
#Frecuencia t�rminos
term.freq<-rowSums(as.matrix(wf))
#Frecuencia t�rminos m�s frecuentes
term.freq <-subset(term.freq, term.freq >= 3)
#Palabras mas frecuentes y su frecuencia
df <- data.frame(word = names(term.freq), freq = term.freq)
#Gr�fico de frecuencias
ggplot(df, aes(x = word, y = freq )) +  geom_bar(stat = "identity") + 
  xlab("Palabras") + ylab("Conteo") + coord_flip()
# gr�fica la nube de palabras (wordcloud)
wordcloud(df$word, df$freq, random.order=TRUE, colors=brewer.pal(8, "Dark2"))