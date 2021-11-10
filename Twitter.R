# INSTALACION DE PAQUETES NECESARIOS PARA EL PROYECTO ----

install.packages("twitteR")
install.packages("tm")
install.packages("syuzhet")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("topicmodels")

 

# CREDENCIALES DE LA API TWITTER ----

API_key <- "HIDE"
API_key_secret <- "HIDE"
Token <- "HIDE"
Access_token <- "HIDE"
Access_token_secret <- "HIDE"

# INVOCACIONES DE LIBRERIAS NECESARIAS ----

library(twitteR)
library(tm)
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(topicmodels)

# OBTENCION DE DATOS TWITTER ----

# LOGIN API TWITTER
setup_twitter_oauth(API_key,API_key_secret,Access_token,Access_token_secret)

# IMPORTACION DE DATOS
tweet_corpus <- searchTwitter('#RavenCoin', n=100000 ,lang = 'en', since = '2021-11-01', until = '2021-11-10')

#EXPORTACION DE DATOS A CSV (ESTE PASO ES MUY IMPORTANTE PORQUE LAS CONSULTAS EN TWITTER SON LIMITADAS)
write.csv(twListToDF(tweet_corpus),'datoscsv')
write.csv2(twListToDF(tweet_corpus),'datoscsv2')

# ELIMINACIN DE RETWEETS
# SE PUEDE UTILIZAR SI SE QUIEREN ANALIZAR UNICAMENTE LOS GENERADORES DE OPINION
tweet_corpus <- strip_retweets(tweet_corpus)

# TRANSFORMACION A DATA FRAME
tweet_df <- twListToDF(tweet_corpus)

# VECTOR DE COMENTARIOS PARA ANALISIS DE SENSIBILIDAD
tweets <- iconv(tweet_df$text)

# GENERACION DE ANALISIS DE SENTIMIENTO ----

# REALIZO 5 ANALISIS DISTINTOS
sent_syuzhet <- get_sentiment(tweets, method = 'syuzhet')
sent_bing <- get_sentiment(tweets, method = 'bing')
sent_afinn <- get_sentiment(tweets, method = 'afinn')
sent_nrc <- get_sentiment(tweets, method = 'nrc')
nrc_data <- get_nrc_sentiment(tweets)

# AGREGO EL ANALISIS AL DATA FRAME
tweet_df_sa <- cbind(tweet_df, sent_syuzhet, sent_bing,
                  sent_afinn, sent_nrc, nrc_data)

# EXPORTO A CSV EL NUEVO DATA FRAME
write.csv(tweet_df_sa,"datos.sa.csv")
write.csv2(tweet_df_sa,"datos.sa.csv2")

#### ANALISIS DE COMPORTAMIENTO (2 HORAS)

hora.inicial <- rep(as.POSIXct('2021-11-01 00:00:00'),120)
for(i in 1:120) {
  hora.inicial[i] <- hora.inicial[i]+((i-1)*7200)
}
hora.final <- rep(as.POSIXct('2021-11-01 00:00:00'),120)
for(i in 1:120) {
  hora.final[i] <- hora.final[i]+(i*7200)
}

a<-rep(0,120)
for (j in 1:120) {
  for (i in 1:length(tweet_df_sa$created)) {
    if (tweet_df_sa$created[i] >= hora.inicial[j] & tweet_df_sa$created[i] < hora.final[j]) {
      a[j]<-a[j]+1
    }
  }
}

b<-rep(0,120)
for (j in 1:120) {
  for (i in 1:length(tweet_df_sa$created)) {
    if (tweet_df_sa$created[i] >= hora.inicial[j] & tweet_df_sa$created[i] < hora.final[j]) {
      b[j]<-b[j]+tweet_df_sa$sent_syuzhet[i]
    }
  }
}
b
c <- rep(0,120)
for(i in 1:length(b)) {
  c[i] <- b[i]/a[i]
}
c
plot(a,type = "l")
getwd()

datosbarplot <- data.frame(hora.inicial,hora.final,a,b,c)

datosbarplot<- datosbarplot[6:107,]

datosbarplot$hora.final <- datosbarplot$hora.final+60*60*2

setwd("/Users/gustavochac/Documents/GitHub/Text Minning Twitter")
mean(sent_bing)
plot(sent_syuzhet)
hist(sent_syuzhet)
hist(sent_bing)
hist(sent_nrc)
hist(sent_afinn)

count.positive <- 0
for (i in 1:6990) {
  if(sent_syuzhet[i]>0) {
    count.positive <- count.positive +1
  }
}
count.positive

count.neutral <- 0
for (i in 1:6990) {
  if(sent_syuzhet[i]==0) {
    count.neutral <- count.neutral +1
  }
}
count.neutral

count.negative <- 0
for (i in 1:6990) {
  if(sent_syuzhet[i]<0) {
    count.negative <- count.negative +1
  }
}
count.negative

library(ggplot2)

gr <- data.frame(c("Positivo","Neutral","Negativo"),c(count.positive,count.neutral,count.negative))
gr

?hist
hist(c(rep(1,count.positive),rep(2,count.neutral),rep(3,count.negative)),break)

## NOTA SINCE y UNTIL EN SERACHTWITTER PARA PONER FECHA ESPECIFICAS
