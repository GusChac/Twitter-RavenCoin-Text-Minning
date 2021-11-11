
# INSTALACION DE PAQUETES NECESARIOS PARA EL PROYECTO ----

install.packages("twitteR")
install.packages("tm")
install.packages("syuzhet")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("topicmodels")
install.packages("lubridate")

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
library(lubridate)

# OBTENCION DE DATOS TWITTER ----

# LOGIN API TWITTER
setup_twitter_oauth(API_key,API_key_secret,Access_token,Access_token_secret)

# IMPORTACION DE DATOS (1 SEMANA)
inicio <- as.character(today()-7) 
final <- as.character(today())
tweet_corpus <- searchTwitter('#RavenCoin', n=20000 ,lang = 'en', since = inicio, until = final)

#EXPORTACION DE DATOS A CSV (ESTE PASO ES MUY IMPORTANTE PORQUE LAS CONSULTAS EN TWITTER SON LIMITADAS)
write.csv(twListToDF(tweet_corpus),'datoscsv')
write.csv2(twListToDF(tweet_corpus),'datoscsv2')

# ELIMINACIN DE RETWEETS
# SE PUEDE UTILIZAR SI SE QUIEREN ANALIZAR UNICAMENTE LOS GENERADORES DE OPINION (ELIMINA RETWEETS)
# SE RECOMENDADO UTILIZARLO PORQUE DE LO CONTRARIA DUPLICA TWEETS
tweet_corpus <- strip_retweets(tweet_corpus)

# TRANSFORMACION A DATA FRAME
tweet_df <- twListToDF(tweet_corpus)

# VECTOR DE COMENTARIOS PARA ANALISIS DE SENSIBILIDAD
tweets <- iconv(tweet_df$text)

# GENERACION DE ANALISIS DE SENTIMIENTO ----

# SE REALIZARAN DOS ANALISIS
# SYUZHET PARA DETERMINAR EL RANGO POSITIVO-NEGATIVO DE CADA TWEET
sent_syuzhet <- get_sentiment(tweets, method = 'syuzhet')
# NRC MODIFICADO PARA DETERMINAR SI EL COMENTARIO FUE POSITIVO O NEGATIVO
# SE MODIFICARA LA ESCALA A -1 = NEGATIVO, 0 = NEUTRAL, 1 = POSITIVO
sent_nrc <- get_sentiment(tweets, method = 'nrc')
sent_nrc <- (ifelse(sent_nrc < 0,-1,ifelse(sent_nrc>0,1,0)))

# AGREGO EL ANALISIS AL DATA FRAME
tweet_df_sa <- cbind(tweet_df, sent_syuzhet, sent_nrc)

# EXPORTO A CSV EL NUEVO DATA FRAME
write.csv(tweet_df_sa,"datos.sa.csv")
write.csv2(tweet_df_sa,"datos.sa.csv2")

# TABLA ANALISIS DE COMPORTAMIENTO (2 HORAS) ----

inicio.time<-make_datetime(year(as.POSIXct(inicio)),month(as.POSIXct(inicio)),day(as.POSIXct(inicio)),hour=0,min=0,sec=0)
final.time<-make_datetime(year(as.POSIXct(final)),month(as.POSIXct(final)),day(as.POSIXct(final)),hour=0,min=0,sec=0)
horas <- seq(from=inicio.time,to=final.time,by=7200)
hora.inicial <- horas[1:(length(horas)-1)]
hora.final <- horas[2:(length(horas))]

cantidad <- rep(0,(length(horas)-1))
for (j in 1:length(cantidad)) {
  for (i in 1:length(tweet_df_sa$created)) {
    if (tweet_df_sa$created[i] >= hora.inicial[j] & tweet_df_sa$created[i] < hora.final[j]) {
      cantidad[j]<-cantidad[j]+1
    }
  }
}

sumsyuzhet <- rep(0,(length(horas)-1))
for (j in 1:length(sumsyuzhet)) {
  for (i in 1:length(tweet_df_sa$created)) {
    if (tweet_df_sa$created[i] >= hora.inicial[j] & tweet_df_sa$created[i] < hora.final[j]) {
      sumsyuzhet[j]<-sumsyuzhet[j]+tweet_df_sa$sent_syuzhet[i]
    }
  }
}

syuzhet <- rep(0,length(sumsyuzhet))
for(i in 1:length(sumsyuzhet)) {
  syuzhet[i] <- sumsyuzhet[i]/cantidad[i]
}

nrc.positive <- rep(0,(length(horas)-1))
for (j in 1:length(nrc.positive)) {
  for (i in 1:length(tweet_df_sa$created)) {
    if (tweet_df_sa$created[i] >= hora.inicial[j] & tweet_df_sa$created[i] < hora.final[j] & tweet_df_sa$sent_nrc[i] > 0) {
      nrc.positive[j]<-nrc.positive[j]+tweet_df_sa$sent_nrc[i]
    }
  }
}
nrc.positive.porc <- nrc.positive/cantidad


nrc.negative <- rep(0,(length(horas)-1))
for (j in 1:length(nrc.negative)) {
  for (i in 1:length(tweet_df_sa$created)) {
    if (tweet_df_sa$created[i] >= hora.inicial[j] & tweet_df_sa$created[i] < hora.final[j] & tweet_df_sa$sent_nrc[i] < 0) {
      nrc.negative[j]<-(nrc.negative[j])+tweet_df_sa$sent_nrc[i]
    }
  }
}
nrc.negative <- (-nrc.negative)
nrc.negative.porc <- nrc.negative/cantidad

nrc.neutral <- rep(0,(length(horas)-1))
for (j in 1:length(nrc.neutral)) {
  for (i in 1:length(tweet_df_sa$created)) {
    if (tweet_df_sa$created[i] >= hora.inicial[j] & tweet_df_sa$created[i] < hora.final[j] & tweet_df_sa$sent_nrc[i] == 0) {
      nrc.neutral[j]<-nrc.neutral[j]+tweet_df_sa$sent_nrc[i]+1
    }
  }
}
nrc.neutral.porc <- nrc.neutral/cantidad

tabla.as <- data.frame(hora.inicial,hora.final,cantidad,syuzhet,nrc.negative.porc,nrc.neutral.porc,nrc.positive.porc)
