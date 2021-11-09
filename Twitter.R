install.packages("twitteR")
install.packages("tm")
install.packages("syuzhet")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("topicmodels")

API_key <- "sWIO7sREQdNQPQzmF55C2BQV4"
API_key_secret <- "KHCdvbCXnkd4BtW7m1ACusELPsYByiTwtmmTVdAiEsITnc0iCF"
Token <- "AAAAAAAAAAAAAAAAAAAAAJWBUgEAAAAAViY9iq15v5RdWDBt8t1mdA0yYB4%3DyXi1UaaaPtGDjDkcstcJe7mJ70ewkCoqygAKMXFHzl157LIUYM"
Access_token <- "351616109-CPuz5AGAX3Ewxsk0I365BqRhLXqUSSKwHPbeX95H"
Access_token_secret <- "uXML5OBbMvNZrMY6M9QCorCm4wxXqTcW0cUW5wSClAOmG"

library(twitteR)
library(tm)
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(topicmodels)

setup_twitter_oauth(API_key,API_key_secret,Access_token,Access_token_secret)
?searchTwitter
tweet_corpus <- searchTwitter('#RavenCoin', n=100000 ,lang = 'en', since = '2021-10-01', until = '2021-10-20')
tweet_corpus <- strip_retweets(tweet_corpus)
tweet_df <- twListToDF(tweet_corpus)
tweets <- iconv(tweet_df$text, to = 'ASCII')

sent_syuzhet <- get_sentiment(tweets, method = 'syuzhet')
sent_bing <- get_sentiment(tweets, method = 'bing')
sent_afinn <- get_sentiment(tweets, method = 'afinn')
sent_nrc <- get_sentiment(tweets, method = 'nrc')
nrc_data <- get_nrc_sentiment(tweets)


tweet_df <- cbind(tweet_df, sent_syuzhet, sent_bing,
                  sent_afinn, sent_nrc, nrc_data)

mean(sent_bing)
plot(sent_syuzhet)
hist(sent_syuzhet)
hist(sent_bing)
hist(sent_nrc)
hist(sent_afinn)

count.positive <- 0
for (i in 1:854) {
  if(sent_syuzhet[i]>0) {
    count.positive <- count.positive +1
  }
}
count.positive

count.neutral <- 0
for (i in 1:854) {
  if(sent_syuzhet[i]==0) {
    count.neutral <- count.neutral +1
  }
}
count.neutral

count.negative <- 0
for (i in 1:854) {
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
