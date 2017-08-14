library(RCurl)
library(twitteR)
library(stringr)
library(RJSONIO)
#twitter api info
api_key <- ""
api_secret <- "" 
token <- "" 
token_secret <- "" 

#twitter connection 
setup_twitter_oauth(api_key, api_secret, token, token_secret)

set.seed(10); ht <- '#soundcloud';
tweets.raw <- searchTwitter(ht, n = 10000, lang = 'en', since = '2017-08-11', until = '2017-08-12');
tweets.df <- twListToDF(tweets.raw);
df <- twListToDF(strip_retweets(tweets.raw, strip_manual = TRUE, strip_mt = TRUE)); df$hashtag <- ht; df$created <- as.POSIXlt(df$created); df$text <- iconv(df$text, 'latin1', 'ASCII', 'byte'); df$url <- paste0('https://twitter.com/', df$screenName, '/status/', df$id); 
df.a <- subset(df, select = c(text, created, url, latitude, longitude, retweetCount, hashtag));
nrow(df.a); head(df.a);
#write.csv(df.a, paste0(ht,'.csv'), row.names = FALSE);
tweets <- df; tweets$z <- 1; tweets$created <- as.POSIXlt(tweets$created); nrow(tweets); min(tweets$created); max(tweets$created); median(tweets$created);

