library(plyr)
library(ggplot2)
library(splitstackshape)
library(stringr)

setwd('/Users/mishcat/documents/github/twitter_emoji/twitter-api');
fname <- c(
  '#spotify'
);
fname <- paste0(fname, '.csv'); df <- do.call(rbind.fill, lapply(fname, read.csv));
df$username <- substr(substr(df$url, 21, nchar(as.character(df$url))), 1, nchar(substr(df$url, 21, nchar(as.character(df$url))))-26);
tweets.full <- df; tweets.full$X <- NULL; tweets.full$z <- 1; 

tweets.full$created <- as.POSIXlt(tweets.full$created); min(tweets.full$created); max(tweets.full$created); median(tweets.full$created); nrow(tweets.full); length(unique(tweets.full$username))

tweets.dupes <- tweets.full[duplicated(tweets.full$url), ]; nrow(tweets.full); nrow(tweets.dupes);
tweets <- tweets.full[!duplicated(tweets.full$url), ]; tweets <- arrange(tweets, url); row.names(tweets) <- NULL; tweets$tweetid <- as.numeric(row.names(tweets)); nrow(tweets);
tweets.final <- tweets;

setwd('/Users/mishcat/documents/github/twitter_emoji/twitter-api');

emdict.la <- read.csv('emoticon_conversion_noGraphic.csv', header = F); #Lauren Ancona; https://github.com/laurenancona/twimoji/tree/master/twitterEmojiProject
emdict.la <- emdict.la[-1, ]; row.names(emdict.la) <- NULL; names(emdict.la) <- c('unicode', 'bytes', 'name'); emdict.la$emojiid <- row.names(emdict.la);
emdict.jpb <- read.csv('emDict.csv', header = F) #Jessica Peterka-Bonetta; http://opiateforthemass.es/articles/emoticons-in-R/
emdict.jpb <- emdict.jpb[-1, ]; row.names(emdict.jpb) <- NULL; names(emdict.jpb) <- c('name', 'bytes', 'rencoding'); emdict.jpb$name <- tolower(emdict.jpb$name);
emdict.jpb$bytes <- NULL;
emojis <- merge(emdict.la, emdict.jpb, by = 'name');  emojis$emojiid <- as.numeric(emojis$emojiid); emojis <- arrange(emojis, emojiid);


tweets <- tweets.final;

df.s <- matrix(NA, nrow = nrow(tweets), ncol = ncol(emojis)); 
system.time(df.s <- sapply(emojis$rencoding, regexpr, tweets$text, ignore.case = T, useBytes = T));
rownames(df.s) <- 1:nrow(df.s); colnames(df.s) <- 1:ncol(df.s); df.t <- data.frame(df.s); df.t$tweetid <- tweets$tweetid;

df.a <- subset(tweets, select = c(tweetid, hashtag)); 
df.u <- merge(df.t, df.a, by = 'tweetid'); df.u$z <- 1; df.u <- arrange(df.u, tweetid); 
tweets.emojis.matrix <- df.u;

df <- subset(tweets.emojis.matrix)[, c(2:843)]; count <- colSums(df > -1);
emojis.m <- cbind(count, emojis); emojis.m <- arrange(emojis.m, desc(count));
emojis.count <- subset(emojis.m, count > 1); emojis.count$dens <- round(1000 * (emojis.count$count / nrow(tweets)), 1); emojis.count$dens.sm <- (emojis.count$count + 1) / (nrow(tweets) + 1);
emojis.count$rank <- as.numeric(row.names(emojis.count));
emojis.count.p <- subset(emojis.count, select = c(name, dens, count, rank));

subset(emojis.count.p, rank <= 20);

ss<-subset(emojis.count.p, rank <= 20);
head(ss)
#ggplot(data=ss, aes(x=name)) + geom_bar()
#hist(ss$name)
sp<- ggplot(ss, aes(x=1, y=count, fill=name)) +
  geom_bar(stat="identity") +
  ggtitle("Pie chart of emojis associated with #spotify on Twitter")

sp <- sp + coord_polar(theta='y')
print(sp)

