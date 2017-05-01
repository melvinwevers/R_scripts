library("ggplot2")
library(tm)
library(scales)
library(plyr)
library("wordcloud")
library("RColorBrewer")
library(gridExtra)
library(data.table)
library(stm)
library(slam)
library(plotly)


#load files and put into correct format
#setwd("/Users/wevers/Dropbox/DH-projects/CaseStudy Coca-Cola/Articles/Multinationals/all") #set working directory
temp = list.files(pattern="*.csv") #load list of files  does not load multiple files
df <- read.csv(temp, header=FALSE, stringsAsFactors=F, sep = '\t') #put files into dataframe

colnames(df) <- c("date", "newspaper", "title", "text") #add column names
df$mergedText = paste(df$title, df$text, sep=" ") #merge title + textcontent
df$date <- as.Date(df$date,"%Y-%m-%d") #add date colum
df$year <- format(as.Date(df$date), "%Y") #year column

total_article <- read.csv("/Users/wevers/Dropbox/DH-projects/article_count_final.csv", sep = ',') # load total number of articles in corpus
total_ads <- read.csv("/Users/wevers/Dropbox/DH-projects/advertisement_count.csv", sep = ',') # load total number of advertisements in corpus

## Additional corpora
origin <- read.csv ("/Users/wevers/Dropbox/DH-projects/CaseStudy_Coca-Cola/origin.csv")
boycot <- read.csv ("/Users/wevers/Dropbox/DH-projects/boycott.csv")

################
# Article counter function
countArticles <- function(timeStamps) {
  Dates <- as.Date(strftime(df$date, "%Y-%m-%d"))
  allDates <- seq(from = min(Dates), to = max(Dates), by = "day")
  article.count <- sapply(allDates, FUN = function(X) sum(Dates == X))
  data.frame(date = allDates, article.count = article.count)
}

df3 <- countArticles(df$date) #count article
df3.year <- setDT(df3)[, lapply(.SD, sum), by=.(year(df3$date))] #aggregate year count

df3.year<- df3.year[year>="1928" & year<="1989"] #select years to analyze
total_article <- total_article[(56:100),] #select years to analyze 1:1890 100:1989 >> fix the index
total_ads <- total_ads[(39:100),] #select years to analyze 1:1890 100:1989 >> fix the index

df3.year$rel.freq <- df3.year$article.count / total_article$number_article * 1000 #calculate rel frequency per 1000 articles
df2.year$rel.freq <- df2.year$article.count / total_ads$number_ad * 1000 #calculate rel frequency per 1000 advertisements


##plot two corpora 
ggplot() +
  theme_bw() +
  geom_line(data = df3.year, aes(color ="Corpus A", x = df3.year$year, y = df3.year$rel.freq)) +
  geom_smooth(data = df3.year, aes(color ="Corpus A", x = df3.year$year, y = df3.year$rel.freq)) +
  geom_line(data = df2.year, aes(color ="Corpus B", x = df2.year$year, y = df2.year$rel.freq)) +
  geom_smooth(data = df2.year, aes(color ="Corpus B", x = df2.year$year, y = df2.year$rel.freq)) +
  scale_colour_manual(name = "Corpus", values=c("#e50000","#029386")) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=20),limit=c(0,NA),oob=squish) +
  xlab("Year")+ylab("Relative Frequency per 1,000 articles")

ggsave("CorpusA_CorpusB_rel_freq.pdf", width=10, height=5)

ggplot() +
  theme_bw()+
  geom_line(data = df3.year, aes(color ="Pepsi in Corpus A", x = df3.year$year, y = df3.year$article.count)) +
  geom_line(data = df2.year, aes(color ="Corpus A", x = df2.year$year, y = df2.year$article.count)) +
  scale_colour_manual(name = "Corpora", values=c("#e50000","#029386")) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=15),limit=c(0,NA),oob=squish) + 
  xlab("Year")+ylab("Article count")

ggsave("Pepsi_CorpusA.pdf", width=10, height=5)

###############
##plot article count
ggplot(df3.year, aes(x = df3.year$year, y = df3.year$rel.freq)) + 
  geom_line(aes(color="black")) + 
  geom_smooth(colour = "red") +
  theme_bw() + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=15),limit=c(0,NA),oob=squish) +
  scale_color_manual(values=c("black"="black"),guide=FALSE) + 
  #scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y"), limits = as.Date(c('1929-01-01','1989-12-31'))) + 
  xlab("Year")+ylab("Relative Frequency per 1,000 articles")
  #labs(title="Number of A with references to 'Average American'  in Newspapers (1890 and 1989)")


ggsave("dollarland.pdf", width=10, height=5)


#function to concatenate words
for (j in seq(a))
{
  #a[[j]] <- gsub("dollar imperialisten", "dollarimperialisme", a[[j]])
  #a[[j]] <- gsub("dollar imperialisme", "dollarimperialisme", a[[j]])
  #a[[j]] <- gsub("amerikaansch imperialisme", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("amerikaansche imperialisme", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("amerikaans imperialisme", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("amerikaanse imperialisten", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("amerikaansche imperialisten", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("amerikaanse imperialisme", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("vereenigde staten", "verenigdestaten", a[[j]])
  
}

#pre-processing / cleaning text
a <- Corpus(DataframeSource(df[c("title", "text")]))
a <- Corpus(DataframeSource(df[c("title")]))
a <- Corpus(DataframeSource(df[c("mergedText")]))
a <- tm_map(a, removePunctuation)
a <- tm_map(a, content_transformer(tolower))
a <- tm_map(a, content_transformer(gsub), pattern = "verfris[a-z]*", replacement = "verfrissend")
a <- tm_map(a, content_transformer(gsub), pattern = "verkwik[a-z]*", replacement = "verkwikkend")
a <- tm_map(a, content_transformer(gsub), pattern = "internationa[a-z]*", replacement = "international")
a <- tm_map(a, content_transformer(gsub), pattern = "fijn[a-z]*", replacement = "fijne")
a <- tm_map(a, content_transformer(gsub), pattern = "apart[a-z]*", replacement = "aparte")
a <- tm_map(a, content_transformer(gsub), pattern = "heerlijk[a-z]*", replacement = "heerlijke")
a <- tm_map(a, content_transformer(gsub), pattern = "bijzonder[a-z]*", replacement = "bijzondere")
a <- tm_map(a, content_transformer(gsub), pattern = "gezellig[a-z]*", replacement = "gezellig")
a <- tm_map(a, content_transformer(gsub), pattern = "standaardfles[a-z]*", replacement = "standaardfles")
a <- tm_map(a, content_transformer(gsub), pattern = "gezinsfles[a-z]*", replacement = "gezinsfles")
a <- tm_map(a, content_transformer(gsub), pattern = "literfles[a-z]*", replacement = "literfles")
a <- tm_map(a, content_transformer(gsub), pattern = "fles[a-z]*", replacement = "fles")
a <- tm_map(a, content_transformer(gsub), pattern = "king?size", replacement = "kingsize")
a <- tm_map(a, content_transformer(gsub), pattern = "echte", replacement = "echt")
a <- tm_map(a, stripWhitespace)
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removeWords, c(stopwords("dutch")))
a <- tm_map(a, PlainTextDocument)

#turn corpus into document term matrix, with different configurations
a.dtm <- DocumentTermMatrix (a)

a.dtm <- DocumentTermMatrix(a, control = 
                              list(wordLengths = c(4,Inf), bounds = list(global = c(length(a)*0.01,length(a)*0.99))))

a.dtm <- DocumentTermMatrix(a, control = 
                              list(wordLengths = c(4,Inf)))

a.dtm <-removeSparseTerms(a.dtm, sparse=.99) #removing really rare words so that the DTM is not too big / this is almost the same as previous line find out which one to keep


###ANALYSIS
#word frequency
findFreqTerms(a.dtm, lowfreq=9000)

#export frequency table
m_freq <- as.matrix(a.dtm)   
dim(m_freq)   
write.csv(m_freq, file="dtm.csv")  

findAssocs(a.dtm, "boycot", corlimit=0.15)

#word cloud
m <- as.matrix(a.dtm)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,100)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 9000, scale=c(1,0.5),
          max.words=150, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

####STM (Structural topic modeling)
#processed <- readCorpus(a.dtm, type =("slam")) # use own dtm

processed <- textProcessor(df$mergedText, metadata = df, lowercase = TRUE, removestopwords = TRUE,
                           removenumbers = TRUE, removepunctuation = TRUE, stem = FALSE,
                           wordLengths = c(4,Inf), sparselevel = 1, language = "nl", 
                           verbose = TRUE, onlycharacter = FALSE, striphtml = TRUE,
                           customstopwords = NULL, onlytxtfiles = FALSE)

plotRemoved(processed$documents, lower.thresh = seq(1, 20, by = 2))
out <- prepDocuments(processed$documents, processed$vocab, 
                     lower.thresh = 10)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


#determine number of topics
storage <- searchK(out$documents, out$vocab, K = seq(10, 25, by = 2),
                   #data = out$meta, 
                   init.type = "Spectral")

#plot number of topic 
plot.searchK(storage)

corpusPrevFit <- stm(out$documents, out$vocab, K = 20,
                     max.em.its = 75, 
                     data=out$meta, init.type = "Spectral")

labelTopics(corpusPrevFit, seq(1,50, by = 1))
labelTopics(corpusPrevFit, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50))
labelTopics(corpusPrevFit, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))

plot.STM(corpusPrevFit, topics = c(1,2,3,5,8,9,10,11,14,15), type = "summary", n = 10, xlim = c(0, .3))
plot.STM(corpusPrevFit, topics = c(1,2,3,5,8,9,10,11,12,13,14,15), type = "labels", width = 1000, n = 10)
plot.STM(corpusPrevFit, topics = c(1,2,3,5,8,9,10,11,12,13,14,15), type = "hist")

cloud(corpusPrevFit, topic=38, type=c("model"),
      thresh=.9, max.words=50, min.freq = 1, scale=c(5,.5), random.order=FALSE, rot.per=.35,
      colors=brewer.pal(8, "Dark2"))

prep <- estimateEffect(1:20 ~ corpusPrevFit,
                       +  metadata = out$meta, uncertainty = "Global")


mod.out.corr <- topicCorr(corpusPrevFit)
plot.topicCorr(mod.out.corr, topics = c(1,2,3,5,8,9,10,11,14,15))

#####
#plotting words within corpus
#turn back into dataframe for plotting word frequencies within corpus
df2 <- as.data.frame(as.matrix(a.dtm))
df2$date <- as.Date(df$date,"%Y-%m-%d")
agg = aggregate(df2[c("atlanta", "candler", "formule", "pemberton")], by=list(year(df2$date)), #enter keywords here
                FUN=sum, na.rm=FALSE)
colnames(agg)[1] <- "year"
#agg <- agg[1:46,] #select time period to analyze (i.e. 1928-1977)
number_ads <- merge(agg, total_ads) #match number of ads to dataframe
number_articles <- merge(agg, total_article) #match number of articles to dataframe

#rel.agg <- agg[, -1] / number_articles$number_article * 1000 # relative frequency aggregates articles
rel.agg <- agg[, -1] / number_ads$number_ad * 1000# relative frequency aggregates advertisements
rel.agg$year <- agg$year

agg <- melt(agg, id="year")
rel.agg <- melt(rel.agg, id="year")
colnames(agg)[2] <- "keyword"
colnames(rel.agg)[2] <- "keyword"

number_articles <- merge(origin, total_article) #match number of articles to dataframe
rel.origin <- origin[, -1] / number_articles$number_article * 1000 # relative frequency aggregates articles

rel.origin$year <- origin$year

rel.origin2 <- melt(rel.origin, id="year")
colnames(rel.origin2)[2] <- "keyword"

number_articles <- merge(boycot, total_article) #match number of articles to dataframe
rel.boycot <- boycot[, -1] / number_articles$number_article * 1000 # relative frequency aggregates articles

rel.boycot$year <- boycot$year

rel.boycot2 <- melt(rel.boycot, id="year")
colnames(rel.boycot2)[2] <- "keyword"
  
#multiple values barchart
  ggplot(data=rel.origin2, 
         aes(x=year, y=value, fill=keyword)) +
  geom_bar(stat="identity", ) +
  theme_bw() +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=15)) +
  xlab("Year")+ylab("Relative Frequency per 1,000 Articles")

ggsave("origin.pdf", width=10, height=5)

#multiple values line chart
ggplot(data=rel.boycot2, aes(x=year, y=value, group = keyword, colour = keyword)) +
  geom_line() +
  theme_bw() +
  geom_point(size=1, shape=1, na.rm=TRUE) +
  scale_x_continuous(breaks=pretty_breaks(n=10), limit = c(1945,1989)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  xlab("Year")+ylab("Relative Frequency per 1,000 articles")

ggsave("boycot.pdf", width=10, height=5)

#word within total corpus
ggplot() +
  geom_line(data = agg, aes(color ="Pepsi", x = year, y = value)) +
  geom_line(data = df2.year, aes(color ="Corpus A", x = df2.year$year, y = df2.year$article.count)) +
  scale_colour_manual(name = "keywords", values=c("#e50000","#029386"))
  


#work in progress
ggplot(agg, aes(x = year, y = value)) + 
  geom_line(aes(color="black")) + 
  geom_smooth(colour = "red") +
  theme_bw() + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=20),limit=c(0,NA),oob=squish) +
  scale_color_manual(values=c("black"="black"),guide=FALSE) + 
  xlab("Year")+ylab("Article Count")

  
ggplot(data=rel.agg, aes(x=year, y=value, fill=keyword)) +
  geom_line(stat="identity") +
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_bw() +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=20)) +
  xlab("Year")+ylab("Article Count")


#work in progress
#plotbigrams
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtmBi <- TermDocumentMatrix(a, control = list(tokenize = BigramTokenizer))

df.bi <- as.data.frame(as.matrix(dtmBi))
df.bi$date <- as.Date(df$date,"%Y-%m-%d")
rownames(df3) <- df$Date #stupid work-around (for calculation relative frequency drop dates again)
df3$date <- NULL
df3$date <- as.Date(df$date,"%Y-%m-%d") #copy date column from original date frame
