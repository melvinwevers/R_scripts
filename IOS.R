library("ggplot2")
library(tm)
library(scales)
library(plyr)
library("wordcloud")
library("RColorBrewer")
library(data.table)
library(stm)
#library(RWeka)
library(slam)
#library(plotly)

#load files and put into correct format
#setwd("/Users/wevers/Dropbox/DH-projects/CaseStudy Coca-Cola/Articles/Multinationals/all") #set working directory
temp = list.files(pattern="*.csv") #load list of files  #fix--does not yet load multiple files
df <- read.csv(temp, header=FALSE, stringsAsFactors=F, sep = '\t') #put files into dataframe

colnames(df) <- c("text") #add column names to csv file (change this if columns differ)
df$mergedText = paste(df$title, df$text, sep=" ") #merge title + textcontent into a new column 
df$date <- as.Date(df$date,"%Y-%m-%d") #add date colum as date column
df$year <- format(as.Date(df$date), "%Y") #year column

total_article <- read.csv("/Users/wevers/Dropbox/DH-projects/article_count_final.csv", sep = ',') # load total number of articles in corpus
total_ads <- read.csv("/Users/wevers/Dropbox/DH-projects/advertisement_count.csv", sep = ',') # load total number of advertisements in corpus
per_issue <- read.csv("/Users/wevers/Dropbox/DH-projects/corpus_count.csv", sep =",")
################# Article counter function ##############
countArticles <- function(timeStamps) {
  Dates <- as.Date(strftime(df$date, "%Y-%m-%d"))
  allDates <- seq(from = min(Dates), to = max(Dates), by = "day") 
  article.count <- sapply(allDates, FUN = function(X) sum(Dates == X))
  data.frame(date = allDates, article.count = article.count)
}

df2 <- countArticles(df$date) #count number article per day
df2.year <- setDT(df2)[, lapply(.SD, sum), by=.(year(df2$date))] #aggregate year count


##calculate relative distribution of articles/advertisements in corpus based on total article/advertisement count
df2.year<- df2.year[year>="1890" & year<="1989"] #select years to analyze
total_article <- total_article[(82:100),] #select years to analyze where 1=1890 100=1989
total_ads <- total_ads[(56:100),] #select years to analyze where 1=1890 100=1989

df2.year$rel.freq <- df2.year$article.count / total_article$number_article * 1000 #calculate rel frequency per 1000 articles
df2.year$rel.freq <- df2.year$article.count / total_ads$number_ad * 100000 #calculate rel frequency per 1000 advertisements

smoking <- read.csv("/Users/wevers/Dropbox/DH-projects/popularitySmokingNL.csv", sep = ',')
american_cigarette <- read.csv("/Users/wevers/Dropbox/DH-projects/american_cigarette_phrase.csv", sep = ',')
american_cigarette2 <- read.csv("/Users/wevers/Dropbox/DH-projects/american_cigarette_phrase_brands.csv", sep = ',')
nationalities_cigarette <- read.csv("/Users/wevers/Dropbox/DH-projects/nationalities_cigarette.csv", sep = ',')
mild <- read.csv("/Users/wevers/Dropbox/DH-projects/mild_cigarette.csv", sep = ',')
features_american <- read.csv("/Users/wevers/Dropbox/DH-projects/features_american.csv", sep = ',')
materiality_american <- read.csv("/Users/wevers/Dropbox/DH-projects/materiality_american.csv", sep = ',')
popularity_cig <- read.csv("/Users/wevers/Dropbox/DH-projects/international_fame_cig.csv", sep = ',')


##plot two corpora 
ggplot() +
  geom_line(data = df3.year, aes(color ="Corpus A", x = df3.year$year, y = df3.year$rel.freq)) +
  geom_smooth(data = df3.year, aes(color ="Corpus A", x = df3.year$year, y = df3.year$rel.freq)) +
  geom_line(data = df2.year, aes(color ="Corpus B", x = df2.year$year, y = df2.year$rel.freq)) +
  geom_smooth(data = df2.year, aes(color ="Corpus B", x = df2.year$year, y = df2.year$rel.freq)) +
  scale_colour_manual(name = "keywords", values=c("#e50000","#029386")) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=20),limit=c(0,NA),oob=squish) +
  xlab("Year")+ylab("Relative Frequency per 1000 articles")

##plot number of cigarettes smoked per capita
ggplot(smoking, aes(x = smoking$year, y = smoking$number)) + 
  geom_line(aes(color="black")) + 
  geom_smooth(colour = "red") +
  geom_point()+
  theme_bw() + 
  scale_x_continuous(breaks=pretty_breaks(n=20),limit=c(1923,1989)) +
  scale_y_continuous(breaks=pretty_breaks(n=10),limit=c(0,NA),oob=squish) +
  scale_color_manual(values=c("black"="black"),guide=FALSE) + 
  #scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y"), limits = as.Date(c('1929-01-01','1989-12-31'))) + 
  xlab("Year")+ylab("Number of Cigarettes (x1000) per Capita")
#labs(title="Number of A with references to 'Average American'  in Newspapers (1890 and 1989)")

ggsave("Smoking.pdf", width=10, height=5)

###############
##plot article/advertisements count
ggplot(df2.year, aes(x = df2.year$year, y = df2.year$rel.freq)) + 
  geom_line(aes(color="black")) + 
  geom_smooth(colour = "red") +
  geom_point()+
  theme_bw() + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=15),limit=c(0,NA),oob=squish) +
  scale_color_manual(values=c("black"="black"),guide=FALSE) + 
  #scale_x_date(breaks = date_breaks("5 years"), labels = date_format("%Y"), limits = as.Date(c('1929-01-01','1989-12-31'))) + 
  xlab("Year")+ylab("Relative Frequency per 1000 Ads")
  #labs(title="Number of A with references to 'Average American'  in Newspapers (1890 and 1989)")
ggsave("CorpusB555_rel_ads.pdf", width=10, height=5)



##Plot two different corpus distributions (for instance, df2 and df3) in one graph

ggplot() +
  geom_line(data = per_issue, aes(color ="Article", x = per_issue$year, y = per_issue$article_issue)) +
  #geom_point(size=4) +
  geom_line(data = per_issue, aes(color = "Advertisement", x = per_issue$year, y = per_issue$ad_issue)) +
  scale_colour_manual(name = "Document type", values=c("#e50000","#029386")) +
  theme_bw()+
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10),limit=c(0,NA),oob=squish) +
  xlab("Year")+ylab("Documents per issue")

ggsave("article_ads_per_issue.pdf", width=10, height=5)



  #pre-processing / cleaning text in corpus 
a <- Corpus(DataframeSource(df[c("text")]))
a <- tm_map(a, removePunctuation)
a <- tm_map(a, content_transformer(tolower))
a <- tm_map(a, content_transformer(gsub), pattern = "advertentie", replacement = "")
a <- tm_map(a, stripWhitespace)
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removeWords, c(stopwords("dutch")))

#function to concatenate words
for (j in seq(a))
{
  a[[j]] <- gsub("image", "", a[[j]])
  a[[j]] <- gsub("omitted", "", a[[j]])
  a[[j]] <- gsub("the", "", a[[j]])
  a[[j]] <- gsub("bram", "", a[[j]])
  a[[j]] <- gsub("steije", "", a[[j]])
  a[[j]] <- gsub("durk", "", a[[j]])
  #a[[j]] <- gsub("amerikaans imperialisme", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("amerikaanse imperialisten", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("amerikaansche imperialisten", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("amerikaanse imperialisme", "amerikaansimperialisme", a[[j]])
  #a[[j]] <- gsub("vereenigde staten", "verenigdestaten", a[[j]])
  
  
}

a <- tm_map(a, PlainTextDocument)

#turn corpus into document term matrix, with different configurations
a.dtm <- DocumentTermMatrix (a)

a.dtm <- DocumentTermMatrix(a, control = 
                              list(wordLengths = c(4,Inf)))

a.dtm <- DocumentTermMatrix(a, control = 
                              list(wordLengths = c(5,Inf)))

a.dtm <-removeSparseTerms(a.dtm, sparse=.99) #removing really rare words so that the DTM is not too big / this is almost the same as previous line find out which one to keep

###ANALYSIS
#word frequency
findFreqTerms(a.dtm, lowfreq=100)

#export frequency table
m_freq <- as.matrix(a.dtm)   
dim(m_freq)   
write.csv(m_freq, file="dtm.csv")  

#find associations to particular words
findAssocs(a.dtm, "joris", corlimit=0.20)

#word cloud
m <- as.matrix(a.dtm)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,100)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 9000, scale=c(5,0.5),
          max.words=250, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

####STM (Structural topic modeling)
#processed <- readCorpus(a.dtm, type =("slam")) # use own dtm
processed <- textProcessor(df$text_content, metadata = df, lowercase = TRUE, removestopwords = TRUE,
                           removenumbers = TRUE, removepunctuation = TRUE, stem = FALSE,
                           wordLengths = c(4,Inf), sparselevel = .95, language = "nl", 
                           verbose = TRUE, onlycharacter = FALSE, striphtml = TRUE,
                           customstopwords = NULL, onlytxtfiles = FALSE)

#determine how many documents to remove
plotRemoved(processed$documents, lower.thresh = seq(1, 50, by = 1))

out <- prepDocuments(processed$documents, processed$vocab, 
                     lower.thresh = 10) #change this number based on graph
docs <- out$documents
vocab <- out$vocab

#determine number of topics
storage <- searchK(out$documents, out$vocab, K = c(5), #change these settings to particular range and interval
                   #data = out$meta, 
                   init.type = "Spectral")

#plot number of topics
plot.searchK(storage)


TopicModelFit <- stm(out$documents, out$vocab, K = 15,
                     max.em.its = 150, 
                     data=out$meta, init.type = "Spectral")

labelTopics(TopicModelFit, seq(1,50, by = 1)) #sequence 
labelTopics(TopicModelFit, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) #collection

#three types of plotting topic model
plot.STM(TopicModelFit, topics = seq(1,50, by =1), type = "summary", n = 10, xlim = c(0, .5))
plot.STM(TopicModelFit, topics = c(1,2,3,4,5), type = "labels", width = 1000, n = 10)
plot.STM(TopicModelFit, topics = c(1,2,3,4,5), type = "hist")

#make word cloud of topics
cloud(TopicModelFit, topic=5, type=c("model"),
      thresh=.9, max.words=50, min.freq = 1, scale=c(5,.5), random.order=FALSE, rot.per=.35,
      colors=brewer.pal(8, "Dark2"))

#plot correlation graph of topics 
mod.out.corr <- topicCorr(TopicModelFit) 
plot.topicCorr(mod.out.corr, topics = seq(1,50, by = 1))

######plotting words within corpus####

#turn back into dataframe for plotting word frequencies within corpus
df3 <- as.data.frame(as.matrix(a.dtm))
df3$date <- as.Date(df$date,"%Y-%m-%d")
agg = aggregate(df3[c("arabische", "amerikaanse")], by=list(year(df3$date)), #enter keywords here
                FUN=sum, na.rm=FALSE)
colnames(agg)[1] <- "year"
#agg <- agg[1:46,] #select time period to analyze (i.e. 1928-1977)
number_ads <- merge(agg, total_ads) #match number of ads to dataframe
number_articles <- merge(agg, total_article) #match number of articles to dataframe

rel.agg <- agg[, -1] / number_articles$number_article * 1000 # relative frequency aggregates articles
rel.agg <- agg[, -1] / number_ads$number_ad * 1000# relative frequency aggregates advertisements
rel.agg$year <- agg$year

agg <- melt(agg, id="year")
rel.agg <- melt(rel.agg, id="year")
colnames(agg)[2] <- "keyword"
colnames(rel.agg)[2] <- "keyword"
  

nat_cig <- melt(nationalities_cigarette, id="year")
colnames(nat_cig)[2] <- "nationality"

ggplot(data=nat_cig, aes(x=year, y=value, group = nationality, colour = nationality)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  geom_point(size=1, shape=1, na.rm=TRUE) +
  #scale_x_continuous(breaks=pretty_breaks(n=10), limit = c(1950,1978)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  xlab("Decade")+ylab("Relative Frequency per 1000 Cigarette Advertisements")

ggsave("nationalities_cigarette.pdf", width=10, height=5)

mild2 <- melt(mild, id="year")
colnames(mild2)[2] <- "corpus"

ggplot(data=mild2, aes(x=year, y=value, group = corpus, fill = corpus)) +
  #geom_line() +
  geom_bar(stat = "identity", position= "dodge") +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  #geom_point(size=1, shape=1, na.rm=TRUE) +
  #scale_x_continuous(breaks=pretty_breaks(n=10), limit = c(1950,1978)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  xlab("Period")+ylab("Relative Frequency per 1,000 Cigarette Ads")

ggsave("mild_cigarette2.pdf", width=10, height=5)

features_american2 <- melt(features_american, id="year")
colnames(features_american2)[2] <- "features"

ggplot(data=features_american2, aes(x=year, y=value, group = features, colour = features)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  geom_point(size=1, shape=1, na.rm=TRUE) +
  #scale_x_continuous(breaks=pretty_breaks(n=10), limit = c(1950,1978)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  xlab("Period")+ylab("Rel. Freq per 1000 Cigarette Ads")

ggsave("features_american.pdf", width=10, height=5)

materiality_american2 <- melt(materiality_american, id="year")
colnames(materiality_american2)[2] <- "features"

ggplot(data=materiality_american2, aes(x=year, y=value, group = features, colour = features)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  geom_point(size=1, shape=1, na.rm=TRUE) +
  #scale_x_continuous(breaks=pretty_breaks(n=10), limit = c(1950,1978)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  xlab("Period")+ylab("Rel. Freq per 1000 Cigarette Ads")

ggsave("materiality_american.pdf", width=10, height=5)

popularity_cig2 <- melt(popularity_cig, id="year")
colnames(popularity_cig2)[2] <- "features"

ggplot(data=popularity_cig2, aes(x=year, y=value, group = features, colour = features)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  geom_point(size=1, shape=1, na.rm=TRUE) +
  #scale_x_continuous(breaks=pretty_breaks(n=10), limit = c(1950,1978)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  xlab("Period")+ylab("Rel. Freq per 1000 Cigarette Ads")

ggsave("popularity_cig.pdf", width=10, height=5)

#multiple values barchart
  ggplot(data=rel.agg, 
         aes(x=year, y=value, fill=keyword)) +
  geom_bar(stat="identity") +
  theme_bw() +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=15)) +
  xlab("Year")+ylab("Relative Frequency per 1000 Articles")

ggplot(american_cigarette2, aes(x=year, y=relative)) + 
  geom_bar(stat = "identity") +
  #geom_line(data = american_cigarette, aes(x = american_cigarette$year, y = american_cigarette$relative)) +
  #geom_smooth(data = american_cigarette2, aes(x = american_cigarette2$year, y = american_cigarette2$relative)) +
  scale_colour_manual(name = "Number of Advertisements", values=c("#e50000","#029386")) +
  theme_bw()+
  scale_x_continuous(breaks=pretty_breaks(n=20)) +
  scale_y_continuous(breaks=pretty_breaks(n=15),limit=c(0,NA),oob=squish) +
  xlab("Year")+ylab("Relative Frequency per 100 Cigarette Brand Advertisements")

ggsave("american_cigarette2.pdf", width=10, height=5)

ggplot(data=american_cigarette, 
       aes(x=year, y=relative)) +
  geom_bar(stat="identity") +
  geom_smooth +
  theme_bw() +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=15)) +
  xlab("Year")+ylab("Relative Frequency per 1000 Articles")

ggsave("american_cigarette.pdf", width=10, height=5)

#multiple values barchart
ggplot(data=mild2, 
       aes(x=year, y=value, fill=keyword)) +
  geom_bar(stat="identity", position= "dodge") +
  theme_bw() +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=15)) +
  xlab("Year")+ylab("Relative Frequency per 1000 Articles")

#multiple values line chart
ggplot(data=rel.agg, aes(x=year, y=value, group = keyword, colour = keyword)) +
  geom_line() +
  theme_bw() +
  geom_point(size=1, shape=1, na.rm=TRUE) +
  scale_x_continuous(breaks=pretty_breaks(n=10), limit = c(1950,1978)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  xlab("Year")+ylab("Relative Frequency per 1000 Ads")

ggsave("keywords_bottle.pdf", width=10, height=5)

#plot particular word within total corpus
ggplot() +
  geom_line(data = agg, aes(color ="Pepsi", x = year, y = value)) +
  geom_line(data = df2.year, aes(color ="Corpus B", x = df2.year$year, y = df2.year$article.count)) +
  scale_colour_manual(name = "keywords", values=c("#e50000","#029386"))

ggplot(data=rel.agg, aes(x=year, y=value, fill=keyword)) +
  geom_line(stat="identity") +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_bw() +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=20)) +
  xlab("Year")+ylab("Article Count")


