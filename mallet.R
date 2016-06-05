options(java.parameters = "-Xmx3000m") 
require(mallet)
require(tm)
require(ape)
data <- read.table("Downloads/Cola_Amerika.csv", header=TRUE, sep="\t", stringsAsFactors=F, fileEncoding="utf-8")
#data <- read.csv("Downloads/1946-1950-cola-clean.txt", stringsAsFactors = FALSE)
#Encoding(data[,28]) <- "latin1" 
#iconv(data[,28], "latin1", "ASCII", sub="")


#create corpus
#corp <- Corpus(VectorSource(data[,28]))

#process text
#skipWords <- function(x) removeWords(x, stopwords("dutch"))
#funcs <- list(content_transformer(tolower), removePunctuation, removeNumbers, stripWhitespace, skipWords)
#corp <- tm_map(corp, FUN = tm_reduce, tmFuns = funcs)

# create document term matrix
#dtm <- DocumentTermMatrix(corp, control = list(wordLengths = c(4,10), 
bounds = list(global = c(length(corp)*0.01,length(corp)*0.95))))

#dtmdf <- data.frame(inspect(dtm))
#sum(dtmdf[, names(dtmdf) == 'amerika'])

#mallet

mallet.instances <- mallet.import(data$id, data$text_content, "Downloads/nl.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

n.topics <- 50
topic.model <- MalletLDA(n.topics)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

topic.model$setAlphaOptimization(20, 50)
topic.model$train(500)
topic.model$maximize(10)
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "topic-docs.csv")


topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=16)$words, collapse=" ")
topics.labels
write.csv(topics.labels, "topics-labels.csv")
# create data.frame with columns as people and rows as topics
topic_docs <- data.frame(topic.docs)
names(topic_docs) <- data$paper_dc_date

hc=hclust(dist(topic.words))

plot(hc, hang= -1, labels=topics.labels)

#' Calculate similarity matrix
#' Shows which documents are similar to each other
#' by their proportions of topics. Based on Matt Jockers' method

library(cluster)
topic_df_dist <- as.matrix(daisy(t(topic_docs), metric = "euclidean", stand = TRUE))
# Change row values to zero if less than row minimum plus row standard deviation
# keep only closely related documents and avoid a dense spagetti diagram
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] <- 0

km <- kmeans(topic_df_dist, n.topics)
# get names for each cluster
allnames <- vector("list", length = n.topics)
for(i in 1:n.topics){
  allnames[[i]] <- names(km$cluster[km$cluster == i])
}
allnames

barplot(topic_df_dist, main="test", xlab="topics")

#install.packages("igraph")
library(igraph)
g <- as.undirected(graph.adjacency(topic_df_dist))
layout1 <- layout.fruchterman.reingold(g, niter=500)
plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1, vertex.color= "grey", edge.arrow.size = 0, vertex.label.dist=0.5, vertex.label = NA)

#install.packages("devtools")

#devtools::install_github("d3Network", "christophergandrud")
require(d3Network)
d3SimpleNetwork(get.data.frame(g),width = 1500, height = 800,
                textColour = "orange", linkColour = "red",
                fontsize = 10,
                nodeClickColour = "#E34A33",
                charge = -100, opacity = 0.9, file = "d3net.html")