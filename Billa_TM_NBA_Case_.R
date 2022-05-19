#' Title: TM NBA Case I
#' NAME: Yeyi Billa
#' Date: Mar 3 2022
#' 

################################################################################



# setting working directory
setwd("C:/Users/Probook G3/Desktop/University/Hult Int Business School/Text Mining/Text-Mining-NLP/Case/Case I/Data")

# Seting up the system language to limit errors
Sys.setlocale('LC_ALL','C')
# Options to keep characters as characters instead of factors
options(stringsAsFactors = FALSE)

memory.limit(size=15000) #Increase memory storage in R


################################################################################
############################SETTING UP some PACKAGES ###########################
################################################################################


library(ggplot2)          # for visual representations
library(ggthemes)         # parameters to enhance the visuals
library(stringi)          # for convenience of string manipulation
library(NLP)              # this package is required to load the tm package
library(tm)               # to make analysis on the tweets
library(readr)            # to load the data
library(qdapDictionaries) # required to load qdap
library(qdapRegex)        # required to load qdap
library(qdapTools)        # required to load qdap
library(RColorBrewer)     # required to load qdap
library(qdap)             # for frequency of words
library(wordcloud)        # for top words
library(wordcloud2)       # more sophisticated words display
library(stopwords)        # for the use of stopwords-iso lexicon
library(RColorBrewer)     # to have a range of color for graphs
library(ggthemes)         # to use the themes functions
library(ggdendro)         # to construct dendograms
library(sentimentr)       # to calculate sentiment probability
library(syuzhet)          # for sentiment analysis




################################################################################
############################ UNDERSTANDING THE DATA ############################
################################################################################


# loading the data Sets 

Oct19  <- read_csv("A_Oct2019.csv", locale = locale(encoding = "Latin1")) # the locale encoding prevents
#View(Oct19)                                                              #an error for the corpus set up(an error about gsub UDF8)
Nov19 <- read_csv("B_Nov2019.csv", locale = locale(encoding = "Latin1"))
#View(Nov19)
Dec19 <- read_csv("C_Dec2019.csv", locale = locale(encoding = "Latin1"))
#View(Dec19)
#Feb20 <- read_csv("E_Feb2020.csv", locale = locale(encoding = "Latin1"))
#View(Feb20)
#Mar20 <- read_csv("F_Mar2020.csv", locale = locale(encoding = "Latin1"))
#View(Mar20)
#Apr20 <- read_csv("G_Apr2020.csv", locale = locale(encoding = "Latin1"))
#View(Apr20)
#May20 <- read_csv("H_May2020.csv", locale = locale(encoding = "Latin1"))
#View(May20)
#Jun20 <- read_csv("I_June2020.csv", locale = locale(encoding = "Latin1"))
#View(Jun20)
#Jul20 <- read_csv("J_July2020.csv", locale = locale(encoding = "Latin1"))
#View(Jul20)
#Aug20 <- read_csv("K_Aug2020.csv", locale = locale(encoding = "Latin1"))
#View(Aug20)
Sep20 <- read_csv("L_Sep2020.csv", locale = locale(encoding = "Latin1"))
#View(Sep20)
Oct20 <- read_csv("M_Oct2020.csv", locale = locale(encoding = "Latin1"))
#View(Oct20)
#head(Oct20$text, 5)


################################################################################
############################ CLEANING THE DATA #################################
################################################################################


# The NBA 2019-2020 season was supposed to start on Oct 22, 2019 and end on Apr 15, 2020
# Unfortunately with COVID 19 and other events, the season was paused on March 11
# and resumed around July 30 2020.
# For a better focus and better quality of insights, we will base our analysis
# on Oct 19, Nov 19 and Dec 19 as it is the first quater of the season
# and they are also marked by Black Friday and Christmas periods.
# We will then analyze the last month, Oct 20 to have a one year general overview 
# as Feb 20, Mar 20, Aug and Sep 20 are not really marked by trends, and remove the 
# COVID-19 suspension period (Mar 20 to JuL 20).

# ******************************************************************************
# Sampling the data set to work on reduced file.
# the percentage for sampling was based on how much R memory could store them, especially
# after turning them into vector and matrices (after corpus and cleaning).

my_Oct19_idx<- sample(1:nrow(Oct19), size=0.03*nrow(Oct19))
my_Nov19_idx<- sample(1:nrow(Nov19), size=0.03*nrow(Nov19))
my_Dec19_idx<- sample(1:nrow(Dec19), size=0.03*nrow(Dec19))
my_Oct20_idx<- sample(1:nrow(Oct20), size=0.02*nrow(Oct20))



# The next step is to convert the vectors into dataframe

my_Oct19 <- Oct19[my_Oct19_idx,]
my_Nov19 <- Nov19[my_Nov19_idx,]
my_Dec19 <- Dec19[my_Dec19_idx,]
my_Oct20 <- Oct20[my_Oct20_idx,]


# cleaning the data sets by removing the URLs, 'Rt', '@', intelligible words and capitalization. 

prepclean <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  x <- gsub('\u0081.', '', x)
  x <- gsub('\u0099.', '', x)
  x <-gsub('\u0092...', '', x)
  x <-gsub('\u009.', '', x)
  x <- gsub('werâ¦', '', x)
  x <- gsub('coâ¦', '', x)
  x <- gsub('toâ¦', '', x)
  x <- gsub('ð.', '', x)
  x <- gsub('â.', '', x)
  x <- tolower(x)
  return(x)
} # closing the prepclean function




# Applying the function prepclean to the data Sets

textOct19 <- prepclean(my_Oct19$text)
textNov19 <- prepclean(my_Nov19$text)
textDec19 <- prepclean(my_Dec19$text)
textOct20 <- prepclean(my_Oct20$text)



# To help deal with the errors
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
} # closing the tryTolower function


# cleancorpus function to clean the datasets from "Noises" to avoid errors 
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
} # closing the cleancorpus function



# Create custom stop words to add some words in English dictionary
stops <- c(stopwords("en", source = "stopwords-iso"), 'lol', 'smh', 'lmao')



# **************************** October_2019  ***********************************

# Applying the VCorpus Function to a VectorSource of the original text dataset
cleanTxt_Oct19 <- VCorpus(VectorSource(textOct19))

# Cleaning the corpuswith the fuction and the stopwords
cleanTxt_Oct19 <- cleanCorpus(cleanTxt_Oct19, stops)

# Construct a DTM in an object called cleanMat
cleanMat_Oct19 <- TermDocumentMatrix(cleanTxt_Oct19)


# Switch this to a simple matrix still called cleanMat
clean_Oct19 <- as.matrix(cleanMat_Oct19)



# **************************** November_2019  **********************************

# Applying the VCorpus Function to a VectorSource of the original text dataset

cleanTxt_Nov19 <- VCorpus(VectorSource(textNov19))

# Cleaning the corpuswith the fuction and the stopwords
cleanTxt_Nov19 <- cleanCorpus(cleanTxt_Nov19, stops)

# Turning cleaned data into a TDM for better data manipulation
cleanMat_Nov19 <- TermDocumentMatrix(cleanTxt_Nov19)


# Turning the TDM into a matrix
clean_Nov19 <- as.matrix(cleanMat_Nov19)


# **************************** December_2019  **********************************


# Applying the VCorpus Function to a VectorSource of the original text dataset
cleanTxt_Dec19 <- VCorpus(VectorSource(textDec19))

# Cleaning the corpuswith the fuction and the stopwords
cleanTxt_Dec19 <- cleanCorpus(cleanTxt_Dec19, stops)

# Construct a DTM in an object called cleanMat
cleanMat_Dec19 <- TermDocumentMatrix(cleanTxt_Dec19)

# Switch this to a simple matrix still called cleanMat
clean_Dec19 <- as.matrix(cleanMat_Dec19)



# **************************** October_2020  ***********************************



# Applying the VCorpus Function to a VectorSource of the original text dataset
cleanTxt_Oct20 <- VCorpus(VectorSource(textOct20))

# Cleaning the corpuswith the fuction and the stopwords
cleanTxt_Oct20 <- cleanCorpus(cleanTxt_Oct20, stops)

# Construct a DTM in an object called cleanMat
cleanMat_Oct20 <- TermDocumentMatrix(cleanTxt_Oct20)


# Switch this to a simple matrix still called cleanMat
clean_Oct20 <- as.matrix(cleanMat_Oct20)



################################################################################
########################### Descriptive Analytics ##############################
################################################################################

### Dendrograms

# **************************** October_2019  ***********************************
# Reducing TDM by removing sparse terms
reducedTDM_Oct19 <- removeSparseTerms(cleanMat_Oct19, sparse=0.95)   

# Turning TDM into a dataframe
reducedTDM_Oct19 <- as.data.frame(as.matrix(reducedTDM_Oct19))

# Basic Hierarchical Clustering
hc_Oct19 <- hclust(dist(reducedTDM_Oct19))
plot(hc_Oct19,yaxt='n')

# Rendering the dendrogram
ggdendrogram(hc_Oct19, rotate=FALSE) 


# **************************** November_2019  **********************************
# Reducing TDM by removing sparse terms
reducedTDM_Nov19 <- removeSparseTerms(cleanMat_Nov19, sparse=0.95)   
reducedTDM_Nov19

# Turning TDM into a dataframe
reducedTDM_Nov19 <- as.data.frame(as.matrix(reducedTDM_Nov19))

# Basic Hierarchical Clustering
hc_Nov19 <- hclust(dist(reducedTDM_Nov19))
plot(hc_Nov19,yaxt='n')

# Rendering the dendrogram
ggdendrogram(hc_Nov19, rotate=FALSE)


# **************************** December_2019  **********************************
# Reducing TDM by removing sparse terms
reducedTDM_Dec19 <- removeSparseTerms(cleanMat_Dec19, sparse=0.95)  
reducedTDM_Dec19

# Organize the smaller TDM
reducedTDM_Dec19 <- as.data.frame(as.matrix(reducedTDM_Dec19))

# Basic Hierarchical Clustering
hc_Dec19 <- hclust(dist(reducedTDM_Dec19))
plot(hc_Dec19,yaxt='n')

# Rendering the dendrogram
ggdendrogram(hc_Dec19, rotate=FALSE) 


# **************************** October_2020  ***********************************
# Reducing TDM by removing sparse terms
reducedTDM_Oct20 <- removeSparseTerms(cleanMat_Oct20, sparse=0.95)  
reducedTDM_Oct20

# Turning TDM into a dataframe
reducedTDM_Oct20 <- as.data.frame(as.matrix(reducedTDM_Oct20))

# Basic Hierarchical Clustering
hc_Oct20 <- hclust(dist(reducedTDM_Oct20))
plot(hc_Oct20,yaxt='n')

# Rendering the dendrogram
ggdendrogram(hc_Oct20, rotate=FALSE) 


################################################################################
### Bar chart for Top words in October 2019 and November 2019


# **************************** October_2019  ***********************************
# Frequency Data Frame
tweetSums_Oct19 <- rowSums(clean_Oct19)
tweetFreq_Oct19 <- data.frame(word=names(tweetSums_Oct19),frequency=tweetSums_Oct19)


# Remove the row attributes 
rownames(tweetFreq_Oct19) <- NULL


# barplot for values greater than 500
topWords_Oct19      <- subset(tweetFreq_Oct19, tweetFreq_Oct19$frequency >= 500) 
topWords_Oct19      <- topWords_Oct19[order(topWords_Oct19$frequency, decreasing=F),]
topWords_Oct19$word <- factor(topWords_Oct19$word, 
                              levels=unique(as.character(topWords_Oct19$word))) 

# Rendering the barplot
ggplot(topWords_Oct19, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)



# **************************** November_2019  **********************************
# Frequency Data Frame
tweetSums_Nov19 <- rowSums(clean_Nov19)
tweetFreq_Nov19 <- data.frame(word=names(tweetSums_Nov19),frequency=tweetSums_Nov19)


# Remove the row attributes 
rownames(tweetFreq_Nov19) <- NULL

# barplot for values greater than 550
topWords_Nov19      <- subset(tweetFreq_Nov19, tweetFreq_Nov19$frequency >= 550) 
topWords_Nov19      <- topWords_Nov19[order(topWords_Nov19$frequency, decreasing=F),]
topWords_Nov19$word <- factor(topWords_Nov19$word, 
                              levels=unique(as.character(topWords_Nov19$word))) 

# Rendering the barplot
ggplot(topWords_Nov19, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='blue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)




################################################################################
### Bar charts for players popularity on a year overview (Oct19-Oct20)


# **************************** October_2019  **********************************

#Count of the name of the payers in tweets
Adidas_Lillard_19  <- sum(stri_count(textOct19, fixed ="lillard"))
Adidas_Harden_19  <- sum(stri_count(textOct19, fixed ="harden"))
Nike_Giannis_19  <- sum(stri_count(textOct19, fixed ="giannis"))

# Organize term objects into a data frame
termFreq19 <- data.frame(terms = c('Nike_Lebron_19','Nike_Giannis_19','Adidas_Lillard_19', 'Adidas_Harden_19'),
                         freq  = c('Nike_Lebron_19','Nike_Giannis_19','Adidas_Lillard_19', 'Adidas_Harden_19'))


# Rendering the graph
ggplot(termFreq19, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none") +
  theme(axis.text.x=element_blank()) +
  labs(title = "Players popularity in October 2019", # title of the chart
       x = "Players") # name of the x axis



# **************************** October_2020  **********************************

#Count of the name of the payers in tweets
Nike_Giannis_20  <- sum(stri_count(textOct20, fixed ="giannis"))
Nike_Lebron_20  <- sum(stri_count(textOct20, fixed ="lebron"))
Adidas_Harden_20 <- sum(stri_count(textOct20, fixed ="harden"))
Adidas_Lillard_20  <- sum(stri_count(textOct20, fixed ="lillard"))

# Organize term objects into a data frame
termFreq20 <- data.frame(terms = c('Nike_Lebron_20','Nike_Giannis_20','Adidas_Lillard_20', 'Adidas_Harden_20'),
                         freq  = c('Nike_Lebron_20','Nike_Giannis_20','Adidas_Lillard_20', 'Adidas_Harden_20'))



# Rendering the graph
ggplot(termFreq20, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")+
  theme(axis.text.x=element_blank()) +
  labs(title = "Players popularity in October 2020", # title of the chart
       x = "Players") # name of the x axis



################################################################################
### Bar charts for Teams popularity on a year overview (Oct19-Oct20)


# **************************** October_2019  ***********************************

#Count of the name of the payers in tweets
LA_Lackers_19 <- sum(stri_count(textOct19, fixed ="lackers"))
Milwaukee_Bucks_19  <- sum(stri_count(textOct19, fixed =" bucks"))
Boston_celtics_19  <- sum(stri_count(textOct19, fixed ="celtics"))
Atlanta_Hawks_19  <- sum(stri_count(textOct19, fixed ="hawks"))
Miami_Heat_19  <- sum(stri_count(textOct19, fixed ="heat"))

# Organize term objects into a data frame
termFreqT19 <- data.frame(terms = c('LA_Lackers_19','Milwaukee_Bucks_19','Boston_celtics_19', 'Atlanta_Hawks_19','Miami_Heat_19'),
                          freq  = c('LA_Lackers_19','Milwaukee_Bucks_19','Boston_celtics_19', 'Atlanta_Hawks_19', 'Miami_Heat_19'))


# Rendering the graph
ggplot(termFreqT19, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")+
  theme(axis.text.x=element_blank()) +
  labs(title = "Teams popularity in October 2019", # title of the chart
       x = "Teams") # name of the x axis


# **************************** October_2020 ************************************

#Count of the name of the payers in tweets
LA_Lackers_20 <- sum(stri_count(textOct20, fixed ="lackers"))
Milwaukee_Bucks_20  <- sum(stri_count(textOct20, fixed =" bucks"))
Atlanta_Hawks_20  <- sum(stri_count(textOct20, fixed ="hawks"))
Miami_Heat_20  <- sum(stri_count(textOct20, fixed ="heat"))
Boston_celtics_20  <- sum(stri_count(textOct20, fixed ="celtics"))

# Organize term objects into a data frame
termFreqT20 <- data.frame(terms = c('LA_Lackers_20','Milwaukee_Bucks_20','Atlanta_Hawks_20', 'Miami_Heat_20', 'Boston_celtics_20'),
                          freq  = c('LA_Lackers_20','Milwaukee_Bucks_20','Atlanta_Hawks_20', 'Miami_Heat_20', 'Boston_celtics_20'))


# Rendering a Barplot
ggplot(termFreqT20, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none") +
  theme(axis.text.x=element_blank()) +
  labs(title = "Teams popularity in October 2020", # title of the chart
       x = "Teams") # name of the x axis


################################################################################
### Bar charts for products popularity during November (Black Market month)


#Count of the products in tweets
ball_Nov19  <- sum(stri_count(textNov19, fixed ="ball"))
hat_Nov19  <- sum(stri_count(textNov19, fixed ="hat"))
jersey_Nov19  <- sum(stri_count(textNov19, fixed ="jersey"))
bag_Nov19  <- sum(stri_count(textNov19, fixed ="bag"))
backpacks_Nov19  <- sum(stri_count(textNov19, fixed ="backpacks"))
sleeves_Nov19  <- sum(stri_count(textNov19, fixed ="sleeves"))
cap_Nov19  <- sum(stri_count(textNov19, fixed =" cap"))


# Organize term objects into a data frame
products_Nov19 <- data.frame(terms = c('ball_Nov19','hat_Nov19','jersey_Nov19', 'bag_Nov19', 'backpacks_Nov19', 'cap_Nov19'),
                             freq  = c('ball_Nov19','hat_Nov19','jersey_Nov19', 'bag_Nov19', 'backpacks_Nov19', 'cap_Nov19'))


# Rendering a Bar plot
ggplot(products_Nov19, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity")  + 
  theme_gdocs() + theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), # deleting the horizontal grid
        panel.grid.minor = element_blank()) + # deleting the vertical grid
  theme(axis.text.y=element_blank()) +
  labs(title = "Products popularity in November 19", # title of the chart
       x = "Products") # name of the x axis




################################################################################
### Word cloud for frequency


# **************************** December_2019  **********************************

## First view on simple wordcloud
# Sum of the rows
topDec19 <- sort(rowSums(clean_Dec19), decreasing = TRUE)
# Organize into a dataframe
topDec19DF   <- data.frame(word = names(topDec19), freq = topDec19)

# choosing colors for word cloud
pal <- brewer.pal(8, "OrRd")
pal <- pal[-(1:2)] # removing the first 2 from the 8 shades of purples because it is too white.

# Rendering the graph
set.seed(1234)
wordcloud(topDec19DF$word,
          topDec19DF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(6,1))



## Second View with a Bigram 
# Bigram function
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
} # closing the function

# Making a bigram TDM 
Dec19_TDM  <- TermDocumentMatrix(cleanTxt_Dec19, 
                                 control=list(tokenize=bigramTokens))

# Sampling to 50% because of large dataset (Error because of space)
Dec19_TDM_idx <- sample(1:nrow(Dec19_TDM), size=0.5*nrow(Dec19_TDM))
Dec19_TDM <- Dec19_TDM[Dec19_TDM_idx,]

# Organizing into a matrix
Dec19_TDMm <- as.matrix(Dec19_TDM)


# Get Row Sums & organize
Dec19_TDMv <- sort(rowSums(Dec19_TDMm), decreasing = TRUE)
Dec19_DF   <- data.frame(word = names(Dec19_TDMv), freq = Dec19_TDMv)

# Rendering only the top 50 binome words
wordcloud2(data = Dec19_DF[1:50,])


# Color choice for graph
pal <- brewer.pal(8, "Dark2")

# Rendering the graph
wordcloud2(Dec19_DF[1:50,], 
           color = pal, 
           backgroundColor = "lightgrey")




################################################################################
### SENTIMENT ANALYSIS


# **************************** October 2019  **********************************

# probability of sentiments in October 2019 (Positive: Good; Negative: Bad) 
sentiment <- sentiment_by(textOct19[1:10])


average_mean(sentiment$ave_sentiment)# 12.5% positive sentiment

#Organizing into a character datatype
review_Oct19  <- as.character(textOct19)

# general emotions(anger, joy, trust...)
s <- get_nrc_sentiment(review_Oct19)

# Rendering a barplot of the sentiment
barplot(colSums(s), col = rainbow(10), ylab= 'count', main = 'General Sentiment in October 2019')



# **************************** October 2020  **********************************

# probability of sentiments in October 2020 (Positive: Good; Negative: Bad) 
sentiment <- sentiment_by(textOct20[1:10])

average_mean(sentiment$ave_sentiment) # 8.8% positive sentiment in Overall

#Organizing into a character datatype
review_Oct20 <- as.character(textOct20)

# general emotions(anger, joy, trust...)
s_2 <- get_nrc_sentiment(review_Oct20)


# Rendering a barplot of the sentiment
barplot(colSums(s_2) , col = rainbow(10), ylab= 'count', main = 'General Sentiment in October 2020')




# ***************** Sentiment on Lebron James in November 2019 *****************

# Subsetting the data set to have only tweets mentioning Lebron
Lebron_Nov19 <- my_Nov19[grep('lebron', my_Nov19$text, ignore.case = TRUE ), 2]

#Organizing into a character datatype
review_Lebron_Nov19 <- as.character(Lebron_Nov19)

# general emotions(anger, joy, trust...)
s_L <- get_nrc_sentiment(review_Lebron_Nov19)

# setting the color for the graph
pal <- brewer.pal(8, "Dark2")

barplot(colSums(s_L) , col = pal, ylab= 'count', main = 'Sentiment on Player Lebron James in November 2019')




# ***************** Sentiment on Lebron James in November 2019 *****************

# Subsetting the data set to have only tweets mentioning Lebron
lakers <- my_Nov19[grep('lakers', my_Nov19$text, ignore.case = TRUE ), 2]

#Organizing into a character datatype
review_Lakers_Nov19 <- as.character(lakers)

# general emotions(anger, joy, trust...)
s_La <- get_nrc_sentiment(review_Lakers_Nov19)
barplot(colSums(s_La) , col = pal, ylab= 'count', main = 'Sentiment on Team Lakers in November 2019')


################################################################################
##################################### END ######################################
################################################################################





