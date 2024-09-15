# Importing Libraries
library(NLP)
library(tm)
library(sentimentr)
library(tidyverse)
library(conflicted)
library(wordcloud)
library(tidytext)
library(RColorBrewer)
library(widyr)
library(syuzhet)
library(ggplot2)

# Import Textdata.
textdata <- readLines(file.choose())

head(textdata)

# Convert data into corpus
corp<-Corpus(VectorSource(textdata))
class(corp)

# Inspecting Corpus. Here [1:9] displays first 9 text lines
inspect(corp[1:9])

# 1. Cleaning the Data; convert to lowercase; remove punctuation, numbers, extra whitespaces, stopwords & tokenise
# Displaying a particular document from corpus and setting to lower case
corp <- tm_map(corp, tolower)
writeLines(as.character(corp[[4]]))

# removing punctions
corp <-tm_map(corp, removePunctuation)
writeLines(as.character(corp[[4]]))

# removing numbers
corp <- tm_map(corp, removeNumbers)
writeLines(as.character(corp[[4]]))

# Removing stop words
corp <-tm_map(corp, removeWords, stopwords ("english"))
writeLines(as.character(corp[[4]]))

# Removing the word: includes, will, long
corp <-tm_map (corp, removeWords, "includes")
corp <-tm_map (corp, removeWords, "will") 
corp <-tm_map (corp, removeWords, "nearly")
writeLines(as.character(corp[[4]]))

# Convert to term-document matrix format
tdm <- TermDocumentMatrix(corp)
findFreqTerms(tdm, 9)


# 2. Finding and Displaying Words with Minimum Frequency 6
# findFreqTerms(tdm, 6) 
cat(" The words with minimum frequency of 6 are: (", findFreqTerms(tdm, 6), ")\n") 


# 3. Filtering for words with at least 0.35 correlation with 'film'
# Correlation 
findAssocs(tdm, 'film', 0.35)


# 4. Count the word with minimum frequency 4
# Converting tdm object to a matrix
matrix<-as.matrix(tdm)
head(matrix, 5)

# Calculating total frequency of words & creating a new data frame 
name <- sort(rowSums(matrix), decreasing=TRUE)
myNames <- names(name)
d <- data.frame(word=myNames, freq=name)
head(d, 10)

# Creating word cloud with words having minimum frequency 4
set.seed(1234) # For reproducibility
# Create color palette
pal2 <- brewer.pal(8, "Dark2")
# Display the Word Cloud
wordcloud(d$word, d$freq, 
          random.order = FALSE , 
          min.freq = 4, 
          colors=pal2)


# 5. List the number of lines having sentiments ‘Sarcasm’, ‘Very Negative’ and ‘Very Positive’
# Display the variables for sentiment analysis
textdata <- readLines(file.choose(), warn = FALSE)
sentiment(textdata)

# Calculating the sentiment score
sentiment_scores <- get_sentiment(textdata, method = "syuzhet")
head(sentiment_scores, 10)

# Displaying emotion s variables 
nrcsentiment <- get_nrc_sentiment(textdata) 
head(nrcsentiment, 9)

# Determining the number of lines for each specific sentiment
sarcasm_count <- sum(nrcsentiment$sarcasm > 0)
very_negative_count <- sum(nrcsentiment$negative > 0 & sentiment_scores < -1.5) 
very_positive_count <- sum(nrcsentiment$positive > 0 & sentiment_scores > 1.5)  

# Listing the counts
cat("The number of lines with 'Sarcasm' is:=", sarcasm_count, "\n") # 0

cat("The number of lines with 'Very Negative' sentiment is:=", very_negative_count, "\n") # = 2

cat("The number of lines with 'Very Positive' sentiment is:=", very_positive_count, "\n") # = 3


# 6. Plot graph showing words occurring more than 3 times (Use tidytext package).
term.freq <- rowSums(matrix)
term.freq  <- subset(term.freq, term.freq >=4)

# Transform as a dataframe
df <- data.frame(term = names (term.freq), freq = term.freq)

# Plotting the graph
ggplot(df, aes(x = term, y = freq) ) + 
  geom_bar (stat = "identity", fill = "blue") + 
  labs(title = "Plot of Words Occurring More Than Three Times") +
  xlab( "Word") + 
  ylab("Frequency") + 
  coord_flip()
