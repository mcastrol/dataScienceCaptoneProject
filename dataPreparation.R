readSampleFile<-function(fileName) {
  paste("Reading 5% sample of file ",fileName)
  con=file(fileName,open="r")
  qtotlines<-0     # number of total lines
  qselectedlines <-0   #number of total selected lines
  qtotchar<-0     # number of total chars in file
  qcharselectedlines <-0   #number of total char in selected lines
  finaldata<-NULL
  block=5000
  longmaxline=0;
  while ( TRUE ) {
    line = readLines(con, n = block,skipNul=TRUE)
    if ( length(line) == 0 ) {
      break
    }
    if(sample(1:20,1)==5) {  #random 1:20==5 to select 5% of the lines
      qselectedlines<-qselectedlines+block
      qcharselectedlines<-qcharselectedlines+sum(lengths(strsplit(line, "\\W+")))
      finaldata<-c(finaldata,line)
    }
    lenline=max(nchar(line))
    if(lenline > longmaxline) {
      longmaxline=lenline
    }
    qtotlines <- qtotlines+block
    qtotchar<-qtotchar+sum(lengths(strsplit(line, "\\W+")))
  }
  cat(sprintf("File: %s Lines: %.2fM Selected: %.2fM Chars: %.2fM  Selected: %.2fM MaxLine %d\n ", basename(fileName),qtotlines/1000000,qselectedlines/1000000,qtotchar/1000000,qcharselectedlines/1000000,longmaxline))
  close(con)
  return(finaldata)
}

#select samplesize of records from the union of all the inputs (totlines)
#input: union of all inputs and samplesize
#returns sample_data
getSampleData <- function(totlines, samplesize) {
  final_data_sample = sample(totlines,samplesize, replace = FALSE)
  return(final_data_sample)
}

#create the corpus object and data frame. save into a disk as final_data_cleaned.RData
#input: sample data
#returns courpus object data frame
getCleanedCorpus<-function(final_data_sample) {
  set.seed(1234)
  final_data_sample2 <- iconv(final_data_sample, "latin1", "ASCII", sub="")
  #creating corpus object to use tn functions
  final_data_cleaned <- VCorpus(VectorSource(final_data_sample2))
  #cleaning operations on sample text: lower, remove punctuation, remove common words, etc
  final_data_cleaned <- tm_map(final_data_cleaned, content_transformer(tolower))
  #final_data_cleaned <- tm_map(final_data_cleaned, content_transformer(removePunctuation))
  #final_data_cleaned <- tm_map(final_data_cleaned, removeWords, stopwords("english"))
  final_data_cleaned <- tm_map(final_data_cleaned, removeNumbers)
  #removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  replace_reg <-function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  replace_url <-function(x) gsub("http[^[:space:]]*", "", x)
  final_data_cleaned <- tm_map(final_data_cleaned,  content_transformer(replace_reg))
  final_data_cleaned <- tm_map(final_data_cleaned,  content_transformer(replace_url))
  final_data_cleaned <- tm_map(final_data_cleaned, stripWhitespace)
  saveRDS(final_data_cleaned, file = "final_data_cleaned.RData")
  final_data_cleaned_df <-data.frame(text=unlist(sapply(final_data_cleaned,`[`, "content")),stringsAsFactors = FALSE)
  return(final_data_cleaned_df)
}

#tokenize the words of the input data 
#input: input data in data frame / num_grams: the number of words to tokenize
#returns a df with the combinations of num_grams words
getGramDf<-function(final_data_cleaned_df, num_grams) {
  #gram <- NGramTokenizer(final_data_cleaned_df, Weka_control(min = num_grams, max = num_grams,delimiters = " \\r\\n\\t.,;:\"()?!"))
  gram <- NGramTokenizer(final_data_cleaned_df, Weka_control(min = num_grams, max = num_grams))
  gram_df <- data.frame(table(gram))
  gram_df <- gram_df[order(gram_df$Freq,decreasing = TRUE),]
  names(gram_df) <- c("words","freq")
  head(gram_df)
  return(gram_df)
}


#generate a bigram from the data in a data frame
#input: input data in data frame 
#returns a df with the combinations of 2 words - save into a file bigram.csv/bigram.RData
getUnigram<-function(final_data_cleaned_df) {
  gram_df<-getGramDf(final_data_cleaned_df,1)  
  gram_df$words <- as.character(gram_df$words)
  word_split <- strsplit(gram_df$words,split=" ")
  unigram_df <- transform(gram_df,word1= sapply(word_split,"[[",1))
  #bigram_df<-bigram_df[bigram_df$freq > 1,]
  write.csv(unigram_df,"unigram.csv",row.names=F)
  unigram <- read.csv("unigram.csv",stringsAsFactors = F)
  saveRDS(unigram,"unigram.RData")
  return(unigram_df)
}  

#generate a bigram from the data in a data frame
#input: input data in data frame 
#returns a df with the combinations of 2 words - save into a file bigram.csv/bigram.RData
getBigram<-function(final_data_cleaned_df) {
  gram_df<-getGramDf(final_data_cleaned_df,2)  
  gram_df$words <- as.character(gram_df$words)
  word_split <- strsplit(gram_df$words,split=" ")
  bigram_df <- transform(gram_df,word1= sapply(word_split,"[[",1),word2= sapply(word_split,"[[",2))
  #bigram_df <- data.frame(word1 = bigram_df$word1,word2 = bigram_df$word2,freq =   bigram_df$freq,stringsAsFactors=FALSE)
  bigram_df<-bigram_df[bigram_df$freq > 1,]
  write.csv(bigram_df,"bigram.csv",row.names=F)
  bigram <- read.csv("bigram.csv",stringsAsFactors = F)
  saveRDS(bigram,"bigram.RData")
  return(bigram_df)
}  

#generate a trigram from the data in a data frame
#input: input data in data frame 
#returns a df with the combinations of 3 words - save into a file trigram.csv/trigram.RData
getTrigram<-function(final_data_cleaned_df) {
  gram_df<-getGramDf(final_data_cleaned_df,3)  
  gram_df$words <- as.character(gram_df$words)
  word_split <- strsplit(gram_df$words,split=" ")
  trigram_df <- transform(gram_df,word1= sapply(word_split,"[[",1),word2= sapply(word_split,"[[",2), word3=sapply(word_split,"[[",3))
  #trigram_df <- data.frame(word1 = trigram_df$word1,word2 = #trigram_df$word2,word3=trigram_df$word3,freq = trigram_df$freq,stringsAsFactors=FALSE)
  trigram_df<-trigram_df[trigram_df$freq > 1,]
  write.csv(trigram_df,"trigram.csv",row.names=F)
  trigram <- read.csv("trigram.csv",stringsAsFactors = F)
  saveRDS(trigram,"trigram.RData")
  return(trigram_df)
}

#generate a guagram from the data in a data frame
#input: input data in data frame 
#returns a df with the combinations of 4 words - save into a file quagram.csv/quagram.RData
getQuagram<-function(final_data_cleaned_df) {
  gram_df<-getGramDf(final_data_cleaned_df,4)  
  gram_df$words <- as.character(gram_df$words)
  word_split <- strsplit(gram_df$words,split=" ")
  quagram_df <- transform(gram_df,word1= sapply(word_split,"[[",1),word2= sapply(word_split,"[[",2), word3=sapply(word_split,"[[",3),word4=sapply(word_split,"[[",4))
  #uagram_df <- data.frame(word1 = trigram_df$word1,word2 = quagram_df$word2, word3=trigram_df$word3, word4=trigram_df$word4, freq = quagram_df$freq,stringsAsFactors=FALSE)
  quagram_df<-quagram_df[quagram_df$freq > 1,]
  write.csv(quagram_df,"quagram.csv",row.names=F)
  quagram <- read.csv("quagram.csv",stringsAsFactors = F)
  saveRDS(quagram,"quagram.RData")
  return(quagram_df)
}

dataPreparation <- function() {
  suppressMessages(library(NLP))
  library(tm)
  library(SnowballC)
  library(stringi)
  library(RWeka)
  gc()
  ptm <- proc.time()
  us_blogs <- readSampleFile("final/en_US/en_US.blogs.txt")
  us_news  <- readSampleFile("final/en_US/en_US.news.txt")
  us_twitter  <- readSampleFile("final/en_US/en_US.twitter.txt")
  final_data=c(us_blogs,us_news, us_twitter)
  sampleSize<-50000
  final_data<-getSampleData(final_data,sampleSize)
  final_data_df<-getCleanedCorpus(final_data)
  unigram<-getUnigram(final_data_df)
  bigram<-getBigram(final_data_df)
  trigram<-getTrigram(final_data_df)
  quagram<-getQuagram(final_data_df)
  proc.time() - ptm
  gc()
}