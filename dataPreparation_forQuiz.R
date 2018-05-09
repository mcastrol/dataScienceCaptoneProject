suppressMessages(library(ggplot2))
suppressMessages(library(NLP))
library(tm)
library(SnowballC)
library(stringi)
library(RWeka)

#read input files
#no input
#returns the union of all the inputs (totlines)

readFiles<-function() {
  in_blogs <- readLines("final/en_US/en_US.blogs.txt",skipNul=TRUE)
  in_news  <- readLines("final/en_US/en_US.news.txt",skipNul=TRUE)
  in_twitter  <- readLines("final/en_US/en_US.twitter.txt",skipNul=TRUE)
  totlines<-c(in_blogs,in_news, in_twitter)
  return(totlines)
}

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
  final_data_cleaned <- tm_map(final_data_cleaned, content_transformer(removePunctuation))
  final_data_cleaned <- tm_map(final_data_cleaned, removeWords, stopwords("english"))
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
  #bigram_df<-bigram_df[bigram_df$freq > 1,]
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
  #trigram_df<-trigram_df[trigram_df$freq > 1,]
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
  #quagram_df<-quagram_df[quagram_df$freq > 1,]
  write.csv(quagram_df,"quagram.csv",row.names=F)
  quagram <- read.csv("quagram.csv",stringsAsFactors = F)
  saveRDS(quagram,"quagram.RData")
  return(quagram_df)
}




next_word_bigram<-function(sentence_split) {
  next_word<-as.character(head((bigram[bigram$word1==sentence_split[1],])$word2,1))
  freq<-as.character(head((bigram[bigram$word1==sentence_split[1],])$freq,1))
  if(identical(next_word,character(0))) {
    next_word<-"it"
    freq<-0
  }
  next_word_list<-list(next_word, freq)
  return(next_word_list)
}

next_word_trigram<-function(sentence_split) {
  next_word<-as.character(head((trigram[trigram$word1==sentence_split[1] & trigram$word2 == sentence_split[2],])$word3,1))
  freq<-as.character(head((trigram[trigram$word1==sentence_split[1] & trigram$word2 == sentence_split[2],])$freq,1))
  next_word_list<-list(next_word, freq)
  if(identical(next_word,character(0))) {
    next_word_list=next_word_and_f(sentence_split[2])
  }
  return(next_word_list)
}

next_word_quagram<-function(sentence_split) {
  next_word<-as.character(head((quagram[quagram$word1==sentence_split[1] 
                                        & quagram$word2 == sentence_split[2]
                                        & quagram$word3 == sentence_split[3]
                                        ,])$word4,1))
  freq<-as.integer(head((quagram[quagram$word1==sentence_split[1] 
                                 & quagram$word2 == sentence_split[2]
                                 & quagram$word3 == sentence_split[3]
                                 ,])$freq,1))
  next_word_list<-list(next_word, freq)
  if(identical(next_word,character(0))) {
    next_word_list=next_word_and_f(paste(sentence_split[2],sentence_split[3],sep=" "))
  }
  return(next_word_list)
}


next_word_and_f<-function(sentence, gram_to_use=0)  {
  sentence_c<-removeWords(sentence,stopwords('en'))
  sentence_c<-stripWhitespace(removeNumbers(removePunctuation(tolower(sentence_c),preserve_intra_word_dashes = TRUE)))
  sentence_split<- strsplit(sentence_c," ")[[1]]
  qwords<-length(sentence_split)
  print(sentence_split)
  print(qwords)
  if(qwords==1 || gram_to_use==2) { ##use bigram find out next_word
    next_word_list<-next_word_bigram(tail(sentence_split,1))
  }  
  else if(qwords==2 || gram_to_use==3) { ##use trigram find out next_word
    next_word_list<-next_word_trigram(tail(sentence_split,2))
  }
  else {
    next_word_list<-next_word_quagram(tail(sentence_split,3))
  }
  return(next_word_list)
}

average_frequency<-function(sentence)  {
  sentence_c<-removeWords(sentence,stopwords('en'))
  sentence_c<-stripWhitespace(removeNumbers(removePunctuation(tolower(sentence_c),preserve_intra_word_dashes = TRUE)))
  sentence_split<- strsplit(sentence_c," ")[[1]]
  qwords<-length(sentence_split)
  tot_frequency<-0
  for (i in c(1,2,3,4)) {
    weight_i<-i/10
    last_words<-tail(sentence_split,i)
    #print(last_words)
    if(i==1) {
      freq<-as.integer(head((unigram[unigram$word1==last_words[1],])$freq,1))
    }
    else  
      if(i==2) {
        freq<-as.integer(head((bigram[bigram$word1==last_words[1] 
                                      & bigram$word2 == last_words[2]                               
                                      ,])$freq,1))
        
      } else if(i==3) {
        freq<-as.integer(head((trigram[trigram$word1==last_words[1] 
                                       & trigram$word2 == last_words[2]
                                       & trigram$word3 == last_words[3]
                                       ,])$freq,1))
      }
    else if(i==4) {
      freq<-as.integer(head((quagram[quagram$word1==last_words[1] 
                                     & quagram$word2 == last_words[2]
                                     & quagram$word3 == last_words[3]
                                     & quagram$word4 == last_words[4]
                                     ,])$freq,1))
      
    }
    if(length(freq)==0) freq<-0
    tot_frequency<-tot_frequency+(weight_i*freq)
    #cat(sprintf("with %d words tot_frequency %.2f\n",i, tot_frequency))
  }
  #print(tot_frequency)
  return(tot_frequency)
}

totlines<-readFiles()
gc()
ptm <- proc.time()
us_blogs <- readSampleFile("final/en_US/en_US.blogs.txt")
us_news  <- readSampleFile("final/en_US/en_US.news.txt")
us_twitter  <- readSampleFile("final/en_US/en_US.twitter.txt")
final_data=c(us_blogs,us_news, us_twitter)
sampleSize<-80000
final_data<-getSampleData(final_data,sampleSize)
final_data_df<-getCleanedCorpus(final_data)
unigram<-getUnigram(final_data_df)
bigram<-getBigram(final_data_df)
trigram<-getTrigram(final_data_df)
quagram<-getQuagram(final_data_df)
proc.time() - ptm
gc()
