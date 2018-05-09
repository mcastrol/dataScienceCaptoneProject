#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr); library(tm)
library(NLP)

# Loading bigram, trigram and quadgram frequencies words matrix frequencies
bigram <- readRDS("bigram.RData"); 
trigram <- readRDS("trigram.RData"); 
quagram <- readRDS("quagram.RData")

#function for searching in bigram frequency table.
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

#function for searching in trigram frequency table.
#if it not found, it use the function for look in bigram
next_word_trigram<-function(sentence_split) {
  next_word<-as.character(head((trigram[trigram$word1==sentence_split[1] & trigram$word2 == sentence_split[2],])$word3,1))
  freq<-as.character(head((trigram[trigram$word1==sentence_split[1] & trigram$word2 == sentence_split[2],])$freq,1))
  next_word_list<-list(next_word, freq)
  if(identical(next_word,character(0))) {
    next_word_list=next_word_and_f(sentence_split[2])
  }
  return(next_word_list)
}

#function for searching in quagram frequency table.
#if it not found, it use the function for look in trigram
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


#function for searching the sentence.
#this function used the proper function depending of the lenght of the sentence
next_word_and_f<-function(sentence, ngrams_words=0)  {
  sentence_c<-stripWhitespace(removeNumbers(tolower(sentence),preserve_intra_word_dashes = TRUE))
  sentence_split<- strsplit(sentence_c," ")[[1]]
  qwords<-length(sentence_split)
  #print(sentence_split)
  #print(qwords)
  if(qwords==1 || ngrams_words==2) { ##use bigram find out next_word
    next_word_list<-next_word_bigram(tail(sentence_split,1))
  }  
  else if(qwords==2 || ngrams_words==3) { ##use trigram find out next_word
    next_word_list<-next_word_trigram(tail(sentence_split,2))
  }
  else if(qwords>2 || ngrams_words==3) {
    next_word_list<-next_word_quagram(tail(sentence_split,3))
  }
  else {
    next_word_list<-list("it",0)
  }
  return(next_word_list)
}

testtime<-function(sentence,ngrams_words=0) {
  ptm <- proc.time()
  next_word_and_f(sentence,0)
  elapsed<-proc.time() - ptm
  return(elapsed)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$next_word<-renderPrint({
     result<-next_word_and_f(input$inputText,0)
    result[[1]]

  });
  output$bigram<-renderPrint({
    result<-next_word_and_f(input$inputText,2)
    result[[1]]
  });
  output$trigram<-renderPrint({
    result<-next_word_and_f(input$inputText,3)
    result[[1]]
  });
  output$quagram<-renderPrint({
    result<-next_word_and_f(input$inputText,4)
    result[[1]]
  });
})
