#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
suppressWarnings(library(markdown))
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title

  titlePanel("Guessing the next word in the sentence"),
  img(src = 'JHU-Logo.png', height = 100, width = 120),
  img(src = 'coursera.png', height = 100, width = 120),
  img(src = 'swiftkey.png', height = 100, width = 120),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        helpText("Enter a sentence to view which is the guessing of next word"),
        hr(),
        textInput("inputText", "Sentence:",value = ""),
        #helpText("Check to see the back calculation with two, three and four words"),
        #checkboxInput("debug", "Debug"),
        br(),
        h5('Instructions'),
        helpText("This application is for guessing the next word you are going to write."),
        helpText("The next word is guessed using the frequency of combinations of two, three and four words."),
        helpText("The frequency is calculated using text extracted from blogs, news and twitter that SwiftKey  gives for this projects"),
        helpText("The word 'it' is used when there are no hint for guessing of no pattern is found")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h1("Next word"),
      verbatimTextOutput("Guessing..."),
      h3(strong(code(textOutput('next_word')))),
      br(),
      br(),
      br(),
      h3(tags$b('Debug trace:')),
      br(),
      h4(tags$b('Word guest using last two words:')),
      textOutput('bigram'),
      br(),
      h4(tags$b('Word guest using last tree words:')),
      textOutput('trigram'),
      br(),
      h4(tags$b('Word guest using last five words:')),
      textOutput('quagram'),
      br(),
      #tags$b('word guessed:'),
      #tags$b(textOutput("next_word")),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      h4(),
      tags$b("Data Science Capstone Project"),
      br(),
      tags$b("Author: Marcela Castro León - May, 2018")
    )
  )
))
