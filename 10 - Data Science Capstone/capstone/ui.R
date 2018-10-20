# Shiny app

#####################################################################
# loading libraries
library(markdown)
library(shiny)
#####################################################################
# UI code
shinyUI(
  navbarPage("Coursera Data Science Specialization Capstone",
    tabPanel ("About",
      fluidRow(
        column(12,
          h3("Coursera Data Science Specialization Capstone offered by Johns Hopkins University and Coursera"),
          h5("This simple text prediction application that was created as a capstone project for the Data Science Specialization course that is a partnership with swiftkey as part of the Coursera & Johns Hopkins University's specialization in Data Science"),
          h4("Purpose:"),
          h5("The purpose of this project is to mine the text sources from blogs, news articles and twitter feed and come up with a word prediction algorithm."),
          h5("This prediction model is available on the Predictor tab."),      h4("Additional Details:"),
          a(href="https://github.com/cardosop/Data-Science-Johns-Hopkins/tree/master/capstone", "GitHub"),
          br(),
          a(href="http://rpubs.com/phcardoso/177809", "Milestone Report"),
		  br(),
          a(href="http://rpubs.com/phcardoso/180791", "Presentation"),
          h4("OBSERVATION"),
          h5("Due to the large amount of terms and consequently the large amount of N-Grams, processing became slow.
		  If the application crashes, with the appearance of the error message, please reload the page.")
        )
      )
    ),
    tabPanel("Predictor",
      sidebarLayout(
        sidebarPanel(
          h4("This application tries to predict the next word."),
          h4("Just type a phrase, (without special characters) in the box below."),
          br(),
          textInput("input_str", label = "Enter some text without punctuation:", value = " ")
        ),
        mainPanel(
          h4("Entered Phrase - with suggested completion of the last word", style = "color:green;"),
          verbatimTextOutput("text1"),
          h4("Next Word Prediction:", style = "color:red"),
          verbatimTextOutput("text2")
        )
      )
    )
  )
)








