#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "styles.css",
  
  # Application title
  titlePanel("The Humble Gram Guesser"),
  fluidRow(helpText(
      "Welcome to The Humble Gram Guesser! You are seeing an interactive demonstration of a natural language processing algorithm!
      The purpose of this model is to assist users in casual conversation, by suggesting a the next word given a set of words leading up to it."),
      helpText(
      "Here are the rules: The user needs to provide at least three words. The words can only contain alphanumeric characters and apostrophes, no other characters.
      All words need to be separated by a single space."),
      helpText(
      "Once the The Humble Gram Guesser returns a word, the user can 'grade' whether that guess is accurate.
      Use the buttons on the right side of the page to keep track of how well the Gram Guesser is doing."),
      helpText(
          "Go ahead and give it a try. Start with common phrases and work up to more obscure sentences. Enjoy!"),
      
      style = "margin-left: 10px; margin-right: 10px;"),
  
  fluidRow(
    column(6,
            fluidRow(
               column(6,
                      h2(helpText("Start here:"))
               )
            ),
            fluidRow(
               column(8, 
                      textInput(inputId = "gram", value = "enter some words to get started", label = NULL)
               ),
               column(1, 
                      actionButton("go", label = "Go!", class = "go")
               )
            ),
            fluidRow(
                column(10, id = "errmsg",
                       span(htmlOutput(outputId = "errmsg"), id = "errmsg")
                )),
            fluidRow(
               column(10, id = "guesscol",
                      h3(helpText("The next word is:")),
                      h4(htmlOutput(outputId = "guess"), id = "guess")
               )       
            )
     ),
  
     column(6, 
             fluidRow(
                 h2(helpText("How'd it do?"))
             ),

             fluidRow(
                 textOutput(outputId = "label_c", inline = TRUE),
                 actionButton("correct", label = "Correct!", class = "action"),
                 textOutput(outputId = "percent_c", inline = TRUE)
             ),
             fluidRow(
                 textOutput(outputId = "label_k", inline = TRUE),
                 actionButton("kinda", label = "kinda close...", class = "action"),
                 textOutput(outputId = "percent_k", inline = TRUE)
             ),
             fluidRow(
                 textOutput(outputId = "label_w", inline = TRUE),
                 actionButton("wayoff", label = "WAY off", class = "action"),
                 textOutput(outputId = "percent_w", inline = TRUE)
             ),
             fluidRow(
                 textOutput(outputId = "label_e", inline = TRUE),
                 actionButton("error", label = "Error", class = "action"),
                 textOutput(outputId = "percent_e", inline = TRUE)
             ),
             fluidRow(
                 textOutput(outputId = "label_r", inline = TRUE),
                 actionButton("reset", label = "Reset", class = "reset")
             )
      )
  )
  
))
