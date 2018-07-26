#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(tm)
source('./scripts/run.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    
    counters <- reactiveValues(denom = 0, correct = 0, kinda = 0, wayoff = 0, error = 0) # clean into a vector
    err <- reactiveValues(msg = 1)
    
    observeEvent(input$correct, {
        counters$correct = counters$correct + 1
        counters$denom = counters$denom + 1
        })
    
    observeEvent(input$kinda, {
        counters$kinda = counters$kinda + 1
        counters$denom = counters$denom + 1
    })
    
    observeEvent(input$wayoff, {
        counters$wayoff = counters$wayoff + 1
        counters$denom = counters$denom + 1
    })
    
    observeEvent(input$error, {
        counters$error = counters$error + 1
        counters$denom = counters$denom + 1
    })
    
    observeEvent(input$reset, {
        counters$denom = 0
        counters$correct = 0
        counters$kinda = 0
        counters$wayoff = 0
        counters$error = 0
    })
    
    output$errmsg <- renderText({ 
        
        g <- input$gram
        g <- trimws(g)
        g <- tolower(g)
        
        err$msg <- 1
        
        validate(
                need(g != "", "Enter some words to get going."),
                need((length(strsplit(g, " ")[[1]]) >= 3) | (g == ""), "The guess works best with at least 3 words."),
                need((length(strsplit(g, " ")[[1]]) <= 10), "There's no need for more than 10 words."),
                need(length(grep("^(\\w|['])+\\s(\\w|['])+\\s(\\w|['])+", g)) > 0 | length(strsplit(g, " ")[[1]]) < 3 | length(grep("[^\\w\\s']", g, perl = TRUE)) > 0, 
                     "Words must be separated by a single space."),
                need((length(grep("[^\\w\\s']", g, perl = TRUE)) == 0), "Use alphanumeric characters only.")
            )
        err$msg <- ""
    })
    
    solve <- eventReactive(input$go, {
        
        g <- input$gram
        g <- trimws(g)
        g <- tolower(g)

        validate(
            need(err$msg == "", "Resolve input errors and try again.")
        )
        
        run.solve(g)
    })
    
    
    # move the error message for the user's validation to a different line, don't let it become the guess.
    output$guess <- reactive({solve()})
    
    
    
    output$label_c  <- renderText(paste(counters$correct, counters$denom, sep = "/"))
    output$label_k  <- renderText(paste(counters$kinda, counters$denom, sep = "/"))
    output$label_w  <- renderText(paste(counters$wayoff, counters$denom, sep = "/"))
    output$label_e  <- renderText(paste(counters$error, counters$denom, sep = "/"))

    
    output$percent_c <- renderText(if (counters$denom > 0){paste(100*round(counters$correct/counters$denom, digits = 2), "%", sep = "")} else {"0%"})
    output$percent_k <- renderText(if (counters$denom > 0){paste(100*round(counters$kinda/counters$denom, digits = 2), "%", sep = "")} else {"0%"})
    output$percent_w <- renderText(if (counters$denom > 0){paste(100*round(counters$wayoff/counters$denom, digits = 2), "%", sep = "")} else {"0%"})
    output$percent_e <- renderText(if (counters$denom > 0){paste(100*round(counters$error/counters$denom, digits = 2), "%", sep = "")} else {"0%"})
    
    #output$guess <- renderText({"the next word will appear here"})
    
})
