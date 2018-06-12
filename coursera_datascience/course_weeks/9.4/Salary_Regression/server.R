library(shiny)
library(caret)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  df.pergame <- read.csv("./data/pergame.csv")
  df.career <- read.csv("./data/career.csv")
  
  m.c <- lm(Salary ~ . - idTeam - Player - Tm - fga - fgm - fg3m - fg3a - fta - ftm - fg2m - fg2a - treb, data = df.career)
  m.p <- lm(Salary ~ . - idTeam - Player - Tm - fga - fgm - fg3m - fg3a - fta - ftm - fg2m - fg2a - treb, data = df.pergame)
  
  pred.c <- predict(m.c, df.career)
  pred.p <- predict(m.p, df.pergame)

  
  output$predPlot <- renderPlot({

       if (input$radio == 1) {
         m.use <- m.c
         df.use <- df.career
         pred.use <- pred.c
       }
       else {
         m.use <- m.p
         df.use <- df.pergame
         pred.use <- pred.p
       }
      
      # paging input
      page <- input$page
      low <- ((page-1) * 15)+1
      high <- page * 15
      
      actual <- df.use$Salary[low:high]>pred.use[low:high] # rows where actual > predicted
      
      # create a new column that only includes salaries if the actual <= predicted
      df.use$Salary0 <- df.use$Salary
      df.use$Salary0[actual] = 0
      
      
          
    # plot X players actual salaries
    # plot X players predicted salaries
    
    # y-axis size
    # x-axis size
    par(mar = c(8, 6, 0.1, 0.1))
    
    
    barplot(df.use$Salary[low:high], names.arg = df.use$Player[low:high], las = 2, beside = TRUE)
    barplot(pred.use[low:high], names.arg = df.use$Player[low:high], las = 2, add = T, col = "navy")
    
    # plot instances where actual <= predicted (on top of the existing plot)
    barplot(df.use$Salary0[low:high], names.arg = df.use$Player[low:high], las = 2, add = T)
    
  })
})
