library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Basketball Salaries Predicted by Box Score Satistics"),
  
  # Sidebar with a slider input for number of players to include in the linear model
  sidebarLayout(
    sidebarPanel(
       numericInput("page",
                   "Page number:",
                   min = 1,
                   max = 10,
                   value = 1),
       radioButtons("radio", 
                    "Statstics to Use", 
                    choices = list("Career Total" = 1, "Per Game Avgerage" = 2), selected = 1)
    ),
    
    # Show a plot of the predicted vs. actual values
    mainPanel(
       plotOutput("predPlot")
    )
  ),
  fluidRow(helpText("This app presents the user with a visualizatoin of NBA basketball player salaries.",
                    "The plot displays the players actual salary with a grey bar.",
                    "The plot shows the player's salary predicted by a linear model with blue bars.",
                    "These bars are printed on top of one another.",
                    "If the top most portion of a bar is grey, then the player's actual salary exceeds their predicted salary, and vise-versa.",
                    br(),br(),
                    "The model used to predict salaries is generated from players statistical performance.",
                    "The user has an option to base the model off of a player's career total statistics, or off of per-game average statistics",
                    br(),br(),
                    "The user also selects which page of data to view.",
                    "This analysis used the top 150 players ranked in order of 2017-2018 salary.", 
                    "Each page displays 15 players, and the user can select pages 1-10 to view."))
))
