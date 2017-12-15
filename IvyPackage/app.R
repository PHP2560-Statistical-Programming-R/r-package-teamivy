#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rvest)
library(pipeR)
library(XML)
library(RCurl)
library(htmltab)
library(xml2)

options(shiny.sanitize.errors = FALSE)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NBA Stats"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        conditionalPanel(condition="input.tabselected ==1",
                         h3("Season Table"),
                         selectInput("STteam",
                                     "Choose a team:", 
                                     c("Atlanta Hawks" = "ATL", 
                                       "Boston Celtics" = "BOS",
                                       "Brooklyn Nets" = "BRK",
                                       "New Jersey Nets" = "NJN",
                                       "Charlotte Hornets" = "CHO",
                                       "Charlotte Bobcats" = "CHA",
                                       "Chicago Bulls" = "CHI",
                                       "Cleveland Cavaliers", "CLE",
                                       "Dallas Mavericks" = "DAL",
                                       "Denver Nuggets" = "DEV",
                                       "Detroit Pistons" = "DET",
                                       "Golden State Warriors" = "GSW",
                                       "Houston Rockets" = "HOU",
                                       "Indiana Pacers" = "IND",
                                       "Los Angeles Clippers" = "LAC",
                                       "Los Angeles Lakers" = "LAL",
                                       "Memphis Grizzlies" = "MEM",
                                       "Miami Heat" = "MIA",
                                       "Milwaukee Bucks" = "MIL",
                                       "Minnesota Timberwolves" = "MIN",
                                       "New Orleans Pelicans" = "NOP",
                                       "New Orleans Hornets" = "NOH",
                                       "OKlahoma City Hornets" = "NOK",
                                       "New York Knicks" = "NYK",
                                       "Oklahoma City Thunder" = "OKC",
                                       "Seattle SuperSonics" = "SEA",
                                       "Orlando Magic" = "ORL",
                                       "Philidelphia 76ers" = "PHI",
                                       "Phoenix Suns" = "PHO",
                                       "Portland Trail Blazers" = "POR",
                                       "Sacramento Kings" = "SAC",
                                       "San Antonio Spurs" = "SAS",
                                       "Toronto Raptors" = "TOR",
                                       "Utah Jazz" = "UTA",
                                       "Washington Wizards" = "WAS",
                                       selectize = TRUE)),
                         #selectInput("STstat", "Select a statistic")
                         selectInput("STyear", "Select a year",
                                     choices = 2008:2018)
        ),
        
        conditionalPanel(condition="input.tabselected ==2",
          h3("Season Plot"),
          selectInput("SPstat", "Choose a Stat:",
                      c("Playoff Schedule" = "schedule",
                        "Stats Per Game" = "per_game_stats",
                        "Advanced Stats" = "advanced_stats",
                        "Team: Per Game Stats" = "team-stats-per_game",
                        "Shooting" = "team_shooting",
                        "Opponent: Per Game Stats" = "opponent-stats-per_game",
                        "Opponent: Shooting" = "opponent_shooting")),
          selectInput("SPYear", "Choose a Year:",
                      choices = 2008:2018),
          selectInput("STteam",
                      "Choose a team:", 
                      c("Atlanta Hawks" = "ATL", 
                        "Boston Celtics" = "BOS",
                        "Brooklyn Nets" = "BRK",
                        "New Jersey Nets" = "NJN",
                        "Charlotte Hornets" = "CHO",
                        "Charlotte Bobcats" = "CHA",
                        "Chicago Bulls" = "CHI",
                        "Cleveland Cavaliers", "CLE",
                        "Dallas Mavericks" = "DAL",
                        "Denver Nuggets" = "DEV",
                        "Detroit Pistons" = "DET",
                        "Golden State Warriors" = "GSW",
                        "Houston Rockets" = "HOU",
                        "Indiana Pacers" = "IND",
                        "Los Angeles Clippers" = "LAC",
                        "Los Angeles Lakers" = "LAL",
                        "Memphis Grizzlies" = "MEM",
                        "Miami Heat" = "MIA",
                        "Milwaukee Bucks" = "MIL",
                        "Minnesota Timberwolves" = "MIN",
                        "New Orleans Pelicans" = "NOP",
                        "New Orleans Hornets" = "NOH",
                        "OKlahoma City Hornets" = "NOK",
                        "New York Knicks" = "NYK",
                        "Oklahoma City Thunder" = "OKC",
                        "Seattle SuperSonics" = "SEA",
                        "Orlando Magic" = "ORL",
                        "Philidelphia 76ers" = "PHI",
                        "Phoenix Suns" = "PHO",
                        "Portland Trail Blazers" = "POR",
                        "Sacramento Kings" = "SAC",
                        "San Antonio Spurs" = "SAS",
                        "Toronto Raptors" = "TOR",
                        "Utah Jazz" = "UTA",
                        "Washington Wizards" = "WAS",
                        selectize = TRUE))
          ),
        
        
        conditionalPanel(condition="input.tabselected ==3",
          h3("Playoffs Table"),
          selectInput("PTyear", "Select a year",
                      choices = 2008:2018)
          ),
        
        conditionalPanel(condition="input.tabselected ==4",
          h3("Playoffs Plot"),
          selectInput("PPyear", "Select a year",
                      choices = 2008:2018),
          selectInput("PPstat", "Select a statistic",
                      choices = 1:10)
        
         ), 
        
        conditionalPanel(condition="input.tabselected ==5",
          h3("Matchups"),
          selectInput("Mteam1",
                      "First Team", 
                      c("Atlanta Hawks" = "ATL", 
                        "Boston Celtics" = "BOS",
                        "Brooklyn Nets" = "BRK",
                        "New Jersey Nets" = "NJN",
                        "Charlotte Hornets" = "CHO",
                        "Charlotte Bobcats" = "CHA",
                        "Chicago Bulls" = "CHI",
                        "Cleveland Cavaliers", "CLE",
                        "Dallas Mavericks" = "DAL",
                        "Denver Nuggets" = "DEV",
                        "Detroit Pistons" = "DET",
                        "Golden State Warriors" = "GSW",
                        "Houston Rockets" = "HOU",
                        "Indiana Pacers" = "IND",
                        "Los Angeles Clippers" = "LAC",
                        "Los Angeles Lakers" = "LAL",
                        "Memphis Grizzlies" = "MEM",
                        "Miami Heat" = "MIA",
                        "Milwaukee Bucks" = "MIL",
                        "Minnesota Timberwolves" = "MIN",
                        "New Orleans Pelicans" = "NOP",
                        "New Orleans Hornets" = "NOH",
                        "OKlahoma City Hornets" = "NOK",
                        "New York Knicks" = "NYK",
                        "Oklahoma City Thunder" = "OKC",
                        "Seattle SuperSonics" = "SEA",
                        "Orlando Magic" = "ORL",
                        "Philidelphia 76ers" = "PHI",
                        "Phoenix Suns" = "PHO",
                        "Portland Trail Blazers" = "POR",
                        "Sacramento Kings" = "SAC",
                        "San Antonio Spurs" = "SAS",
                        "Toronto Raptors" = "TOR",
                        "Utah Jazz" = "UTA",
                        "Washington Wizards" = "WAS",
                        selectize = TRUE)),
          selectInput("Mteam2",
                      "Second Team", 
                      c("Atlanta Hawks" = "ATL", 
                        "Boston Celtics" = "BOS",
                        "Brooklyn Nets" = "BRK",
                        "New Jersey Nets" = "NJN",
                        "Charlotte Hornets" = "CHO",
                        "Charlotte Bobcats" = "CHA",
                        "Chicago Bulls" = "CHI",
                        "Cleveland Cavaliers", "CLE",
                        "Dallas Mavericks" = "DAL",
                        "Denver Nuggets" = "DEV",
                        "Detroit Pistons" = "DET",
                        "Golden State Warriors" = "GSW",
                        "Houston Rockets" = "HOU",
                        "Indiana Pacers" = "IND",
                        "Los Angeles Clippers" = "LAC",
                        "Los Angeles Lakers" = "LAL",
                        "Memphis Grizzlies" = "MEM",
                        "Miami Heat" = "MIA",
                        "Milwaukee Bucks" = "MIL",
                        "Minnesota Timberwolves" = "MIN",
                        "New Orleans Pelicans" = "NOP",
                        "New Orleans Hornets" = "NOH",
                        "OKlahoma City Hornets" = "NOK",
                        "New York Knicks" = "NYK",
                        "Oklahoma City Thunder" = "OKC",
                        "Seattle SuperSonics" = "SEA",
                        "Orlando Magic" = "ORL",
                        "Philidelphia 76ers" = "PHI",
                        "Phoenix Suns" = "PHO",
                        "Portland Trail Blazers" = "POR",
                        "Sacramento Kings" = "SAC",
                        "San Antonio Spurs" = "SAS",
                        "Toronto Raptors" = "TOR",
                        "Utah Jazz" = "UTA",
                        "Washington Wizards" = "WAS",
                        selectize = TRUE)),
          numericInput("Mnumber", "Input a Number",
                       value=4)
        ),
        
        conditionalPanel(condition="input.tabselected ==6",
                         h3("Betting"),
                         textInput("Bdate", "Pick a date (YYYYMMDD")
        )
        
        ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
                    tabPanel("Season Table", 
                             value=1,
                             verbatimTextOutput("teamSummary")),
                    tabPanel("Season Plot",
                             value=2,
                             tableOutput("playoff")),
                    tabPanel("Playoff Table",
                             value=3,
                             tableOutput("odds"),
                             verbatimTextOutput("oddsum")),
                    tabPanel("Playoff Plot",
                             value=4,
                             tableOutput("playoff")),
                    tabPanel("Matchups",
                             value=5,
                             tableOutput("playoff")),
                    tabPanel("Betting",
                             value=6,
                             tableOutput("playoff")),
                    id = "tabselected"
        )
      )
))
      





# Define server logic required to draw a histogram
server <- function(input, output) {

   output$odds <- renderTable({
     odds <- DailyOdds(input$oddsDay)
     head(odds)
   })
   
   output$oddsum <- renderPrint({
     summary(odds)
   })
   
   output$playoff <- renderTable({
     playoff <- get.playoffs(input$playoffYear, input$playoffType, input$playoffTable)
     head(playoff)
   })
   
   output$teamSummary <- renderPrint({
     print("test")
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

