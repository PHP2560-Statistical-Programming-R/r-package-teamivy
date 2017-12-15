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
library(NBAStats)

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
                         #selectInput("STstat", "Select a statistic"),
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
                        selectize = TRUE)),
          h4("________________"),
          dateInput("SPmap","Plot all scheduled games by date",
                    value = "2017-12-18")
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
          h3("Get Previous Matchups"),
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
                         dateInput("Bdate", "Choose a date",
                                   value="2017-11-02"),
                         uiOutput("table")
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
                             plotOutput("usMap")),
                    tabPanel("Playoff Table",
                             value=3,
                             tableOutput("odds1"),
                             verbatimTextOutput("oddsum")),
                    tabPanel("Playoff Plot",
                             value=4,
                             tableOutput("playoff2")),
                    tabPanel("Matchups",
                             value=5,
                             tableOutput("match")),
                    tabPanel("Betting",
                             value=6,
                             tableOutput("odds"),
                             verbatimTextOutput("vsum")),
                    id = "tabselected"
        )
      )
))
      





# Define server logic required to draw a histogram
server <- function(input, output) {

   output$odds <- renderTable({
     odds <- DailyOdds(input$Bday)
     odds
   })
   
   output$oddsum <- renderPrint({
     summary(odds)
   })
   
   
   output$table <- renderUI({
     dat <- DailyOdds("20121205")
     colnames <- names(dat)
     
     # Create the checkboxes and select them all by default
     selectInput("vodd", "Choose variable", 
                        choices  = colnames)
   })
   
   output$vsum <- renderPrint({
     dat <- DailyOdds("20121205")
     dat <- dat %>%
       select(input$vodd) %>%
       na.omit()
     summary(dat)

   })
   
   output$usMap <- renderPlot({
     schedule_map(input$SPmap)
   })
   
   output$match <- renderTable({
     match <- GetLastMatchups(input$Mteam1, input$Mteam2, input$Mnumber)
     match
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

