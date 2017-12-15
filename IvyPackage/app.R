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
                         h3("Team Schedule"),
                         selectInput("TSteam",
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
                         selectInput("TSyear", "Select a year",
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
          h3("Standings"),
          selectInput("sYear", "Select a year",
                      choices = 2008:2018)
          ),
        
        conditionalPanel(condition="input.tabselected ==4",
          h3("Playoffs Plot"),
          selectInput("PPyear", "Select a year",
                      choices = 2008:2018),
          selectInput("PPtable", "Choose a table",
                      c(
                        "Team Stats Per Game" = 13,
                        "Team Shooting" = 15
                      )),
          selectInput("PPstat", "Select a statistic",
                      c("Games Played" = "G",
                        "Minutes Played" = "MP",
                        "Field Goals" = "FG",
                        "Field Goals Attempted" = "FGA",
                        "Field Goal Percentage" = "FG%",
                        "3-Point Field Goals" = "3P",
                        "3-Point Field Goal Attempted" = "3PA",
                        "3-Point Field Goal Percentage" = "3P%",
                        "2-Point Field Goals Made" = "2P",
                        "2-Point Field Goal Attemped" = "2PA",
                        "2-Point Field Goal Percentage" = "2P%",
                        "Free Throws Made" = "FT",
                        "Free Throws Attempted" = "FTA",
                        "Free Throw Percentage" = "FT%",
                        "Offensive Rebounds" = "ORB",
                        "Defensive Rebounds" = "DRB",
                        "Total Rebounds" = "TRB",
                        "Assists" = "AST",
                        "Steals" = "STL",
                        "Blocks" = "BLK",
                        "Turnovers" = "TOV",
                        "Personal Fouls" = "PF",
                        "Points" = "PTS"))
        
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
                                   value="2017-11-02")
        )
        
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
                    tabPanel("Team Schedule", 
                             value=1,
                             tableOutput("tSchedule")),
                    tabPanel("Season Plot",
                             value=2,
                             plotOutput("usMap")),
                    tabPanel("Standings",
                             value=3,
                             tableOutput("standings")),
                    tabPanel("Playoff Plot",
                             value=4,
                             plotOutput("plot")),
                    tabPanel("Matchups",
                             value=5,
                             tableOutput("match")),
                    tabPanel("Betting",
                             value=6,
                             tableOutput("odds")),
                    id = "tabselected"
        )
      )
))
      





# Define server logic required to draw a histogram
server <- function(input, output) {

  #Betting Odds 
   output$odds <- renderTable({
     odds <- DailyOdds(input$Bday)
     odds
   })
   
   output$plot <- renderPlot({
     stat_plot(input$PPyear, input$PPtable, input$PPstat)

   })

   
   
   #standings
   output$standings <- renderTable({
     standings <- webscrape_standings(input$sYear)
     head(standings, n=15)
   })
   
   #second piece of playoff plot, game location  
   output$usMap <- renderPlot({
     schedule_map(input$SPmap)
   })
   
   #matchups 
   output$match <- renderTable({
     match <- GetLastMatchups(input$Mteam1, input$Mteam2, input$Mnumber)
     match
   })
   
   #team schedule 
   output$tSchedule <- renderTable({
     team <- GetTeamSchedule.shiny(input$TSteam,input$TSyear)
     head(team)
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

