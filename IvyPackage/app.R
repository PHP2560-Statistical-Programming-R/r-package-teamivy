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
        h3("Team Stats"),
         selectInput("team",
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
         selectInput("teamtable", "Choose a Stat:",
                     c("Roster" = "roster",
                       "Advanced" = "advanced",
                       "Shooting" = "shooting",
                       "Play by Play" = "advanced_pbp",
                       "Salary" = "salaries2",
                       "Pay Roll" = "contracts",
                       "Team Pay Roll" = "div_salary_cap_history"
                       )),
          selectInput("teamyear", "Choose a Year:",
                      choices = 2008:2018),
        h3("Playoff Stats"),
          selectInput("playOffTable", "Choose a Stat:",
                      c("Playoff Schedule" = "div_schedule",
                        "Stats Per Game" = "div_per_game_stats",
                        "Advanced Stats" = "div_advanced_stats",
                        "Team: Per Game Stats" = "div_team-stats-per_game",
                        "Shooting" = "div_team_shooting",
                        "Opponent: Per Game Stats" = "div_opponent-stats-per_game",
                        "Opponent: Shooting" = "div_opponent_shooting")),
          selectInput("teamyear", "Choose a Year:",
                      choices = 2008:2018),
        h3("Season Stats"),
         selectInput("seasonTable", "Choose a Stat:",
                     c("Schedule & Results" = "div_schedule",
                       "Standings" = "confs_standing_E")),
         selectInput("seasonMonth", "Select a Month:",
                     choices = 1:12),
         selectInput("seasonYear", "Choose a Year:",
                     choices = 2008:2018),
        h3("Betting Odds")
      ), 
      
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Team Stats", tableOutput("teamSummary")),
                    tabPanel("Playoff Stats", dataTableOutput("Posts")),
                    tabPanel("Season Stats", dataTableOutput("Posts2")),
                    tabPanel("Betting Stats")
        )
      )
   )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  team_url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fteams%2F"
  caphistoryurl <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fcontracts%2Fsalary-cap-history.html&div=div_salary_cap_history"
  contract_url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fcontracts%2F"
  playoffs_url<- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fplayoffs%2FNBA_"

  
  get.team <- function(team, year, element){
    if (element == "roster") {
      urltable <- (paste0(team_url, team,"%2F", year, ".html&div=div_", element))
      table <- htmltab(urltable) %>% 
        mutate(Team = team) %>% 
        rename(Nationality = V1) 
    } else if ((element == "per_game") || (element == "salaries2")) {
      urltable <- (paste0(team_url, team,"%2F", year, ".html&div=div_", element))
      table <- htmltab(urltable) %>% 
        mutate(Team = team) %>% 
        rename(Player = V1)
      table <- table[,-1]
    } else if (element == "contracts") {
      urltable <- (paste0(contract_url,team,".html&div=div_contracts"))
      table <- htmltab(urltable)
    } else if (element == "salary_cap_history") {
      table <- htmltab("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fcontracts%2Fsalary-cap-history.html&div=div_salary_cap_history")
    }
    else {
      urltable <- (paste0(team_url, team,"%2F", year, ".html&div=div_", element))
      table <- htmltab(urltable) %>% 
        mutate(Team = team) %>% 
        within(rm(Rk))
    }
    return(table)
  }
  dataInput <- reactive({
    data <- get.team(input$team, input$teamyear, input$teamtable)
  })
   output$teamSummary <- renderTable({
     head(data)
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

