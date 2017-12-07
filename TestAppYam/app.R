#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvest)
library(pipeR)
library(knitr)
library(xml2)
library(htmltab)
library(readxl)
library(tidyr)
library(lubridate)
library(dplyr)
library(stringr)

# Define UI for application that draws a histogram

   # Application title
   
  
   ui <- basicPage(
     titlePanel("Testing Data Tables"),
     AdvancedPBP <- htmltab("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fteams%2FBOS%2F2018.html&div=div_advanced"),
     AdvancedPBP <- AdvancedPBP %>% 
       mutate(Team = "BOS") %>% 
       rename(Player = V1) %>% 
       dplyr::select(-Rk),
     h2("The mtcars data"),
     
     DT::dataTableOutput("mytable")
   )
   
   server <- function(input, output) {
     output$mytable = DT::renderDataTable({
       mtcars
     })
   }
   server <- function(i)
   
   
   head(AdvancedPBP)
   
   dataTableOutput(renderDataTable())
   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#      sidebarPanel(
#         sliderInput("bins",
#                     "Number of bins:",
#                     min = 1,
#                     max = 50,
#                     value = 30)
#      ),
#      
#      # Show a plot of the generated distribution
#      mainPanel(
#         plotOutput("distPlot")
#      )
#   )
#)
#
## Define server logic required to draw a histogram
#server <- function(input, output) {
#   
#   output$distPlot <- renderPlot({
#      # generate bins based on input$bins from ui.R
#      x    <- faithful[, 2] 
#      bins <- seq(min(x), max(x), length.out = input$bins + 1)
#      
#      # draw the histogram with the specified number of bins
#      hist(x, breaks = bins, col = 'darkgray', border = 'white')
#   })


)

# Run the application 
shinyApp(ui = ui, server = server)

