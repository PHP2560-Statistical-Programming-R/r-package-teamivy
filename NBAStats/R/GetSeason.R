#stores the url for the table that is being scraped
url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_"

get.season <- function(year){

  element <- "team-stats-per_game"

  urltable <- paste0(url, year, ".html&div=div_", element)

  playoffspage <- readLines(urltable)
  playoffs.tib <- as.tibble(playoffspage)
  temp.text <- playoffs.tib[104,] #This stores as a list
  table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F)
  table <- table[,-1]
   return(table)
}
