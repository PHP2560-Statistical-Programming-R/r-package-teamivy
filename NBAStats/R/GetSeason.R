url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_"
year <- "2018"
element <- "team-stats-per_game"

get.season <- function(year, table_code){
  if (table_code > 4 || table_code < 1) {
    print("Please enter a valid numeric value for league table code")
    break
  } else {
    element <- TableCode(table_code)
  }

  urltable <- paste0(url, year, ".html&div=div_", element)

  playoffspage <- readLines(urltable)
  playoffs.tib <- as.tibble(playoffspage)
  temp.text <- playoffs.tib[104,] #This stores as a list
  table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F)
  table <- table[,-1]
   return(table)
}
