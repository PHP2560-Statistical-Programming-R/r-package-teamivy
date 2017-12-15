playoffs_url<-"https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fplayoffs%2FNBA_"

get.playoffs <- function(year, type, table) {
  if (type == "games") {
    urltable <- paste0(playoffs_url, year, "_", type, ".html&div=div_", table)
    playoffspage <- readLines(urltable)
    playoffs.tib <- as.tibble(playoffspage)
    temp.text <- playoffs.tib[104,] #This stores as a list
    table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F)
    names(table)[names(table) == "Visitor/Neutral"] <- "Away"
    names(table)[names(table) == "Home/Neutral"] <- "Home"
    names(table)[4] <- "PTS AWAY"
    names(table)[6] <- "PTS HOME"
    table <- table[,-7:-9]
  } else if ((type == "per_game") || (type == "advanced")) {
    urltable <- paste0(playoffs_url, year, "_", type, ".html&div=div_", table)
    playoffspage <- readLines(urltable)
    playoffs.tib <- as.tibble(playoffspage)
    temp.text <- playoffs.tib[104,] #This stores as a list
    table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F)
    table <- table %>% select(-Rk) %>% filter(Player != "Player")
  } else {
    urltable <- paste0(playoffs_url,year,".html&div=div_", table)
    playoffspage <- readLines(urltable)
    playoffs.tib <- as.tibble(playoffspage)
    temp.text <- playoffs.tib[104,] #This stores as a list
    table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F)
  }
  return(table)
}
