

playoffs_url<-"https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fplayoffs%2FNBA_"

get.playoffs <- function(year, type, table) {
  if (type == "games") {
    urltable <- paste0(playoffs_url, year, "_", type, ".html&div=div_", table)
    table <- htmltab(urltable)
    names(table)[names(table) == "Visitor/Neutral"] <- "Away"
    names(table)[names(table) == "Home/Neutral"] <- "Home"
    names(table)[names(table) == "PTS"] <- "PTS Away"
    names(table)[names(table) == "PTS.1"] <- "PTS Home"
    table <- table[,-7]
  } else if ((type == "per_game") || (type == "advanced")) {
    urltable <- paste0(playoffs_url, year, "_", type, ".html&div=div_", table)
    table <- htmltab(urltable)
    table <- table[,-1]
  } else {
    urltable <- paste0(playoffs_url,year,".html&div=div_", table)
    table <- htmltab(urltable)
    table <- table[,-1]
  }
  return(table)
}
