
team_url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fteams%2F"
caphistoryurl <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fcontracts%2Fsalary-cap-history.html&div=div_salary_cap_history"
contract_url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fcontracts%2F"

get.team <- function(team, year, table_code){

  if (table_code < 4 || table_code > 11) {
    print("Please enter a valid numeric value for teams table code")
    break
    } else {
    element <- TableCode(table_code)
    }

    if (element == "roster") {
      urltable <- paste0(team_url, team,"%2F", year, ".html&div=div_", element)
      playoffspage <- readLines(urltable)
      playoffs.tib <- as.tibble(playoffspage)
      temp.text <- playoffs.tib[104,] #This stores as a list
      table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F) %>%
        mutate(Team = team) %>%
        rename(Nationality = V1)
    } else if ((element == "per_game") || (element == "salaries2")) {
      urltable <- paste0(team_url, team,"%2F", year, ".html&div=div_", element)
      playoffspage <- readLines(urltable)
      playoffs.tib <- as.tibble(playoffspage)
      temp.text <- playoffs.tib[104,] #This stores as a list
      table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F) %>%
        mutate(Team = team) %>%
        rename(Player = V1)
      table <- table[,-1]
    } else if (element == "contracts") {
      urltable <- paste0(contract_url,team,".html&div=div_contracts")
      playoffspage <- readLines(urltable)
      playoffs.tib <- as.tibble(playoffspage)
      temp.text <- playoffs.tib[104,] #This stores as a list
      table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F)
    } else if (element == "salary_cap_history") {
      urltable <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fcontracts%2Fsalary-cap-history.html&div=div_salary_cap_history"
      playoffspage <- readLines(urltable)
      playoffs.tib <- as.tibble(playoffspage)
      temp.text <- playoffs.tib[104,] #This stores as a list
      table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F)
    }
    else {
      urltable <- paste0(team_url, team,"%2F", year, ".html&div=div_", element)
      playoffspage <- readLines(urltable)
      playoffs.tib <- as.tibble(playoffspage)
      temp.text <- playoffs.tib[104,] #This stores as a list
      table <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F) %>%
        mutate(Team = team) %>%
        within(rm(Rk))
    }
    return(table)
}
