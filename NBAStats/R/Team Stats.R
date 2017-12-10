

team_url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fteams%2F"
caphistoryurl <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fcontracts%2Fsalary-cap-history.html&div=div_salary_cap_history"
contract_url <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fcontracts%2F"

get.team <- function(team, year, element){
  if (element == "roster") {
    urltable <- paste0(team_url, team,"%2F", year, ".html&div=div_", element)
    table <- htmltab(urltable) %>%
      mutate(Team = team) %>%
      rename(Nationality = V1)
  } else if ((element == "per_game") || (element == "salaries2")) {
    urltable <- paste0(team_url, team,"%2F", year, ".html&div=div_", element)
    table <- htmltab(urltable) %>%
      mutate(Team = team) %>%
      rename(Player = V1)
    table <- table[,-1]
  } else if (element == "contracts") {
    urltable <- paste0(contract_url,team,".html&div=div_contracts")
    table <- htmltab(urltable)
  } else if (element == "salary_cap_history") {
    table <- htmltab("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fcontracts%2Fsalary-cap-history.html&div=div_salary_cap_history")
  }
  else {
    urltable <- paste0(team_url, team,"%2F", year, ".html&div=div_", element)
    table <- htmltab(urltable) %>%
      mutate(Team = team) %>%
      within(rm(Rk))
  }
  return(table)
}
