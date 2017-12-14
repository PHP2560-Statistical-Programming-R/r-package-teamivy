##This function does not include playoff games

GetLastMatchups <- function(team, Team2, number = 5){
  Matchups <- bind_rows(GetTeamSchedule(team),
                        GetTeamSchedule(team, Year = "2017"),
                        GetTeamSchedule(team, Year = "2016"))
  if(str_length(Team2) == 3){
    Team2 <- AbbrevToNames(Team2)
  } else {
    Team2 <- Team2
  }
  Matchups <- Matchups %>%
    filter(Opponent == Team2 & !is.na(Result)) %>%
    arrange(desc(Date)) %>%
    tail(number)
  return(Matchups)
}

