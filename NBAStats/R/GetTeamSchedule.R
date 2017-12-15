##Team Schedule

GetTeamSchedule <- function(team, Year = "2018") {
  if(str_length(team) == 3){
  team <- paste(team)
  urltable <- paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fteams%2F",
                     team, "%2F", Year, "_games.html&div=div_games")
  thepage <-  readLines(urltable)
  #Convert the webpage into an easily subsettable tibble
  thepage.tib <- as.tibble(thepage)
  #Find the row including the data table
  temp.text <- thepage.tib[104,] #This stores as a list
  #Read the html code as a data table
  schedule.team <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F) #Call the data table element in   the list

  names(schedule.team)[3] <- "Time"
  names(schedule.team)[6] <- "Home"
  names(schedule.team)[8] <- "Result"
  names(schedule.team)[10] <- "TeamPoints"
  names(schedule.team)[11] <- "OppPoints"

  schedule.team <- schedule.team[, c(-1, -4, -5, -9, -15)]

  teamname <- AbbrevToNames(team)
  schedule.team <- schedule.team %>%
    mutate(Team = teamname)
  } else {
    team <- NamesToAbbrev(team)
    urltable <- paste0("https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fteams%2F",
                       team, "%2F", Year, "_games.html&div=div_games")
    thepage <-  readLines(urltable)
    #Convert the webpage into an easily subsettable tibble
    thepage.tib <- as.tibble(thepage)
    #Find the row including the data table
    temp.text <- thepage.tib[104,] #This stores as a list
    #Read the html code as a data table
    schedule.team <- htmltab(temp.text[[1]], which =1, rm_nodata_cols = F) #Call the data table element in   the list

    names(schedule.team)[3] <- "Time"
    names(schedule.team)[6] <- "Home"
    names(schedule.team)[8] <- "Result"
    names(schedule.team)[10] <- "TeamPoints"
    names(schedule.team)[11] <- "OppPoints"

    schedule.team <- schedule.team[, c(-1, -4, -5, -9, -15)]

    teamname <- AbbrevToNames(team)
    schedule.team <- schedule.team %>%
      mutate(Team = teamname)
      }

  schedule.team <- schedule.team %>%
    filter(Opponent != "Opponent") %>%
    mutate(Date = as.Date(Date, '%a, %b %d, %Y'),
           Home = ifelse(is.na(Home), 1, 0))
  schedule.team <- schedule.team[, c(11, 1,2,4,3,5,6,7,8,9,10)]
  return(schedule.team)
}

