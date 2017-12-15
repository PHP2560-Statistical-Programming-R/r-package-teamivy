##This function strips the daily betting odds
DailyOdds <- function(BettingDate2 = "2017-12-15"){
  BettingDate2 <- as.Date(BettingDate2)
  BettingDate <- format(BettingDate2, "%Y%m%d")

  if(ymd(BettingDate) > today() +1) {
    print("Sorry, the betting lines are not set yet! Lines are only released for today and the following day. Check back again!")
  } else {
    ##Spreads Tables
    url.spread <- paste0("http://www.donbest.com/nba/odds/spreads/", BettingDate, ".html")

    SpreadsTable <- url.spread %>%
      read_html %>%
      html_nodes(xpath = '//*[@id="oddsHolder"]/div[1]/table') %>%
      html_table(header = F, fill =T) %>%
      data.frame() %>%
      tbl_df()

    SpreadsTable <- SpreadsTable[-1, ]

    names(SpreadsTable) <- SpreadsTable[1,]
    SpreadsTable <- SpreadsTable[-1, ]

    SpreadsTable <- SpreadsTable %>%
      select(Rot, Opener, Team, Time, Bovada, Pinnacle, Mirage)

    #Rot works
    SpreadsTable <- SpreadsTable %>%
      mutate(Rot1 = as.character(Rot),
             Rot2 = as.character(Rot))

    SpreadsTable$Rot1 <- str_sub(SpreadsTable$Rot1, 1, 3)
    SpreadsTable$Rot2 <- str_sub(SpreadsTable$Rot2, 4, 6)

    #We need to create a regex pattern
    SpreadsTable <- SpreadsTable %>%
      mutate(OS = Opener)

    #Opening Spread
    pattern <- str_c("[+-]", "[:digit:]+", ".?", "[:digit:]*","[\\s]")
    SpreadsTable$OSAway <- str_match(SpreadsTable$OS, pattern)
    SpreadsTable$OSAway <- sub("\\s+", "", SpreadsTable$OSAway)

    SpreadsTable$Bovada <- str_match(SpreadsTable$Bovada, pattern)
    SpreadsTable$Bovada <- sub("\\s+", "", SpreadsTable$Bovada)

    SpreadsTable$Pinnacle <- str_match(SpreadsTable$Pinnacle, pattern)
    SpreadsTable$Pinnacle <- sub("\\s+", "", SpreadsTable$Pinnacle)

    SpreadsTable$Mirage <- str_match(SpreadsTable$Mirage, pattern)
    SpreadsTable$Mirage <- sub("\\s+", "", SpreadsTable$Mirage)

    #Teams

    #Team Away City
    patternteam <- str_c("((^[:upper:][:lower:]+)[:blank:][:upper:][:lower:]+[:blank:])|((^[:upper:][:lower:]+)[:blank:])")
    SpreadsTable$TeamAwayCity <- str_match(SpreadsTable$Team, patternteam)
    SpreadsTable$Team <- str_replace(SpreadsTable$Team, patternteam, "")

    #Team Away
    patternteam2 <- str_c("(^[:upper:][:lower:]+|76ers)")
    SpreadsTable$TeamAway <- str_match(SpreadsTable$Team, patternteam2)
    SpreadsTable$Team <- str_replace(SpreadsTable$Team, patternteam2, "")

    #Put the city and nickname together
    SpreadsTable <- as.data.frame(SpreadsTable)
    SpreadsTable$Bovada <- SpreadsTable$Bovada[,1]
    SpreadsTable$Mirage <- SpreadsTable$Mirage[,1]
    SpreadsTable$Pinnacle <- SpreadsTable$Pinnacle[,1]
    SpreadsTable$OSAway <- SpreadsTable$OSAway[,1]
    SpreadsTable$TeamAwayCity <- SpreadsTable$TeamAwayCity[,1]
    SpreadsTable$TeamAway <- SpreadsTable$TeamAway[,1]

    SpreadsTable <- SpreadsTable %>%
      mutate(Away = paste0(TeamAwayCity, TeamAway))

    SpreadsTable <- SpreadsTable %>%
      select(Team, Time, Bovada, Pinnacle, Mirage, Rot1, Rot2, OSAway, Away)

    names(SpreadsTable)[1] <- "Home"
    names(SpreadsTable)[6] <- "RotAway"
    names(SpreadsTable)[7] <- "RotHome"

    #MoneyLine Table
    url.ML <- paste0("http://www.donbest.com/nba/odds/money-lines/", BettingDate, ".html")

    MoneyLineTable <- url.ML %>%
      read_html %>%
      html_nodes(xpath = '//*[@id="oddsHolder"]/div[1]/table') %>%
      html_table(header = F, fill =T) %>%
      data.frame() %>%
      tbl_df()

    MoneyLineTable <- MoneyLineTable[-1, ]

    names(MoneyLineTable) <- MoneyLineTable[1,]
    MoneyLineTable <- MoneyLineTable[-1, ]

    MoneyLineTable <- MoneyLineTable %>%
      select(Rot, Opener, Team, Time, Bovada, Pinnacle, Mirage)

    #Rot works
    MoneyLineTable <- MoneyLineTable %>%
      mutate(Rot1 = as.character(Rot),
             Rot2 = as.character(Rot))

    MoneyLineTable$Rot1 <- str_sub(MoneyLineTable$Rot1, 1, 3)
    MoneyLineTable$Rot2 <- str_sub(MoneyLineTable$Rot2, 4, 6)

    #Money Line Away
    pattern <- str_c("^[+-]", "[:digit:]+")
    MoneyLineTable$OpenerAwayML <- str_match(MoneyLineTable$Opener, pattern)
    MoneyLineTable$BovadaAwayML <- str_match(MoneyLineTable$Bovada, pattern)
    MoneyLineTable$PinnacleAwayML <- str_match(MoneyLineTable$Pinnacle, pattern)
    MoneyLineTable$MirageAwayML <- str_match(MoneyLineTable$Mirage, pattern)

    #Money Line Home
    pattern <- str_c("[+-]", "[:digit:]+$")
    MoneyLineTable$OpenerHomeML <- str_match(MoneyLineTable$Opener, pattern)
    MoneyLineTable$BovadaHomeML <- str_match(MoneyLineTable$Bovada, pattern)
    MoneyLineTable$PinnacleHomeML <- str_match(MoneyLineTable$Pinnacle, pattern)
    MoneyLineTable$MirageHomeML <- str_match(MoneyLineTable$Mirage, pattern)

    #Teams
    #Team Away City
    patternteam <- str_c("((^[:upper:][:lower:]+)[:blank:][:upper:][:lower:]+[:blank:])|((^[:upper:][:lower:]+)[:blank:])")
    MoneyLineTable$TeamAwayCity <- str_match(MoneyLineTable$Team, patternteam)
    MoneyLineTable$Team <- str_replace(MoneyLineTable$Team, patternteam, "")

    #Team Away
    patternteam2 <- str_c("(^[:upper:][:lower:]+|76ers)")
    MoneyLineTable$TeamAway <- str_match(MoneyLineTable$Team, patternteam2)
    MoneyLineTable$Team <- str_replace(MoneyLineTable$Team, patternteam2, "")

    #Put the city and nickname together
    MoneyLineTable <- as.data.frame(MoneyLineTable)

    #Weird Structure

    MoneyLineTable$BovadaAwayML <- MoneyLineTable$BovadaAwayML[,1]
    MoneyLineTable$MirageAwayML <- MoneyLineTable$MirageAwayML[,1]
    MoneyLineTable$PinnacleAwayML <- MoneyLineTable$PinnacleAwayML[,1]
    MoneyLineTable$OpenerAwayML <- MoneyLineTable$OpenerAwayML[,1]
    MoneyLineTable$OpenerHomeML <- MoneyLineTable$OpenerHomeML[,1]
    MoneyLineTable$PinnacleHomeML <- MoneyLineTable$PinnacleHomeML[,1]
    MoneyLineTable$MirageHomeML <- MoneyLineTable$MirageHomeML[,1]
    MoneyLineTable$BovadaHomeML <- MoneyLineTable$BovadaHomeML[,1]

    #Team
    MoneyLineTable$TeamAwayCity <- MoneyLineTable$TeamAwayCity[,1]
    MoneyLineTable$TeamAway <- MoneyLineTable$TeamAway[,1]

    MoneyLineTable <- MoneyLineTable %>%
      mutate(Away = paste0(TeamAwayCity, TeamAway))

    MoneyLineTable <- MoneyLineTable %>%
      select(-Rot, -Opener, -Bovada, - Pinnacle, - Mirage, - TeamAwayCity, -TeamAway)

    names(MoneyLineTable)[1] <- "Home"
    names(MoneyLineTable)[3] <- "RotAway"
    names(MoneyLineTable)[4] <- "RotHome"

    url.OU <- paste0("http://www.donbest.com/nba/odds/totals/", BettingDate, ".html")

    OverUnderTable <- url.OU %>%
      read_html %>%
      html_nodes(xpath = '//*[@id="oddsHolder"]/div[1]/table') %>%
      html_table(header = F, fill =T) %>%
      data.frame() %>%
      tbl_df()

    OverUnderTable <- OverUnderTable[-1, ]

    names(OverUnderTable) <- OverUnderTable[1,]
    OverUnderTable <- OverUnderTable[-1, ]

    OverUnderTable <- OverUnderTable %>%
      select(Rot, Opener, Team, Time, Bovada, Pinnacle, Mirage)

    #Rot works
    OverUnderTable <- OverUnderTable %>%
      mutate(Rot1 = as.character(Rot),
             Rot2 = as.character(Rot))

    OverUnderTable$Rot1 <- str_sub(OverUnderTable$Rot1, 1, 3)
    OverUnderTable$Rot2 <- str_sub(OverUnderTable$Rot2, 4, 6)

    #OverUnder
    pattern <- str_c("[:digit:]{3}\\.[:digit:]")
    OverUnderTable$OpenerOU <- str_match(OverUnderTable$Opener, pattern)
    OverUnderTable$BovadaOU <- str_match(OverUnderTable$Bovada, pattern)
    OverUnderTable$PinnacleOU <- str_match(OverUnderTable$Pinnacle, pattern)
    OverUnderTable$MirageOU <- str_match(OverUnderTable$Mirage, pattern)

    #Teams
    #Team Away City
    patternteam <- str_c("((^[:upper:][:lower:]+)[:blank:][:upper:][:lower:]+[:blank:])|((^[:upper:][:lower:]+)[:blank:])")
    OverUnderTable$TeamAwayCity <- str_match(OverUnderTable$Team, patternteam)
    OverUnderTable$Team <- str_replace(OverUnderTable$Team, patternteam, "")

    #Team Away
    patternteam2 <- str_c("(^[:upper:][:lower:]+|76ers)")
    OverUnderTable$TeamAway <- str_match(OverUnderTable$Team, patternteam2)
    OverUnderTable$Team <- str_replace(OverUnderTable$Team, patternteam2, "")

    #Put the city and nickname together
    OverUnderTable <- as.data.frame(OverUnderTable)
    #Weird Structure

    OverUnderTable$BovadaOU <- OverUnderTable$BovadaOU[,1]
    OverUnderTable$MirageOU <- OverUnderTable$MirageOU[,1]
    OverUnderTable$PinnacleOU <- OverUnderTable$PinnacleOU[,1]
    OverUnderTable$OpenerOU <- OverUnderTable$OpenerOU[,1]

    #Team
    OverUnderTable$TeamAwayCity <- OverUnderTable$TeamAwayCity[,1]
    OverUnderTable$TeamAway <- OverUnderTable$TeamAway[,1]

    OverUnderTable <- OverUnderTable %>%
      mutate(Away = paste0(TeamAwayCity, TeamAway))

    OverUnderTable <- OverUnderTable %>%
      select(-Rot, -Opener, -Bovada, - Pinnacle, - Mirage, - TeamAwayCity, -TeamAway)

    names(OverUnderTable)[1] <- "Home"
    names(OverUnderTable)[3] <- "RotAway"
    names(OverUnderTable)[4] <- "RotHome"

    ##Join the tables together

    BettingTable <- left_join(SpreadsTable, MoneyLineTable, by = c("Home", "Away", "Time", "RotAway", "RotHome")) %>%
      left_join(OverUnderTable, by = c("Home", "Away", "Time", "RotAway", "RotHome"))
    head(BettingTable)
    BettingTable <- BettingTable[, c(1, 9, 2:8, 10:21)]

    names(BettingTable)[4] <- "BovadaAwayS"
    names(BettingTable)[5] <- "PinnacleAwayS"
    names(BettingTable)[6] <- "MirageAwayS"
    names(BettingTable)[9] <- "OpenerAwayS"

    BettingTable <- BettingTable %>%
      mutate(Time = hm(Time))

    for(i in 4:length(BettingTable)){
      BettingTable[,i] <- as.numeric(BettingTable[,i])
    }

    #Create a win probability from the Bovada money line
    BettingTable <- BettingTable %>%
      mutate(WP.HB = ifelse(PinnacleHomeML > 0, 100/(PinnacleHomeML + 100),
                            PinnacleHomeML/(PinnacleHomeML -100)),
             WP.VB = ifelse(PinnacleAwayML > 0, 100/(PinnacleAwayML + 100),
                            PinnacleAwayML/(PinnacleAwayML -100)),
             WP.H = (WP.HB + (1-WP.VB))/2,
             WP.V = 1 - WP.H)  %>%
      select(-WP.HB, -WP.VB) %>%
      mutate(Date = lubridate::ymd(BettingDate))
    return(BettingTable)
  }

}



