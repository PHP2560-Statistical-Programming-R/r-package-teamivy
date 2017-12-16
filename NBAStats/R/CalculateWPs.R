###This model will calculate the win probabilities for the games on any given date.
TodaysDate <- lubridate::today()
DateDO <- format(TodaysDate, "%Y%m%d")

url.df <- as.tibble()

CalculateWPs <- function(DateWP = TodaysDate){

  print("This function is coming soon!!!")

#  for(i in 1:dim(StatsLinks)[1]){
#    url2 <- paste0(StatsLinks$URL[i], "?date=", DateWP)
#
#      stats <- paste0(StatsLinks$Stat[i])
#
#      Lines <- readLines(url2)
#      Lines.tib <- as.tibble(Lines)
#      Lines.text <- Lines.tib[1731:2055,]
#      schedule.team <- htmltab(Lines.text[[1]],  rm_nodata_cols = F)
#      schedule.team <- schedule.team %>%
#        mutate(Date = DateWP) %>%
#        mutate(Stats = stats)
#
#      names(schedule.team)[3] <- "YTD"
#      names(schedule.team)[8] <- "LastYear"
#      head(schedule.team, 2)
#      url.df <- bind_rows(url.df, schedule.team)
#
#  }
#
#  DailyOdds(DateWP = DateDO)
#

}

