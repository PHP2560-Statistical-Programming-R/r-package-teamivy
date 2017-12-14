###Win Probability Model
TodaysGames <- DailyOdds("20171214")
df <- as.tibble()
for(i in 1:dim(TodaysGames)[1]){
  df1 <- GetTeamSchedule(TodaysGames$Home[i]) %>%
    filter(Date < ymd("20171214")) %>%
    arrange(desc(Date)) %>%
    tail(5)
  df1 <- df1 %>%
    summarise(PlusMinus = sum(TeamPoints) - sum(OppPoints),
              WinPerc = as.numeric(W)/(as.numeric(W) + as.numeric(L))
  df <- bind_rows(df, df1 )
}

