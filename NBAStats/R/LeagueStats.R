year.vect <- c("2018" ,"2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008")

LeagueStats <- function(variable){
    l <- enquo(variable)
    df.out <- as.tibble()
    for(i in 1:10){
      Season <- get.season(year.vect[i], "team-stats-per_game") %>%
        mutate(Varss = as.numeric(as.character(!!l))) %>%
        summarise(Avg = mean(Varss)) %>%
        mutate(Year = year.vect[i])
      df.out <- bind_rows(df.out, Season)
    }
  return(df.out)
}
