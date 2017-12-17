
#creates a vector for that contains the individual years for the past 10 years
year.vect <- c("2018" ,"2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008")

#creates a function that calls upon the get.season function in order to average the data from the last 10 seasons
LeagueStats <- function(variable){
    l <- enquo(variable)
    df.out <- as.tibble()
    for(i in 1:10){
      Season <- get.season(year.vect[i]) %>%
        mutate(Varss = as.numeric(as.character(!!l))) %>% 
        summarise(Avg = mean(Varss)) %>%
        mutate(Year = year.vect[i])
      df.out <- bind_rows(df.out, Season)
    }
  return(df.out)
}
