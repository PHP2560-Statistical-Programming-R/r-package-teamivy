###This model will calculate the win probabilities for the games on any given date.

DatesData <- read.csv("Dates To Scrape.csv")
DatesData$Date <- mdy(DatesData$Date)
URLData <- read.csv("Links to stats to scrape.csv")
URLData$URL <- paste0(URLData$URL, "?date=")
DatesData <- DatesData[1:10,]
URLData <- URLData[1:2,]

df.tib2 <- as.tibble()
for(i in 1:dim(URLData)[1]){
  url <- paste0(URLData$URL[i])
  url.df <- as.tibble()
  stats <- paste0(URLData$Stat[i])

  for(j in 1:dim(DatesData)[1]){
    url2 <- paste0(url, DatesData$Date[j])
    Lines <- readLines(url2)
    Lines.tib <- as.tibble(Lines)
    Lines.text <- Lines.tib[1739:2055,]
    schedule.team <- htmltab(Lines.text[[1]],  rm_nodata_cols = F)
    schedule.team <- schedule.team %>%
      mutate(Date = DatesData$Date[j]) %>%
      mutate(Stats = stats)

    names(schedule.team)[3] <- "YTD"
    names(schedule.team)[8] <- "LastYear"
    head(schedule.team, 2)
    url.df <- bind_rows(url.df, schedule.team)
  }

  df.tib <- bind_rows(df.tib, url.df)

}

        write.csv(df.tib, "statsonstats.csv")
