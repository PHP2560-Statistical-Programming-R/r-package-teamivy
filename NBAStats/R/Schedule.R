#schedule

schedule_url<-"https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_"

#schedule
schedule_table<-function(year){
  month=c("october","november","december","january","february",
          "march","april","may","june")
  result=NA
  for(a in 1:9){
    url<-paste0(schedule_url,year,"_games-",month[a],".html&div=div_schedule")
    table<-htmltab(url)
    result=rbind(result,table)}
  result<-result %>% 
    drop_na() %>%
    filter(Date !="Playoffs")
  names(result)[3]<-"Visitor"
  names(result)[4]<-"PTSV"
  names(result)[5]<-"Home"
  names(result)[6]<-"PTSH"
  result$Date<-as.Date(result$Date, '%a, %b %d, %Y')
  result$PTSH<-as.numeric(result$PTSH)
  result$PTSV<-as.numeric(result$PTSV)
  return(result)
}
