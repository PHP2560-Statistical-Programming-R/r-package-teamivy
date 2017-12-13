#schedule

schedule_url<-"https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_"

#schedule
schedule_table<-function(year){
  if(year>2018){
    print("The season doesn't start")
  }else if(year<2001){
    print("The season isn't included")
  }else{
    if(year==2012){
      month=c("december","january","february",
              "march","april","may","june")
    }else{
      month=c("october","november","december","january","february",
              "march","april","may","june")
      result=NA
      for(a in 1:length(month)){
        url<-readLines(paste0(schedule_url,year,"_games-",month[a],
                              ".html&div=div_schedule"))
        thepage.tib<-as_tibble(url)
        temp.text<-thepage.tib[104,]
        print(temp.text)
        table<-htmltab(temp.text[[1]], which=1,rm_nodata_cols = F)
        result=rbind(result,table)}
      names(result)[3]<-"Visitor"
      names(result)[4]<-"PTSV"
      names(result)[5]<-"Home"
      names(result)[6]<-"PTSH"
      names(result)[7]<-"Box"
      names(result)[8]<-"OT"
      result<-result %>% 
        filter(Date !="Playoffs")
      result$Date<-as.Date(result$Date, '%a, %b %d, %Y')
      return(result)
    }
  }
}