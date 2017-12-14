#webscape standings for east or west conference and clean it.
standing_url<-"https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_"

#webscape standings for east or west conference and clean it.
webscrape_standings<-function(year){
  url<-readLines(paste0(standing_url,year,
                        '_standings.html&div=div_expanded_standings'))
  thepage.tib<-as_tibble(url)
  temp.text<-thepage.tib[104,]
  table<-htmltab(temp.text[[1]], which=1,rm_nodata_cols = F)
  table<-table[-1]
  #  names(table)[1]<-"Team"
  #  table$Team<-gsub("[*(0-9)]","",table$Team)
  #  table$Team<-sub("\\s+$", "", table$Team)
  #  table$Team<-gsub(" ers"," 76ers", table$Team)
  return(table)
}