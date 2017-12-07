#webscape standings for east or west conference and clean it.
standing_url<-"https://www.basketball-reference.com/leagues/NBA_"

webscrape_standings<-function(year,conference){
  url<-paste(standing_url,year,'_standings.html',sep="")
  table<-url %>% 
    read_html()%>% 
    html_nodes(paste0("#confs_standings_",conference)) %>% 
    html_table(header = T) %>%
    data.frame() %>%
    tbl_df()
  names(table)[1]<-"Team"
  table$Team<-gsub("[*(0-9)]","",table$Team)
  table$Team<-sub("\\s+$", "", table$Team)
  table$Team<-gsub(" ers"," 76ers", table$Team)
  return(table)
}