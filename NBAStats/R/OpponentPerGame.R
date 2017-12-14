#opponent per game stats
opsg_table <- function(year){
  url<-readLines(paste0(opgs_url,year,".html&div=div_opponent-stats-per_game"))
  thepage.tib <- as_tibble(url) 
  #Find the row including the data table
  temp.text <- thepage.tib[104,] #This stores as a list
  #Read the html code as a data table
  table <- htmltab(temp.text[[1]], which =1 ) #Call the data table element 
  return(table)
}