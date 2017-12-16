season_plot <- function(year, col_name){

  table <- get.season(year)
  table <- table %>%
    select(Team, col_name)
  table[,2]<- as.numeric(as.character(table[,2]))

  plot <- ggplot(table, aes(x= Team, y = table[,2])) +
    geom_point() +
    xlab("Team") +
    ylab(names(table)[[2]]) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(aes(yintercept=mean(table[,2], na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1)
  return(plot)
}
