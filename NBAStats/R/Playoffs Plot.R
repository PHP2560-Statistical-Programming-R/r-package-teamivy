stat_plot <- function(year, table_code , variable){
  if (table_code < 12 || table_code >18) {
    print("Please enter a valid numeric value for playoffs table code")
    break
  } else {
    table <- TableCode(table_code)
  }

  table <- get.playoffs(year,table)
  per <- table %>%
    drop_na() %>%
    select(Team,col_name)
  per[,2]<- as.numeric(as.character(per[,2]))

  plot <- ggplot(per, aes(x= Team, y = per[,2])) +
    geom_point() +
    xlab("Team") +
    ylab(names(per)[[2]]) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(aes(yintercept=mean(per[,2], na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1)
  return(plot)
}
