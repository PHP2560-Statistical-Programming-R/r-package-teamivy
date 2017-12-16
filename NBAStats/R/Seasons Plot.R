stat_plot <- function(year, table_code , col_name){
  if (table_code == 13 || table_code == 17) {
    table <- TableCode(table_code)
  } else {
    print("Please enter a valid numeric value for playoffs table code")
    break
  }

  table.data <- get.playoffs(year,table_code)
  per <- table.data %>%
    drop_na() %>%
    select(Team, col_name)
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
