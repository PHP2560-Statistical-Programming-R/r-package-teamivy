###Team Names to their Abbreviation
NamesToAbrev <- function(Name) {
  if(str_length(Name) == 3){
    print("You must enter the full franchise name with nickname now the abbreviation")
  } else {
    Teams <- read.csv("NBA Teams and Their Abbreviation.csv") %>%
      mutate(Abbreviation = as.character(Abbreviation)) %>%
      mutate(Franchise = as.character(Franchise)) %>%
      filter(Franchise == Name)
    return(Teams$Abbreviation[1])
  }
}

