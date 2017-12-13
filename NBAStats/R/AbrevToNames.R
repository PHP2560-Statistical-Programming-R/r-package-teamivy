###Abbreviations to their team names
AbrevToNames <- function(Abb) {
  if(str_length(Abb) != 3){
    print("You must enter a three digit abbrevation for the team in string form")
  } else {
    Teams <- read.csv("NBA Teams and Their Abbreviation.csv") %>%
      mutate(Abbreviation = as.character(Abbreviation)) %>%
      mutate(Franchise = as.character(Franchise)) %>%
      filter(Abbreviation == Abb)
    return(Teams$Franchise[1])
  }
}
