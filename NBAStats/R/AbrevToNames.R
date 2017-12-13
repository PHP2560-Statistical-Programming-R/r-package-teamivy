###Team Names to their Abbreviation
AbrevToNames <- function(Abb) {
  if(str_length(Abb) != 3){
    print("You must enter a three digit abbrevation for the team in string form")
  } else {
    Teams <- read.table("NBA Teams and Their Abbreviation.csv") %>%
      filter(Abbreviation == Abb)
    return(Teams$Franchise)
  }
}

AbrevToNames("ATL")

Teams <- read.table("NBA Teams and Their Abbreviation.csv")
Teams <- system.file("data", "NBA Teams and Their Abbreviation.csv", package = "NBAStats")
