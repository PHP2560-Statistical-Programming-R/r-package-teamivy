###Abbreviations to their team names
AbbrevToNames <- function(Abb) {
  if(str_length(Abb) != 3){
    print("You must enter a three digit abbrevation for the team in string form")
  } else {
    Teams <- TeamsAbbrevs %>%
      mutate(Abbreviation = as.character(Abbreviation)) %>%
      mutate(Franchise = as.character(Franchise)) %>%
      filter(Abbreviation == Abb)
    return(Teams$Franchise[1])
  }
}
