###Team Names to their Abbreviation
NamesToAbbrev <- function(Name) {
  if(str_length(Name) == 3){
    print("You must enter the full franchise name with nickname not the abbreviation")
  } else {
    Teams <- TeamsAbbrevs %>%
      mutate(Abbreviation = as.character(Abbreviation)) %>%
      mutate(Franchise = as.character(Franchise)) %>%
      filter(Franchise == Name)
    return(Teams$Abbreviation[1])
  }
}

