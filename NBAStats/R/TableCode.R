TableCode <- function(element) {
  if(element > 18 || element < 1){
    print("You must enter a valid table code in numeric form")
  } else {
    tbl <- Glossary %>%
      mutate(Table_Code = as.numeric(Table_Code)) %>%
      mutate(CSS_Selector = as.character(CSS_Selector)) %>%
      filter(Table_Code == element)
    return(tbl$CSS_Selector[1])
  }
}
