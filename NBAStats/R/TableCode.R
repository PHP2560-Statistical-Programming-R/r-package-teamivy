
#Creates function TableCode
TableCode <- function(element) {
  if(element > 18 || element < 1){ #If statement to check for valid table code entry
    print("You must enter a valid table code in numeric form")
  } else {
    tbl <- Glossary %>% 
      mutate(Table_Code = as.numeric(Table_Code)) %>%
      mutate(CSS_Selector = as.character(CSS_Selector)) %>%
      filter(Table_Code == element) #filters the glossary for when the column Table_Code is the same as the element entered
    return(tbl$CSS_Selector[1]) #returns the corresponding CSS_Selector value for the element/table code
  }
}
