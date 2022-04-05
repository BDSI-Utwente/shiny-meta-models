filter_exclusive_variable_choices <- function(choices, set, self, selected){
  for (other in set %>% setdiff(self)){
    choices <- choices %>% setdiff(selected[[other]])
  }

  choices
}

update_exclusive_selectize_input_set <- function(choices, set, selected, session){
  for (id in set){
    updateSelectizeInput(
      session,
      id,
      choices = filter_exclusive_variable_choices(
        choices,
        set,
        id,
        selected
      ), 
      selected = selected[[id]]
    )  
  }
}
