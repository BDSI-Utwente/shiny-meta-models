filter_exclusive_variable_choices <- function(choices, set, self, input){
  for (other in set %>% setdiff(self)){
    choices <- choices %>% setdiff(input[[other]])
  }

  choices
}

update_exclusive_selectize_input_set <- function(choices, set, input, session){
  for (id in set){
    updateSelectizeInput(
      session,
      id,
      choices = filter_exclusive_variable_choices(
        choices,
        set,
        id,
        input
      ), 
      selected = input[[id]]
    )  
  }
}
