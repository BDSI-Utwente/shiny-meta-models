# UI ----
predictionsUI <- tabItem(
  "predictions",
  box(
    width = 12,
    title = "Metamodel input values for predictions",
    p(
      "In this box, you can modify the input values of the metamodel in order to obtain predictions of the outcome using the metamodel.",
      br(),
      "Default values are the mean of the original parameter value."
    ),
    uiOutput("prediction-inputs")
  ),
  box(
    width = 12,
    title = "Predicted outcomes using the metamodel",
    collapsed = TRUE,
    conditionalPanel(
      "output['prediction-count'] >= 1",
      dataTableOutput("predictor-values")
    ),
    conditionalPanel(
      "!output['prediction-count'] || output['prediction-count'] == 0",
      bs4Callout(
        title = "Make predictions",
        status = "info",
        "After you have fit a model in the 'Metamodelling'-tab and requested one or more predictions, predicted outcomes will be shown here."
      )
    )
  )
)

# SERVER ----
predictionsServer <- function(input, output, session, context) {
  context$predictions <-
    reactiveValues(
      observers = NULL,
      current_predictor_values = list(),
      predictor_values = tibble(.rows = 0),
      warnings = list()
    )
  
  ## REACTIVE OBSERVERS ----
  updateObservers <- observe({
    cat("clearing previous cached values\n")
    context$predictions$current_predictor_values <- list()
    context$predictions$predictor_values <- tibble(.rows = 0)
    context$predictions$warnings <- list()
    
    cat("updating dynamic observers\n")
    context$predictions$observers <-
      c(
        context$relations$predictor_variables,
        context$relations$predictor_variables_poly_2,
        context$relations$predictor_variables_poly_3,
        context$relations$predictor_variables_exponential,
        context$relations$predictor_variables_log
      ) %>% map(function(predictor) {
        button_id <- paste0("predictor-reset-", predictor)
        value_id <- paste0("predictor-value-", predictor)
        mean <-
          context$model$data_filtered()[[predictor]] %>% mean() %>% round(3)
        range <-
          context$model$data_filtered()[[predictor]] %>% range() %>% round(3)
        
        onReset <- observe({
          cat("resetting", predictor, "to", mean, "\n")
          updateNumericInput(session, value_id, value = mean)
        }) %>% bindEvent(input[[button_id]])
        onChange <- observe({
          value <-
            context$predictions$current_predictor_values[[predictor]] <-
            input[[value_id]]
          cat("value", predictor, "changed: ", value, "\n")
          
          if (!is.na(value) &&
              (value < range[1] || value > range[2])) {
            context$predictions$warnings[[predictor]] <-
              span(
                "Value is outside of the observed range for",
                code(predictor),
                ", predictions may be inaccurate."
              )
          } else if (!is.null(context$predictions$warnings[[predictor]])) {
            context$predictions$warnings[[predictor]] <- NULL
          }
        }) %>% bindEvent(input[[value_id]])
        list(onReset, onChange)
      })
  }) %>% bindEvent(context$relations$lm())
  
  ## NORMAL OBSERVERS
  makePrediction <- observe({
    context$predictions$predictor_values <-
      bind_rows(
        context$predictions$predictor_values,
        context$predictions$current_predictor_values
      )
  }) %>% bindEvent(input$`predictions-make-prediction`)
  
  predictions <- reactive({
    if (!is.null(context$relations$lm()$fit) &&
        nrow(context$predictions$predictor_values) >= 1) {
      predict(
        context$relations$lm()$fit,
        context$predictions$predictor_values,
        interval = "prediction"
      )
    }
  })
  
  ## OUTPUTS ----
  ### ui/prediction-inputs ----
  output$`prediction-inputs` <- renderUI({
    if (!is.null(context$relations$lm()$fit)) {
      
      list(
        fluidRow(
          id = "predictions-predictor-values",
          c(
            context$relations$predictor_variables,
            context$relations$predictor_variables_poly_2,
            context$relations$predictor_variables_poly_3,
            context$relations$predictor_variables_exponential,
            context$relations$predictor_variables_log
          ) %>% map(
            create_predictor_input,
            values = context$predictions$current_predictor_values,
            data = context$model$data_filtered(),
            warnings = context$predictions$warnings
          )
        ),
        actionButton(
          "predictions-make-prediction",
          "Predict outcome",
          status = "primary"
        )
      )
    } else {
      bs4Callout(
        title = "Fit a model",
        status = "info",
        "You can only make predictions after fitting a model in the 'Metamodelling' tab."
      )
    }
  }) %>% bindEvent(context$relations$lm(),
                   context$predictions$warnings)
  
  ### dataTable/predictor-values ----
  output$`predictor-values` <- renderDataTable({
    if (!is.null(predictions())) {
      bind_cols(context$predictions$predictor_values,
                predictions() %>% as_tibble()) %>%
        mutate(
          outcome = fit %>% round(3),
          `95% prediction interval` = glue("[{lwr %>% round(3)}, {upr %>% round(3)}]"),
          fit = NULL,
          lwr = NULL,
          upr = NULL
        )
    }
  })
  
  output$`prediction-count` <- renderText({
    nrow(context$predictions$predictor_values)
  })
  
  shiny::outputOptions(output, "prediction-count", suspendWhenHidden = FALSE)
}

# FUNCTIONS ----
create_predictor_input <-
  function(predictor, values, data, warnings) {
    mean_value <- data[[predictor]] %>% mean() %>% round(3)
    current_value <-
      ifelse(is.null(values[[predictor]]), NA, as.numeric(values[[predictor]]))
    warning <- NULL
    if (!is.null(warnings[[predictor]])) {
      warning <-
        div(class = 'text-muted text-warning', warnings[[predictor]])
    }
    
    cat(
      "creating input for",
      predictor,
      "using default value",
      coalesce(current_value, mean_value),
      "\n"
    )
    
    column(width = 4,
           class = "predictor-value-input",
           fluidRow(
             column(width = 11, numericInput(
               paste0("predictor-value-", predictor),
               predictor,
               coalesce(current_value, mean_value)
             )),
             column(
               width = 1,
               actionButton(
                 paste0("predictor-reset-", predictor),
                 label = "",
                 style = "position: absolute; bottom: 2.5em; left: -.2em;",
                 icon = icon("undo", class = "fa-xs"),
                 status = "warning",
                 outline = TRUE,
                 size = "xs",
                 title = "reset to mean"
               )
             )
           ),
           warning)
  }
