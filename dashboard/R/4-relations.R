







# UI ----------------------------------------------------------------------


relationsUI <- tabItem(
  "relations",
  box(
    title = "Linear fit",
    width = 12,
    selectizeInput(
      "relations-lm-outcome-variable",
      "Outcome",
      choices = c("No data loaded..." = "")
    ),
    selectizeInput(
      "relations-lm-predictor-variables",
      "Predictors",
      choices = c("No data loaded..." = ""),
      multiple = TRUE
    ),
    conditionalPanel(
      "input['relations-lm-outcome-variable'] != '' && input['relations-lm-predictor-variables'].length >= 1",
      fluidRow(
        column(6,
               # TODO: Add some styling to raw text outputs?
               verbatimTextOutput("relations-lm-summary")),
        column(
          6,
          plotlyOutput("relations-lm-plot"),
          conditionalPanel(
            "input['relations-lm-predictor-variables'].length > 1",
            selectizeInput(
              "relations-lm-plot-predictor-variable",
              "Predictor",
              choices = c("Choose x axis..." = "")
            )
          )
        )
      )
    )
  )
)


# SERVER ------------------------------------------------------------------
relationsServer <- function(input, output, session, context) {
  context$relations <- reactiveValues(lm = NULL)
  
  ## update selectize choices ----
  updateModelChoices <- observe({
    updateSelectizeInput(
      session,
      "relations-lm-outcome-variable",
      choices = context$model$variables,
      selected = input$`relations-lm-outcome-variable`
    )
    updateSelectizeInput(
      session,
      "relations-lm-predictor-variables",
      choices = context$model$variables,
      selected = input$`relations-lm-predictor-variables`
    )
  }) %>% bindEvent(context$model$variables)
  
  updatePredictorChoices <- observe({
    updateSelectizeInput(
      session,
      "relations-lm-plot-predictor-variable",
      choices = input$`relations-lm-predictor-variables`,
      selected = input$`relations-lm-plot-predictor-variable`
    )
  }) %>% bindEvent(input$`relations-lm-predictor-variables`)
  
  ## update model data ----
  context$relations$lm <- reactive({
    if (input$`relations-lm-outcome-variable` != "" &&
        !is.null(input$`relations-lm-predictor-variables`)) {
      formula <-
        as.formula(paste(
          input$`relations-lm-outcome-variable`,
          "~",
          paste(input$`relations-lm-predictor-variables`, collapse = "+")
        ))
      lm(formula, context$model$data_filtered(), na.action = "na.omit")
    }
  }) %>% debounce(500)
  
  ## outputs ----
  output$`relations-lm-summary` <- renderPrint({
    if (!is.null(context$relations$lm())) {
      context$relations$lm() %>%
        summary()
    }
  })
  
  output$`relations-lm-plot` <- renderPlotly({
    if(!is.null(context$relations$lm())) {
      if(!is.null(input$`relations-lm-plot-predictor-variable`) && input$`relations-lm-plot-predictor-variable` != "") {
        x_var <- input$`relations-lm-plot-predictor-variable`
      } else {
        x_var <- input$`relations-lm-predictor-variables` %>% first()
      }
      y_var <- input$`relations-lm-outcome-variable`
      
      print(x_var)
      print(y_var)
      
      # TODO: fix other predictors (e.g. mean, representatitive value)
      data <- predict(context$relations$lm(), interval = "conf") %>% 
        as_tibble() %>%
        bind_cols(context$model$data_filtered() %>% dplyr::select(!!x_var, !!y_var))
      
      print(data)
      str(data)
      
      ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], ymin = lwr, ymax = upr)) + 
        geom_point(alpha = .2, data = data %>% slice_sample(n = 500)) + 
        geom_ribbon(alpha = .4, fill = "steelblue") + 
        geom_line(aes(y = fit))
    }
  }) %>% bindEvent(context$relations$lm(), input$`relations-lm-plot-predictor-variable`)
}
