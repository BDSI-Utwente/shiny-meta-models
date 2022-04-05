











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
            ),
            htmlOutput("relations-lm-margins")
          )
        )
      )
    )
  ),
  box(
    title = "Deterministic Sensitivity Analysis",
    width = 12,
    div(style = "overflow: auto;",
        dataTableOutput("relations-dsa"))
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
  
  context$relations$margins <- reactiveVal()
  
  ## outputs ----
  output$`relations-lm-summary` <- renderPrint({
    if (!is.null(context$relations$lm())) {
      context$relations$lm() %>%
        summary()
    }
  })
  
  output$`relations-lm-plot` <- renderPlotly({
    if (!is.null(context$relations$lm())) {
      y_var <- input$`relations-lm-outcome-variable`
      x_var <- input$`relations-lm-plot-predictor-variable`
      x_vars <- input$`relations-lm-predictor-variables`
      
      if (is.null(x_var) || x_var == "") {
        x_var <- x_vars %>% first()
      }
      
      data <- context$model$data_filtered() %>%
        dplyr::select(!!!x_vars, !!y_var)
      
      # predict at 100 points across the range of x
      pred_data <- tibble(.rows = 100)
      pred_data[[x_var]] <-
        seq(data[[x_var]] %>% min(na.rm = TRUE),
            data[[x_var]] %>% max(na.rm = TRUE),
            length.out = 100)
      
      # pin other variables at their means
      margins <- list()
      for (var in x_vars) {
        if (var == x_var)
          next
        
        pred_data[[var]] <-
          margins[[var]] <- mean(data[[var]], na.rm = TRUE)
      }
      context$relations$margins(margins)
      
      predictions <- predict(context$relations$lm(),
                             pred_data,
                             interval = "conf") %>%
        as_tibble() %>%
        bind_cols(pred_data)
      
      ggplot(predictions, aes(
        x = .data[[x_var]],
        y = fit,
        ymin = lwr,
        ymax = upr
      )) +
        labs(x = x_var, y = y_var) +
        geom_ribbon(alpha = .4, fill = "steelblue") +
        geom_line() +
        geom_point(
          aes(x = .data[[x_var]], y = .data[[y_var]]),
          data = data %>% slice_sample(n = 500),
          inherit.aes = FALSE,
          alpha = 0.2,
          shape = 16
        )
    }
  }) %>% bindEvent(context$relations$lm(),
                   input$`relations-lm-plot-predictor-variable`)
  
  output$`relations-lm-margins` <- renderPrint({
    margins <- context$relations$margins()
    map2(margins,
         names(margins),
         ~ div(class = "lm-margin-tag", code(.y), "held at", round(.x, 2))) %>% div(style =
                                                                                      "display: flex;")
  }) %>% bindEvent(context$relations$margins())
  
  output$`relations-dsa` <- renderDataTable({
    pacheck::dsa_lm_metamodel(context$model$data_filtered(), context$relations$lm())
  })
}
