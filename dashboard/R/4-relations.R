# UI ----------------------------------------------------------------------
relationsUI <- tabItem(
  "relations",
  ## Linear fit ----
  box(
    title = "Linear fit",
    width = 12,
    ### selectize/relations-lm-outcome-variable ----
    selectizeInput(
      "relations-lm-outcome-variable",
      "Outcome",
      choices = c("No data loaded..." = "")
    ),
    ### selectize/relations-lm-predictor-variables ----
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
               ### verbatimText/relations-lm-summary ----
               verbatimTextOutput("relations-lm-summary")),
        column(
          6,
          ### plotly/relations-lm-plot ----
          plotlyOutput("relations-lm-plot"),
          conditionalPanel(
            "input['relations-lm-predictor-variables'].length > 1",
            #### selectize/relations-lm-plot-predictor-variable ----
            selectizeInput(
              "relations-lm-plot-predictor-variable",
              "Predictor",
              choices = c("Choose x axis..." = "")
            ),
            #### html/relations-lm-margins ----
            htmlOutput("relations-lm-margins")
          )
        )
      )
    )
  ),
  ## Validation linear metamodel ----
  box(
    title = "Validation metamodel",
    width = 12,
    collapsed = TRUE
    # first lm object veranderen
    # dan conditionele dat er wel een model gefit is, anders warning message
  ),
  
  ## DSA ----
  box(
    title = "Deterministic Sensitivity Analysis",
    width = 12,
    collapsed = TRUE,
    
    conditionalPanel(
      "input['relations-lm-outcome-variable'] != '' && input['relations-lm-predictor-variables'].length >= 2",
      
      fluidRow(column(6,
                      ### plotly/relations-dsa-plot ----
                      plotlyOutput(
                        "relations-dsa-plot"
                      )),
               column(
                 6,
                 ### dataTable/relations-dsa-table ----
                 dataTableOutput("relations-dsa-table")
               ))
    )
  )
)


# SERVER ------------------------------------------------------------------
relationsServer <- function(input, output, session, context) {
  context$relations <-
    reactiveValues(
      lm = NULL,
      outcome_variable = "",
      predictor_variables = c()
    )
  
  model_variables <- reactive({
    c(context$relations$outcome_variable, context$relations$predictor_variables)
  }) %>% debounce(1000)
  
  ## update selectize choices ----
  updateModelChoices <- observe({
    inputs <-
      c("relations-lm-outcome-variable",
        "relations-lm-predictor-variables")
    selected <- list(
      `relations-lm-outcome-variable` = context$relations$outcome_variable,
      `relations-lm-predictor-variables` = context$relations$predictor_variables
    )
    choices <- c("Select..." = "", context$model$variables)
    
    update_exclusive_selectize_input_set(choices, inputs, selected, session)
  }) %>%
    bindEvent(
      context$model$variables,
      
      # bind to model_variables() instead of outcome/predictors because we can
      # put a debounce on model_variables()
      model_variables()
    )
  
  updatePredictorChoices <- observe({
    updateSelectizeInput(
      session,
      "relations-lm-plot-predictor-variable",
      choices = context$relations$predictor_variables,
      selected = input$`relations-lm-plot-predictor-variable`
    )
  }) %>% bindEvent(context$relations$predictor_variables)
  
  ## update context ----
  updateOutcomeVariable <- observe({
    context$relations$outcome_variable <- input$`relations-lm-outcome-variable`
  })
  
  updateOutcomeVariableNB <- observe({
    context$relations$outcome_variable <- ""
  }) %>% bindEvent(context$model$data_filtered())
  
  updatePredictorVariables <- observe({
    context$relations$predictor_variables <- input$`relations-lm-predictor-variables`
  })
  
  ## update model data ----
  context$relations$lm <- reactive({
    if (context$relations$outcome_variable %in% names(context$model$data_filtered()) &&
        context$relations$outcome_variable != "" &&
        length(context$relations$predictor_variables) >= 1) {
      
      l_out <- pacheck::fit_lm_metamodel(
        df = context$model$data_filtered(),
        y = context$relations$outcome_variable,
        x = context$relations$predictor_variables,
        partition = 0.75)
      return(l_out)
      # formula <-
      #   as.formula(paste(
      #     context$relations$outcome_variable,
      #     "~",
      #     paste(context$relations$predictor_variables, collapse = "+")
      #   ))
      # lm(formula, context$model$data_filtered(), na.action = "na.omit")
    }
  }) %>% debounce(500)
  
  context$relations$margins <- reactiveVal()
  
  ## OUTPUTS ----
  ### print/relations-lm-summary ----
  output$`relations-lm-summary` <- renderPrint({
    if (!is.null(context$relations$lm())) {
      context$relations$lm$fit() %>%
        summary()
    }
  })
  
  ### plotly/relations-lm-plot ----
  output$`relations-lm-plot` <- renderPlotly({
    if (!is.null(context$relations$lm)) {
      y_var <- context$relations$outcome_variable
      x_var <- input$`relations-lm-plot-predictor-variable`
      x_vars <- context$relations$predictor_variables
      
      if (is.null(x_var) || x_var == "") {
        x_var <- x_vars %>% dplyr::first()
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
      
      predictions <- predict(context$relations$lm()$fit,
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
  
  ### print/relations-lm-margins ----
  output$`relations-lm-margins` <- renderPrint({
    margins <- context$relations$margins()
    map2(margins,
         names(margins),
         ~ div(class = "tag", code(.y), "held at", round(.x, 2))) %>% div(class = "tag-list")
  }) %>% bindEvent(context$relations$margins())
  
  context$relations$dsa <- reactive({
    pacheck::dsa_lm_metamodel(context$model$data_filtered(), context$relations$lm$fit())
  }) %>% bindEvent(context$relations$lm())
  
  ### dataTable/relations-dsa-table ----
  output$`relations-dsa-table` <- renderDataTable({
    context$relations$dsa() %>%
      mutate(across(where(is.numeric), round, digits = 3)) %>%
      transmute(
        Parameter,
        `Lower Bound` = glue::glue("{Lower_Bound_low} – {Lower_Bound_upp}"),
        `Upper Bound` = glue::glue("{Upper_Bound_low} – {Upper_Bound_upp}")
      )
  })
  
  ### plotly/relations-dsa-plot ----
  output$`relations-dsa-plot` <- renderPlotly({
    pacheck::plot_tornado(
      context$relations$dsa(),
      context$model$data_filtered() %>% as.data.frame(),
      context$relations$outcome_variable
    )
  }) %>%
    # dsa takes a dependency on lm, which depends on data, inputs and output.
    # therefore, we only need a dependency on dsa here.
    bindEvent(context$relations$dsa())
}
