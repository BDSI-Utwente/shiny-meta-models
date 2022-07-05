# UI ----------------------------------------------------------------------
relationsUI <- tabItem(
  "relations",
  ## Linear fit ----
  box(
    title = "Linear fit",
    width = 12,
    ### selectize/relations-lm-outcome-variable ----
    fluidRow(
      column(
        width = 4,
        selectizeInput(
          "relations-lm-outcome-variable",
          "Outcome",
          choices = c("No data loaded..." = "")
          )
        ),
      ### selectize/relations-lm-predictor-variables ----
      column(
        width = 4,
        selectizeInput(
          "relations-lm-predictor-variables",
          "Predictors",
          choices = c("No data loaded..." = ""),
          multiple = TRUE
          )
        ),
      ### selectize/relations-lm-predictor-variables-poly-2 ----
      column(
        width = 4,
        selectizeInput(
          "relations-lm-predictor-variables-poly-2",
          "Predictors to transform in second-degree polynomials",
          choices = c("No data loaded..." = ""),
          multiple = TRUE
          )
        )
      ),
    fluidRow(
      ### selectize/relations-lm-predictor-variables-poly-3 ----
      column(
        width = 4,
        selectizeInput(
          "relations-lm-predictor-variables-poly-3",
          "Predictors to transform in third-degree polynomials",
          choices = c("No data loaded..." = ""),
          multiple = TRUE
          )
        ),
      ### selectize/relations-lm-predictor-variables-exponential ----
      column(
        width = 4,
        selectizeInput(
          "relations-lm-predictor-variables-exponential",
          "Predictors to exponentiate",
          choices = c("No data loaded..." = ""),
          multiple = TRUE
          )
        ),
      ### selectize/relations-lm-predictor-variables-log ----
      column(
        width = 4,
        selectizeInput(
          "relations-lm-predictor-variables-log",
          "Predictors to log-transform",
          choices = c("No data loaded..." = ""),
          multiple = TRUE
          )
        )
      ),
    fluidRow(
      ### checkboxInput/relation-lm-validation ----
      column(
        width = 4,
        checkboxInput(
          "relations-lm-validation",
          "Should validation of the metamodel be performed?",
          value = FALSE
          )
        ),
      ### numericInput/relation-lm-validation ----
      column(
        width = 4,
        numericInput(
          "relations-lm-partition",
          "Proportion of data to use to train the linear model (remainder will be used for validation)",
          min = 0,
          max = 1,
          step = 0.01,
          value = 1
          )
        )
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
    collapsed = TRUE,
        fluidRow(
          p("This panel shows the calibration statistics in the validation set (R-squared, mean absolute error, and mean relative error) and the calibration plot in the validation set."),
          column(
            6,
            dataTableOutput("relations-metamodel-validation-table")
            ),
          column(
            6,
            plotOutput("relations-metamodel-validation-plot")
            )
          )
    ), # dan conditionele dat er wel een model gefit is, anders warning message
  
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
      predictor_variables = c(),
      predictor_variables_poly_2 = c(),
      predictor_variables_poly_3 = c(),
      predictor_variables_exponential = c(),
      predictor_variables_log = c(),
      predictor_variable_plot = ""
    )
  
  model_variables <- reactive({
    c(context$relations$outcome_variable, 
      context$relations$predictor_variables, 
      context$relations$predictor_variables_poly_2,
      context$relations$predictor_variables_poly_3,
      context$relations$predictor_variables_exponential,
      context$relations$predictor_variables_log)
  }) %>% debounce(1000)
  
  ## update selectize choices ----
  updateModelChoices <- observe({
    inputs <-
      c("relations-lm-outcome-variable",
        "relations-lm-predictor-variables",
        "relations-lm-predictor-variables-poly-2",
        "relations-lm-predictor-variables-poly-3",
        "relations-lm-predictor-variables-exponential",
        "relations-lm-predictor-variables-log")
    selected <- list(
      `relations-lm-outcome-variable` = context$relations$outcome_variable,
      `relations-lm-predictor-variables` = context$relations$predictor_variables,
      `relations-lm-predictor-variables-poly-2` = context$relations$predictor_variables_poly_2,
      `relations-lm-predictor-variables-poly-3` = context$relations$predictor_variables_poly_3,
      `relations-lm-predictor-variables-exponential` = context$relations$predictor_variables_exponential,
      `relations-lm-predictor-variables-log` = context$relations$predictor_variables_log
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
  }) %>% bindEvent(context$relations$predictor_variables,
                   context$relations$predictor_variables_poly_2,
                   context$relations$predictor_variables_poly_3,
                   context$relations$predictor_variables_exponential,
                   context$relations$predictor_variables_log)
  
  updatePredictorChoicesPoly2 <- observe({
    updateSelectizeInput(
      session,
      "relations-lm-plot-predictor-variable-poly-2",
      choices = context$relations$predictor_variables_poly_2,
      selected = input$`relations-lm-plot-predictor-variable-poly-2`
    )
  }) %>% bindEvent(context$relations$predictor_variables_poly_2)

  updatePredictorChoicesPoly3 <- observe({
    updateSelectizeInput(
      session,
      "relations-lm-plot-predictor-variable-poly-3",
      choices = context$relations$predictor_variables_poly_3,
      selected = input$`relations-lm-plot-predictor-variable-poly-3`
    )
  }) %>% bindEvent(context$relations$predictor_variables_poly_3)

  updatePredictorChoicesExponential <- observe({
    updateSelectizeInput(
      session,
      "relations-lm-plot-predictor-variable-exponential",
      choices = context$relations$predictor_variables_exponential,
      selected = input$`relations-lm-plot-predictor-variable-exponential`
    )
  }) %>% bindEvent(context$relations$predictor_variables_exponential)

  updatePredictorChoicesLog <- observe({
    updateSelectizeInput(
      session,
      "relations-lm-plot-predictor-variable-log",
      choices = context$relations$predictor_variables_log,
      selected = input$`relations-lm-plot-predictor-variable-log`
    )
  }) %>% bindEvent(context$relations$predictor_variables_log)

  ## update context ----
  updateOutcomeVariable <- observe({
    context$relations$outcome_variable <- input$`relations-lm-outcome-variable`
  })
  
  updateOutcomeVariableNB <- observe({
    context$relations$outcome_variable <- ""
  }) %>% bindEvent(context$model$data_filtered()) # to prevent crash when incrementals and NB's are (not) calculated
  
  updatePredictorVariables <- observe({
    context$relations$predictor_variables <- input$`relations-lm-predictor-variables`
  })
  
  updatePredictorVariablesPoly2 <- observe({
    context$relations$predictor_variables_poly_2 <- input$`relations-lm-predictor-variables-poly-2`
  })
  
  updatePredictorVariablesPoly3 <- observe({
    context$relations$predictor_variables_poly_3 <- input$`relations-lm-predictor-variables-poly-3`
  })
  
  updatePredictorVariablesExponential <- observe({
    context$relations$predictor_variables_exponential <- input$`relations-lm-predictor-variables-exponential`
  })
  
  updatePredictorVariablesLog <- observe({
    context$relations$predictor_variables_log <- input$`relations-lm-predictor-variables-log`
  })
  
  l_lm_input <- list(
    validation = reactive({
      input$`relations-lm-validation`
    }) %>% 
      debounce(500),
    partition = reactive({
      input$`relations-lm-partition`
    }) %>% 
      debounce(500)
    )
  
  ## update model data ----
  context$relations$lm <- reactive({
    if (context$relations$outcome_variable %in% names(context$model$data_filtered()) &&
        context$relations$outcome_variable != "" &&
        (length(context$relations$predictor_variables) >= 1 |
         length(context$relations$predictor_variables_poly_2) >= 1 |
         length(context$relations$predictor_variables_poly_3) >= 1 |
         length(context$relations$predictor_variables_exponential) >= 1 |
         length(context$relations$predictor_variables_log) >= 1) &&
        ((l_lm_input$validation() == TRUE && 
          l_lm_input$partition() < 1 &&
          l_lm_input$partition() > 0) | 
         l_lm_input$validation() == FALSE)
        ) {
      l_out <- pacheck::fit_lm_metamodel(
        df = context$model$data_filtered() %>% as.data.frame(),
        y_var  = context$relations$outcome_variable,
        x_vars  = context$relations$predictor_variables,
        x_poly_2 = context$relations$predictor_variables_poly_2,
        x_poly_3 = context$relations$predictor_variables_poly_3,
        x_exp = context$relations$predictor_variables_exponential,
        x_log = context$relations$predictor_variables_log,
        validation = l_lm_input$validation(),
        partition  = l_lm_input$partition()
        )
      return(l_out)
    }
  }) %>% debounce(500)
  
  context$relations$margins <- reactiveVal()
  
  ## OUTPUTS ----
  ### print/relations-lm-summary ----
  output$`relations-lm-summary` <- renderPrint({
    if (!is.null(context$relations$lm())) {
      context$relations$lm()$fit %>%
        summary()
    }
  })
  
  ### plotly/relations-lm-plot ----
  output$`relations-lm-plot` <- renderPlotly({
    if (!is.null(context$relations$lm())) {
      y_var <- context$relations$outcome_variable
      x_var <- input$`relations-lm-plot-predictor-variable`
      x_vars <- context$relations$predictor_variables
      x_vars_poly_2 <- context$relations$predictor_variables_poly_2
      x_vars_poly_3 <- context$relations$predictor_variables_poly_3
      x_vars_exponential <- context$relations$predictor_variables_exponential
      x_vars_log <- context$relations$predictor_variables_log
      
      if (is.null(x_var) || x_var == "") {
        x_var <- c(x_vars, 
                   x_vars_poly_2,
                   x_vars_poly_3,
                   x_vars_exponential,
                   x_vars_log) %>% 
          dplyr::first()
      }
      
      data <- context$model$data_filtered() %>%
        dplyr::select(!!!x_vars, 
                      !!x_vars_poly_2, 
                      !!x_vars_poly_3,
                      !!x_vars_exponential,
                      !!x_vars_log,
                      !!y_var)
      
      # predict at 100 points across the range of x
      pred_data <- tibble(.rows = 100)
      pred_data[[x_var]] <-
        seq(data[[x_var]] %>% min(na.rm = TRUE),
            data[[x_var]] %>% max(na.rm = TRUE),
            length.out = 100)
      
      # pin other variables at their means
      margins <- list()
      for (var in c(x_vars, 
                    x_vars_poly_2, 
                    x_vars_poly_3,
                    x_vars_exponential,
                    x_vars_log)) {
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
                   input$`relations-lm-plot-predictor-variable`,
                   input$`relations-lm-plot-predictor-variable-poly-2`,
                   input$`relations-lm-plot-predictor-variable-poly-3`,
                   input$`relations-lm-plot-predictor-variable-exponential`,
                   input$`relations-lm-plot-predictor-variable-log`)
  
  ### print/relations-lm-margins ----
  output$`relations-lm-margins` <- renderPrint({
    margins <- context$relations$margins()
    map2(margins,
         names(margins),
         ~ div(class = "tag", code(.y), "held at", round(.x, 2))) %>% div(class = "tag-list")
  }) %>% bindEvent(context$relations$margins())
  
  context$relations$dsa <- reactive({
    pacheck::dsa_lm_metamodel(context$model$data_filtered(), context$relations$lm()$fit)
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
  
  output$`relations-metamodel-validation-table` <- renderDataTable(
    if(l_lm_input$validation() == TRUE &&
       l_lm_input$partition() > 0 &&
       l_lm_input$partition() < 1){
    context$relations$lm()$stats_validation
    }
    ) %>% bindEvent(context$relations$lm())

  output$`relations-metamodel-validation-plot` <- renderPlot(
    if(l_lm_input$validation() == TRUE &&
       l_lm_input$partition() > 0 &&
       l_lm_input$partition() < 1){
    context$relations$lm()$calibration_plot
    }
    ) %>% bindEvent(context$relations$lm())
}
