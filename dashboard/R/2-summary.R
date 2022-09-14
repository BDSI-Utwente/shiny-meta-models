library(plotly)
library(pacheck)

FIT_DISTRIBUTIONS <- c("norm", "beta", "gamma", "lnorm")
FIT_METHODS <- c("lm", "glm", "loess", "gam", "custom")

source("./functions/cautiously.R")
check_cautiously <- cautiously(do_quick_check)
cautiously_vis_1_param <- cautiously(pacheck::vis_1_param)
cautiously_fit_dist <- cautiously(pacheck::fit_dist)


# UI ----------------------------------------------------------------------
summaryUI <- tabItem(
  "summary",
  # summary statistics
  box(
    width = 12,
    title = "Select variables for summary statistics",
    p(
      "Using the input field below, you can select the input and output variables which you want to include in the", em("Summary statistics"), "-box (e.g. min, max, mean,...) below and which should be included in the correlation matrix(", em("Correlation matrix"), "-box).", 
      br(),
      "You can also use the buttons to select an entire group of parameter (defined in the", strong("Prepare data-tab"), ").",
    ),
    ## selectizeInput/summary-statistics-variables ----
    selectizeInput(
      "summary-statistics-variables",
      "Variables",
      multiple = TRUE,
      choices = c()
    ),
    p("or select all..."),
    div(
      class = "flex",
      ### action/summary-add-cost-variables ----
      actionButton("summary-add-cost-variables", "Costs"),
      
      ### action/summary-add-utility-variables ----
      actionButton("summary-add-utility-variables", "Utilities"),
      
      ### action/summary-add-disutility-variables ----
      actionButton("summary-add-disutility-variables", "Disutilities"),
      
      ### action/summary-add-probability-variables ----
      actionButton("summary-add-probability-variables", "Probabilities"),
      
      ### action/summary-add-relative-effectiveness-variables ----
      actionButton(
        "summary-add-relative-effectiveness-variables",
        "Relative effectiveness"
      ),
      
      ### action/summary-add-all-variables ----
      actionButton("summary-add-all-variables", "Everything!")
    )
  ),
  ## dataTable/summary-statistics ----
  box(
    width = 12,
    title = "Summary statistics",
    collapsed = TRUE,
    p(
      "This box shows the summary statistics of the variables selected above."
    ),
    div(
      width = "100%",
      style = "overflow: auto;",
      dataTableOutput("summary-statistics")
    )
  ),
  box(
    width = 12,
    title = "Correlation matrix",
    collapsed = TRUE,
    p(
      "This box shows the correlation between the variables selected above."
    ),
    div(
      width = "100%",
      style = "overflow: auto;",
      ## plotly/summary-correlation-matrix ----
      plotlyOutput("summary-correlation-matrix"),
      ## html/summary-correlation-matrix-tags ----
      htmlOutput("summary-correlation-matrix-tags")
    )
  ),
  ## plotly/summary-distribution-plot ----
  box(
    width = 12,
    title = "Single variable exploration",
    collapsed = TRUE,
    p(
      "In this box, you can investigate the distribution of a single variable.",
      br(),
      "You can also fit different statistical distributions to values of the variable.",
      br(),
      "Please wait for the calculations to be performed after selecting a (or multiple) distribution(s)"
    ),
    ### selectizeInput/summary-distribution-variable ----
    selectizeInput(
      "summary-distribution-variable",
      "Variable",
      multiple = FALSE,
      choices = c("No data loaded..." = "")
    ),
    conditionalPanel(
      "input['summary-distribution-variable'] != ''",
      fluidRow(
        column(
          width = 6,
          p("Select a distribution type and then fill in the parameter values of this distribution."),
          ### radioButtons/summary-distribution-type ----
          radioButtons(
            "summary-distribution-type",
            "Plot type",
            c("density", "histogram"),
            selected = "density",
            inline = TRUE
          )
        ),
        column(
          width = 6,
          ### checkboxGroup/summary-distribution-fits ----
          checkboxGroupInput(
            "summary-distribution-fits",
            label = "Select distribution(s) to fit on variable:",
            choiceNames = c("normal", "beta", "gamma", "log-normal", "custom"),
            choiceValues  = c(FIT_DISTRIBUTIONS, "custom"),
            selected = NULL,
            inline = TRUE
          )
        )
      ),
      conditionalPanel(
        "input['summary-distribution-fits'].indexOf('custom') >= 0",
        {
          fluidRow(
            column(
              width = 6,
              ### selectize/summary-distribution-custom-type ----
              selectizeInput(
                "summary-distribution-custom-type",
                label = "Custom fit type",
                choices = FIT_DISTRIBUTIONS
              )
            ),
            column(
              width = 2,
              ### selectize/summary-distribution-custom-mean ----
              numericInput(
                "summary-distribution-custom-mean",
                label = "Average?",
                0,
                step = .01
              )
            ),
            column(
              width = 2,
              ### selectize/summary-distribution-custom-param-1 ----
              numericInput("summary-distribution-custom-param-1", "Mean", 0, step = .01)
            ),
            column(
              width = 2,
              ### selectize/summary-distribution-custom-param-2 ----
              numericInput("summary-distribution-custom-param-2", "SD", 1, step = .01)
            )
          )
        }
      ),
      fluidRow(### plotly/summary-distribution-plot ----
               column(
                 width = 6, 
                 plotOutput("summary-distribution-plot"
                            # , 
                            # brushOpts("plot_brush",
                            #           direction = "x")
                            )
                 ),
               ### dataTable/summary-distribution-stats ----
               column(
                 width = 6,
                 tableOutput("summary-distribution-fit"),
                 #verbatimTextOutput("brush-info")
               )
               )
      )
  ),
  
  ## Two variables exploration ----
  box(
    width = 12,
    title = "Two variables exploration",
    collapsed = TRUE,
    p(
      "In this box, you can draw a scatterplot of two variables and check whether the first is greater than the second.", 
      br(),
      "You can also fit a regression line through the scatterplot to investigate a potential relation between these two variables."
    ),
    fluidRow(column(
      6,
      ### selectize/summary-bivariate-distribution-x-variable ----
      selectizeInput(
        "summary-bivariate-distribution-x-variable",
        "X variable",
        choices = c("No data loaded..." = "")
      )
    ),
    column(
      6,
      ### selectize/summary-bivariate-distribution-y-variable ----
      selectizeInput(
        "summary-bivariate-distribution-y-variable",
        "Y variable",
        choices = c("No data loaded..." = "")
      )
    )),
    conditionalPanel(
      "input['summary-bivariate-distribution-x-variable'] != '' && input['summary-bivariate-distribution-y-variable'] != ''",
      fluidRow(column(
        ### plotly/summary-bivariate-distribution-plot ----
        8, plotOutput("summary-bivariate-distribution-plot")
      ),
      column(
        4,
        h4("Additional options"),
        div(
          class = "dense-form-inputs",
          ### checkbox/summary-bivariate-distribution-check-y-gt-x ----
          checkboxInput(
            "summary-bivariate-distribution-check-y-gt-x",
            span("Check", code("y"), ">", code("x"))
          ),
          
          ### checkbox/summary-bivariate-distribution-fit ----
          checkboxInput("summary-bivariate-distribution-fit",
                        "Fit regression line"),
          conditionalPanel(
            "input['summary-bivariate-distribution-fit']",
            
            ### checkboxGroup/summary-bivariate-distribution-fit-method ----
            checkboxGroupInput(
              "summary-bivariate-distribution-fit-method",
              "Method(s)",
              choices = FIT_METHODS,
              inline = TRUE
            ),
            conditionalPanel(
              "input['summary-bivariate-distribution-fit-method'].includes('custom')",
              fluidRow(column(
                6,
                ### numeric/summary-bivariate-distribution-fit-custom-intercept ----
                numericInput(
                  "summary-bivariate-distribution-fit-custom-intercept",
                  "Intercept",
                  0
                )
              ), column(
                6,
                ### numeric/summary-bivariate-distribution-fit-custom-slope ----
                numericInput(
                  "summary-bivariate-distribution-fit-custom-slope",
                  "Slope",
                  1
                )
              ))
            )
          )
        )
      ))
    )
  ),
  ## Sum of variables ----
  box(
    width = 12,
    title = "Check sum of probabilities",
    collapsed = TRUE,
    p(
      "In this box, you can sum up multiple variables containing probability input values to check whether they remain lower than or are equal to 1."
    ),
    fluidRow(column(
      6,
      ### selectize/summary-variables-for-sum ----
      selectizeInput(
        "summary-variables-for-sum",
        "Variables to sum up with each other",
        choices = c("no data loaded..." = ""),
        multiple = TRUE
      )
    ),
    column(
      6,
      ### numeric/summary-number-digits-sum ----
      numericInput(
        "summary-number-digits-sum",
        "Number of digits for rounding", 
        value = 9999,
        min = 0,
        max = Inf,
        step = 1
      )
    ),
    column(
      6,
      ### checkbox/summary-checkbox-sum-equal-one ----
      checkboxInput(
        "summary-checkbox-sum-equal-one",
        "Check whether sum is strictly equal to 1 (other lower or equal is checked)",
        value = FALSE
      )
    ),
    column(
      6,
      ### numeric/summary-max-view-number-error ----
      numericInput(
        "summary-max-view-number-error",
        "Number of error to display",
        value = 100,
        min = 0,
        max = Inf
      )
    ),
    column(
      12,
      ### text/summary-sum-p-text
      textOutput("summary-sum-p-text")
    )
    )
  )
)

# SERVER ------------------------------------------------------------------
summaryServer <- function(input, output, session, context) {
  context$summary <- reactiveValues(variables = c(),
                                    distribution_variable = "",
                                    summary_variables_for_sum = c()
                                    )
  
  ## UI update observers ----
  updateSummaryVariableChoices <- observe({
    ### selectizeInput/summary-statistics-variables ----
    updateSelectizeInput(
      session,
      "summary-statistics-variables",
      choices = context$model$variables,
      selected = context$summary$variables %>% intersect(context$model$variables)
    )
    
    ### selectizeInput/summary-distribution-variable ----
    updateSelectizeInput(
      session,
      "summary-distribution-variable",
      choices = context$model$variables,
      selected = context$summary$distribution_variable
    )
    
    ### selectizeInput/summary-variables-for-sum ----
     updateSelectizeInput(
       session,
       "summary-variables-for-sum",
       choices = context$model$variables,
       selected = context$summary$summary_variables_for_sum
    )
    
  }) %>% bindEvent(context$model$variables)
  
  updateBivariateDistributionChoices <- observe({
    inputs <-
      c(
        "summary-bivariate-distribution-x-variable",
        "summary-bivariate-distribution-y-variable"
      )
    update_exclusive_selectize_input_set(context$model$variables, inputs, input, session)
  }) %>% bindEvent(
    context$model$variables,
    input$`summary-bivariate-distribution-x-variable`,
    input$`summary-bivariate-distribution-y-variable`
  )
  
  ## SERVER update observers ----
  ### context$summary$distribution_variable ----
  updateSummaryDistributionVariable <- observe({
    context$summary$distribution_variable <-
      input$`summary-distribution-variable`
  }) %>% bindEvent(input$`summary-distribution-variable`)
  
  ### context$summary$variables ----
  updateSummaryStatisticsVariables <- observe({
    context$summary$variables <- input$`summary-statistics-variables`
  }) %>% bindEvent(input$`summary-statistics-variables`)
  
  ### context$summary$summary_variables_for_sum ----
  updateSummaryVariablesForSum <- observe({
    context$summary$summary_variables_for_sum <- input$`summary-variables-for-sum`
  }) %>% bindEvent(input$`summary-variables-for-sum`)
  
  
  ## SERVER update reactive ----
  
  ### debounce rapidly changing inputs ----
  sum_p_variables <- list(
    # only apply changes after 500ms without any change
    digits = reactive({
      input$`summary-number-digits-sum`
    }) %>% debounce(500),
    check = reactive({
      ifelse(input$`summary-checkbox-sum-equal-one` == TRUE, "equal", "lower") 
    }) %>% debounce(500),
    max_view = reactive({
      input$`summary-max-view-number-error`
    }) %>% debounce(500)
  )
  
  ## UI event handlers ----
  ### action/summary-add-cost-variables ----
  addCostVariables <- observe({
    context$summary$variables <-
      c(context$summary$variables, context$model$cost_variables) %>% unique()
    updateSelectizeInput(session,
                         "summary-statistics-variables",
                         selected = context$summary$variables)
  }) %>% bindEvent(input$`summary-add-cost-variables`)
  
  ### action/summary-add-utility-variables ----
  addUtilityVariables <- observe({
    context$summary$variables <-
      c(context$summary$variables,
        context$model$utility_variables) %>% unique()
    updateSelectizeInput(session,
                         "summary-statistics-variables",
                         selected = context$summary$variables)
  }) %>% bindEvent(input$`summary-add-utility-variables`)
  
  ### action/summary-add-disutility-variables ----
  addDisutilityVariables <- observe({
    context$summary$variables <-
      c(context$summary$variables,
        context$model$disutility_variables) %>% unique()
    updateSelectizeInput(session,
                         "summary-statistics-variables",
                         selected = context$summary$variables)
  }) %>% bindEvent(input$`summary-add-disutility-variables`)
  
  ### action/summary-add-probability-variables ----
  addProbabilityVariables <- observe({
    context$summary$variables <-
      c(context$summary$variables,
        context$model$probability_variables) %>% unique()
    updateSelectizeInput(session,
                         "summary-statistics-variables",
                         selected = context$summary$variables)
  }) %>% bindEvent(input$`summary-add-probability-variables`)
  
  ### action/summary-add-relative-effectiveness-variables ----
  addRelativeEffectivenessVariables <- observe({
    context$summary$variables <-
      c(context$summary$variables,
        context$model$relative_effectiveness_variables) %>% unique()
    updateSelectizeInput(session,
                         "summary-statistics-variables",
                         selected = context$summary$variables)
  }) %>% bindEvent(input$`summary-add-relative-effectiveness-variables`)
  
  ### action/summary-add-all-variables ----
  addAllVariables <- observe({
    context$summary$variables <-
      c(context$summary$variables, context$model$variables) %>% unique()
    updateSelectizeInput(session,
                         "summary-statistics-variables",
                         selected = context$summary$variables)
  }) %>% bindEvent(input$`summary-add-all-variables`)
  
  ### change/summary-distribution-custom-type
  updateParameterLabels <- observe({
    attrs <- switch(
      input$`summary-distribution-custom-type`,
      "norm" = list("Mean", 0, "SD", 1),
      "beta" = list("Shape 1", 1, "Shape 2", 1),
      "gamma" = list("Shape", 1, "Rate", 1),
      "lnorm" = list("Mean log", 0, "SD log", 1)
    )
    
    updateNumericInput(
      session,
      "summary-distribution-custom-param-1",
      label = attrs[1],
      value = attrs[2]
    )
    updateNumericInput(
      session,
      "summary-distribution-custom-param-2",
      label = attrs[3],
      value = attrs[4]
    )
  }) %>% bindEvent(input$`summary-distribution-custom-type`)
  
  ## OUTPUTS ----
  ### dataTable/summary-statistics ----
  output$`summary-statistics` <- renderDataTable({
    if (context$summary$variables %>% length() > 0) {
      pacheck::generate_sum_stats(context$model$data_filtered(),
                                  context$summary$variables) %>%
        suppressWarnings()
    }
  })
  
  correlation_matrix_tags <- reactiveVal()
  
  # TODO: split into separate reactives for correlation matrix and plot so that
  # we can cache plots without negatively affecting other reactives that depend
  # on the cor matrix
  ## plotly/summary-correlation-matrix ----
  output$`summary-correlation-matrix` <- renderPlotly({
    if (is.null(context$model$data_filtered()) ||
        context$summary$variables %>% length() == 0) {
      correlation_matrix_tags(NULL)
      return()
    }
    
    selected_cols <- context$model$data_filtered() %>%
      dplyr::select(!!!context$summary$variables)
    
    invalid_cols_non_numeric <- selected_cols %>%
      dplyr::select(where( ~ !is.numeric(.x))) %>%
      names()
    
    invalid_cols_no_variance <- selected_cols %>%
      dplyr::select(-any_of(invalid_cols_non_numeric)) %>%
      dplyr::select(where( ~ var(.x, na.rm = TRUE) <= 1e-4)) %>%
      names()
    
    correlation_matrix_tags(bind_rows(
      tibble(var = invalid_cols_non_numeric,
             reason = "is not numeric"),
      tibble(var = invalid_cols_no_variance,
             reason = "has no variance")
    ))
    
    valid_data <- selected_cols %>%
      dplyr::select(-any_of(invalid_cols_non_numeric),
                    -any_of(invalid_cols_no_variance))
    
    correlations <- valid_data %>%
      cor(use = "pair") %>%
      as_tibble(rownames = "x") %>%
      pivot_longer(-x, names_to = "y", values_to = "cor") %>%
      mutate(
        x = factor(x, context$summary$variables),
        y = factor(y, context$summary$variables %>% rev())
      )
    plot <- correlations %>%
      ggplot(aes(x, y, fill = cor)) +
      geom_raster() +
      scale_fill_continuous(limits = c(min(correlations$cor, 0), max(correlations$cor, 1))) +
      theme(axis.text.x = element_text(angle = 45),
            axis.title = element_blank())
    plot %>% ggplotly()
  }) %>%
    bindEvent(context$model$data_filtered(),
              context$summary$variables)
  
  
  ### html/summary-correlation-matrix-tags ----
  output$`summary-correlation-matrix-tags` <- renderPrint({
    tags <- correlation_matrix_tags()
    
    if (!is.null(tags) && nrow(tags) >= 1) {
      div(
        span(style = "font-size: smaller;", "Excluded variables"),
        div(class = "tag-list",
            tags %>%
              purrr::pmap(function(var, reason) {
                div(class = "tag", code(var), reason)
              }))
      )
    }
  }) %>% bindEvent(correlation_matrix_tags(),
                   ignoreInit = FALSE,
                   ignoreNULL = FALSE)
  
  
  ### plotly/summary-distribution-plot ----
  output$`summary-distribution-plot` <- renderPlot({
    if (!is.null(context$model$data_filtered()) &&
        context$summary$distribution_variable != "") {
      user <- list()
      if ('custom' %in% input$`summary-distribution-fits`) {
        user$type <- input$`summary-distribution-custom-type`
        user$param_1 <-
          input$`summary-distribution-custom-param-1`
        user$param_2 <-
          input$`summary-distribution-custom-param-2`
        user$mean <- input$`summary-distribution-custom-mean`
      }
      
      plot <- cautiously_vis_1_param(
        context$model$data_filtered() %>% as.data.frame(),
        context$summary$distribution_variable,
        type = input$`summary-distribution-type`,
        dist = input$`summary-distribution-fits`,
        user_dist = user$type,
        user_param_1 = user$param_1,
        user_param_2 = user$param_2,
        user_mean = user$mean
      )
      
      str(plot)
      if (!is.null(plot$error)) {
        stop(
          plot$error$message,
          "\nYou may be trying to fit a model that is inappropriate for this variable, or the variable selected may have no variance."
        )
      }
      plot$result
    }
  })
  
  context$summary$distribution_stats <- reactive({
    if (is.null(context$model$data_filtered()) ||
        nrow(context$model$data_filtered()) <= 0 ||
        context$summary$distribution_variable == "" ||
        length(input$`summary-distribution-fits`) <= 0) {
      return(list(result = NULL))
    }
    
    cautiously_fit_dist(
      context$model$data_filtered() %>% as.data.frame(),
      context$summary$distribution_variable,
      input$`summary-distribution-fits` %>% intersect(FIT_DISTRIBUTIONS) # filter out custom
    )
  }) %>% debounce(500)
  
  ### table/summary-distribution-fit-stats ----
  output$`summary-distribution-fit` <- renderTable({
    stats = context$summary$distribution_stats()
    if (!is.null(stats$error)) {
      stop(
        stats$error,
        "You may be trying to fit a model that is inappropriate for this variable, or the variable selected may have no variance."
      )
    }
    if (!is.null(stats$result)) {
      .data <- reduce(stats$result, left_join)
      .data %>%
        dplyr::select(-tidyselect::starts_with("Name_")) %>%
        rename_with(~ str_remove_all(.x, "Value_") %>%
                      str_replace_all("_", " ") %>%
                      str_to_title()) %>%
        arrange(Distribution)
    }
  })
  
  check <- reactive({
    if(input$`summary-bivariate-distribution-check-y-gt-x`) {
      return("param_2 > param_1")
    } 
    return(NULL)
  }) %>% throttle(250)
  
  fit_methods <- reactive({
    input$`summary-bivariate-distribution-fit-method`
  }, label = "fit_methods") %>% throttle(250)
  
  fit_custom <- reactive({
    "custom" %in% fit_methods()
  }, label = "fit_custom")
  
  fit_custom_pars <- reactive({
    pars <- list()
    if (fit_custom()) {
      pars$intercept <-
        input$`summary-bivariate-distribution-fit-custom-intercept`
      pars$slope <-
        input$`summary-bivariate-distribution-fit-custom-slope`
    }
    pars
  }, label = "fit_custom_pars") %>% throttle(250)
  
  ### plotly/summary-bivariate-distribution-plot ----
  output$`summary-bivariate-distribution-plot` <- renderPlot({
    if (input$`summary-bivariate-distribution-x-variable` != "" &&
        input$`summary-bivariate-distribution-y-variable` != "") {
      p <- pacheck::vis_2_params(
        context$model$data_filtered(),
        param_1 = input$`summary-bivariate-distribution-x-variable`,
        param_2 = input$`summary-bivariate-distribution-y-variable`,
        slope = fit_custom_pars()$slope,
        intercept = fit_custom_pars()$intercept,
        check = check()
      )
      
      # pacheck::vis_2_params(...) only allows one type of fit, I don't see why we
      # should have that restriction.
      for (type in (fit_methods() %>% setdiff(c("custom")))) {
        p <- p + geom_smooth(aes(colour = !!type), method = type)
      }
      
      p
    }})
  
  ### text/summary-bivariate-distribution-plot ----
  output$`summary-sum-p-text` <- renderText({
    if(context$summary$summary_variables_for_sum %>% length() > 0){
      pacheck::check_sum_probs(
        context$summary$summary_variables_for_sum,
        df       = context$model$data_filtered(),
        digits   = sum_p_variables$digits(),
        check    = sum_p_variables$check(),
        max_view = sum_p_variables$max_view() 
      )
    }
  })
  
  output$`brush-info` <- renderPrint({
    
    df <- brushedPoints(context$model$data_filtered(), input$plot_brush, xvar = context$summary$distribution_variable)
    summary(df)
    
  })
}

