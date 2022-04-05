library(plotly)
library(pacheck)

FIT_TYPES <- c("norm", "beta", "gamma", "lnorm")

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
      
      ### action/summary-add-probability-variables ----
      actionButton("summary-add-probability-variables", "Probabilities"),
      
      ### action/summary-add-relative-effectiveness-variables ----
      actionButton(
        "summary-add-relative-effectiveness-variables",
        "Relative effectiveness"
      )
    )
  ),
  ## dataTable/summary-statistics ----
  box(
    width = 12,
    title = "Summary statistics",
    div(
      width = "100%",
      style = "overflow: auto;",
      dataTableOutput("summary-statistics")
    )
  ),
  ## plotly/summary-correlation-matrix ----
  box(
    width = 12,
    title = "Correlation matrix",
    collapsed = TRUE,
    div(
      width = "100%",
      style = "overflow: auto;",
      plotlyOutput("summary-correlation-matrix")
    )
  ),
  ## plotly/summary-distribution-plot ----
  box(
    width = 12,
    title = "Distributions",
    div(
      width = "100%",
      style = "overflow: hidden;",
      ### selectizeInput/summary-distribution-variable ----
      selectizeInput(
        "summary-distribution-variable",
        "Variable",
        multiple = FALSE,
        choices = c()
      ),
      # TODO: move to global css
      tags$style(
        ".shiny-options-group label { font-weight: unset !important; } \n
        label.radio-inline + label.radio-inline { margin-left: 1em; }\n
        label.checkbox-inline + label.checkbox-inline { margin-left: 1em; }\n
        label.checkbox-inline span, label.radio-inline span { position: relative; bottom: 1px; margin-left: .25em;"
      ),
      ### radioButtons/summary-distribution-type ----
      radioButtons(
        "summary-distribution-type",
        "Plot type",
        c("density", "histogram"),
        selected = "density",
        inline = TRUE
      ),
      ### checkboxGroup/summary-distribution-fits ----
      checkboxGroupInput(
        "summary-distribution-fits",
        "Fit to...",
        choices = c(FIT_TYPES, "custom"),
        selected = c(FIT_TYPES),
        inline = TRUE
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
                choices = FIT_TYPES
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
                 width = 6, plotlyOutput("summary-distribution-plot")
               ),
               
               ### dataTable/summary-distribution-stats ----
               column(
                 width = 6,
                 tableOutput("summary-distribution-fit")
               ))
    )
  )
)



# SERVER ------------------------------------------------------------------
summaryServer <- function(input, output, session, context) {
  context$summary <- reactiveValues(variables = c(),
                                    distribution_variable = "")
  
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
  }) %>% bindEvent(context$model$variables)
  
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
  
  ### plotly/summary-correlation-matrix ----
  output$`summary-correlation-matrix` <- renderPlotly({
    if (is.null(context$model$data_filtered()) ||
        context$summary$variables %>% length() == 0) {
      return()
    }
    correlations <- context$model$data_filtered() %>%
      dplyr::select(!!!context$summary$variables) %>%
      dplyr::select(where(is.numeric)) %>%
      cor(use = "pair") %>%
      as_tibble(rownames = "x") %>%
      pivot_longer(-x, names_to = "y", values_to = "cor") %>%
      mutate(
        x = factor(x, context$summary$variables),
        y = factor(y, context$summary$variables)
      )
    plot <- correlations %>%
      ggplot(aes(x, y, fill = cor)) +
      geom_raster()
    plot %>% ggplotly()
  }) %>%
    bindCache(context$model$data_filtered(),
              context$summary$variables) %>%
    bindEvent(context$model$data_filtered(),
              context$summary$variables)
  
  
  ### plotly/summary-distribution-plot ----
  output$`summary-distribution-plot` <- renderPlotly({
    if (!is.null(context$model$data_filtered()) &&
        context$summary$distribution_variable != "") {
      user <- list()
      if ('custom' %in% input$`summary-distribution-fits`) {
        user$type <- input$`summary-distribution-custom-type`
        user$param_1 <- input$`summary-distribution-custom-param-1`
        user$param_2 <- input$`summary-distribution-custom-param-2`
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
      input$`summary-distribution-fits` %>% intersect(FIT_TYPES) # filter out custom
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
        rename_with( ~ str_remove_all(.x, "Value_") %>%
                       str_replace_all("_", " ") %>%
                       str_to_title()) %>%
        arrange(Distribution)
    }
  })
}
