library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(pacheck)
library(purrr)
library(readr)
library(vroom)
library(glue)
library(pacheck)

source("functions/cautiously.R")
safeRead <- cautiously(vroom)

dataUI <- tabItem(
  # upload data -------------------------------------------------------------
  "data",
  box(
    title = "Upload data",
    width = 12,
    fileInput("model_file",
              "Model file",
              accept = c(".csv",
                         # ".xls",
                         # ".xlsx",
                         # ".rdata",
                         # ".rds",
                         # "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                         # # is apparently also used for csv - confusing!
                         # "application/vnd.ms-excel"
                         "text/csv"))
  ),
  
  # # select variables
  # box(
  #     title = "Drop variables",
  #     status = "danger",
  #     width = 12,
  #     selectizeInput("drop-variable",
  #                    "Variable to drop",
  #                    choices = c("loading...")),
  #     actionButton(
  #         "drop-variable-button",
  #         "Drop",
  #         icon = icon("trash"),
  #         class = "btn-danger"
  #     )
  # ),
  
  # categorize variables
  box(
    title = "Categorize input variables",
    width = 12,
    p(
      class = "text-muted",
      "Please mark variables as a cost, utility or probability to aid further analysis"
    ),
    selectizeInput(
      "cost-variables",
      "Costs",
      choices = c("loading..."),
      multiple = TRUE
    ),
    selectizeInput(
      "utility-variables",
      "Utilities",
      choices = c("loading..."),
      multiple = TRUE
    ),
    selectizeInput(
      "disutility-variables",
      "Disutilities (when implemented as negative values, otherwise consider disutilities as utility variables)",
      choices = c("loading..."),
      multiple = TRUE
    ),
    selectizeInput(
      "probability-variables",
      "Probabilities",
      choices = c("loading..."),
      multiple = TRUE
    ),
    selectizeInput(
      "relative-effectiveness-variables",
      "Relative effectiveness (RR, OR, HR)",
      choices = c("loading..."),
      multiple = TRUE
    ),
    p(
      class = "text-muted",
      "If there are multiple scenarios, please select the variable that identifies the scenario."
    ),
    selectizeInput("scenario-variable", "Scenario", choices = c("loading..."))
  ),
  
  ## outcome vars ----
  box(
    title = "Select outcome variables",
    width = 12,
    span(
      class = "text-muted",
      "Please select variables representing total and incremental costs and effects for both the comparator and intervention conditions."
    ),
    # fluidRow(
    #   column(
    #     6,
    h4("Intervention"),
    fluidRow(column(
      6,
      ### selectize/outcomes-intervention-total-costs ----
      selectizeInput(
        "outcomes-intervention-total-costs",
        "Total costs",
        choices = c("no data loaded..." = "")
      )
    ),
    column(
      6,
      ### selectize/outcomes-intervention-total-effects ----
      selectizeInput(
        "outcomes-intervention-total-effects",
        "Total effects",
        choices = c("no data loaded..." = "")
      )
    )),
    fluidRow(column(
      6,
      ### selectize/outcomes-intervention-incremental-costs ----
      selectizeInput(
        "outcomes-intervention-incremental-costs",
        "Incremental costs",
        choices = c("no data loaded..." = "")
      )
    ),
    column(
      6,
      ### selectize/outcomes-intervention-incremental-effects ----
      selectizeInput(
        "outcomes-intervention-incremental-effects",
        "Incremental effects",
        choices = c("no data loaded..." = "")
      )
    )),
    # ),
    # column(
    #   6,
    h4("Comparator"),
    fluidRow(column(
      6,
      ### selectize/outcomes-comparator-total-costs ----
      selectizeInput(
        "outcomes-comparator-total-costs",
        "Total costs",
        choices = c("no data loaded..." = "")
      )
    ),
    column(
      6,
      ### selectize/outcomes-comparator-total-effects ----
      selectizeInput(
        "outcomes-comparator-total-effects",
        "Total effects",
        choices = c("no data loaded..." = "")
      )
    )) 
  ),
  
  ## choose scenario ----
  conditionalPanel(
    "input['scenario-variable'] != ''",
    box(
      title = "Choose scenario",
      width = 12,
      selectizeInput("scenario", "Scenario", choices = c())
    )
  ),
  
  ## ui/summary-quick-checks ----
  box(
    width = 12,
    title = "Quick checks",
    uiOutput("summary-quick-checks")
  ),
  
  # preview
  box(
    title = "Data Preview",
    width = 12,
    div(style = "overflow: auto;",
        dataTableOutput("modelPreview"))
  )
)

dataServer <- function(input, output, session, context) {
  context$model <- reactiveValues(
    file = reactiveValues(status = NULL,
                          initialized = FALSE),
    variables = c(),
    cost_variables = c(),
    utility_variables = c(),
    disutility_variables = c(),
    probability_variables = c(),
    relative_effect_variables = c(),
    scenario_variable = "",
    scenarios = c(),
    scenario = ""
  )
  
  # upload data -------------------------------------------------------------
  context$model$file <- reactiveValues(status = NULL,
                                       initialized = FALSE)
  
  context$model$data_filtered <- reactive({
    if (scenarioValid()) {
      return(context$model$data %>% filter(.data[[context$model$scenario_variable]] == input$scenario))
    } else {
      return(context$model$data)
    }
  }) %>% bindEvent(scenarioValid(), context$model$data)
  
  scenarioVariableValid <- reactive({
    context$model$scenario_variable != "" &&
      context$model$scenario_variable %in% context$model$variables
  })
  
  updateScenario <- observe({
    context$model$scenario <- input$scenario
  }) %>% bindEvent(input$scenario)
  
  updateScenarios <- observe({
    if (scenarioVariableValid()) {
      context$model$scenarios <-
        context$model$data %>%
        pull(!!context$model$scenario_variable) %>%
        unique() %>% c(None = "", None = "none", .)
    } else {
      context$model$scenarios <- c(None = "")
    }
  })
  
  scenarioValid <- reactive({
    scenarioVariableValid() &&
      context$model$scenario != "" &&
      context$model$scenario != "none" &&
      context$model$scenario %in% context$model$scenarios
  })
  
  loadData <- observe({
    file <- input$model_file %>%
      pull(name) %>%
      first()
    path <- input$model_file %>%
      pull(datapath) %>%
      first()
    
    result <- safeRead(path, show_col_types = FALSE)
    
    context$model$file$message <- as.character(result)
    if (context$model$file$message == "") {
      print("success")
      shinyWidgets::show_toast(
        glue::glue("Succesfully loaded '{name}'", name = file),
        glue::glue(
          "{rows} observations of {cols} variables.",
          rows = nrow(result$result),
          cols = ncol(result$result)
        ),
        "success"
      )
    }
    
    if (!is.null(result$error)) {
      context$model$data <- NULL
      shinyWidgets::show_toast(glue::glue("Error loading '{name}'", name = file),
                               result$message,
                               "error")
      context$model$file$initialized <- FALSE
    } else {
      context$model$data <- result$result
      if (context$model$file$warnings %>% length() > 0) {
        context$model$file$status <- "warning"
        shinyWidgets::show_toast(
          glue::glue("Loaded '{name}' with warnings", name = file),
          glue::glue(
            "{rows} observations of {cols} variables. \n\n{warnings}",
            rows = nrow(result$result),
            cols = ncol(result$result),
            warnings = result$message
          ),
          "warning"
        )
      } else {
        context$model$file$status <- "success"
        shinyWidgets::show_toast(
          glue::glue("Successfully loaded '{name}'", name = file),
          glue::glue(
            "{rows} observations of {cols} variables. \n\n{message}",
            rows = nrow(result$result),
            cols = ncol(result$result),
            message = result$message
          ),
          "success"
        )
      }
      context$model$file$initialized <- TRUE
    }
  }) %>%
    bindEvent(input$model_file)
  
  updateModelVariables <- observe({
    context$model$variables <- names(context$model$data)
  }) %>% bindEvent(context$model$data)
  
  updateCostVariables <- observe({
    context$model$cost_variables <- input$`cost-variables`
  }) %>% bindEvent(input$`cost-variables`)
  
  updateUtilityVariables <- observe({
    context$model$utility_variables <- input$`utility-variables`
  }) %>% bindEvent(input$`utility-variables`)
  
  updateDisutilityVariables <- observe({
    context$model$disutility_variables <- input$`disutility-variables`
  }) %>% bindEvent(input$`disutility-variables`)
  
  updateProbabilityVariables <- observe({
    context$model$probability_variables <- input$`probability-variables`
  }) %>% bindEvent(input$`probability-variables`)
  
  updateRelativeEffectivenessVariables <- observe({
    context$model$relative_effectiveness_variables <-
      input$`relative-effectiveness-variables`
  }) %>% bindEvent(input$`relative-effectiveness-variables`)
  
  updateScenarioVariable <- observe({
    context$model$scenario_variable <- input$`scenario-variable`
  }) %>% bindEvent(input$`scenario-variable`)
  
  context$model$selected_variables <- reactive({
    c(
      context$model$cost_variables,
      context$model$utility_variables,
      context$model$disutility_variables,
      context$model$probability_variables,
      context$model$relative_effectiveness_variables,
      context$model$scenario_variable
    )
  }) %>% debounce(1000)
  
  updateSelectizeChoices <- observe({
    set <- c(
      "cost-variables",
      "utility-variables",
      "disutility-variables",
      "probability-variables",
      "relative-effectiveness-variables",
      "scenario-variable"
    )
    selected <- list(
      `cost-variables` = context$model$cost_variables,
      `utility-variables` = context$model$utility_variables,
      `disutility-variables` = context$model$disutility_variables,
      `probability-variables` = context$model$probability_variables,
      `relative-effectiveness-variables` = context$model$relative_effectiveness_variables,
      `scenario-variable` = context$model$scenario_variable
    )
    update_exclusive_selectize_input_set(context$model$variables, set, selected, session)
    
  }, priority = 50) %>% bindEvent(context$model$variables, context$model$selected_variables())
  
  updateScenarioChoices <- observe({
    updateSelectizeInput(session, "scenario", choices = context$model$scenarios)
  }) %>% bindEvent(context$model$scenarios, ignoreNULL = FALSE)
  
  ### ui/summary-quick-checks ----
  output$`summary-quick-checks` <- renderUI({
    checks <- list()
    
    if (context$model$probability_variables %>% length()) {
      checks$prob_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$probability_variables,
        ~ .x > 0,
        "greater than zero",
        "all probabilities are {label_check}"
      )
      checks$prob_lt1 <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$probability_variables,
        ~ .x <= 1,
        "less than or equal to one",
        "all probabilities are {label_check}"
      )
    } else {
      checks$prob <- list(messages = tibble(ok = FALSE,
                                            message = "no variables were marked as probabilities"))
    }
    
    if (context$model$cost_variables %>% length()) {
      checks$costs_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$cost_variables,
        ~ .x >= 0,
        "positive",
        "all costs are {label_check}"
      )
    } else {
      checks$costs <-
        list(messages = tibble(ok = FALSE, message = "no variables were marked as costs"))
    }
    
    if (context$model$utility_variables %>% length()) {
      checks$util_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$utility_variables,
        ~ .x >= 0,
        "positive",
        "all utilities are {label_check}"
      )
    } else {
      checks$utilities <-
        list(messages = tibble(ok = FALSE, message = "no variables were marked as utilities"))
    }
    
    if (context$model$disutility_variables %>% length()) {
      checks$util_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$disutility_variables,
        ~ .x < 0,
        "negative",
        "all disutilities are {label_check}"
      )
    } else {
      checks$disutilities <-
        list(messages = tibble(ok = FALSE, message = "no variables were marked as disutilities"))
    }
    
    if (context$model$relative_effectiveness_variables %>% length()) {
      checks$rel_eff_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$relative_effectiveness_variables,
        ~ .x >= 0,
        "positive",
        "all RR, OR, and HR are {label_check}"
      )
    } else {
      checks$relative_effectiveness <-
        list(messages = tibble(ok = FALSE, message = "no variables were marked as RR, OR, HR"))
    }
    
    msgs <- checks %>%
      map_dfr("messages") %>%
      rowwise() %>%
      mutate(html = list(div(
        class = ifelse(ok, "text-success", "text-danger"),
        icon(ifelse(ok, "check", "warning"), verify_fa = FALSE),
        message
      )))
    msgs %>% pull(html)
  })
  
  output$modelPreview <-
    renderDataTable(context$model$data_filtered())
}
