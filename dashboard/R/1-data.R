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
    title = "Categorize variables",
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
  
  # choose scenario
  box(
    title = "Choose scenario",
    width = 12,
    p(
      class = "text-muted",
      "If multiple scenarios are present and a variable for scenarios has selected, please select the scenario."
    ),
    selectizeInput("scenario", "Scenario", choices = c())
  ),
  
  # calculate net benefits
  box(
    title = "Calculate net benefits",
    width = 12,
    span(
      class = "text-muted",
      "Please select variables representing total costs and utility for the comparator and intervention conditions respectively."
    ),
    fluidRow(
      column(
        6,
        h4("Total costs"),
        selectizeInput(
          "total-cost-variable-control",
          "Comparator",
          choices = c("loading...")
        ),
        selectizeInput(
          "total-cost-variable-experimental",
          "Intervention",
          choices = c("loading...")
        )
      ),
      column(
        6,
        h4("Total utility"),
        selectizeInput(
          "total-utility-variable-control",
          "Comparator",
          choices = c("loading...")
        ),
        selectizeInput(
          "total-utility-variable-experimental",
          "Intervention",
          choices = c("loading...")
        )
      )
    ),
    div(class = "callout callout-warning", "TODO: Not yet implemented"),
    actionButton(
      "calculate-net-benefits",
      "Calculate net benefits",
      icon("calculator"),
      status = "primary",
      disabled = TRUE
    )
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
    file = reactiveValues(
      status = NULL,
      initialized = FALSE
    ),
    variables = c(),
    cost_variables = c(),
    utility_variables = c(),
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
    if(scenarioVariableValid()) {
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
  
  updateProbabilityVariables <- observe({
    context$model$probability_variables <- input$`probability-variables`
  }) %>% bindEvent(input$`probability-variables`)
  
  updateRelativeEffectivenessVariables <- observe({
    context$model$relative_effectiveness_variables <- input$`relative-effectiveness-variables`
  }) %>% bindEvent(input$`relative-effectiveness-variables`)
  
  updateScenarioVariable <- observe({
    context$model$scenario_variable <- input$`scenario-variable`
  }) %>% bindEvent(input$`scenario-variable`)
  
  selected_variables <- reactive({
    c(
      input$`cost-variables`,
      context$model$utility_variables,
      context$model$probability_variables,
      context$model$relative_effectiveness_variables,
      context$model$scenario_variable
    )
  }) %>% debounce(1000)
  
  updateSelectizeChoices <- observe({
    set <- c(
      "cost-variables",
      "utility-variables",
      "probability-variables",
      "relative-effectiveness-variables",
      "scenario-variable"
    )
    update_exclusive_selectize_input_set(context$model$variables, set, input, session)
    
  }, priority = 50) %>% bindEvent(context$model$variables, selected_variables())
  
  updateTotalCostChoices <- observe({
    updateSelectizeInput(
      session,
      "total-cost-variable-control",
      choices = c("", context$model$cost_variables, context$model$variables)
    )
    updateSelectizeInput(
      session,
      "total-cost-variable-experimental",
      choices = c("", context$model$cost_variables, context$model$variables)
    )
  }) %>% bindEvent(context$model$cost_variables, context$model$variables)
  
  updateTotalUtilityChoices <- observe({
    updateSelectizeInput(
      session,
      "total-utility-variable-control",
      choices = c("", context$model$utility_variables, context$model$variables)
    )
    updateSelectizeInput(
      session,
      "total-utility-variable-experimental",
      choices = c("", context$model$utility_variables, context$model$variables)
    )
  }) %>% bindEvent(context$model$utility_variables, context$model$variables)
  
  updateScenarioChoices <- observe({
    updateSelectizeInput(session, "scenario", choices = context$model$scenarios)
  }) %>% bindEvent(context$model$scenarios, ignoreNULL = FALSE)
  
  output$modelPreview <-
    renderDataTable(context$model$data_filtered())
}
