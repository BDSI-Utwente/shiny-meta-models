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
  # upload data -------------------------------------------------------------
  context$modelFile <- reactiveValues(status = NULL,
                                      initialized = FALSE)
  context$modelData <- reactiveVal()
  context$filteredModelData <- reactive({
    if (scenarioValid()) {
      return(context$modelData() %>% filter(.data[[input$`scenario-variable`]] == input$scenario))
    } else {
      return(context$modelData())
    }
  }) %>% bindEvent(scenarioValid(),
                   context$modelData(),
                   input$scenario,
                   input$`scenario-variable`)
  context$modelVariables <- reactive({
    names(context$modelData())
  })
  scenarioVariableValid <- reactive({
    input$`scenario-variable` != "" &&
      input$`scenario-variable` %in% context$modelVariables()
  })
  scenarioValid <- reactive({
    scenarioVariableValid() &&
      input$scenario != "" &&
      input$scenario != "none" &&
      input$scenario %in% context$scenarios()
  })
  context$scenarios <- reactive({
    if (scenarioVariableValid()) {
      context$modelData() %>%
        pull(!!input$`scenario-variable`) %>%
        unique() %>%
        c(None = "", None = "none", .)
    } else {
      c(None = "")
    }
  })
  
  loadData <- observe({
    file <- input$model_file %>%
      pull(name) %>%
      first()
    path <- input$model_file %>%
      pull(datapath) %>%
      first()
    
    result <- safeRead(path, show_col_types = FALSE)
    
    context$modelFile$message <- as.character(result)
    if (context$modelFile$message == "") {
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
      context$modelData(NULL)
      shinyWidgets::show_toast(glue::glue("Error loading '{name}'", name = file),
                               result$message,
                               "error")
      context$modelFile$initialized <- FALSE
    } else {
      context$modelData(result$result)
      if (context$modelFile$warnings %>% length() > 0) {
        context$modelFile$status <- "warning"
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
        context$modelFile$status <- "success"
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
      context$modelFile$initialized <- TRUE
    }
  }) %>%
    bindEvent(input$model_file)
  
  useExampleData <- observe({
    env <- environment()
    dataSetName <- data(df_pa, envir = env)
    get(dataSetName, envir = env) %>%
      tibble() %>%
      context$modelData()
    context$modelFile$status <- "success"
    context$modelFile$initialized <- TRUE
    shinyWidgets::show_toast(
      glue::glue("Successfully loaded test data."),
      glue::glue(
        "{rows} observations of {cols} variables.",
        rows = nrow(context$modelData()),
        cols = ncol(context$modelData())
      ),
      type = "success"
    )
  }) %>% bindEvent(input$model_file_example)
  
  updateSelectizeChoices <- observe({
    cat("bliep\n")
    set <-
      c(
        "cost-variables",
        "utility-variables",
        "probability-variables",
        "relative-effectiveness-variables",
        "scenario-variable"
      )
    update_exclusive_selectize_input_set(context$modelVariables(), set, input, session)
    
  }) %>% bindEvent(
    context$modelVariables(),
    input$`cost-variables`,
    input$`utility-variables`,
    input$`probability-variables`,
    input$`relative-effectiveness-variables`,
    input$`scenario-variable`
  )
  
  updateTotalCostChoices <- observe({
    updateSelectizeInput(
      session,
      "total-cost-variable-control",
      choices = c("", input$`cost-variables`, context$modelVariables())
    )
    updateSelectizeInput(
      session,
      "total-cost-variable-experimental",
      choices = c("", input$`cost-variables`, context$modelVariables())
    )
  }) %>% bindEvent(input$`cost-variables`, context$modelVariables())
  
  updateTotalUtilityChoices <- observe({
    updateSelectizeInput(
      session,
      "total-utility-variable-control",
      choices = c("", input$`utility-variables`, context$modelVariables())
    )
    updateSelectizeInput(
      session,
      "total-utility-variable-experimental",
      choices = c("", input$`utility-variables`, context$modelVariables())
    )
  }) %>% bindEvent(input$`utility-variables`, context$modelVariables())
  
  updateScenarioChoices <- observe({
    updateSelectizeInput(session, "scenario", choices = context$scenarios())
  }) %>% bindEvent(context$scenarios(), ignoreNULL = FALSE)
  
  output$modelPreview <-
    renderDataTable(context$filteredModelData())
}
