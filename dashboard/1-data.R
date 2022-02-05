library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(pacheck)
library(purrr)
library(readr)
library(glue)
library(pacheck)

source("functions/cautiously.R")
safeRead <- cautiously(read_csv)

dataUI <- tabItem(
  # upload data -------------------------------------------------------------
  "data",
  box(
    title = "Upload data",
    status = "primary",
    width = 12,
    fileInput("model_file",
      "Model file",
      accept = c(
        ".csv",
        # ".xls",
        # ".xlsx",
        # ".rdata",
        # ".rds",
        # "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        # # is apparently also used for csv - confusing!
        # "application/vnd.ms-excel"
        "text/csv"
      )
    ),
    actionButton("model_file_example", "Use test data")
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
    status = "primary",
    width = 12,
    p(
      class = "text-muted",
      "Please mark variables as a cost, utility or probability to aid further analysis"
    ),
    selectizeInput("cost-variables", "Costs", choices = c("loading..."), multiple = TRUE),
    selectizeInput("utility-variables", "Utilities", choices = c("loading..."), multiple = TRUE),
    selectizeInput("probability-variables", "Probabilities", choices = c("loading..."), multiple = TRUE),
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
    p(class = "text-muted", "If multiple scenarios are present and a variable for scenarios has selected, please select the scenario."),
    selectizeInput("scenario", "Scenario", choices = c())
  ),

  # calculate net benefits
  box(
    title = "Calculate net benefits",
    status = "primary",
    width = 12,
    span(
      class = "text-muted",
      "Please select variables representing total costs and utility for the control and experimental condition respectively."
    ),
    fluidRow(
      column(
        6,
        h4("Total costs"),
        selectizeInput(
          "total-cost-variable-control",
          "Control",
          choices = c("loading...")
        ),
        selectizeInput(
          "total-cost-variable-experimental",
          "Experimental",
          choices = c("loading...")
        )
      ),
      column(
        6,
        h4("Total utility"),
        selectizeInput(
          "total-utility-variable-control",
          "Control",
          choices = c("loading...")
        ),
        selectizeInput(
          "total-utility-variable-experimental",
          "Experimental",
          choices = c("loading...")
        )
      )
    ),
    actionButton(
      "calculate-net-benefits",
      "Calculate net benefits",
      icon("calculator"),
      class = "btn-primary"
    )
  ),

  # preview
  box(
    title = "Data Preview",
    width = 12,
    dataTableOutput("modelPreview")
  )
)

dataServer <- function(input, output, session) {
  # upload data -------------------------------------------------------------
  modelFile <- reactiveValues(
    status = NULL,
    initialized = FALSE
  )
  modelData <- reactiveVal()
  modelVariables <- reactive({
    names(modelData())
  })
  scenarios <- reactive({
    if (modelFile$initialized &&
      input$`scenario-variable` != "" &&
      (input$`scenario-variable` %in% modelVariables())) {
      modelData() %>%
        pull(!!input$`scenario-variable`) %>%
        unique()
    } else {
      c()
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

    modelFile$message <- as.character(result)
    if (modelFile$message == "") {
      print("success")
      shinyWidgets::show_toast(
        glue::glue("Succesfully loaded '{name}'", name = file),
        glue::glue("{rows} observations of {cols} variables.",
          rows = nrow(result$result), cols = ncol(result$result)
        ),
        "success"
      )
    }

    if (!is.null(result$error)) {
      modelData(NULL)
      shinyWidgets::show_toast(
        glue::glue("Error loading '{name}'", name = file),
        result$message,
        "error"
      )
      modelFile$initialized <- FALSE
    } else {
      modelData(result$result)
      if (modelFile$warnings %>% length() > 0) {
        modelFile$status <- "warning"
        shinyWidgets::show_toast(
          glue::glue("Loaded '{name}' with warnings", name = file),
          glue::glue("{rows} observations of {cols} variables. \n\n{warnings}",
            rows = nrow(result$result), cols = ncol(result$result), warnings = result$message
          ),
          "warning"
        )
      } else {
        modelFile$status <- "success"
        shinyWidgets::show_toast(
          glue::glue("Successfully loaded '{name}'", name = file),
          glue::glue("{rows} observations of {cols} variables. \n\n{message}",
            rows = nrow(result$result), cols = ncol(result$result), message = result$message
          ),
          "success"
        )
      }
      modelFile$initialized <- TRUE
    }
  }) %>%
    bindEvent(input$model_file)

  useExampleData <- observe({
    env <- environment()
    dataSetName <- data(df_pa, envir = env)
    get(dataSetName, envir = env) %>%
      tibble() %>%
      modelData()
    modelFile$status <- "success"
    shinyWidgets::show_toast(
      glue::glue("Successfully loaded test data."),
      glue::glue("{rows} observations of {cols} variables.",
        rows = nrow(modelData()), cols = ncol(modelData())
      )
    )
  }) %>%
    bindEvent(input$model_file_example)

  updateSelectizeChoices <- observe({
    if (!modelFile$initialized) {
      return()
    }

    print("updating")

    # categorize
    updateSelectizeInput(session, "cost-variables", choices = modelVariables())
    updateSelectizeInput(session, "utility-variables", choices = modelVariables())
    updateSelectizeInput(session, "probability-variables", choices = modelVariables())
    updateSelectizeInput(session, "scenario-variable", choices = modelVariables())

    # net benefits
    updateSelectizeInput(session, "total-costs-variable-control", choices = modelVariables())
    updateSelectizeInput(session, "total-costs-variable-experimental", choices = modelVariables())
    updateSelectizeInput(session, "total-utility-variable-control", choices = modelVariables())
    updateSelectizeInput(session, "total-utility-variable-control", choices = modelVariables())
  }) %>% bindEvent(modelData(), ignoreNULL = FALSE, ignoreInit = TRUE)

  updateScenarioChoices <- observe({
    updateSelectizeInput(session, "scenario", choices = scenarios())
  }) %>% bindEvent(scenarios(), ignoreNULL = FALSE, ignoreInit = TRUE)

  output$modelPreview <- renderDataTable(modelData())
}