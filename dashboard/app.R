#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(pacheck)
library(purrr)
library(readr)
library(glue)

source("functions/cautiously.R")

safe_read <- cautiously(read_csv)

ui <- shinyUI(dashboardPage(
    dashboardHeader(title = "PASHBOARD"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Upload data", tabName = "upload", icon = icon("upload")),
            menuItem(
                "Investigate model inputs",
                tabName = "inputs",
                icon = icon("search")
            ),
            menuItem(
                "Investigate model inputs & outputs",
                tabName = "outputs",
                icon = icon("chart-bar")
            ),
            menuItem(
                "Investigate relations",
                tabName = "relations",
                icon = icon("chart-line")
            ),
            menuItem(
                "Metamodel predictions",
                tabName = "predictions",
                icon = icon("calendar")
            )
        )
    ),
    dashboardBody(tabItems(tabItem(
# upload data -------------------------------------------------------------
        "upload",
        box(
            title = "Upload data",
            status = "primary",
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
                                 "text/csv")),
            actionButton("model_file_example", "Use test data"),
            htmlOutput("model_file_message")
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

# calculate net benefits
        box(
            title = "Calculate net benefits",
            status = "primary",
            width = 12,
            fluidRow(
                column(
                    6,
                    h4("Total costs"),
                    span(
                        style = "font-size: 90%; color: #333; font-style: italic;",
                        "Please select variables representing total costs for both the control and experimental condition."
                    ),
                    selectizeInput("cost-variable-control",
                                   "Control",
                                   choices = c("loading...")),
                    selectizeInput(
                        "cost-variable-experimental",
                        "Experimental",
                        choices = c("loading...")
                    )
                ),
                column(
                    6,
                    h4("Total effects"),
                    span(
                        style = "font-size: 90%; color: #333; font-style: italic;",
                        "Please select variables representing total effects for both the control and experimental condition."
                    ),
                    selectizeInput("effect-variable-control",
                                   "Control",
                                   choices = c("loading...")),
                    selectizeInput(
                        "effect-variable-experimental",
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
            dataTableOutput("model_preview")
        )
    )))
))

server <- function(input, output) {
  # upload data -------------------------------------------------------------
  model_data <- reactiveVal()
  model_file <- reactiveValues(
  message = NULL,
  status = NULL,
  name = NULL,
  initialized = FALSE
  )

  loadData <- observe({
    file <- input$model_file %>% pull(name) %>% first()
    path <- input$model_file %>% pull(datapath) %>% first()

    result <- safe_read(path, show_col_types = FALSE)

    model_file$message <- as.character(result)
    print(result)
    if (model_file$message == "") {
      model_file$message <- glue("Succesfully loaded '{name}'.\n{rows} observations of {cols} variables.", name = file, rows = nrow(result$result), cols = ncol(result$result))
    }

    if (!is.null(result$error)) {
      model_data(NULL)
      model_file$status <- "danger"
      model_file$initialized <- FALSE
    } else {
      model_data(result$result)
      model_file$status <- ifelse(model_file$warnings %>% length() >= 0, "warning", "success")
      model_file$initialized <- TRUE
    }
  }) %>% bindEvent(input$model_file)

  useExampleData <- observe({
    env <- environment()
    dataSetName <- data(df_pa, envir = env)
    get(dataSetName, envir = env) %>% tibble() %>% model_data()
    model_file$status <- "success"
    model_file$message <- glue("Successfully loaded test data.\n{rows} observations of {cols} variables.", rows = nrow(model_data()), cols = ncol(model_data()))
  }) %>% bindEvent(input$model_file_example)

  output$model_preview <- renderDataTable(model_data())
  output$model_file_message <- renderPrint(div(model_file$message, class = paste(model_file$status, "box")))
}

# Run the application
shinyApp(ui = ui, server = server)
