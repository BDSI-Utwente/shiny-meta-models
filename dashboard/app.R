library(shiny)
library(tidyverse)

options(shiny.autoreload = TRUE)

ui <- dashboardPage(
  title = "PACHBOARD",
  dark = FALSE,
  dashboardHeader(
    title = "PACHBOARD",
    actionButton("setup-test-data", "Use test data", status = "info", size = "sm", class = "ml-auto mr-0")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(

        "Welcome",
        tabName = "welcome",
        icon = icon("magic")
      ),
      menuItem(
        "Prepare data",
        tabName = "data",
        icon = icon("upload")
      ),
      menuItem(
        "Summary statistics",
        tabName = "summary",
        icon = icon("search"),
        selected = TRUE
      ),
      menuItem(
        "Model outcomes",
        tabName = "outcomes",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Relations",
        tabName = "relations",
        icon = icon("chart-line")
      ),
      menuItem(
        "Meta-model predictions",
        tabName = "predictions",
        icon = icon("calendar")
      )
    )
  ),
  dashboardBody(tabItems(welcomeUI, dataUI, summaryUI, outcomesUI, relationsUI, predictionsUI))
)

server <- function(input, output, session) {
  context <- environment()
  dataServer(input, output, session, context)
  summaryServer(input, output, session, context)

  ## test data binding ----
  observe({
    env <- environment()
    dataSetName <- data(df_pa, envir = env)
    context$model$data <- tibble(get(dataSetName, envir = env))
    context$model$file$status <- "success"
    context$model$file$initialized <- TRUE
    
    # set variables
    context$model$cost_variables <- context$model$data %>% dplyr::select(starts_with("c_")) %>% names()
    updateSelectizeInput(session, "cost-variables", selected = context$model$cost_variables)
    
    context$model$utility_variables <- context$model$data %>% dplyr::select(starts_with("u_")) %>% names()
    updateSelectizeInput(session, "utility-variables", selected = context$model$utility_variables)
    
    context$model$probability_variables <- context$model$data %>% dplyr::select(starts_with("p_")) %>% names()
    updateSelectizeInput(session, "probability-variables", selected = context$probability_variables)
    
  }) %>% bindEvent(input$`setup-test-data`)
}

# Run the application
# if (interactive()) {
  shinyApp(ui, server)
# } 
