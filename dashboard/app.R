library(shiny)
library(tidyverse)

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

  observe({
    env <- environment()
    dataSetName <- data(df_pa, envir = env)
    get(dataSetName, envir = env) %>%
      tibble() %>%
      context$modelData()
    context$modelFile$status <- "success"
    context$modelFile$initialized <- TRUE

    updateSelectizeInput(session, "cost-variables", selected = modelData() %>% select(starts_with("c_"), contains("cost")) %>% names())
    updateSelectizeInput(session, "utility-variables", selected = modelData() %>% select(starts_with("u_"), contains("qaly")) %>% names())
    updateSelectizeInput(session, "probability-variables", selected = modelData() %>% select(starts_with("p_")) %>% names())
  }) %>% bindEvent(input$`setup-test-data`)
}

# Run the application
if (interactive()) {
  shinyApp(ui, server)
}