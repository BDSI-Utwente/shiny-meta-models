library(shiny)
library(tidyverse)

options(
  shiny.autoreload = TRUE,
  shiny.maxRequestSize = 30 * 1024 ^ 2
)

ui <- dashboardPage(
  title = "PACBOARD",
  dark = FALSE,
  dashboardHeader(
    title = "PACBOARD",
    tags$link(href = "style.css", rel="stylesheet"),
    actionButton(
      "setup-test-data",
      "Use test data",
      status = "info",
      size = "sm",
      class = "ml-auto mr-0"
    ),
    div(id = "test-data-loading")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome",
               tabName = "welcome",
               icon = icon("magic")),
      menuItem("Prepare data",
               tabName = "data",
               icon = icon("upload")),
      menuItem(
        "Data inspection",
        tabName = "summary",
        icon = icon("search")
      ),
      menuItem(
        "Model outcomes",
        tabName = "outcomes",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Metamodelling",
        tabName = "relations",
        icon = icon("chart-line")
      ),
      menuItem(
        "Metamodel predictions",
        tabName = "predictions",
        icon = icon("calendar")
      ),
      menuItem(
        "Survival analysis",
        tabName = "survival",
        icon = icon("chart-line")
      )
    )
  ),
  dashboardBody(
    tabItems(
      welcomeUI,
      dataUI,
      summaryUI,
      outcomesUI,
      relationsUI,
      predictionsUI,
      survivalUI
    )
  )
)

server <- function(input, output, session) {
  context <- environment()
  dataServer(input, output, session, context)
  summaryServer(input, output, session, context)
  outcomesServer(input, output, session, context)
  relationsServer(input, output, session, context)
  predictionsServer(input, output, session, context)
  survivalServer(input, output, session, context)
  
  ## test data binding ----
  observe({
    shiny::insertUI("#test-data-loading", ui = icon("spinner", class = "loading fa-spin ml-2"), immediate = TRUE)
    
    env <- environment()
    dataSetName <- data(df_pa, envir = env)
    context$model$data <- tibble(get(dataSetName, envir = env))
    context$model$file$status <- "success"
    context$model$file$initialized <- TRUE
    
    # set variables
    context$model$cost_variables <-
      context$model$data %>% dplyr::select(starts_with("c_")) %>% names()
    updateSelectizeInput(session,
                         "cost-variables",
                         selected = context$model$cost_variables)
    
    context$model$utility_variables <-
      context$model$data %>% dplyr::select(starts_with("u_")) %>% names()
    updateSelectizeInput(session,
                         "utility-variables",
                         selected = context$model$utility_variables)
    
    context$model$probability_variables <-
      context$model$data %>% dplyr::select(starts_with("p_")) %>% names()
    updateSelectizeInput(session,
                         "probability-variables",
                         selected = context$probability_variables)
    
    context$relations$outcome_variable <- "inc_qaly"
    context$relations$predictor_variables <- c(context$model$utility_variables, context$model$probability_variables) %>% setdiff(c("u_d", "p_dd"))
    
    shiny::removeUI("#test-data-loading .loading")
    shiny::insertUI("#test-data-loading", ui = icon("check", style="color: green;", class="ml-2"), immediate = FALSE)
    
  }, label = "test-data-observer") %>% bindEvent(input$`setup-test-data`)
}

# Run the application
# if (interactive()) {
shinyApp(ui, server)
# }
