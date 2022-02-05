
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

source("./0-welcome.R")
source("./1-data.R")
source("./2-summary.R")
source("./3-outcomes.R")
source("./4-relations.R")
source("./5-predictions.R")

test_data <- data(df_pa)

ui <- dashboardPage(
  dashboardHeader(title = "PACHBOARD"),
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
        icon = icon("upload"),
        selected = TRUE
      ),
      menuItem(
        "Summary statistics",
        tabName = "summary",
        icon = icon("search")
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
  dataServer(input, output, session)
  summaryServer(input, output, session)
}

# Run the application
if (interactive()) {
  shinyApp(ui, server)
}