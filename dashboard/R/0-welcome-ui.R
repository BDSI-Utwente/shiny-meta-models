library(bs4Dash)
library(shiny)

welcomeUI <- tabItem(
  "welcome",
  box(
    title = "Welcome to the PASHBOARD",
    status = "primary",
    width = 12,
    p("There will be some explanation here"),
    p("lorem ipsum")
  )
)