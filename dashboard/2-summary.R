summaryUI <- tabItem("summary", h1("Summary"), textOutput("test"))

summaryServer <- function(input, output, session) {
  output$test <- renderText(input$`cost-variable-control`)
}