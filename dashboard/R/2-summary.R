library(plotly)
library(pacheck)

source("./functions/cautiously.R")
check_cautiously <- cautiously(do_quick_check)

summaryUI <- tabItem(
  "summary",
  # summary statistics
  box(
    width = 12,
    title = "Select variables",
    selectizeInput("summary-statistics-variables", "Variables", multiple = TRUE, choices = c()),
    p("or select all of..."),
    div(
      class = "flex",
      actionButton("summary-statistics-cost-variables", "Costs"),
      actionButton("summary-statistics-utility-variables", "Utilities"),
      actionButton("summary-statistics-probability-variables", "Probabilities")
    )
  ),
  box(
    width = 12,
    title = "Summary statistics",
    div(
      width = "100%", style = "overflow: auto;",
      tableOutput("summary-statistics")
    )
  ),
  box(
    width = 12,
    title = "Correlation matrix",
    div(
      width = "100%", style = "overflow: auto;",
      plotlyOutput("summary-correlation-matrix")
    )
  ),
  box(
    width = 12,
    title = "Quick checks",
    uiOutput("summary-quick-checks")
  )
)

summaryServer <- function(input, output, session, context) {
  observe({
    updateSelectizeInput(session,
      "summary-statistics-variables",
      choices = context$modelVariables(),
      selected = input$`summary-statistics-variables` %>% intersect(context$modelVariables())
    )
  }) %>% bindEvent(context$modelVariables())

  observe({
    updateSelectizeInput(session,
      "summary-statistics-variables",
      selected = input$`cost-variables`
    )
  }) %>% bindEvent(input$`summary-statistics-cost-variables`)

  observe({
    updateSelectizeInput(session,
      "summary-statistics-variables",
      selected = input$`utility-variables`
    )
  }) %>% bindEvent(input$`summary-statistics-utility-variables`)

  observe({
    updateSelectizeInput(session,
      "summary-statistics-variables",
      selected = input$`probability-variables`
    )
  }) %>% bindEvent(input$`summary-statistics-probability-variables`)

  output$`summary-statistics` <- renderTable({
    if (input$`summary-statistics-variables` %>% length() > 0) {
      generate_sum_stats(context$filteredModelData(), input$`summary-statistics-variables`) %>% suppressWarnings()
    }
  })

  output$`summary-correlation-matrix` <- renderPlotly({
    if (is.null(context$filteredModelData()) || input$`summary-statistics-variables` %>% length() == 0) {
      return()
    }
    correlations <- context$filteredModelData() %>%
      select(!!!input$`summary-statistics-variables`) %>%
      select(where(is.numeric)) %>%
      cor(use = "pair") %>%
      as_tibble(rownames = "x") %>%
      pivot_longer(-x, names_to = "y", values_to = "cor") %>%
      mutate(
        x = factor(x, input$`summary-statistics-variables`),
        y = factor(y, input$`summary-statistics-variables`)
      )
    plot <- correlations %>%
      ggplot(aes(x, y, fill = cor)) +
      geom_raster()
    plot %>% ggplotly()
  }) %>%
    bindCache(context$filteredModelData(), input$`summary-statistics-variables`) %>%
    bindEvent(context$filteredModelData(), input$`summary-statistics-variables`)

  output$`summary-quick-checks` <- renderUI({
    checks <- list(
      prob_pos = pacheck:::do_check(
        context$filteredModelData(),
        input$`probability-variables`,
        ~ .x > 0,
        "greater than zero",
        "all probabilities are {label_check}"
      ),
      prob_lt1 = pacheck:::do_check(
        context$filteredModelData(),
        input$`probability-variables`,
        ~ .x <= 1,
        "less than or equal to one",
        "all probabilities are {label_check}"
      ),
      costs_pos = pacheck:::do_check(
        context$filteredModelData(),
        input$`cost-variables`,
        ~ .x >= 0,
        "positive",
        "all costs are {label_check}"
      ),
      util_pos = pacheck:::do_check(
        context$filteredModelData(),
        input$`utility-variables`,
        ~ .x >= 0,
        "positive",
        "all utilities are {label_check}"
      )
    )
    
    msgs <- checks %>% 
      map_dfr("messages") %>%
      rowwise() %>%
      mutate(html = list(div(class = ifelse(ok, "text-success", "text-danger"), message)))
    msgs %>% pull( html )
  })
}