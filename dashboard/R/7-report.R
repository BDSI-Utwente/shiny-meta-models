# source("functions/render_report.R")
# Copy report to temporary directory. This is mostly important when
# deploying the app, since often the working directory won't be writable
report_path <- tempfile(fileext = ".Rmd")
file.copy("report.Rmd", report_path, overwrite = TRUE)

render_report <- function(input, output, params) {
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}
# UI ----
downloadUI <- tabItem(
  "report",
  box(
    width = 12,
    title = "Download report",
    p(
      "In this box, you can download a report contening the performed validation checks and the different plots you have generated.",
      br(),
      "Select which elements you would like to include in the generated report by using the checkboxes below.",
      br(),
      "The variables included in the reports are the ones selected as inputs and outputs under the ", 
      strong("Prepare data-tab")
      ),
    column(width = 12, 
           shiny::textInput("person", "Please write your name", value = ""),
           shiny::checkboxInput("summary_stats_choice",
                                "Summary statistics of uploaded dataset",
                                value = TRUE),
           shiny::checkboxInput("corr_matrix_choice",
                                "Correlation matrix of uploaded dataset",
                                value = TRUE),
           shiny::checkboxInput("validation_checks_choice",
                                "Validation checks",
                                value = TRUE),
           shiny::checkboxInput("ce_plot_choice",
                                "Cost-effectiveness plane",
                                value = TRUE),
           shiny::checkboxInput("ice_plot_choice",
                                "Incremental cost-effectiveness plane",
                                value = TRUE),
           fluidRow(
             column(
               width = 3,
               shiny::numericInput(
                 "report-willingness-to-pay",
                 "Willingness-to-pay to use in report",
                 value = NULL,
                 min = 0,
                 max = Inf
               )
             ),
             column(
               width = 3,
               selectizeInput(
                 "report-n-it",
                 "Iterations to highlight in incremental cost effectiveness plot",
                 multiple = TRUE,
                 choice = c()
               )
             ),
             column(
               width = 3,
               selectizeInput(
                 "report-ice-colour",
                 label = "Colour ICE plane by",
                 multiple = FALSE,
                 choices = c()
               )
             ),
             column(
               width = 3,
               selectInput(
                 "report-currency",
                 label = "Currency",
                 choices = c(
                   "Euros" = "euro",
                   "US Dollars" = "dollar",
                   "Yen" = "yen",
                   "None" = "none"
                 ),
                 selected = "Euro"
               )
             )
           ), 
           shiny::checkboxInput("ice_summary_choice",
                                "Summary incremental cost-effectiveness plane",
                                value = TRUE),
           shiny::checkboxInput("ceac_plot_choice",
                                "Cost-effectiveness acceptability curve",
                                value = TRUE),
           fluidRow(
             column(width = 4,
                    shiny::numericInput(
                      "report-ceac-min",
                      "Minimum value willingness-to-pay for CEAC",
                      value = 0,
                      min = 0,
                      max = Inf,
                      step = 1000
                    )
             ),
             column(width = 4,
                    shiny::numericInput(
                      "report-ceac-max",
                      "Maximum value willingness-to-pay for CEAC",
                      value = 100000,
                      min = 0,
                      max = Inf,
                      step = 1000
                    )
             ),
             column(width = 4,
                    shiny::numericInput(
                      "report-ceac-step",
                      "Steps willingness-to-pay for CEAC",
                      value = 5000,
                      min = 0,
                      max = Inf,
                      step = 1000
                    )
             )
           )
           ),
    column(width = 12,
           conditionalPanel(
             "input['model_file'] != ''",
             downloadButton("downloadreport", 
                            label = "Download report",
                            class = "btn-block")
           )
           
           )
  )
  )

# SERVER ----
downloadServer <- function(input, output, session, context) {

  ## Update report parameters (using parameters defined in dashboard) ----
  UpdateChoices <- observe({
    ### selectizeInput/outcomes-iterations-highlight ----
    updateSelectizeInput(
      session,
      "report-n-it",
      choices = 1:nrow(context$model$data_filtered()),
      selected = c(),
      server = TRUE
    )
    ### selectizeInput/outcomes-ice-plane-colour-variable ----
    updateSelectizeInput(
      session,
      "report-ice-colour",
      choices = context$model$variables,
      selected = input$`report-ice-colour`
    )
  }) %>% bindEvent(context$model$variables)
  
  ### update/report-willingness-to-pay ----
  observeEvent(input$`outcomes-willingness-to-pay`, {
    val <- input$`outcomes-willingness-to-pay`
    shiny::updateNumericInput(session,
                              "report-willingness-to-pay",
                              value = val)
  })
  
  ### update/report-n-it ----
  observeEvent(input$`outcomes-iterations-highlight`, {
    val <- input$`outcomes-iterations-highlight`
    shiny::updateSelectizeInput(session,
                                "report-n-it",
                                choices = 1:nrow(context$model$data_filtered()),
                                selected = val)
  })
  ### update/report-ice-colour ----
  observeEvent(input$`outcomes-ice-plane-colour-variable`, {
    val <- input$`outcomes-ice-plane-colour-variable`
    shiny::updateSelectizeInput(
      session,
      "report-ice-colour",
      choices = context$model$variables,
      selected = val
    )
  })
  ### update/report-currency ----
  observeEvent(input$`outcomes-ice-currency`, {
    val <- input$`outcomes-ice-currency`
    shiny::updateSelectInput(
      session,
      "report-currency",
      choices = c(
        "Euros" = "euro",
        "US Dollars" = "dollar",
        "Yen" = "yen",
        "None" = "none"
      ),
      selected = val
    )
  })

  
  ## Render report ----
  output$downloadreport <- downloadHandler(
    filename = paste0(Sys.Date(), "-", "PACBOARD_report.docx"),
    content = function(file) {
      params <- list(
        person = input$person,
        df_pacboard = context$model$data_filtered() %>% 
          as.data.frame(),
        cost_variables = context$model$cost_variables,
        utility_variables = context$model$utility_variables,
        disutility_variables = context$model$disutility_variables,
        probability_variables = context$model$probability_variables,
        relative_effectiveness_variables = context$model$relative_effectiveness_variables,
        intervention_total_discounted_qalys = context$outcomes$intervention_total_discounted_qalys,
        intervention_total_discounted_costs = context$outcomes$intervention_total_discounted_costs,
        comparator_total_discounted_qalys = context$outcomes$comparator_total_discounted_qalys,
        comparator_total_discounted_costs = context$outcomes$comparator_total_discounted_costs, 
        intervention_total_undiscounted_qalys = context$outcomes$intervention_total_undiscounted_qalys,
        intervention_total_undiscounted_costs = context$outcomes$intervention_total_undiscounted_costs,
        comparator_total_undiscounted_qalys = context$outcomes$comparator_total_undiscounted_qalys,
        comparator_total_undiscounted_costs = context$outcomes$comparator_total_undiscounted_costs,
        summary_stats = input$summary_stats_choice,
        corr_matrix = input$corr_matrix_choice,
        validation_checks_choice = input$validation_checks_choice,
        ce_plot_choice = input$ce_plot_choice,
        ice_plot_choice = input$ice_plot_choice,
        ice_summary_choice = input$ice_summary_choice,
        ceac_plot_choice = input$ceac_plot_choice,
        col_ice = input$`report-ice-colour`,
        n_it_highlight  = input$`report-n-it`,
        wtp_ice = input$`report-willingness-to-pay`,
        ceac_min = input$`report-ceac-min`,
        ceac_max = input$`report-ceac-max`,
        ceac_step = input$`report-ceac-step`
      )
      callr::r(render_report,
               list(
                 input = report_path,
                 output = file,
                 params = params
               ))
    }
  )
}
