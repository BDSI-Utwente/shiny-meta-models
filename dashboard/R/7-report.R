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
      strong("THIS IS STILL WORK IN PROGRESS, USE AT YOUR OWN PERIL ;)"),
      br(),
      "In this box, you can download a report contening the performed validation steps and the different plots you have generated.",
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
                                value = TRUE)
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

  output$downloadreport <- downloadHandler(
    filename = paste0(Sys.Date(), "-", "PACBOARD_report.docx"),
    content = function(file) {
      params <- list(
        person = input$person,
        df_pacboard = context$model$data_filtered() %>%
          dplyr::select(!!!c(context$model$cost_variables,
                             context$model$utility_variables,
                             context$model$probability_variables,
                             context$model$relative_effectiveness_variables) # from 1-data.R
                        ) %>% 
          as.data.frame(),
        summary_stats = input$summary_stats_choice,
        corr_matrix = input$corr_matrix_choice
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
