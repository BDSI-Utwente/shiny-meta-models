library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(pacheck)
library(purrr)
library(readr)
library(vroom)
library(glue)

source("functions/cautiously.R")
safeRead <- cautiously(vroom)

dataUI <- tabItem(
  # upload data -------------------------------------------------------------
  "data",
  box(
    title = "Upload data",
    p("Browse your computer to upload your data. The accepted format is '.csv'.", 
      br(),
      strong("The first row of the dataset should contain the names of the inputs and outputs."),
      br(),
      "If you would like your input variables to be automatically categorised, please ensure that the name of the variable starts with:",
      br(),
      "'c_' for costs inputs",
      br(),
      "'p_' for probabilities inputs",
      br(),
      "'u_' for utility values inputs",
      br(),
      "'disu_' for disutility values inputs",
      br(),
      "'rr_', 'hr_', or 'or_' for relative effectiveness inputs",
      br(),
      "If you would like your total QALYs and costs variables to be automatically categorised, please ensure that the name of the variable starts with:",
      br(),
      "'t_qaly_d' for the discounted QALYs of each strategy. The name of the strategy should be separated from 't_qaly_d' by a '_', e.g. 't_qaly_d_intervention'",
      br(),
      "'t_costs_d' for the discounted costs of each strategy. The name of the strategy should be separated from 't_costs_d' by a '_', e.g. 't_costs_d_intervention'"
      ),
    width = 12,
    fileInput("model_file",
              "Model file",
              accept = c(".csv",
                         #".xls",
                         #".xlsx",
                         #".rdata",
                         #".rds",
                         # "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                         # # is apparently also used for csv - confusing!
                         # "application/vnd.ms-excel"
                         "text/csv"))
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
  
  ## input vars ----
  # categorize variables
  box(
    title = "Categorize input variables",
    width = 12,
    p(
      "Please indicate which variables are costs, (dis)utility, probability, or relative effectiveness inputs for further analysis"
    ),
    br(),
    selectizeInput(
      "cost-variables",
      "Costs",
      choices = c("loading..."),
      multiple = TRUE
    ),
    selectizeInput(
      "utility-variables",
      "Utilities",
      choices = c("loading..."),
      multiple = TRUE
    ),
    selectizeInput(
      "disutility-variables",
      "Disutilities (when implemented as negative values, otherwise consider disutilities as utility variables)",
      choices = c("loading..."),
      multiple = TRUE
    ),
    selectizeInput(
      "probability-variables",
      "Probabilities",
      choices = c("loading..."),
      multiple = TRUE
    ),
    selectizeInput(
      "relative-effectiveness-variables",
      "Relative effectiveness (RR, OR, HR)",
      choices = c("loading..."),
      multiple = TRUE
    ),
    p(
      "If there are multiple scenarios, please select the variable that identifies the scenario."
      ),
    selectizeInput("scenario-variable", "Scenario", choices = c("loading..."))
    ),
  
  ## outcome vars ----
  box(
    title = "Select outcome variables",
    width = 12,
    p(
      "Please select variables representing total costs and effects for both the comparator and intervention conditions."
    ),
    # fluidRow(
    #   column(
    #     6,
    h4("All strategies"),
    fluidRow(column(
      6,
      ### selectize/outcomes-all-total-discounted-qalys ----
      selectizeInput(
        "outcomes-all-total-discounted-qalys",
        "Total discounted QALYs - all strategies",
        choices = c("no data loaded..." = ""),
        multiple = TRUE
        )
      ),
      column(
        6,
        ### selectize/outcomes-all-total-discounted-costs ----
        selectizeInput(
          "outcomes-all-total-discounted-costs",
          "Total discounted costs - all strategies",
          choices = c("no data loaded..." = ""),
          multiple = TRUE
        )
      )
    ),
    h4("Intervention"),
    fluidRow(column(
      4,
      ### selectize/outcomes-intervention-total-discounted-qalys ----
      selectizeInput(
        "outcomes-intervention-total-discounted-qalys",
        "Total discounted QALYs",
        choices = c("no data loaded..." = "")
      )
    ),column(
      4,
      ### selectize/outcomes-intervention-total-discounted-lys ----
      selectizeInput(
        "outcomes-intervention-total-discounted-lys",
        "Total discounted LYs",
        choices = c("no data loaded..." = "")
      )
    ),column(
      4,
      ### selectize/outcomes-intervention-total-discounted-costs ----
      selectizeInput(
        "outcomes-intervention-total-discounted-costs",
        "Total discounted costs",
        choices = c("no data loaded..." = "")
      )
    )
    ),
    fluidRow(column(
      4,
      ### selectize/outcomes-intervention-total-undiscounted-qalys ----
      selectizeInput(
        "outcomes-intervention-total-undiscounted-qalys",
        "Total undiscounted QALYs",
        choices = c("no data loaded..." = "")
      )
    ),
    column(
      4,
      ### selectize/outcomes-intervention-total-undiscounted-lys ----
      selectizeInput(
        "outcomes-intervention-total-undiscounted-lys",
        "Total undiscounted LYs",
        choices = c("no data loaded..." = "")
      )
    ),
    column(
      4,
      ### selectize/outcomes-intervention-total-undiscounted-costs ----
      selectizeInput(
        "outcomes-intervention-total-undiscounted-costs",
        "Total undiscounted costs",
        choices = c("no data loaded..." = "")
      )
    )
    ),
    h4("Comparator"),
    fluidRow(column(
      4,
      ### selectize/outcomes-comparator-total-discounted-qalys ----
      selectizeInput(
        "outcomes-comparator-total-discounted-qalys",
        "Total discounted QALYs",
        choices = c("no data loaded..." = "")
      )
    ),
    column(
      4,
      ### selectize/outcomes-comparator-total-discounted-lys ----
      selectizeInput(
        "outcomes-comparator-total-discounted-lys",
        "Total discounted LYs",
        choices = c("no data loaded..." = "")
      )
    ),
    column(
      4,
      ### selectize/outcomes-comparator-total-discounted-costs ----
      selectizeInput(
        "outcomes-comparator-total-discounted-costs",
        "Total discounted costs",
        choices = c("no data loaded..." = "")
      )
    )),
    fluidRow(column(
      4,
      ### selectize/outcomes-comparator-total-undiscounted-qalys ----
      selectizeInput(
        "outcomes-comparator-total-undiscounted-qalys",
        "Total undiscounted QALYs",
        choices = c("no data loaded..." = "")
      )
    ),
    column(
      4,
      ### selectize/outcomes-comparator-total-undiscounted-lys ----
      selectizeInput(
        "outcomes-comparator-total-undiscounted-lys",
        "Total undiscounted LYs",
        choices = c("no data loaded..." = "")
      )
    ),
    column(
      4,
      ### selectize/outcomes-comparator-total-undiscounted-costs ----
      selectizeInput(
        "outcomes-comparator-total-undiscounted-costs",
        "Total undiscounted costs",
        choices = c("no data loaded..." = "")
      )
    )
    ) 
  ),
  
  ## choose scenario ----
  conditionalPanel(
    "input['scenario-variable'] != ''",
    box(
      title = "Choose scenario",
      width = 12,
      selectizeInput("scenario", "Scenario", choices = c())
    )
  ),
  
  ## calculate net benefits ----
  box(
    title = "Calculate incrementals and net benefits",
    width = 12,
    p(
      "Using the buttons below, you can calculate the incremental QALYs and costs, and the (incremental) net benefits at the willingness-to-pay of your choice if these outcomes are not already available in your dataset."
      ),
    p(
      "If you choose to calculate the incremental QALYs and costs, these will be calculated using the variables your have selected above under 'Total discounted QALYs' and 'Total discounted costs' for the intervention and the comparator. The calculation will add two columns to the uploaded dataset, the incremental QALYs ('incremental_qaly') and the incremental costs ('incremental_cost')."
    ), 
    p(
      "If you choose to calculate the net benefits, five columns will be added to the loaded data: the net health & monetary benefits of the intervention ('NHB_int' and 'NMB_int'), the net health & monetary benefits of the comparator ('NHB_comp' and 'NMB_comp'), and the incremental net health an monetary benefits of the intervention versus the comparator ('iNHB' and 'iNMB')."
    ),
    p(
      "If you choose to calculate both incremental QALYs and costs and net benefits, all seven columns will be added to the uploaded dataset."
    ),
    p(
      "Please wait for the calculations to be performed after clicking a button."
    ),
    numericInput("wtp_data", 
                 "Choose your willingness to pay",
                 value = 50000,
                 min = 0,
                 max = Inf,
                 step = 1000),
    radioButtons(
      "outcome-to-calculate",
      label = "Which outcome should be calculated?",
      choices = list("None" = 1, 
                     "Incremental QALYs and costs" = 2, 
                     "Net benefits" = 3,
                     "Incremental QALYs and costs and net benefits" = 4),
      selected = 1  )
    ),
    
  ## ui/summary-quick-checks ----
  box(
    title = "Quick validation checks",
    width = 12,
    collapsed = TRUE,
    p(
      "This box shows the results of quick checks which are automatically performed once you have defined the different types of inputs and outputs in your dataset."
    ),
    uiOutput("summary-quick-checks")
  ),
  
  # preview
  box(
    title = "Data Preview",
    width = 12,
    collapsed = TRUE,
    p(
      "This box shows the uploaded dataset."
    ),
    div(style = "overflow: auto;",
        dataTableOutput("modelPreview"))
  )
)

dataServer <- function(input, output, session, context) {
  context$model <- reactiveValues(
    file = reactiveValues(status = NULL,
                          initialized = FALSE),
    variables = c(),
    cost_variables = c(),
    utility_variables = c(),
    disutility_variables = c(),
    probability_variables = c(),
    relative_effect_variables = c(),
    scenario_variable = "",
    scenarios = c(),
    scenario = ""
  )
  
  # upload data -------------------------------------------------------------
  context$model$file <- reactiveValues(status = NULL,
                                       initialized = FALSE)
  
  # debounce rapidly changing inputs ----
  data_nb <- list(
    # only apply changes after 500ms without any change
    calculation = reactive({
      input$`outcome-to-calculate`
    }) %>% debounce(500),
    wtp = reactive({
      input$wtp_data
    }) %>% debounce(500)
  )
  
  context$model$data_filtered <- reactive({
    if (scenarioValid()) {
      df_nb <- context$model$data %>% filter(.data[[context$model$scenario_variable]] == input$scenario)
      if(data_nb$calculation() == 2 &&
         context$outcomes$intervention_total_discounted_qalys != "" &&
         context$outcomes$intervention_total_discounted_costs != "" &&
         context$outcomes$comparator_total_discounted_qalys != "" &&
         context$outcomes$comparator_total_discounted_costs != ""
      ){
        df_nb$incremental_qaly <- as.numeric(as.character(unlist(df_nb[, context$outcomes$intervention_total_discounted_qalys]))) - as.numeric(as.character(unlist(df_nb[, context$outcomes$comparator_total_discounted_qalys])))
        df_nb$incremental_cost <- as.numeric(as.character(unlist(df_nb[, context$outcomes$intervention_total_discounted_costs]))) - as.numeric(as.character(unlist(df_nb[, context$outcomes$comparator_total_discounted_costs])))
      }
      if (data_nb$calculation() == 3 &&
          context$outcomes$intervention_total_discounted_qalys != "" &&
          context$outcomes$intervention_total_discounted_costs != "" &&
          context$outcomes$comparator_total_discounted_qalys != "" &&
          context$outcomes$comparator_total_discounted_costs != "" 
                 ){
          df_nb <- pacheck::calculate_nb(
            df = df_nb %>% as.data.frame(),
            e_int = context$outcomes$intervention_total_discounted_qalys,
            c_int = context$outcomes$intervention_total_discounted_costs,
            e_comp = context$outcomes$comparator_total_discounted_qalys,
            c_comp = context$outcomes$comparator_total_discounted_costs,
            wtp = data_nb$wtp()
          )    
        }
      if (data_nb$calculation() == 4 &&
                   context$outcomes$intervention_total_discounted_qalys != "" &&
             context$outcomes$intervention_total_discounted_costs != "" &&
             context$outcomes$comparator_total_discounted_qalys != "" &&
             context$outcomes$comparator_total_discounted_costs != ""
          ){
            df_nb <- pacheck::calculate_nb(
              df = df_nb %>% as.data.frame(),
              e_int = context$outcomes$intervention_total_discounted_qalys,
              c_int = context$outcomes$intervention_total_discounted_costs,
              e_comp = context$outcomes$comparator_total_discounted_qalys,
              c_comp = context$outcomes$comparator_total_discounted_costs,
              wtp = data_nb$wtp()
            )
            df_nb$incremental_qaly <- as.numeric(as.character(unlist(df_nb[, context$outcomes$intervention_total_discounted_qalys]))) - as.numeric(as.character(unlist(df_nb[, context$outcomes$comparator_total_discounted_qalys])))
            df_nb$incremental_cost <- as.numeric(as.character(unlist(df_nb[, context$outcomes$intervention_total_discounted_costs]))) - as.numeric(as.character(unlist(df_nb[, context$outcomes$comparator_total_discounted_costs])))
          }
      return(df_nb)
    } else {
      df_nb <- context$model$data
      if(data_nb$calculation() == 2 &&
         context$outcomes$intervention_total_discounted_qalys != "" &&
         context$outcomes$intervention_total_discounted_costs != "" &&
         context$outcomes$comparator_total_discounted_qalys != "" &&
         context$outcomes$comparator_total_discounted_costs != "" 
      ){
        df_nb$incremental_qaly <- as.numeric(as.character(unlist(df_nb[, context$outcomes$intervention_total_discounted_qalys]))) - as.numeric(as.character(unlist(df_nb[, context$outcomes$comparator_total_discounted_qalys])))
        df_nb$incremental_cost <- as.numeric(as.character(unlist(df_nb[, context$outcomes$intervention_total_discounted_costs]))) - as.numeric(as.character(unlist(df_nb[, context$outcomes$comparator_total_discounted_costs])))
      } 
      if (data_nb$calculation() == 3 &&
                 context$outcomes$intervention_total_discounted_qalys != "" &&
           context$outcomes$intervention_total_discounted_costs != "" &&
           context$outcomes$comparator_total_discounted_qalys != "" &&
           context$outcomes$comparator_total_discounted_costs != "" 
        ){
          df_nb <- pacheck::calculate_nb(
            df = df_nb %>% as.data.frame(),
            e_int = context$outcomes$intervention_total_discounted_qalys,
            c_int = context$outcomes$intervention_total_discounted_costs,
            e_comp = context$outcomes$comparator_total_discounted_qalys,
            c_comp = context$outcomes$comparator_total_discounted_costs,
            wtp = data_nb$wtp()
          )      
        } 
      if(data_nb$calculation() == 4 && 
         context$outcomes$intervention_total_discounted_qalys != "" &&
             context$outcomes$intervention_total_discounted_costs != "" &&
             context$outcomes$comparator_total_discounted_qalys != "" &&
             context$outcomes$comparator_total_discounted_costs != ""
          ){
            df_nb <- pacheck::calculate_nb(
              df = df_nb %>% as.data.frame(),
              e_int = context$outcomes$intervention_total_discounted_qalys,
              c_int = context$outcomes$intervention_total_discounted_costs,
              e_comp = context$outcomes$comparator_total_discounted_qalys,
              c_comp = context$outcomes$comparator_total_discounted_costs,
              wtp = data_nb$wtp()
            )
            df_nb$incremental_qaly <- as.numeric(as.character(unlist(df_nb[, context$outcomes$intervention_total_discounted_qalys]))) - as.numeric(as.character(unlist(df_nb[, context$outcomes$comparator_total_discounted_qalys])))
            df_nb$incremental_cost <- as.numeric(as.character(unlist(df_nb[, context$outcomes$intervention_total_discounted_costs]))) - as.numeric(as.character(unlist(df_nb[, context$outcomes$comparator_total_discounted_costs])))
          }
      return(df_nb)
    }
       #return(context$model$data)
  }) %>% bindEvent(scenarioValid(), context$model$data,
                   data_nb$calculation(), data_nb$wtp()
                   )
  
  scenarioVariableValid <- reactive({
    context$model$scenario_variable != "" &&
      context$model$scenario_variable %in% context$model$variables
  })
  
  updateScenario <- observe({
    context$model$scenario <- input$scenario
  }) %>% bindEvent(input$scenario)
  
  updateScenarios <- observe({
    if (scenarioVariableValid()) {
      context$model$scenarios <-
        context$model$data %>%
        pull(!!context$model$scenario_variable) %>%
        unique() %>% c(None = "", None = "none", .)
    } else {
      context$model$scenarios <- c(None = "")
    }
  })
  
  scenarioValid <- reactive({
    scenarioVariableValid() &&
      context$model$scenario != "" &&
      context$model$scenario != "none" &&
      context$model$scenario %in% context$model$scenarios
  })
  
  loadData <- observe({
    file <- input$model_file %>%
      pull(name) %>%
      first()
    path <- input$model_file %>%
      pull(datapath) %>%
      first()
    
    result <- safeRead(path, show_col_types = FALSE)
    
    context$model$file$message <- as.character(result)
    if (context$model$file$message == "") {
      print("success")
      shinyWidgets::show_toast(
        glue::glue("Succesfully loaded '{name}'", name = file),
        glue::glue(
          "{rows} observations of {cols} variables.",
          rows = nrow(result$result),
          cols = ncol(result$result)
        ),
        "success"
      )
    }
    
    if (!is.null(result$error)) {
      context$model$data <- NULL
      shinyWidgets::show_toast(glue::glue("Error loading '{name}'", name = file),
                               result$message,
                               "error")
      context$model$file$initialized <- FALSE
      
      
    } else {
      context$model$data <- result$result
      if (context$model$file$warnings %>% length() > 0) {
        context$model$file$status <- "warning"
        shinyWidgets::show_toast(
          glue::glue("Loaded '{name}' with warnings", name = file),
          glue::glue(
            "{rows} observations of {cols} variables. \n\n{warnings}",
            rows = nrow(result$result),
            cols = ncol(result$result),
            warnings = result$message
          ),
          "warning"
        )
      } else {
        context$model$file$status <- "success"
        shinyWidgets::show_toast(
          glue::glue("Successfully loaded '{name}'", name = file),
          glue::glue(
            "{rows} observations of {cols} variables. \n\n{message}",
            rows = nrow(result$result),
            cols = ncol(result$result),
            message = result$message
          ),
          "success"
        )
      }
      context$model$file$initialized <- TRUE
    }
  }) %>%
    bindEvent(input$model_file)
  
  # update variables and fill in automatically upon upload ----
  updateModelVariables <- observe({
    context$model$variables <- names(context$model$data)
    
    # Fill in input categories
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
    
    context$model$disutility_variables <-
      context$model$data %>% dplyr::select(starts_with("disu_")) %>% names()
    updateSelectizeInput(session,
                         "disutility-variables",
                         selected = context$model$disutility_variables)
    
    context$model$probability_variables <-
      context$model$data %>% dplyr::select(starts_with("p_")) %>% names()
    updateSelectizeInput(session,
                         "probability-variables",
                         selected = context$probability_variables)
    
    context$model$relative_effectiveness_variables <-
      context$model$data %>% dplyr::select(starts_with("hr_") |
                                             starts_with("rr_") | 
                                             starts_with("or_")) %>% names()
    updateSelectizeInput(session,
                         "relative-effectiveness-variables",
                         selected = context$relative_effectiveness_variables)
    
    context$outcomes$all_total_discounted_qalys <- 
      context$model$data %>% dplyr::select(starts_with("t_qaly_d")) %>% names()
    updateSelectizeInput(session,
                         "outcomes-all-total-discounted-qalys",
                         selected = context$outcomes$all_total_discounted_qalys)
    
    context$outcomes$all_total_discounted_costs <- 
      context$model$data %>% dplyr::select(starts_with("t_costs_d")) %>% names()
    updateSelectizeInput(session,
                         "outcomes-all-total-discounted-costs",
                         selected = context$outcomes$all_total_discounted_costs)
    
    
  }) %>% bindEvent(context$model$data)
  
  updateModelVariablesIncrementsNB <- observe({
    context$model$variables <- names(context$model$data_filtered())
  }) %>% bindEvent(context$model$data_filtered())

  updateCostVariables <- observe({
    context$model$cost_variables <- input$`cost-variables`
  }) %>% bindEvent(input$`cost-variables`)
  
  updateUtilityVariables <- observe({
    context$model$utility_variables <- input$`utility-variables`
  }) %>% bindEvent(input$`utility-variables`)
  
  updateDisutilityVariables <- observe({
    context$model$disutility_variables <- input$`disutility-variables`
  }) %>% bindEvent(input$`disutility-variables`)
  
  updateProbabilityVariables <- observe({
    context$model$probability_variables <- input$`probability-variables`
  }) %>% bindEvent(input$`probability-variables`)
  
  updateRelativeEffectivenessVariables <- observe({
    context$model$relative_effectiveness_variables <-
      input$`relative-effectiveness-variables`
  }) %>% bindEvent(input$`relative-effectiveness-variables`)
  
  updateScenarioVariable <- observe({
    context$model$scenario_variable <- input$`scenario-variable`
  }) %>% bindEvent(input$`scenario-variable`)
  
  context$model$selected_variables <- reactive({
    c(
      context$model$cost_variables,
      context$model$utility_variables,
      context$model$disutility_variables,
      context$model$probability_variables,
      context$model$relative_effectiveness_variables,
      context$model$scenario_variable
    )
  }) %>% debounce(1000)
  
  updateSelectizeChoices <- observe({
    set <- c(
      "cost-variables",
      "utility-variables",
      "disutility-variables",
      "probability-variables",
      "relative-effectiveness-variables",
      "scenario-variable"
    )
    selected <- list(
      `cost-variables` = context$model$cost_variables,
      `utility-variables` = context$model$utility_variables,
      `disutility-variables` = context$model$disutility_variables,
      `probability-variables` = context$model$probability_variables,
      `relative-effectiveness-variables` = context$model$relative_effectiveness_variables,
      `scenario-variable` = context$model$scenario_variable
    )
    update_exclusive_selectize_input_set(context$model$variables, set, selected, session)
    
  }, priority = 50) %>% bindEvent(context$model$variables, context$model$selected_variables())
  
  updateScenarioChoices <- observe({
    updateSelectizeInput(session, "scenario", choices = context$model$scenarios)
  }) %>% bindEvent(context$model$scenarios, ignoreNULL = FALSE)
  
  # ui/summary-quick-checks ----
  output$`summary-quick-checks` <- renderUI({
    checks <- list()
    
    if (context$model$probability_variables %>% length()) {
      checks$prob_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$probability_variables,
        ~ .x > 0,
        "greater than zero",
        "all probabilities are {label_check}"
      )
      checks$prob_lt1 <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$probability_variables,
        ~ .x <= 1,
        "less than or equal to one",
        "all probabilities are {label_check}"
      )
    } else {
      checks$prob <- list(messages = tibble(ok = FALSE,
                                            message = "no variables were marked as probabilities"))
    }
    
    if (context$model$cost_variables %>% length()) {
      checks$costs_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$cost_variables,
        ~ .x >= 0,
        "positive",
        "all costs are {label_check}"
      )
    } else {
      checks$costs <-
        list(messages = tibble(ok = FALSE, message = "no variables were marked as costs"))
    }
    
    if (context$model$utility_variables %>% length()) {
      checks$util_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$utility_variables,
        ~ .x >= 0,
        "positive",
        "all utilities are {label_check}"
      )
    } else {
      checks$utilities <-
        list(messages = tibble(ok = FALSE, message = "no variables were marked as utilities"))
    }
    
    if (context$model$disutility_variables %>% length()) {
      checks$util_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$disutility_variables,
        ~ .x < 0,
        "negative",
        "all disutilities are {label_check}"
      )
    } else {
      checks$disutilities <-
        list(messages = tibble(ok = FALSE, message = "no variables were marked as disutilities"))
    }
    
    if (context$model$relative_effectiveness_variables %>% length()) {
      checks$rel_eff_pos <- pacheck:::do_check(
        context$model$data_filtered(),
        context$model$relative_effectiveness_variables,
        ~ .x >= 0,
        "positive",
        "all RR, OR, and HR are {label_check}"
      )
    } else {
      checks$relative_effectiveness <-
        list(messages = tibble(ok = FALSE, message = "no variables were marked as RR, OR, HR"))
    }
    
    if (context$outcomes$intervention_total_discounted_qalys != "") {
      checks$t_qalys_discounted_int <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$intervention_total_discounted_qalys,
        ~ .x > 0,
        "positive",
        "total discounted QALYs are {label_check} for the intervention"
      )
    } else {
      checks$t_qalys_discounted_int <-
        list(messages = tibble(ok = FALSE, message = "no discounted total QALYs selected for the intervention"))
    }
    
    if (context$outcomes$intervention_total_undiscounted_qalys != "") {
      checks$t_qalys_unidscounted_int <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$intervention_total_undiscounted_qalys,
        ~ .x > 0,
        "positive",
        "total undiscounted QALYs are {label_check} for the intervention"
      )
    } else {
      checks$t_qalys_unidscounted_int <-
        list(messages = tibble(ok = FALSE, message = "no undiscounted total QALYs selected for the intervention"))
    }
    
    if (context$outcomes$comparator_total_discounted_qalys != "") {
      checks$t_qalys_discounted_comp <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$comparator_total_discounted_qalys,
        ~ .x > 0,
        "positive",
        "total discounted QALYs are {label_check} for the comparator"
      )
    } else {
      checks$t_qalys_discounted_comp <-
        list(messages = tibble(ok = FALSE, message = "no discounted total QALYs selected for the comparator"))
    }
    
    if (context$outcomes$comparator_total_undiscounted_qalys != "") {
      checks$t_qalys_undiscounted_comp <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$comparator_total_undiscounted_qalys,
        ~ .x > 0,
        "positive",
        "total undiscounted QALYs are {label_check} for the comparator"
      )
    } else {
      checks$t_qalys_undiscounted_comp <-
        list(messages = tibble(ok = FALSE, message = "no undiscounted total QALYs selected for the comparator"))
    }
    
    if (context$outcomes$intervention_total_discounted_lys != "") {
      checks$t_lys_discounted_int <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$intervention_total_discounted_lys,
        ~ .x > 0,
        "positive",
        "total discounted LYs are {label_check} for the intervention"
      )
    } else {
      checks$t_lys_discounted_int <-
        list(messages = tibble(ok = FALSE, message = "no discounted total LYs selected for the intervention"))
    }
    
    if (context$outcomes$intervention_total_undiscounted_lys != "") {
      checks$t_lys_unidscounted_int <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$intervention_total_undiscounted_lys,
        ~ .x > 0,
        "positive",
        "total undiscounted LYs are {label_check} for the intervention"
      )
    } else {
      checks$t_lys_unidscounted_int <-
        list(messages = tibble(ok = FALSE, message = "no undiscounted total LYs selected for the intervention"))
    }
    
    if (context$outcomes$comparator_total_discounted_lys != "") {
      checks$t_lys_discounted_comp <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$comparator_total_discounted_lys,
        ~ .x > 0,
        "positive",
        "total discounted LYs are {label_check} for the comparator"
      )
    } else {
      checks$t_lys_discounted_comp <-
        list(messages = tibble(ok = FALSE, message = "no discounted total LYs selected for the comparator"))
    }
    
    if (context$outcomes$comparator_total_undiscounted_lys != "") {
      checks$t_lys_undiscounted_comp <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$comparator_total_undiscounted_lys,
        ~ .x > 0,
        "positive",
        "total undiscounted LYs are {label_check} for the comparator"
      )
    } else {
      checks$t_lys_undiscounted_comp <-
        list(messages = tibble(ok = FALSE, message = "no undiscounted total LYs selected for the comparator"))
    }
    
    if (context$outcomes$intervention_total_discounted_costs != "") {
      checks$t_costs_discounted_int <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$intervention_total_discounted_costs,
        ~ .x > 0,
        "positive",
        "total discounted costs are {label_check} for the intervention"
      )
    } else {
      checks$t_costs_discounted_int <-
        list(messages = tibble(ok = FALSE, message = "no discounted total costs selected for the intervention"))
    }
    
    if (context$outcomes$intervention_total_undiscounted_costs != "") {
      checks$t_costs_unidscounted_int <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$intervention_total_undiscounted_costs,
        ~ .x > 0,
        "positive",
        "total undiscounted costs are {label_check} for the intervention"
      )
    } else {
      checks$t_costs_unidscounted_int <-
        list(messages = tibble(ok = FALSE, message = "no undiscounted total costs selected for the intervention"))
    }
    
    if (context$outcomes$comparator_total_discounted_costs != "") {
      checks$t_costs_discounted_comp <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$comparator_total_discounted_costs,
        ~ .x > 0,
        "positive",
        "total discounted costs are {label_check} for the comparator"
      )
    } else {
      checks$t_costs_discounted_comp <-
        list(messages = tibble(ok = FALSE, message = "no discounted total costs selected for the comparator"))
    }
    
    if (context$outcomes$comparator_total_undiscounted_costs != "") {
      checks$t_costs_undiscounted_comp <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$comparator_total_undiscounted_costs,
        ~ .x > 0,
        "positive",
        "total undiscounted costs are {label_check} for the comparator"
      )
    } else {
      checks$t_costs_undiscounted_comp <-
        list(messages = tibble(ok = FALSE, message = "no undiscounted total costs selected for the comparator"))
    }
    
    if ((context$outcomes$intervention_total_discounted_qalys != "" &
         context$outcomes$intervention_total_undiscounted_qalys != "")) {
      checks$disc_qalys_int <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$intervention_total_discounted_qalys,
        ~ .x < context$model$data_filtered()[, context$outcomes$intervention_total_undiscounted_qalys],
        "lower than undiscounted QALYs",
        "discounted QALYs are {label_check} for the intervention"
      )
    } else {
      checks$disc_qalys_int <-
        list(messages = tibble(ok = FALSE, message = "no discounted and undiscounted total QALYs selected for the intervention"))
    }
    
    if ((context$outcomes$comparator_total_discounted_qalys != "" &
         context$outcomes$comparator_total_undiscounted_qalys != "")) {
      checks$disc_qalys_comp <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$comparator_total_discounted_qalys,
        ~ .x < context$model$data_filtered()[, context$outcomes$comparator_total_undiscounted_qalys],
        "lower than undiscounted QALYs",
        "discounted QALYs are {label_check} for the comparator"
      )
    } else {
      checks$disc_qalys_comp <-
        list(messages = tibble(ok = FALSE, message = "no discounted and undiscounted total QALYs selected for the comparator"))
    }
    
    if ((context$outcomes$intervention_total_discounted_lys != "" &
         context$outcomes$intervention_total_undiscounted_lys != "")) {
      checks$disc_lys_int <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$intervention_total_discounted_lys,
        ~ .x < context$model$data_filtered()[, context$outcomes$intervention_total_undiscounted_lys],
        "lower than undiscounted LYs",
        "discounted LYs are {label_check} for the intervention"
      )
    } else {
      checks$disc_lys_int <-
        list(messages = tibble(ok = FALSE, message = "no discounted and undiscounted total LYs selected for the intervention"))
    }
    
    if ((context$outcomes$comparator_total_discounted_lys != "" &
         context$outcomes$comparator_total_undiscounted_lys != "")) {
      checks$disc_lys_comp <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$comparator_total_discounted_lys,
        ~ .x < context$model$data_filtered()[, context$outcomes$comparator_total_undiscounted_lys],
        "lower than undiscounted LYs",
        "discounted LYs are {label_check} for the comparator"
      )
    } else {
      checks$disc_lys_comp <-
        list(messages = tibble(ok = FALSE, message = "no discounted and undiscounted total LYs selected for the comparator"))
    }
    
    if ((context$outcomes$intervention_total_discounted_costs != "" &
         context$outcomes$intervention_total_undiscounted_costs != "")) {
      checks$disc_costs_int <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$intervention_total_discounted_costs,
        ~ .x < context$model$data_filtered()[, context$outcomes$intervention_total_undiscounted_costs],
        "lower than undiscounted costs",
        "discounted costs are {label_check} for the intervention"
      )
    } else {
      checks$disc_costs_int <-
        list(messages = tibble(ok = FALSE, message = "no discounted and undiscounted total costs selected for the intervention"))
    }
    
    if ((context$outcomes$comparator_total_discounted_costs != "" &
         context$outcomes$comparator_total_undiscounted_costs != "")) {
      checks$disc_costs_comp <- pacheck:::do_check(
        context$model$data_filtered(),
        context$outcomes$comparator_total_discounted_costs,
        ~ .x < context$model$data_filtered()[, context$outcomes$comparator_total_undiscounted_costs],
        "lower than undiscounted costs",
        "discounted costs are {label_check} for the comparator"
      )
    } else {
      checks$disc_costs_comp <-
        list(messages = tibble(ok = FALSE, message = "no discounted and undiscounted total costs selected for the comparator"))
    }
    
    msgs <- checks %>%
      map_dfr("messages") %>%
      rowwise() %>%
      mutate(html = list(div(
        class = ifelse(ok, "text-success", "text-danger"),
        icon(ifelse(ok, "check", "warning"), verify_fa = FALSE),
        message
      )))
    msgs %>% pull(html)
  })
  
  output$modelPreview <-
    renderDataTable(context$model$data_filtered())
}
