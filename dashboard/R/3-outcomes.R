library(plotly)
library(pacheck)

# UI ----------------------------------------------------------------------
outcomesUI <- tabItem("outcomes", 
                      h1("Outcomes"),
                      box(
                        width = 12,
                        title = "Incremental cost-effectiveness plane",
                        fluidRow(
                          column(
                            width = 2,
                            selectizeInput(
                              "incremental-effects-intervention",
                              label = "Incremental effects intervention",
                              multiple = FALSE,
                              choices = c()
                            )),
                          column(
                            width = 2,
                            selectizeInput(
                              "incremental-costs-intervention",
                              label = "Incremental costs intervention",
                              multiple = FALSE,
                              choices = c()
                            )),
                          column(
                            width = 2,
                            numericInput(
                              "willingness-to-pay",
                              label = "Willingness to pay threshold",
                              value = NULL,
                              min = 0,
                              max = Inf
                            )),
                          column(
                            width = 2,
                            selectizeInput(
                              "iterations-to-highlight",
                              label = "Which iterations should be highlighted?",
                              multiple = TRUE,
                              choices = c()
                            )),
                          column(
                            width = 4,
                            selectizeInput(
                              "colour-ice-plane",
                              label = "Select input parameter for colouring the dots on the graph",
                              multiple = FALSE,
                              choices = c()
                            ))),
                          fluidRow(
                            column(
                              width = 9,
                              plotlyOutput("ice_plane")
                              ),
                            column(
                              width = 3,
                              tableOutput("ice_summary")
                            )
                          )
                          ),
                      box(
                        width = 12,
                        title = "Cost-effectiveness acceptability curve",
                        fluidRow(
                          column(
                            width = 3,
                            selectizeInput(
                              "total-effects-intervention",
                              label = "Total effects intervention",
                              multiple = FALSE,
                              choices = c()
                            )),
                          column(
                            width = 3,
                            selectizeInput(
                              "total-effects-comparator",
                              label = "Total effects comparator",
                              multiple = FALSE,
                              choices = c()
                            )),
                          column(
                            width = 3,
                            selectizeInput(
                              "total-costs-intervention",
                              label = "Total costs intervention",
                              multiple = FALSE,
                              choices = c()
                            )),
                          column(
                            width = 3,
                            selectizeInput(
                              "total-costs-comparator",
                              label = "Total costs comparator",
                              multiple = FALSE,
                              choices = c()
                            )),
                          column(
                            width = 4,
                            numericInput(
                              "minimal-wtp",
                              label = "Minimum value willingness to pay",
                              min = 0,
                              max = Inf,
                              value = 0
                            )),
                          column(
                            width = 4,
                            numericInput(
                              "maximal-wtp",
                              label = "Maximum value willingness to pay",
                              min = 0,
                              max = Inf,
                              value = 100000
                            )),
                          column(
                            width = 4,
                            numericInput(
                              "steps-wtp",
                              label = "Steps willingness-to-pay between minimum and maximum value",
                              min = 0,
                              max = Inf,
                              value = 1000
                            ))
                          ),
                        fluidRow(
                          column(
                            width = 8,
                            plotlyOutput("ceac_plot")
                          ),
                          column(
                            width = 4,
                            div(
                              width = "100%",
                              style = "overflow: auto;",
                              dataTableOutput("ceac_table")
                            )
                          )
                        )
                        ),
                      box(
                        width = 12,
                        title = "Convergence check",
                        fluidRow(
                          column(
                            width = 4,
                            selectizeInput(
                              "output-convergence",
                              label = "Select output for which you want to check the convergence",
                              multiple = FALSE,
                              choices = c()
                            )
                          ),
                          column(
                            width = 4,
                            numericInput(
                              "size-block-convergence",
                              label = "Size of the iteration-blocks at which relative change in output is assessed (not used yet)",
                              min = 0,
                              max = Inf,
                              value = 500
                            )
                          ),
                          column(
                            width = 4,
                            numericInput(
                              "convergence-limit",
                              label = "At which relative change percentage should a vertical line be drawn? (not used yet)",
                              min = 0,
                              max = 1,
                              value = 0.01
                            )
                          )
                        ),
                        fluidRow(
                          column(
                            width = 12,
                            plotlyOutput("convergence_plot")
                          )
                        )
                        )
                      )

# SERVER ------------------------------------------------------------------
outcomesServer <- function(input, output, session, context) {
  
  context$outcomes <- reactiveValues(variables = c(),
                                    incremental_effects_intervention = "",
                                    incremental_costs_intervention = "",
                                    iterations_to_highlight = "",
                                    colour_ice_plane = "",
                                    total_effects_intervention = "",
                                    total_effects_comparator = "",
                                    total_costs_intervention = "",
                                    total_costs_comparator = "",
                                    output_convergence = ""
                                    )
  
  ## UI update observers ----
  updateICEplaneVariableChoices <- observe({
    
    ### selectizeInput/incremental-effects-intervention ----
    updateSelectizeInput( 
      session,
      "incremental-effects-intervention",
      choices = context$model$variables,
      selected = context$outcomes$incremental_effects_intervention
    )
    
    ### selectizeInput/incremental-costs-intervention ----
    updateSelectizeInput( 
      session,
      "incremental-costs-intervention",
      choices = context$model$variables,
      selected = context$outcomes$incremental_costs_intervention
    )
    
    ### selectizeInput/colour-ice-plane ----
    updateSelectizeInput( 
      session,
      "colour-ice-plane",
      choices = context$model$variables,
      selected = context$outcomes$colour_ice_plane
    )
    
    ### selectizeInput/total-effects-intervention ----
    updateSelectizeInput( 
      session,
      "total-effects-intervention",
      choices = context$model$variables,
      selected = context$outcomes$total_effects_intervention
    )
    
    ### selectizeInput/total-effects-comparator ----
    updateSelectizeInput( 
      session,
      "total-effects-comparator",
      choices = context$model$variables,
      selected = context$outcomes$total_effects_comparator
    )
    
    ### selectizeInput/total-costs-intervention ----
    updateSelectizeInput( 
      session,
      "total-costs-intervention",
      choices = context$model$variables,
      selected = context$outcomes$total_costs_intervention
    )
    
    ### selectizeInput/total-costs-comparator ----
    updateSelectizeInput( 
      session,
      "total-costs-comparator",
      choices = context$model$variables,
      selected = context$outcomes$total_costs_comparator
    )
    
    ### selectizeInput/iterations-to-highlight ----
    updateSelectizeInput( 
      session,
      "iterations-to-highlight",
      choices = 1:nrow(context$model$data_filtered() %>% as.data.frame()),
      selected = context$outcomes$iterations_to_highlight
    )
    
    ### selectizeInput/output-convergence ----
    updateSelectizeInput( 
      session,
      "output-convergence",
      choices = context$model$variables,
      selected = context$outcomes$output_convergence
    )
    
  }) %>% bindEvent(context$model$variables)
  
  ## SERVER update observers ----
  ### context$outcomes$incremental_effects_intervention ----
  updateIncrementalEffectsIntervention <- observe({
     context$outcomes$incremental_effects_intervention <- input$`incremental-effects-intervention`
   }) %>% bindEvent(input$`incremental-effects-intervention`)
  
  ### context$outcomes$incremental_costs_intervention ---- 
  updateIncrementalCostsIntervention <- observe({
     context$outcomes$incremental_costs_intervention <- input$`incremental-costs-intervention`
   }) %>% bindEvent(input$`incremental-costs-intervention`)
  
  ### context$outcomes$colour_ice_plane ----
  updateColourICEPlane <- observe({
     context$outcomes$colour_ice_plane <- input$`colour-ice-plane`
   }) %>% bindEvent(input$`colour-ice-plane`)
  
  ### context$outcomes$total_effects_intervention ----
  updateTotalEffectsIntervention <- observe({
    context$outcomes$total_effects_intervention <- input$`total-effects-intervention`
  }) %>% bindEvent(input$`total-effects-intervention`)
  
  ### context$outcomes$total_effects_comparator ----
  updateTotalEffectsComparator <- observe({
    context$outcomes$total_effects_comparator <- input$`total-effects-comparator`
  }) %>% bindEvent(input$`total-effects-comparator`)
  
  ### context$outcomes$total_costs_intervention ----
  updateTotalCostsIntervention <- observe({
    context$outcomes$total_costs_intervention <- input$`total-costs-intervention`
  }) %>% bindEvent(input$`total-costs-intervention`)
  
  ### context$outcomes$total_costs_comparator ----
  updateTotalCostsComparator <- observe({
    context$outcomes$total_costs_comparator <- input$`total-costs-comparator`
  }) %>% bindEvent(input$`total-costs-comparator`)
  
  ### context$outcomes$iterations_to_highlight ----
  updateIterationsToHighlight <- observe({
    context$outcomes$iterations_to_highlight <- input$`iterations-to-highlight`
  }) %>% bindEvent(input$`iterations-to-highlight`)
  
  ### context$outcomes$output_convergence ----
  updateTotalCostsComparator <- observe({
    context$outcomes$output_convergence <- input$`output-convergence`
  }) %>% bindEvent(input$`output-convergence`)
  
  ## SERVER update reactive ----
  ## ceac_inputs ----
  ceac_inputs <- reactive({
    if(context$outcomes$total_effects_intervention != "" &&
                              context$outcomes$total_effects_comparator != "" &&
                              context$outcomes$total_costs_intervention != "" &&
                              context$outcomes$total_costs_comparator != ""){
      
      pacheck::calculate_ceac(df = context$model$data_filtered() %>% as.data.frame(),
                            e_int = context$outcomes$total_effects_intervention,
                            e_comp = context$outcomes$total_effects_comparator,
                            c_int = context$outcomes$total_costs_intervention,
                            c_comp = context$outcomes$total_costs_comparator,
                            v_wtp = seq(from = input$`minimal-wtp`, to = input$`maximal-wtp`, by = input$`steps-wtp`))
      }
  })
  
  
  ## OUTPUTS ----
  ### plotly/ice_plane ----
  output$ice_plane <- renderPlotly({
    
    if(context$outcomes$incremental_effects_intervention != "" &&
       context$outcomes$incremental_costs_intervention != "") {
    user <- list()
    user$colour <- if(context$outcomes$colour_ice_plane != "") {context$outcomes$colour_ice_plane} else{NULL}
    user$n_it <- if(context$outcomes$iterations_to_highlight != "") {context$outcomes$iterations_to_highlight} else{NULL}
    
    pacheck::plot_ice(df = context$model$data_filtered() %>% as.data.frame(),
                      param_1 = context$outcomes$incremental_effects_intervention,
                      param_2 = context$outcomes$incremental_costs_intervention,
                      col = user$colour,
                      n_it = user$n_it,
                      wtp = input$`willingness-to-pay`)
    }
  })
  
  ###datatable/ice_summary ----
  output$ice_summary <- renderTable({
    
    if(context$outcomes$incremental_effects_intervention != "" &&
       context$outcomes$incremental_costs_intervention != ""){
    
      pacheck::summary_ice(df = context$model$data_filtered() %>% as.data.frame(),
                           context$outcomes$incremental_effects_intervention,
                           context$outcomes$incremental_costs_intervention)
    
      }
  })
  
  ### plotly/ceac_plot ----
  output$ceac_plot <- renderPlotly({
    
    if(context$outcomes$total_effects_intervention != "" &&
       context$outcomes$total_effects_comparator != "" &&
       context$outcomes$total_costs_intervention != "" &&
       context$outcomes$total_costs_comparator != ""){
    
    pacheck::plot_ceac(df = ceac_inputs(),
                       wtp = "WTP_threshold")
        }
  })
  
  ### datatable/ceac_table ----
  output$ceac_table <- renderDataTable({
    
    df_ceac_out <- ceac_inputs()
    df_ceac_out[, which(names(df_ceac_out) != "WTP_threshold")] <- apply(df_ceac_out[, which(names(df_ceac_out) != "WTP_threshold")], 2, function(x) paste(round(x * 100), "%"))
    df_ceac_out
    
  })
  
  ### plotly/convergence_plot ----
  output$convergence_plot <- renderPlotly({
    
    if(context$outcomes$output_convergence != "") {
      
      pacheck::plot_convergence(df = context$model$data_filtered() %>% as.data.frame(),
                                outcome = context$outcomes$output_convergence,
                                block_size = input$`size-block-convergence`,
                                conv_limit = input$`convergence-limit`)
    }
  })
}