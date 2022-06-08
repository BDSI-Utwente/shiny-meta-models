library(plotly)
library(pacheck)

# UI ----------------------------------------------------------------------
outcomesUI <- tabItem(
  "outcomes",
  
  ## ICE plane ----
  box(
    width = 12,
    title = "Incremental cost-effectiveness plane",
    collapsed = TRUE,
    conditionalPanel(
      "input['outcomes-intervention-total-costs'] != '' && input['outcomes-intervention-total-effects'] != '' && input['outcomes-comparator-total-costs'] != '' && input['outcomes-comparator-total-effects'] != ''",
      fluidRow(
        column(
          width = 3,
          ### numeric/outcomes-willingness-to-pay ----
          numericInput(
            "outcomes-willingness-to-pay",
            label = "Willingness to pay threshold",
            value = NULL,
            # TODO: set meaningful default value?
            min = 0,
            max = Inf
          )
        ),
        column(
          width = 3,
          ### selectize/outcomes-iterations-highlight ----
          selectizeInput(
            "outcomes-iterations-highlight",
            label = "Highlight iterations",
            multiple = TRUE,
            choices = c()
          )
        ),
        column(
          width = 6,
          ### selectize/outcomes-ice-plane-colour-variable ----
          selectizeInput(
            "outcomes-ice-plane-colour-variable",
            label = "Colour by",
            multiple = FALSE,
            choices = c()
          )
        )
      ),
      fluidRow(column(width = 9,
                      ### plotly/ice_plane ----
                      plotOutput("ice_plane")),
               column(width = 3,
                      ### table/ice_summary ----
                      tableOutput("ice_summary")))
    ),
    conditionalPanel(
      "input['outcomes-intervention-total-costs'] == '' || input['outcomes-intervention-total-effects'] == '' || input['outcomes-comparator-total-costs'] == '' || input['outcomes-comparator-total-effects'] == ''",
      bs4Dash::bs4Callout(
        "You must load a data set and select total costs and effects variables for the intervention and comparator conditions before a cost-effectiveness plane can be created.",
        title = "Select outcome variables",
        status = "info",
        width = 12
      )
    )
  ),
  ## CE plane ----
  box(
    width = 12,
    title = "Cost-effectiveness plane",
    collapsed = TRUE,
    fluidRow(column(
      width = 12,
      conditionalPanel(
        "input['outcomes-intervention-total-costs'] != '' && input['outcomes-intervention-total-effects'] != '' && input['outcomes-comparator-total-costs'] != '' && input['outcomes-comparator-total-effects'] != ''",
        ### plotly/ce_plane ----
        plotOutput("ce_plane")
      ),
      conditionalPanel(
        "input['outcomes-intervention-total-costs'] == '' || input['outcomes-intervention-total-effects'] == '' || input['outcomes-comparator-total-costs'] == '' || input['outcomes-comparator-total-effects'] == ''",
        bs4Dash::bs4Callout(
          "You must load a data set and select total costs and effects variables for the intervention and comparator conditions before a cost-effectiveness plane can be created.",
          title = "Select outcome variables",
          status = "info",
          width = 12
        )
      )
    )
    )
    ),
  ## NB plane ----
  box(
    width = 12,
    title = "Net benefits plane",
    collapsed = TRUE,
    conditionalPanel(
        "input['outcomes-intervention-total-costs'] != '' && input['outcomes-intervention-total-effects'] != '' && input['outcomes-comparator-total-costs'] != '' && input['outcomes-comparator-total-effects'] != ''",
        fluidRow(
          ### numeric/outcomes-nb-wtp ----
        column(
          width = 3,
          numericInput(
            "outcomes-nb-wtp",
            label = "Willingness to pay",
            min = 0,
            max = Inf,
            value = 50000
          )
        ),
        ### checkbox/outcomes-nb-nmb ----
        column(
          width = 3,
          checkboxInput(
            "outcomes-nb-nmb", 
            label = "Plot NMB?", 
            value = TRUE
            )
          ),
        ### checkbox/outcomes-nb-comparators ----
        column(
          width = 3,
          checkboxInput(
            "outcomes-nb-comparators", 
            label = "Plot curves for each comparator?", 
            value = TRUE
            )
        ),
        ### checkbox/outcomes-nb-incremental ----
        column(
          width = 3,
          checkboxInput(
            "outcomes-nb-incremental", 
            label = "Plot incremental curve?", 
            value = FALSE
            )
        ),
        ### plotly/nb_plot ----
        plotOutput("nb_plot")
      ),
      conditionalPanel(
        "input['outcomes-intervention-total-costs'] == '' || input['outcomes-intervention-total-effects'] == '' || input['outcomes-comparator-total-costs'] == '' || input['outcomes-comparator-total-effects'] == ''",
        bs4Dash::bs4Callout(
          "You must load a data set and select total costs and effects variables for the intervention and comparator conditions before the net benefit curves can be created.",
          title = "Select outcome variables",
          status = "info",
          width = 12
        )
      )
    )
    ),
  ## CEAC ----
  box(
    width = 12,
    title = "Cost-effectiveness acceptability curve",
    collapsed = TRUE,
    conditionalPanel(
      "input['outcomes-intervention-total-costs'] != '' && input['outcomes-intervention-total-effects'] != '' && input['outcomes-comparator-total-costs'] != '' && input['outcomes-comparator-total-effects'] != ''",
      
      h5("Willingness to pay"),
      fluidRow(
        column(
          width = 4,
          ### numeric/outcomes-ceac-wtp-min ----
          numericInput(
            "outcomes-ceac-wtp-min",
            label = "Minimum",
            min = 0,
            max = Inf,
            value = 0
          )
        ),
        column(
          width = 4,
          ### numeric/outcomes-ceac-wtp-max ----
          numericInput(
            "outcomes-ceac-wtp-max",
            label = "Maximum",
            min = 0,
            max = Inf,
            value = 100000
          )
        ),
        column(
          width = 4,
          ### numeric/outcomes-ceac-wtp-step ----
          numericInput(
            "outcomes-ceac-wtp-step",
            label = "Step size",
            min = 0,
            max = Inf,
            value = 1000
          )
        )
      ),
      fluidRow(column(width = 8,
                      ### plotly/ceac_plot ----
                      plotlyOutput("ceac_plot")),
               column(
                 width = 4,
                 div(
                   width = "100%",
                   style = "overflow: auto;",
                   ### dataTable/ceac_table ----
                   dataTableOutput("ceac_table")
                 )
               ))
    ),
    conditionalPanel(
      "input['outcomes-intervention-total-costs'] == '' || input['outcomes-intervention-total-effects'] == '' || input['outcomes-comparator-total-costs'] == '' || input['outcomes-comparator-total-effects'] == ''",
      bs4Dash::bs4Callout(
        "You must load a data set and select total costs and effects variables for the intervention and comparator conditions before a cost-effectiveness acceptability curve can be created.",
        title = "Select outcome variables",
        status = "info",
        width = 12
      )
    )
  ),
  ## Convergence ----
  box(
    width = 12,
    title = "Convergence check",
    collapsed = TRUE,
    fluidRow(
      column(
        width = 4,
        ### selectize/outcomes-convergence-variable ----
        selectizeInput(
          "outcomes-convergence-variable",
          label = "Outcome variable",
          multiple = FALSE,
          choices = c("no data loaded..." = "")
        )
      ),
      column(
        width = 4,
        ### numeric/outcomes-convergence-block-size ----
        numericInput(
          "outcomes-convergence-block-size",
          label = "Iteration block size (not yet implemented)",
          min = 0,
          max = Inf,
          value = 500
        )
      ),
      column(
        width = 4,
        ### numeric/outcomes-convergence-threshold ----
        numericInput(
          "outcomes-convergence-threshold",
          label = "Relative change threshold (not yet implemented)",
          min = 0,
          max = 1,
          value = 0.01
        )
      )
    ),
    conditionalPanel(
      "input['outcomes-convergence-variable'] != ''",
      fluidRow(column(
        width = 12,
        ### plotly/convergence_plot ----
        plotlyOutput("convergence_plot")
      ))
    )
  )
)

# SERVER ------------------------------------------------------------------
outcomesServer <- function(input, output, session, context) {
  context$outcomes <- reactiveValues(
    intervention_incremental_effects = "",
    intervention_incremental_costs = "",
    intervention_total_effects = "",
    intervention_total_costs = "",
    # comparator_incremental_effects = "",
    # comparator_incremental_costs = "",
    comparator_total_effects = "",
    comparator_total_costs = ""
  )
  
  ## UI update observers ----
  updateICEplaneVariableChoices <- observe({
    ### selectizeInput/outcomes-intervention-incremental-effects ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-incremental-effects",
      choices = context$model$variables,
      selected = context$outcomes$intervention_incremental_effects
    )
    
    ### selectizeInput/outcomes-intervention-incremental-costs ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-incremental-costs",
      choices = context$model$variables,
      selected = context$outcomes$intervention_incremental_costs
    )
    
    ### selectizeInput/outcomes-intervention-total-effects ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-total-effects",
      choices = context$model$variables,
      selected = context$outcomes$intervention_total_effects
    )
    
    ### selectizeInput/outcomes-comparator-total-effects ----
    updateSelectizeInput(
      session,
      "outcomes-comparator-total-effects",
      choices = context$model$variables,
      selected = context$outcomes$comparator_total_effects
    )
    
    ### selectizeInput/outcomes-intervention-total-costs ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-total-costs",
      choices = context$model$variables,
      selected = context$outcomes$intervention_total_costs
    )
    
    ### selectizeInput/outcomes-comparator-total-costs ----
    updateSelectizeInput(
      session,
      "outcomes-comparator-total-costs",
      choices = context$model$variables,
      selected = context$outcomes$comparator_total_costs
    )
    
    ### selectizeInput/outcomes-ice-plane-colour-variable ----
    updateSelectizeInput(
      session,
      "outcomes-ice-plane-colour-variable",
      choices = context$model$variables,
      selected = input$`outcomes-ice-plane-colour-variable`
    )
    
    ### selectizeInput/outcomes-iterations-highlight ----
    updateSelectizeInput(
      session,
      "outcomes-iterations-highlight",
      choices = 1:nrow(context$model$data_filtered()),
      selected = c(),
      server = TRUE
    )
    
    ### selectizeInput/outcomes-convergence-variable ----
    updateSelectizeInput(
      session,
      "outcomes-convergence-variable",
      choices = context$model$variables,
      selected = input$`outcomes-convergence-variable`
    )
    
  }) %>% bindEvent(context$model$variables)
  
  ## SERVER update observers ----
  ### context$outcomes$intervention_incremental_effects ----
  updateIncrementalEffectsIntervention <- observe({
    context$outcomes$intervention_incremental_effects <-
      input$`outcomes-intervention-incremental-effects`
  }) %>% bindEvent(input$`outcomes-intervention-incremental-effects`)
  
  ### context$outcomes$intervention_incremental_costs ----
  updateIncrementalCostsIntervention <- observe({
    context$outcomes$intervention_incremental_costs <-
      input$`outcomes-intervention-incremental-costs`
  }) %>% bindEvent(input$`outcomes-intervention-incremental-costs`)
  
  ### context$outcomes$intervention_total_effects ----
  updateTotalEffectsIntervention <- observe({
    context$outcomes$intervention_total_effects <-
      input$`outcomes-intervention-total-effects`
  }) %>% bindEvent(input$`outcomes-intervention-total-effects`)
  
  ### context$outcomes$comparator_total_effects ----
  updateTotalEffectsComparator <- observe({
    context$outcomes$comparator_total_effects <-
      input$`outcomes-comparator-total-effects`
  }) %>% bindEvent(input$`outcomes-comparator-total-effects`)
  
  ### context$outcomes$intervention_total_costs ----
  updateTotalCostsIntervention <- observe({
    context$outcomes$intervention_total_costs <-
      input$`outcomes-intervention-total-costs`
  }) %>% bindEvent(input$`outcomes-intervention-total-costs`)
  
  ### context$outcomes$comparator_total_costs ----
  updateTotalCostsComparator <- observe({
    context$outcomes$comparator_total_costs <-
      input$`outcomes-comparator-total-costs`
  }) %>% bindEvent(input$`outcomes-comparator-total-costs`)
  
  ## SERVER update reactive ----
  
  ### debounce rapidly changing inputs ----
  ice <- list(
    # only apply changes after 500ms without any change
    colour = reactive({
      input$`outcomes-ice-plane-colour-variable`
    }) %>% debounce(500),
    highlights = reactive({
      input$`outcomes-iterations-highlight`
    }) %>% debounce(500),
    wtp = reactive({
      input$`outcomes-willingness-to-pay`
    }) %>% debounce(500)
  )
  
  wtp <- list(
    min = reactive({
      input$`outcomes-ceac-wtp-min`
    }) %>% debounce(500),
    max = reactive({
      input$`outcomes-ceac-wtp-max`
    }) %>% debounce(500),
    step = reactive({
      input$`outcomes-ceac-wtp-step`
    }) %>% debounce(500)
  )
  
  nb <- list(
    wtp = reactive({
      input$`outcomes-nb-wtp`
    }) %>% debounce(500),
    nmb = reactive({
      input$`outcomes-nb-nmb`
    }) %>% debounce(500),
    comparators = reactive({
      input$`outcomes-nb-comparators`
    }) %>% debounce(500),
    incremental = reactive({
      input$`outcomes-nb-incremental`
    }) %>% debounce(500)
  )
  
  convergence <- list(
    block_size = reactive({
      input$`outcomes-convergence-block-size`
    }) %>% debounce(500),
    threshold = reactive({
      input$`outcomes-convergence-threshold`
    }) %>% debounce(500)
  )
  
  ### nb_inputs ----
  nb_inputs <- reactive({
    if (context$outcomes$intervention_total_effects != "" &&
        context$outcomes$intervention_total_costs != "" &&
        context$outcomes$comparator_total_effects != "" &&
        context$outcomes$comparator_total_costs != "") {
      pacheck::calculate_nb(
        df = context$model$data_filtered() %>% as.data.frame(),
        e_int = context$outcomes$intervention_total_effects,
        c_int = context$outcomes$intervention_total_costs,
        e_comp = context$outcomes$comparator_total_effects,
        c_comp = context$outcomes$comparator_total_costs,
        wtp = nb$wtp()
      )
    }
  })
  
  ### ceac_inputs ----
  ceac_inputs <- reactive({
    if (context$outcomes$intervention_total_effects != "" &&
        context$outcomes$intervention_total_costs != "" &&
        context$outcomes$comparator_total_effects != "" &&
        context$outcomes$comparator_total_costs != "") {
      pacheck::calculate_ceac(
        df = context$model$data_filtered() %>% as.data.frame(),
        e_int = context$outcomes$intervention_total_effects,
        c_int = context$outcomes$intervention_total_costs,
        e_comp = context$outcomes$comparator_total_effects,
        c_comp = context$outcomes$comparator_total_costs,
        v_wtp = seq(
          from = wtp$min(),
          to = wtp$max(),
          by = wtp$step()
        )
      )
    }
  })
  
  ## OUTPUTS ----
  ### plotly/ice_plane ----
  output$ice_plane <- renderPlot({
    if (context$outcomes$intervention_total_effects != "" &&
        context$outcomes$comparator_total_effects != "" &&
        context$outcomes$intervention_total_costs != "" &&
        context$outcomes$comparator_total_costs != "") {
      user <- list()
      user$colour <-
        if (ice$colour() != "") {
          ice$colour()
        } else{
          NULL
        }
      user$n_it <-
        if (length(ice$highlights()) >= 1) {
          ice$highlights()
        } else{
          NULL
        }
      
      pacheck::plot_ice(
        df = context$model$data_filtered() %>% as.data.frame(),
        e_int = context$outcomes$intervention_total_effects,
        c_int = context$outcomes$intervention_total_costs,
        e_comp = context$outcomes$comparator_total_effects,
        c_comp = context$outcomes$comparator_total_costs,
        col = user$colour,
        n_it = user$n_it,
        wtp = ice$wtp()
      )
    }
  })
  
  ###datatable/ice_summary ----
  output$ice_summary <- renderTable({
    if (context$outcomes$intervention_total_effects != "" &&
        context$outcomes$comparator_total_effects != "" &&
        context$outcomes$intervention_total_costs != "" &&
        context$outcomes$comparator_total_costs != "") {
      pacheck::summary_ice(
        df = context$model$data_filtered() %>% as.data.frame(),
        e_int = context$outcomes$intervention_total_effects,
        c_int = context$outcomes$intervention_total_costs,
        e_comp = context$outcomes$comparator_total_effects,
        c_comp = context$outcomes$comparator_total_costs
      )
    }
  })
  
  ### plotly/ce_plane ----
  output$ce_plane <- renderPlot({
    if (context$outcomes$intervention_total_effects != "" &&
        context$outcomes$comparator_total_effects != "" &&
        context$outcomes$intervention_total_costs != "" &&
        context$outcomes$comparator_total_costs != "") {
      pacheck::plot_ce(
        df = context$model$data_filtered() %>% as.data.frame(),
        e_int = context$outcomes$intervention_total_effects,
        c_int = context$outcomes$intervention_total_costs,
        e_comp = context$outcomes$comparator_total_effects,
        c_comp = context$outcomes$comparator_total_costs
      )
    }
  })
  
  
  ### plotly/nb_plot ----
  output$nb_plot <- renderPlot({
    if (context$outcomes$intervention_total_effects != "" &&
        context$outcomes$comparator_total_effects != "" &&
        context$outcomes$intervention_total_costs != "" &&
        context$outcomes$comparator_total_costs != "") {
      pacheck::plot_nb(
        df = nb_inputs(),
        NMB = nb$nmb(),
        comparators = nb$comparators(),
        incremental = nb$incremental()
      )
    }
  })
  
  ### plotly/ceac_plot ----
  output$ceac_plot <- renderPlotly({
    if (context$outcomes$intervention_total_effects != "" &&
        context$outcomes$comparator_total_effects != "" &&
        context$outcomes$intervention_total_costs != "" &&
        context$outcomes$comparator_total_costs != "") {
      pacheck::plot_ceac(df = ceac_inputs(),
                         wtp = "WTP_threshold")
    }
  })
  
  ### datatable/ceac_table ----
  output$ceac_table <- renderDataTable({
    if (!is.null(ceac_inputs())) {
      df_ceac_out <- ceac_inputs()
      df_ceac_out[, which(names(df_ceac_out) != "WTP_threshold")] <-
        apply(df_ceac_out[, which(names(df_ceac_out) != "WTP_threshold")], 2, function(x)
          paste(round(x * 100), "%"))
      df_ceac_out
    }
  })
  
  ### plotly/convergence_plot ----
  output$convergence_plot <- renderPlotly({
    if (input$`outcomes-convergence-variable` != "") {
      pacheck::plot_convergence(
        df = context$model$data_filtered() %>% as.data.frame(),
        outcome = input$`outcomes-convergence-variable`,
        block_size = convergence$block_size(),
        conv_limit = convergence$threshold()
      )
    }
  })
}
