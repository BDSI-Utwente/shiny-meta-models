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
    p(
      "This box displays the incremental QALYs versus the incremental effects of the intervention versus the comparator.",
      br(),
      "The proportion of iterations in each of the four quadrants of the plane is also provided.",
      br(),
      "You can add a willingness-to-pay threshold line on the plane, highlight specific iterations, and add a colour based on values of a selected variable."
    ),
    conditionalPanel(
      "input['outcomes-intervention-total-discounted-costs'] != '' && input['outcomes-intervention-total-discounted-qalys'] != '' && input['outcomes-comparator-total-discounted-costs'] != '' && input['outcomes-comparator-total-discounted-qalys'] != ''",
      fluidRow(
        column(
          width = 3,
          ### numeric/outcomes-willingness-to-pay ----
          numericInput(
            "outcomes-willingness-to-pay",
            label = "Willingness-to-pay threshold",
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
      "input['outcomes-intervention-total-discounted-costs'] == '' || input['outcomes-intervention-total-discounted-qalys'] == '' || input['outcomes-comparator-total-discounted-costs'] == '' || input['outcomes-comparator-total-discounted-qalys'] == ''",
      bs4Dash::bs4Callout(
        "You must load a dataset and select total discounted costs and QALYs variables for the intervention and comparator in the 'Prepare data'-tab before the incremental cost-effectiveness plane can be displayed.",
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
    p(
      "This box displays the total discounted QALYs versus the total discounted  effects of the intervention and the comparator.",
      br()
    ),
    fluidRow(column(
      width = 12,
      conditionalPanel(
        "input['outcomes-intervention-total-discounted-costs'] != '' && input['outcomes-intervention-total-discounted-qalys'] != '' && input['outcomes-comparator-total-discounted-costs'] != '' && input['outcomes-comparator-total-discounted-qalys'] != ''",
        ### plotly/ce_plane ----
        plotOutput("ce_plane")
      ),
      conditionalPanel(
        "input['outcomes-intervention-total-discounted-costs'] == '' || input['outcomes-intervention-total-discounted-qalys'] == '' || input['outcomes-comparator-total-discounted-costs'] == '' || input['outcomes-comparator-total-discounted-qalys'] == ''",
        bs4Dash::bs4Callout(
          "You must load a dataset and select total discounted costs and QALYs variables for the intervention and comparator in the 'Prepare data'-tab before the cost-effectiveness plane can be displayed.",
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
    p(
      "This box displays the (incremental) net benefits.",
      br(),
      "You can modify the willingness-to-pay threshold at which these net benefits are calculated and decide whether you want to display the net health or monetary benefits of the intervention and the comparator, eventually in combination with the incremental net health or monetary benefit.",
      br()
    ),
    conditionalPanel(
        "input['outcomes-intervention-total-discounted-costs'] != '' && input['outcomes-intervention-total-discounted-qalys'] != '' && input['outcomes-comparator-total-discounted-costs'] != '' && input['outcomes-comparator-total-discounted-qalys'] != ''",
        fluidRow(
          ### numeric/outcomes-nb-wtp ----
        column(
          width = 3,
          numericInput(
            "outcomes-nb-wtp",
            label = "Willingness-to-pay threshold",
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
        )
        ),
      conditionalPanel(
        "input['outcomes-intervention-total-discounted-costs'] == '' || input['outcomes-intervention-total-discounted-qalys'] == '' || input['outcomes-comparator-total-discounted-costs'] == '' || input['outcomes-comparator-total-discounted-qalys'] == ''",
        bs4Dash::bs4Callout(
          "You must load a dataset and select total discounted costs and QALYs variables for the intervention and comparator in the 'Prepare data'-tab before the net benefit curves can be created.",
          title = "Select outcome variables",
          status = "info",
          width = 12
        )
      )
    ),
  ## CEAC ----
  box(
    width = 12,
    title = "Cost-effectiveness acceptability curve",
    collapsed = TRUE,
    p(
      "This box displays the cost-effectiveness acceptability curves (CEACs) for the intervention and the comparator.",
      br(),
      "You can modify the willingness-to-pay range at which the CEACs are calculated and displayed.",
      br()
    ),
    conditionalPanel(
      "input['outcomes-intervention-total-discounted-costs'] != '' && input['outcomes-intervention-total-discounted-qalys'] != '' && input['outcomes-comparator-total-discounted-costs'] != '' && input['outcomes-comparator-total-discounted-qalys'] != ''",
      p(strong("Willingness-to-pay threshold range to display")),
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
      "input['outcomes-intervention-total-discounted-costs'] == '' || input['outcomes-intervention-total-discounted-qalys'] == '' || input['outcomes-comparator-total-discounted-costs'] == '' || input['outcomes-comparator-total-discounted-qalys'] == ''",
      bs4Dash::bs4Callout(
        "You must load a dataset and select total discounted costs and QALYs variables for the intervention and comparator in the 'Prepare data'-tab before the cost-effectiveness acceptability curve can be created.",
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
    p(
      "This box displays the moving average of a variable of your choice in a plot and table. By default, the moving average is displayed in blocks of 500 iterations.",
      br(),
      "You can also plot the relative difference in mean between the blocks by typing a number between 0 and 1 in the", strong(em("Relative change threshold")), " input field.",
      br(),
      "These outcomes can also be calculated using the variance of the variable by ticking the box.",
      br()
      ),
    collapsed = TRUE,
    fluidRow(
      column(
        width = 4,
        ### selectize/outcomes-convergence-variable ----
        selectizeInput(
          "outcomes-convergence-variable",
          label = "Variable",
          multiple = FALSE,
          choices = c("no data loaded..." = "")
        )
      ),
      column(
        width = 4,
        ### numeric/outcomes-convergence-block-size ----
        numericInput(
          "outcomes-convergence-block-size",
          label = "Number of iteration per block",
          min = 0,
          max = Inf,
          value = 500,
          step = 1
        )
      ),
      column(
        width = 4,
        ### numeric/outcomes-convergence-threshold ----
        numericInput(
          "outcomes-convergence-threshold",
          label = "Relative change threshold",
          min = 0,
          max = 1,
          value = 0,
          step = 0.01
        )
      ),
      column(
        width = 4,
        ### numeric/outcomes-convergence-breaks ----
        numericInput(
          "outcomes-convergence-breaks",
          label = "Number of iterations at which the breaks should be placed on the graph",
          min = 0,
          max = Inf,
          value = 500,
          step = 1
        )
      ),
      column(
        width = 4,
        ### logical/outcomes-convergence-variance ----
        checkboxInput(
          "outcomes-convergence-variance",
          label = "Should the variance of the variable be plotted instead of the mean?",
          value = FALSE
        )
      )
    ),
    conditionalPanel(
      "input['outcomes-convergence-variable'] != ''",
      fluidRow(column(
        width = 8,
        ### plotly/convergence_plot ----
        plotlyOutput("convergence_plot")
      ),
      column(
        width = 4,
        div(
          width = "100%",
          style = "overflow: auto;",
          ### dataTable/convergence_table ----
          dataTableOutput("convergence_table")
        )
      ))
    ),
    conditionalPanel(
      "input['outcomes-convergence-variable'] == ''",
      bs4Dash::bs4Callout(
        title = "Select variable",
        status = "info",
        "To see the 'convergence plot and table', you first have to select a variable."
      )
    )
  )
)

# SERVER ------------------------------------------------------------------
outcomesServer <- function(input, output, session, context) {
  context$outcomes <- reactiveValues(
    intervention_total_discounted_qalys = "",
    intervention_total_discounted_lys = "",
    intervention_total_discounted_costs = "",
    intervention_total_undiscounted_qalys = "",
    intervention_total_undiscounted_lys = "",
    intervention_total_undiscounted_costs = "",
    comparator_total_discounted_qalys = "",
    comparator_total_discounted_lys = "",
    comparator_total_discounted_costs = "",
    comparator_total_undiscounted_qalys = "",
    comparator_total_undiscounted_lys = "",
    comparator_total_undiscounted_costs = ""
  )
  
  ## UI update observers ----
  updateICEplaneVariableChoices <- observe({
    ### selectizeInput/outcomes-intervention-total-discounted-qalys ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-total-discounted-qalys",
      choices = context$model$variables,
      selected = context$outcomes$intervention_total_discounted_qalys
    )
    
    ### selectizeInput/outcomes-intervention-total-discounted-lys ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-total-discounted-lys",
      choices = context$model$variables,
      selected = context$outcomes$intervention_total_discounted_lys
    )
    
    ### selectizeInput/outcomes-intervention-total-discounted-costs ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-total-discounted-costs",
      choices = context$model$variables,
      selected = context$outcomes$intervention_total_discounted_costs
    )
    
    ### selectizeInput/outcomes-intervention-total-undiscounted-qalys ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-total-undiscounted-qalys",
      choices = context$model$variables,
      selected = context$outcomes$intervention_total_undiscounted_qalys
    )
    
    ### selectizeInput/outcomes-intervention-total-undiscounted-lys ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-total-undiscounted-lys",
      choices = context$model$variables,
      selected = context$outcomes$intervention_total_undiscounted_lys
    )
    
    ### selectizeInput/outcomes-intervention-total-undiscounted-costs ----
    updateSelectizeInput(
      session,
      "outcomes-intervention-total-undiscounted-costs",
      choices = context$model$variables,
      selected = context$outcomes$intervention_total_undiscounted_costs
    )
    
    ### selectizeInput/outcomes-comparator-total-discounted-qalys ----
    updateSelectizeInput(
      session,
      "outcomes-comparator-total-discounted-qalys",
      choices = context$model$variables,
      selected = context$outcomes$comparator_total_discounted_qalys
    )
    
    ### selectizeInput/outcomes-comparator-total-discounted-lys ----
    updateSelectizeInput(
      session,
      "outcomes-comparator-total-discounted-lys",
      choices = context$model$variables,
      selected = context$outcomes$comparator_total_discounted_lys
    )
    
    ### selectizeInput/outcomes-comparator-total-discounted-costs ----
    updateSelectizeInput(
      session,
      "outcomes-comparator-total-discounted-costs",
      choices = context$model$variables,
      selected = context$outcomes$comparator_total_discounted_costs
    )
    
    ### selectizeInput/outcomes-comparator-total-undiscounted-qalys ----
    updateSelectizeInput(
      session,
      "outcomes-comparator-total-undiscounted-qalys",
      choices = context$model$variables,
      selected = context$outcomes$comparator_total_undiscounted_qalys
    )
    
    ### selectizeInput/outcomes-comparator-total-undiscounted-lys ----
    updateSelectizeInput(
      session,
      "outcomes-comparator-total-undiscounted-lys",
      choices = context$model$variables,
      selected = context$outcomes$comparator_total_undiscounted_lys
    )
    
    ### selectizeInput/outcomes-comparator-total-undiscounted-costs ----
    updateSelectizeInput(
      session,
      "outcomes-comparator-total-undiscounted-costs",
      choices = context$model$variables,
      selected = context$outcomes$comparator_total_undiscounted_costs
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
  
  ### context$outcomes$intervention_total_discounted_qalys ----
  updateTotalDiscQALYIntervention <- observe({
    context$outcomes$intervention_total_discounted_qalys <-
      input$`outcomes-intervention-total-discounted-qalys`
  }) %>% bindEvent(input$`outcomes-intervention-total-discounted-qalys`)
  
  ### context$outcomes$intervention_total_discounted_lys ----
  updateTotalDiscLYIntervention <- observe({
    context$outcomes$intervention_total_discounted_lys <-
      input$`outcomes-intervention-total-discounted-lys`
  }) %>% bindEvent(input$`outcomes-intervention-total-discounted-lys`)
  
  ### context$outcomes$intervention_total_discounted_costs ----
  updateTotalDiscCostsIntervention <- observe({
    context$outcomes$intervention_total_discounted_costs <-
      input$`outcomes-intervention-total-discounted-costs`
  }) %>% bindEvent(input$`outcomes-intervention-total-discounted-costs`)
  
  ### context$outcomes$intervention_total_undiscounted_qalys ----
  updateTotalUndiscQALYIntervention <- observe({
    context$outcomes$intervention_total_undiscounted_qalys <-
      input$`outcomes-intervention-total-undiscounted-qalys`
  }) %>% bindEvent(input$`outcomes-intervention-total-undiscounted-qalys`)
  
  ### context$outcomes$intervention_total_undiscounted_lys ----
  updateTotalUndiscLYIntervention <- observe({
    context$outcomes$intervention_total_undiscounted_lys <-
      input$`outcomes-intervention-total-undiscounted-lys`
  }) %>% bindEvent(input$`outcomes-intervention-total-undiscounted-lys`)
  
  ### context$outcomes$intervention_total_undiscounted_costs ----
  updateTotalUndiscCostsIntervention <- observe({
    context$outcomes$intervention_total_undiscounted_costs <-
      input$`outcomes-intervention-total-undiscounted-costs`
  }) %>% bindEvent(input$`outcomes-intervention-total-undiscounted-costs`)
  
  ### context$outcomes$comparator_total_discounted_qalys ----
  updateTotalDiscQALYComparator <- observe({
    context$outcomes$comparator_total_discounted_qalys <-
      input$`outcomes-comparator-total-discounted-qalys`
  }) %>% bindEvent(input$`outcomes-comparator-total-discounted-qalys`)
  
  ### context$outcomes$comparator_total_discounted_lys ----
  updateTotalDiscLYComparator <- observe({
    context$outcomes$comparator_total_discounted_lys <-
      input$`outcomes-comparator-total-discounted-lys`
  }) %>% bindEvent(input$`outcomes-comparator-total-discounted-lys`)
  
  ### context$outcomes$comparator_total_discounted_costs ----
  updateTotalDiscCostsComparator <- observe({
    context$outcomes$comparator_total_discounted_costs <-
      input$`outcomes-comparator-total-discounted-costs`
  }) %>% bindEvent(input$`outcomes-comparator-total-discounted-costs`)
  
  ### context$outcomes$comparator_total_undiscounted_qalys ----
  updateTotalUndiscQALYComparator <- observe({
    context$outcomes$comparator_total_undiscounted_qalys <-
      input$`outcomes-comparator-total-undiscounted-qalys`
  }) %>% bindEvent(input$`outcomes-comparator-total-undiscounted-qalys`)
  
  ### context$outcomes$comparator_total_undiscounted_lys ----
  updateTotalUndiscLYComparator <- observe({
    context$outcomes$comparator_total_undiscounted_lys <-
      input$`outcomes-comparator-total-undiscounted-lys`
  }) %>% bindEvent(input$`outcomes-comparator-total-undiscounted-lys`)
  
  ### context$outcomes$comparator_total_undiscounted_costs ----
  updateTotalUndiscCostsComparator <- observe({
    context$outcomes$comparator_total_undiscounted_costs <-
      input$`outcomes-comparator-total-undiscounted-costs`
  }) %>% bindEvent(input$`outcomes-comparator-total-undiscounted-costs`)
  
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
    conv_limit = reactive({
      input$`outcomes-convergence-threshold`
    }) %>% debounce(500),
    breaks = reactive({
      input$`outcomes-convergence-breaks`
    }) %>% debounce(500),
    variance = reactive({
      input$`outcomes-convergence-variance`
    }) %>% debounce(500)
  )
  
  ### nb_inputs ----
  nb_inputs <- reactive({
    if (context$outcomes$intervention_total_discounted_qalys != "" &&
        context$outcomes$intervention_total_discounted_costs != "" &&
        context$outcomes$comparator_total_discounted_qalys != "" &&
        context$outcomes$comparator_total_discounted_costs != "") {
      pacheck::calculate_nb(
        df = context$model$data_filtered() %>% as.data.frame(),
        e_int = context$outcomes$intervention_total_discounted_qalys,
        c_int = context$outcomes$intervention_total_discounted_costs,
        e_comp = context$outcomes$comparator_total_discounted_qalys,
        c_comp = context$outcomes$comparator_total_discounted_costs,
        wtp = nb$wtp()
      )
    }
  })
  
  ### ceac_inputs ----
  ceac_inputs <- reactive({
    if (context$outcomes$intervention_total_discounted_qalys != "" &&
        context$outcomes$intervention_total_discounted_costs != "" &&
        context$outcomes$comparator_total_discounted_qalys != "" &&
        context$outcomes$comparator_total_discounted_costs != "") {
      pacheck::calculate_ceac(
        df = context$model$data_filtered() %>% as.data.frame(),
        e_int = context$outcomes$intervention_total_discounted_qalys,
        c_int = context$outcomes$intervention_total_discounted_costs,
        e_comp = context$outcomes$comparator_total_discounted_qalys,
        c_comp = context$outcomes$comparator_total_discounted_costs,
        v_wtp = seq(
          from = wtp$min(),
          to = wtp$max(),
          by = wtp$step()
        )
      )
    }
  })
  
  ### convergence_inputs ----
  convergence_inputs <- reactive({
    if (input$`outcomes-convergence-variable` != "") {
      pacheck::plot_convergence(
        df = context$model$data_filtered() %>% as.data.frame(),
        outcome = input$`outcomes-convergence-variable`,
        block_size = convergence$block_size(),
        conv_limit = convergence$conv_limit(),
        breaks = convergence$breaks(),
        variance = convergence$variance()
      )
    }
  })
  
  ## OUTPUTS ----
  ### plotly/ice_plane ----
  output$ice_plane <- renderPlot({
    if (context$outcomes$intervention_total_discounted_qalys != "" &&
        context$outcomes$intervention_total_discounted_costs != "" &&
        context$outcomes$comparator_total_discounted_qalys != "" &&
        context$outcomes$comparator_total_discounted_costs != "") {
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
        e_int = context$outcomes$intervention_total_discounted_qalys,
        c_int = context$outcomes$intervention_total_discounted_costs,
        e_comp = context$outcomes$comparator_total_discounted_qalys,
        c_comp = context$outcomes$comparator_total_discounted_costs,
        col = user$colour,
        n_it = user$n_it,
        wtp = ice$wtp()
      )
    }
  })
  
  ###datatable/ice_summary ----
  output$ice_summary <- renderTable({
    if (context$outcomes$intervention_total_discounted_qalys != "" &&
        context$outcomes$intervention_total_discounted_costs != "" &&
        context$outcomes$comparator_total_discounted_qalys != "" &&
        context$outcomes$comparator_total_discounted_costs != "") {
      pacheck::summary_ice(
        df = context$model$data_filtered() %>% as.data.frame(),
        e_int = context$outcomes$intervention_total_discounted_qalys,
        c_int = context$outcomes$intervention_total_discounted_costs,
        e_comp = context$outcomes$comparator_total_discounted_qalys,
        c_comp = context$outcomes$comparator_total_discounted_costs
      )
    }
  })
  
  ### plotly/ce_plane ----
  output$ce_plane <- renderPlot({
    if (context$outcomes$intervention_total_discounted_qalys != "" &&
        context$outcomes$intervention_total_discounted_costs != "" &&
        context$outcomes$comparator_total_discounted_qalys != "" &&
        context$outcomes$comparator_total_discounted_costs != "") {
      pacheck::plot_ce(
        df = context$model$data_filtered() %>% as.data.frame(),
        e_int = context$outcomes$intervention_total_discounted_qalys,
        c_int = context$outcomes$intervention_total_discounted_costs,
        e_comp = context$outcomes$comparator_total_discounted_qalys,
        c_comp = context$outcomes$comparator_total_discounted_costs
      )
    }
  })
  
  
  ### plotly/nb_plot ----
  output$nb_plot <- renderPlot({
    if (context$outcomes$intervention_total_discounted_qalys != "" &&
        context$outcomes$intervention_total_discounted_costs != "" &&
        context$outcomes$comparator_total_discounted_qalys != "" &&
        context$outcomes$comparator_total_discounted_costs != "") {
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
    if (context$outcomes$intervention_total_discounted_qalys != "" &&
        context$outcomes$intervention_total_discounted_costs != "" &&
        context$outcomes$comparator_total_discounted_qalys != "" &&
        context$outcomes$comparator_total_discounted_costs != "") {
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
    convergence_inputs()
  })
  
  ### datatable/convergence_table
  output$convergence_table <- renderDataTable({
    convergence_inputs()$data
  })
}
