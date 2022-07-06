# UI ----
survivalUI <- tabItem(
  "survival",
  box(
    width = 12,
    title = "Survival analysis check",
    p("This tab allows to check whether survival models cross each other. In this tab, PACBOARD identify whether the first survival curve is higher than the second at different point in time."),
    hr(),
    fluidRow(
      column(
        width = 6,
        selectInput(
          "survival-surv-mod-1",
          "First survival model",
          choices = c("Please select" = "", 
                      "exponential" = "exp", 
                      "Weibull" = "weibull", 
                      "gamma" = "gamma", 
                      "log-normal" = "lnorm",
                      "log-logistic" = "logis"),
          selected = NULL)
        ),
      column(
        width = 6,
        textInput(
          "survival-surv-mod-1-name",
          "Type here the name of the first survival model",
          value = "first survival")
        )
      ),
    conditionalPanel(
      condition = "input['survival-surv-mod-1'] == 'exp'",
      {
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-1-exponential-rate",
            label = "Rate",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
            )
          )
        )
      }
      ),
    conditionalPanel(
      condition = "input['survival-surv-mod-1'] == 'weibull'",
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-1-weibull-shape",
            label = "Shape",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
            )
          ),
        column(
          width = 4,
            selectizeInput(
              "survival-model-1-weibull-scale",
              label = "Scale",
              multiple = FALSE,
              choices = c("no data loaded..." = "")          
              )
          )
        )
      ),
    conditionalPanel(
      condition = "input['survival-surv-mod-1'] == 'gamma'",
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-1-gamma-shape",
            label = "Shape",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
          )
        ),
        column(
          width = 4,
          selectizeInput(
            "survival-model-1-gamma-rate",
            label = "Rate",
            multiple = FALSE,
            choices = c("no data loaded..." = "")          
          )
          )
        )
      ),
    conditionalPanel(
      condition = "input['survival-surv-mod-1'] == 'lnorm'",
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-1-log-normal-meanlog",
            label = "Meanlog",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
          )
        ),
        column(
          width = 4,
          selectizeInput(
            "survival-model-1-log-normal-sdlog",
            label = "Sdlog",
            multiple = FALSE,
            choices = c("no data loaded..." = "")          
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input['survival-surv-mod-1'] == 'logis'",
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-1-log-logistic-location",
            label = "Location",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
          )
        ),
        column(
          width = 4,
          selectizeInput(
            "survival-model-1-log-logistic-scale",
            label = "Scale",
            multiple = FALSE,
            choices = c("no data loaded..." = "")          
          )
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 6,
        selectInput(
          "survival-surv-mod-2",
          "Second survival model",
          choices = c("Please select" = "", 
                      "exponential" = "exp", 
                      "Weibull" = "weibull", 
                      "gamma" = "gamma", 
                      "log-normal" = "lnorm",
                      "log-logistic" = "logis"),
          selected = NULL)
      ),
      column(
        width = 6,
        textInput(
          "survival-surv-mod-2-name",
          "Type here the name of the second survival model",
          value = "second survival")
      )
    ),
    conditionalPanel(
      condition = "input['survival-surv-mod-2'] == 'exp'",
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-2-exponential-rate",
            label = "Rate",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input['survival-surv-mod-2'] == 'weibull'",
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-2-weibull-shape",
            label = "Shape",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
          )
        ),
        column(
          width = 4,
          selectizeInput(
            "survival-model-2-weibull-scale",
            label = "Scale",
            multiple = FALSE,
            choices = c("no data loaded..." = "")          
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input['survival-surv-mod-2'] == 'gamma'",
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-2-gamma-shape",
            label = "Shape",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
          )
        ),
        column(
          width = 4,
          selectizeInput(
            "survival-model-2-gamma-rate",
            label = "Rate",
            multiple = FALSE,
            choices = c("no data loaded..." = "")          
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input['survival-surv-mod-2'] == 'lnorm'",
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-2-log-normal-meanlog",
            label = "Meanlog",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
          )
        ),
        column(
          width = 4,
          selectizeInput(
            "survival-model-2-log-normal-sdlog",
            label = "Sdlog",
            multiple = FALSE,
            choices = c("no data loaded..." = "")          
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input['survival-surv-mod-2'] == 'logis'",
      fluidRow(
        column(
          width = 4,
          selectizeInput(
            "survival-model-2-log-logistic-location",
            label = "Location",
            multiple = FALSE,
            choices = c("no data loaded..." = "")
          )
        ),
        column(
          width = 4,
          selectizeInput(
            "survival-model-2-log-logistic-scale",
            label = "Scale",
            multiple = FALSE,
            choices = c("no data loaded..." = "")          
          )
        )
      )
    ),
    hr(),
    p("With the following inputs, determine the time period over which the survival should be compared"),
    fluidRow(
      column(
        width = 3,
        numericInput(
          "survival-time-from",
          "Start time period",
          min = 0,
          max = Inf,
          value = 0
        )
      ),
      column(
        width = 3,
        numericInput(
          "survival-time-to",
          "End time period",
          min = 0,
          max = Inf,
          value = 5
        )
      ),
      column(
        width = 3,
        numericInput(
          "survival-time-seq",
          "Intervals between the start and end of the time period",
          min = 0,
          max = Inf,
          value = 0.1
        )
      ),
      column(
        width = 3,
        numericInput(
          "survival-n-view-crossing",
          "Number of iterations to mention in which the first survival curve is higher than the second",
          min = 1,
          max = Inf,
          value = 10
        )
      )
    ),
    ### textoutput/txt_surv_mod_check ----
    textOutput("txt_surv_mod_check")
    ),
  box(
    width = 12,
    title = "Survival analysis plot",
    collapsed = TRUE,
    p("This tab allows you to plot the survival model for a specific iteration, using the above-selected inputs."),
    fluidRow(
      column(
        width = 4,
        numericInput(
          "survival-plot-iteration",
          "Interation to plot",
          min = 0,
          max = Inf,
          value = 0)
        )
      ),
    ### plotoutput/plot_survival_models ----
    plotOutput("plot_survival_models")
    )
  )

# SERVER ----
survivalServer <- function(input, output, session, context) {
  context$survival <- reactiveValues(
    survival_model_check = NULL,
    survival_model_1_exponential_rate = "",
    survival_model_1_weibull_shape = "",
    survival_model_1_weibull_scale = "",
    survival_model_1_gamma_shape = "",
    survival_model_1_gamma_rate = "",
    survival_model_1_log_normal_meanlog = "",
    survival_model_1_log_normal_sdlog = "",
    survival_model_1_log_logistic_location = "",
    survival_model_1_log_logistic_scale = "",
    survival_model_2_exponential_rate = "",
    survival_model_2_weibull_shape = "",
    survival_model_2_weibull_scale = "",
    survival_model_2_gamma_shape = "",
    survival_model_2_gamma_rate = "",
    survival_model_2_log_normal_meanlog = "",
    survival_model_2_log_normal_sdlog = "",
    survival_model_2_log_logistic_location = "",
    survival_model_2_log_logistic_scale = "",
    v_params_mod_1 = c(),
    v_params_mod_2 = c()
  )
  
  ## UI update observers ----
  updateSurvivalVariableChoices <- observe({
    ### survival-model-1-exponential-rate ----
    updateSelectizeInput(
      session,
      "survival-model-1-exponential-rate",
      choices = context$model$variables,
      selected = context$survival$survival_model_1_exponential_rate
    )
    ### survival-model-1-weibull-shape ----
    updateSelectizeInput(
      session,
      "survival-model-1-weibull-shape",
      choices = context$model$variables,
      selected = context$survival$survival_model_1_weibull_shape
    )
    ### survival-model-1-weibull-scale ----
    updateSelectizeInput(
      session,
      "survival-model-1-weibull-scale",
      choices = context$model$variables,
      selected = context$survival$survival_model_1_weibull_scale
    )
    ### survival-model-1-gamma-shape ----
    updateSelectizeInput(
      session,
      "survival-model-1-gamma-shape",
      choices = context$model$variables,
      selected = context$survival$survival_model_1_gamma_shape
    )
    ### survival-model-1-gamma-rate ----
    updateSelectizeInput(
      session,
      "survival-model-1-gamma-rate",
      choices = context$model$variables,
      selected = context$survival$survival_model_1_gamma_rate
    )
    ### survival-model-1-log-normal-meanlog ----
    updateSelectizeInput(
      session,
      "survival-model-1-log-normal-meanlog",
      choices = context$model$variables,
      selected = context$survival$survival_model_1_log_normal_meanlog
    )
    ### survival-model-1-log-normal-sdlog ----
    updateSelectizeInput(
      session,
      "survival-model-1-log-normal-sdlog",
      choices = context$model$variables,
      selected = context$survival$survival_model_1_log_normal_sdlog
    )
    ### survival-model-1-log-logistic-location ----
    updateSelectizeInput(
      session,
      "survival-model-1-log-logistic-location",
      choices = context$model$variables,
      selected = context$survival$survival_model_1_log_logistic_location
    )
    ### survival-model-1-log-logistic-scale ----
    updateSelectizeInput(
      session,
      "survival-model-1-log-logistic-scale",
      choices = context$model$variables,
      selected = context$survival$survival_model_1_log_logistic_scale
    )
    ### survival-model-2-exponential-rate ----
    updateSelectizeInput(
      session,
      "survival-model-2-exponential-rate",
      choices = context$model$variables,
      selected = context$survival$survival_model_2_exponential_rate
    )
    ### survival-model-2-weibull-shape ----
    updateSelectizeInput(
      session,
      "survival-model-2-weibull-shape",
      choices = context$model$variables,
      selected = context$survival$survival_model_2_weibull_shape
    )
    ### survival-model-2-weibull-scale ----
    updateSelectizeInput(
      session,
      "survival-model-2-weibull-scale",
      choices = context$model$variables,
      selected = context$survival$survival_model_2_weibull_scale
    )
    ### survival-model-2-gamma-shape ----
    updateSelectizeInput(
      session,
      "survival-model-2-gamma-shape",
      choices = context$model$variables,
      selected = context$survival$survival_model_2_gamma_shape
    )
    ### survival-model-2-gamma-rate ----
    updateSelectizeInput(
      session,
      "survival-model-2-gamma-rate",
      choices = context$model$variables,
      selected = context$survival$survival_model_2_gamma_rate
    )
    ### survival-model-2-log-normal-meanlog ----
    updateSelectizeInput(
      session,
      "survival-model-2-log-normal-meanlog",
      choices = context$model$variables,
      selected = context$survival$survival_model_2_log_normal_meanlog
    )
    ### survival-model-2-log-normal-sdlog ----
    updateSelectizeInput(
      session,
      "survival-model-2-log-normal-sdlog",
      choices = context$model$variables,
      selected = context$survival$survival_model_2_log_normal_sdlog
    )
    ### survival-model-2-log-logistic-location ----
    updateSelectizeInput(
      session,
      "survival-model-2-log-logistic-location",
      choices = context$model$variables,
      selected = context$survival$survival_model_2_log_logistic_location
    )
    ### survival-model-2-log-logistic-scale ----
    updateSelectizeInput(
      session,
      "survival-model-2-log-logistic-scale",
      choices = context$model$variables,
      selected = context$survival$survival_model_2_log_logistic_scale
    )
      }) %>% bindEvent(context$model$variables)
  
  ## SERVER update observers ----
  ### context$survival$survival_model_1_exponential_rate ----
  updateModel1ExponentialRate <- observe({
    context$survival$survival_model_1_exponential_rate <- input$`survival-model-1-exponential-rate`
  }) %>% bindEvent(input$`survival-model-1-exponential-rate`)
  ### context$survival$survival_model_1_weibull_shape ----
  updateModel1WeibullShape <- observe({
    context$survival$survival_model_1_weibull_shape <- input$`survival-model-1-weibull-shape`
  }) %>% bindEvent(input$`survival-model-1-weibull-shape`)
  ### context$survival$survival_model_1_weibull_scale ----
  updateModel1WeibullScale <- observe({
    context$survival$survival_model_1_weibull_scale <- input$`survival-model-1-weibull-scale`
  }) %>% bindEvent(input$`survival-model-1-weibull-scale`)
  ### context$survival$survival_model_1_gamma_shape ----
  updateModel1GammaShape <- observe({
    context$survival$survival_model_1_gamma_shape <- input$`survival-model-1-gamma-shape`
  }) %>% bindEvent(input$`survival-model-1-gamma-shape`)
  ### context$survival$survival_model_1_gamma_rate ----
  updateModel1GammaRate <- observe({
    context$survival$survival_model_1_gamma_rate <- input$`survival-model-1-gamma-rate`
  }) %>% bindEvent(input$`survival-model-1-gamma-rate`)
  ### context$survival$survival_model_1_log_normal_meanlog ----
  updateModel1LogNormalMeanlog <- observe({
    context$survival$survival_model_1_log_normal_meanlog <- input$`survival-model-1-log-normal-meanlog`
  }) %>% bindEvent(input$`survival-model-1-log-normal-meanlog`)
  ### context$survival$survival_model_1_log_normal_sdlog ----
  updateModel1LogNormalSdlog <- observe({
    context$survival$survival_model_1_log_normal_sdlog <- input$`survival-model-1-log-normal-sdlog`
  }) %>% bindEvent(input$`survival-model-1-log-normal-sdlog`)
  ### context$survival$survival_model_1_log_logistic_location ----
  updateModel1LoglogisticLocation <- observe({
    context$survival$survival_model_1_log_logistic_location <- input$`survival-model-1-log-logistic-location`
  }) %>% bindEvent(input$`survival-model-1-log-logistic-location`)
  ### context$survival$survival_model_1_log_logistic_sdlog ----
  updateModel1LoglogisticScale <- observe({
    context$survival$survival_model_1_log_logistic_scale <- input$`survival-model-1-log-logistic-scale`
  }) %>% bindEvent(input$`survival-model-1-log-logistic-scale`)
  ### context$survival$survival_model_2_exponential_rate ----
  updateModel2ExponentialRate <- observe({
    context$survival$survival_model_2_exponential_rate <- input$`survival-model-2-exponential-rate`
  }) %>% bindEvent(input$`survival-model-2-exponential-rate`)
  ### context$survival$survival_model_2_weibull_shape ----
  updateModel2WeibullShape <- observe({
    context$survival$survival_model_2_weibull_shape <- input$`survival-model-2-weibull-shape`
  }) %>% bindEvent(input$`survival-model-2-weibull-shape`)
  ### context$survival$survival_model_2_weibull_scale ----
  updateModel2WeibullScale <- observe({
    context$survival$survival_model_2_weibull_scale <- input$`survival-model-2-weibull-scale`
  }) %>% bindEvent(input$`survival-model-2-weibull-scale`)
  ### context$survival$survival_model_2_gamma_shape ----
  updateModel2GammaShape <- observe({
    context$survival$survival_model_2_gamma_shape <- input$`survival-model-2-gamma-shape`
  }) %>% bindEvent(input$`survival-model-2-gamma-shape`)
  ### context$survival$survival_model_2_gamma_rate ----
  updateModel2GammaRate <- observe({
    context$survival$survival_model_2_gamma_rate <- input$`survival-model-2-gamma-rate`
  }) %>% bindEvent(input$`survival-model-2-gamma-rate`)
  ### context$survival$survival_model_2_log_normal_meanlog ----
  updateModel2LogNormalMeanlog <- observe({
    context$survival$survival_model_2_log_normal_meanlog <- input$`survival-model-2-log-normal-meanlog`
  }) %>% bindEvent(input$`survival-model-2-log-normal-meanlog`)
  ### context$survival$survival_model_2_log_normal_sdlog ----
  updateModel2LogNormalSdlog <- observe({
    context$survival$survival_model_2_log_normal_sdlog <- input$`survival-model-2-log-normal-sdlog`
  }) %>% bindEvent(input$`survival-model-2-log-normal-sdlog`)
  ### context$survival$survival_model_2_log_logistic_location ----
  updateModel2LoglogisticLocation <- observe({
    context$survival$survival_model_2_log_logistic_location <- input$`survival-model-2-log-logistic-location`
  }) %>% bindEvent(input$`survival-model-2-log-logistic-location`)
  ### context$survival$survival_model_2_log_logistic_sdlog ----
  updateModel2LoglogisticScale <- observe({
    context$survival$survival_model_2_log_logistic_scale <- input$`survival-model-2-log-logistic-scale`
  }) %>% bindEvent(input$`survival-model-2-log-logistic-scale`)
  
  # ### context$survival$v_params_mod_1 ----
  updateSurvivialModdel1Parameters <- observe({
    if(l_inputs_surv_mod$surv_mod_1() == "exp") {
      context$survival$v_params_mod_1 <- context$survival$survival_model_1_exponential_rate
    } else if(l_inputs_surv_mod$surv_mod_1() == "weibull"){
      context$survival$v_params_mod_1 <- c(input$`survival-model-1-weibull-shape`, input$`survival-model-1-weibull-scale`)
    } else if(l_inputs_surv_mod$surv_mod_1() == "gamma"){
      context$survival$v_params_mod_1 <- c(input$`survival-model-1-gamma-shape`, input$`survival-model-1-gamma-rate`)
    } else if(l_inputs_surv_mod$surv_mod_1() == "lnorm"){
      context$survival$v_params_mod_1 <- c(input$`survival-model-1-log-normal-meanlog`, input$`survival-model-1-log-normal-sdlog`)
    } else if(l_inputs_surv_mod$surv_mod_1() == "logis"){
      context$survival$v_params_mod_1 <- c(input$`survival-model-1-log-logistic-location`, input$`survival-model-1-log-logistic-shape`)
    }
  }) %>% bindEvent(input$`survival-model-1-exponential-rate`,
                   input$`survival-model-1-weibull-shape`, input$`survival-model-1-weibull-scale`,
                   input$`survival-model-1-gamma-shape`, input$`survival-model-1-gamma-rate`,
                   input$`survival-model-1-log-normal-meanlog`, input$`survival-model-1-log-normal-sdlog`,
                   input$`survival-model-1-log-logistic-location`, input$`survival-model-1-log-logistic-shape`,
                   l_inputs_surv_mod$surv_mod_1())

  ### context$survival$v_params_mod_2 ----
  updateSurvivialModdel2Parameters <- observe({
    if(l_inputs_surv_mod$surv_mod_2() == "exp") {
      context$survival$v_params_mod_2 <- context$survival$survival_model_2_exponential_rate
    } else if(l_inputs_surv_mod$surv_mod_2() == "weibull"){
      context$survival$v_params_mod_2 <- c(input$`survival-model-2-weibull-shape`, input$`survival-model-2-weibull-scale`)
    } else if(l_inputs_surv_mod$surv_mod_2() == "gamma"){
      context$survival$v_params_mod_2 <- c(input$`survival-model-2-gamma-shape`, input$`survival-model-2-gamma-rate`)
    } else if(l_inputs_surv_mod$surv_mod_2() == "lnorm"){
      context$survival$v_params_mod_2 <- c(input$`survival-model-2-log-normal-meanlog`, input$`survival-model-2-log-normal-sdlog`)
    } else if(l_inputs_surv_mod$surv_mod_2() == "logis"){
      context$survival$v_params_mod_2 <- c(input$`survival-model-2-log-logistic-location`, input$`survival-model-2-log-logistic-shape`)
    }
  }) %>% bindEvent(input$`survival-model-2-exponential-rate`,
                   input$`survival-model-2-weibull-shape`, input$`survival-model-2-weibull-scale`,
                   input$`survival-model-2-gamma-shape`, input$`survival-model-2-gamma-rate`,
                   input$`survival-model-2-log-normal-meanlog`, input$`survival-model-2-log-normal-sdlog`,
                   input$`survival-model-2-log-logistic-location`, input$`survival-model-2-log-logistic-shape`,
                   l_inputs_surv_mod$surv_mod_2())

  ## SERVER update other objects ----
  l_inputs_surv_mod <- list(
    surv_mod_1  = reactive({
      input$`survival-surv-mod-1`
    }) %>% debounce(500),
    surv_mod_2  = reactive({
      input$`survival-surv-mod-2`
    }) %>% debounce(500),
    label_surv_1  = reactive({
      input$`survival-surv-mod-1-name`
    }) %>% debounce(500),
    label_surv_2  = reactive({
      input$`survival-surv-mod-2-name`
    }) %>% debounce(500),
    time_from = reactive({
      input$`survival-time-from`
    }) %>% debounce(500),
    time_to = reactive({
      input$`survival-time-to`
    }) %>% debounce(500),
    time_seq = reactive({
      input$`survival-time-seq`
    }) %>% debounce(500),
    n_view = reactive({
      input$`survival-n-view-crossing`
    }) %>% debounce(500),
    iteration = reactive({
      input$`survival-plot-iteration`
    }) %>% debounce(500)
  )
  
  ## OUTPUT ----
  ### context$survival$survival_model_check----
  context$survival$survival_model_check <- reactive({
    if(l_inputs_surv_mod$surv_mod_1() != "" &&
       l_inputs_surv_mod$surv_mod_2() != "" &&
       ((length(context$survival$v_params_mod_1) == 1 &&
         l_inputs_surv_mod$surv_mod_1() == "exp") |
        (length(context$survival$v_params_mod_1) == 2 &&
        l_inputs_surv_mod$surv_mod_1() != "exp")
        ) &&
       ((length(context$survival$v_params_mod_2) == 1 &&
         l_inputs_surv_mod$surv_mod_2() == "exp") |
        (length(context$survival$v_params_mod_2) == 2 &&
         l_inputs_surv_mod$surv_mod_2() != "exp")
       )
       ) {

    l_check_surv_mod <- pacheck::check_surv_mod(
      df = context$model$data_filtered() %>% as.data.frame(),
      surv_mod_1 = l_inputs_surv_mod$surv_mod_1(),
      surv_mod_2 = l_inputs_surv_mod$surv_mod_2(),
      v_names_param_mod_1 = context$survival$v_params_mod_1,
      v_names_param_mod_2 = context$survival$v_params_mod_2,
      time = seq(l_inputs_surv_mod$time_from(),
                 l_inputs_surv_mod$time_to(),
                 l_inputs_surv_mod$time_seq()),
      label_surv_1 = l_inputs_surv_mod$label_surv_1(),
      label_surv_2 = l_inputs_surv_mod$label_surv_2(),
      n_view = l_inputs_surv_mod$n_view()
      )
    }
    })
  ### output$txt_surv_mod_check ----
  output$txt_surv_mod_check <- renderText({
    context$survival$survival_model_check()$message
    })
  ### output$plot_survival_models----
  output$plot_survival_models <- renderPlot({
    if(l_inputs_surv_mod$surv_mod_1() != "" &&
       l_inputs_surv_mod$surv_mod_2() != "" &&
       l_inputs_surv_mod$iteration() != 0 &&
       ((length(context$survival$v_params_mod_1) == 1 &&
         l_inputs_surv_mod$surv_mod_1() == "exp") |
        (length(context$survival$v_params_mod_1) == 2 &&
         l_inputs_surv_mod$surv_mod_1() != "exp")
       ) &&
       ((length(context$survival$v_params_mod_2) == 1 &&
         l_inputs_surv_mod$surv_mod_2() == "exp") |
        (length(context$survival$v_params_mod_2) == 2 &&
         l_inputs_surv_mod$surv_mod_2() != "exp")
       )
    ) {
      
      p_out <- pacheck::plot_surv_mod(
        df = context$model$data_filtered() %>% as.data.frame(),
        surv_mod_1 = l_inputs_surv_mod$surv_mod_1(),
        surv_mod_2 = l_inputs_surv_mod$surv_mod_2(),
        v_names_param_mod_1 = context$survival$v_params_mod_1,
        v_names_param_mod_2 = context$survival$v_params_mod_2,
        time = seq(l_inputs_surv_mod$time_from(),
                   l_inputs_surv_mod$time_to(),
                   l_inputs_surv_mod$time_seq()),
        label_surv_1 = l_inputs_surv_mod$label_surv_1(),
        label_surv_2 = l_inputs_surv_mod$label_surv_2(),
        iteration = l_inputs_surv_mod$iteration()
      )
      p_out
    }
  })
   }
