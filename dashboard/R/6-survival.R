# UI ----
survivalUI <- tabItem(
  "survival",
  box(
    width = 12,
    title = "Survival analysis check",
    p("This tab allows to check whether survival models cross each other. <br> In this tab, PACBOARD identify whether the first survival curve is higher than the second at different point in time."),
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
      condition = "input['survival-surv-mod-1'] == 'exponential'",
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
      ),
    conditionalPanel(
      condition = "input['survival-surv-mod-1'] == 'Weibull'",
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
      condition = "input['survival-surv-mod-1'] == 'log-normal'",
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
      condition = "input['survival-surv-mod-1'] == 'log-logistic'",
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
    fluidRow(
      column(
        width = 6,
        selectInput(
          "survival-surv-mod-2",
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
          "survival-surv-mod-2-name",
          "Type here the name of the first survival model",
          value = "first survival")
      )
    ),
    conditionalPanel(
      condition = "input['survival-surv-mod-2'] == 'exponential'",
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
      condition = "input['survival-surv-mod-2'] == 'Weibull'",
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
      condition = "input['survival-surv-mod-2'] == 'log-normal'",
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
      condition = "input['survival-surv-mod-2'] == 'log-logistic'",
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
    fluidRow(
      p("With the following inputs, determine the time period over which the survival should be compared"),
      column(
        widht = 3,
        numericInput(
          "survival-time-from",
          "Start time period",
          min = 0,
          max = Inf,
          value = 0
        )
      ),
      column(
        widht = 3,
        numericInput(
          "survival-time-to",
          "End time period",
          min = 0,
          max = Inf,
          value = 5
        )
      ),
      column(
        widht = 3,
        numericInput(
          "survival-time-seq",
          "Intervals between the start and end of the time period",
          min = 0,
          max = Inf,
          value = 0.1
        )
      ),
      column(
        widht = 3,
        numericInput(
          "survival-n-view-crossing",
          "Number of iterations to mention in which the first survival curve is higher than the second",
          min = 1,
          max = Inf,
          value = 10
        )
      )
    )
    )
  )

# SERVER ----
survivalServer <- function(input, output, session, context) {
  context$survival <- reactiveValues(
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
    survival_model_2_log_logistic_scale = ""
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
  
  ## SERVER update other objects ----
  l_inputs_surv_mod <- list(
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
    }) %>% debounce(500)
  )
  
  ## OUTPUT ----
  ### ----
  
  }
