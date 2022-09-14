library(bs4Dash)
library(shiny)

welcomeUI <- tabItem(
  "welcome",
  box(
    title = "Welcome to the PACBOARD",
    status = "primary",
    width = 12,
    p("The Probabilistic Analysis Check dashBOARD (PACBOARD) is an interactive tool to systematically explore and validate the inputs and outputs of a probabilistic analysis obtained from a health economic model.", 
      br(),
      "To use PACBOARD, you need a dataset (or dataframe) containing both inputs and (intermediate) outputs of a probabilistic analysis. The first row of the dataset should contain the input and output names."),
    p("PACBOARD is composed of 6 different tabs:", 
      br(),
      "1. The", strong("Prepare data-tab"), ": upload your data and group variables in 'costs', '(dis)utilities', 'probabilities', and 'relative effectiveness'. In this tab, the first quick validation checks are performed on your inputs and outputs. You can also define whether you want to calculate the incremental costs and effects of your intervention and the (incremental) net benefits if the dataset does not contain it already.",
      br(),
      "2. The", strong("Data inspection-tab"),": obtain summary statistics of the inputs and outputs, investigate the correlation between inputs and outputs.",
      br(),
      "3. The", strong("Model outcomes-tab"), ": displays the incremental cost-effectiveness plane, the cost-effectiveness acceptability curve and other visuals to investigate your model outcomes.",
      br(),
      "4. The", strong("Metamodelling-tab"), ": investigate the relation between model inputs and outputs using metamodelling and interactive graphs.",
      br(),
      "5. The", strong("Metamodel predictions-tab"), ": use the metamodel you have fitted in the", em(strong("Metamodelling-tab")),  "to make predictions.",
      br(),
      "6. The", strong("Survival analysis-tab"), ": investigate whether two parametric survival models are crossing each other in any iteration based on parameters of the survival curves. Plot the survival models from specific iterations."),
    p("In case you are only curious, you can use the pre-loaded data, by clicking the 'Use test data' button on the top of the screen.")
  )
)
