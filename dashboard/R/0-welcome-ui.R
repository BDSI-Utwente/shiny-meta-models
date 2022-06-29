library(bs4Dash)
library(shiny)

welcomeUI <- tabItem(
  "welcome",
  box(
    title = "Welcome to the PACBOARD",
    status = "primary",
    width = 12,
    p("The Probabilistic Analysis Check dashBOARD (PACBOARD) is an interactive tool to systematically validate and explore the results of a probabilistic analysis from a health economic model."),
    p("To use PACBOARD, you need a dataset (or dataframe) containing both inputs and (intermediate) outputs of a probabilistic analysis. The column of the dataset should represent the input or output name."),
    p("PACBOARD is composed of 5 different tabs."),
    p("The Prepare data-tab: upload your data and group variables in 'costs', '(dis)utilities', 'probabilities', and 'relative effectiveness'. In this tab, the first quick checks are performed on your inputs and outputs You can also define whether you want to calculate the net benefits if the dataset does not contain it already."),
    p("The Data inspection-tab: obtain summary statistics of the inputs and outputs, investigate the correlation between inputs and outputs."),
    p("The Model outcomes-tab: displays the incremental cost-effectiveness plane and the cost-effectiveness acceptability curve and other visuals to investigate your model outcomes."),
    p("The Relations-tab: investigate the relation between model inputs and outputs using interactive graphs and metamodelling."),
    p("The Meta-model predictions-tab: use the metamodel you have fitted to make predictions."),
    p("In case you are only curious, you can use the pre-loaded data, by clicking the 'Use test data' button on the top of the tabs.")
  )
)