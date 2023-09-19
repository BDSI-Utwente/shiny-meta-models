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
      "1.", strong("Prepare data"), ": upload your data and group variables in 'costs', '(dis)utilities', 'probabilities', and 'relative effectiveness'. In this tab, the first quick validation checks are performed on your inputs and outputs. You can also define whether you want to calculate the incremental costs and effects of your intervention and the (incremental) net benefits if the dataset does not contain it already.",
      br(),
      "2.", strong("Data inspection"),": obtain summary statistics of the inputs and outputs, investigate the correlation between inputs and outputs.",
      br(),
      "3.", strong("Model outcomes"), ": displays the incremental cost-effectiveness plane, the cost-effectiveness acceptability curve and other graphs to investigate your model outcomes.",
      br(),
      "4.", strong("Metamodelling"), ": investigate the relation between model inputs and outputs using metamodelling and interactive graphs.",
      br(),
      "5.", strong("Metamodel predictions"), ": use the metamodel you have fitted in the", em(strong("Metamodelling-tab")),  "to make predictions.",
      br(),
      "6.", strong("Survival analysis"), ": investigate whether two parametric survival models are crossing each other in any iteration based on parameters of the survival curves. Plot the survival models from specific iterations."),
    p("In case you are only curious, you can use the pre-loaded probabilistic inputs and outputs, by clicking the 'Use test data' button on top of the screen."),
    p("The descriptions of the", 
      a(href = "https://xa4p.github.io/pacheck/articles/Appendix_A-HE_model_description.html", "health economic model"),  
      "and the",  
      a(href = "https://xa4p.github.io/pacheck/reference/df_pa.html", "preloaded probabilistic inputs and outputs"), 
      "are available on the website of the `pacheck` package"),
    p("This software is realease under the", a(href="https://www.gnu.org/licenses/gpl-3.0.en.html", "GNU GLP v3 license.")),
    p("If you have any questions about PACBOARD or would like to contribute to its further development, please reach out at:",
      a(href = "mailto: x.g.l.v.pouwels@utwente.nl", "x.g.l.v.pouwels@utwente.nl")
      )
  )
)
