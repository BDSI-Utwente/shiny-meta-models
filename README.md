# shiny-meta-models
Shiny dashboard to validate and explore probabilistic inputs and outputs of health economic models, using metamodelling methods

## contents
The `dashboard` folder contains the source code for a shiny frontend to the `pacheck` package (which is referenced as a submodule in the `pacheck` folder). 
The `explore` folder contains scripts and snippets used while developing and testing.

## demo
A live demo of the PACBOARD application can be found at https://bdsi.shinyapps.io/pacboard/

## development
In a terminal, clone the repository and `cd` into it.

- `git clone https://github.com/BDSI-Utwente/shiny-meta-models.git`
- `cd shiny-meta-models`

Initialize the `pacheck` submodule.
- `git submodule update --init --recursive`

Open `dashboard/dashboard.Rproj` in your favourite editor (probably RStudio), then install the required R packages.
- `renv::restore()`
Note that at the moment of writing this, the `MRAN` repository used by `renv` for historical versions of packages is in limbo, and `renv::restore()` may fail. If this occurs, setting `options(renv.config.mran.enabled = FALSE)` may solve the problem.

You can now start a local server directly from the RStudio UI
- open `dashboard/app.R`, then click the `Run app` button

or from the R console by running
- `shiny::runapp("app.R")`

Application logic is contained in `dashboard/app.R`, with handler functions for the specific tabs located in `dashboard/R/`, and various helper functions in `dashboard/functions`. Most of the core functionality underlying the dashboard is provided by the [`pacheck`](https://github.com/Xa4P/pacheck) package.
