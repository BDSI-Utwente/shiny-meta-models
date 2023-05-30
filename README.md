# shiny-meta-models
Shiny dashboard to validate and explore probabilistic inputs and outputs of health economic models, using metamodelling methods

## demo
A live demo of the PACBOARD application can be found at https://bdsi.shinyapps.io/pacboard/

## development
In a terminal, clone the repository and `cd` into it:

- `git clone https://github.com/BDSI-Utwente/shiny-meta-models.git`
- `cd shiny-meta-models`

Initialize the `pacheck` submodule:
- `git submodule update --init --recursive`

Open the project in your favourite editor (probably RStudio), then install the required R packages:
- open `dashboard/dashboard.Rproj`
- in an R console, run `renv::restore()`

> Note that at time of writing, the `MRAN` repository used by `renv` for historical binary versions of packages is in limbo, and `renv::restore()` may fail. If this occurs, setting `options(renv.config.mran.enabled = FALSE)` may solve the problem, but will take much longer to complete as historical binaries will have to be compiled from source.

You can now start a local server directly from the RStudio UI;
- open `dashboard/app.R`, then click the `Run app` button

or from the R console by running:
- `shiny::runapp("app.R")`

The local server should automatically update and refresh on changes, but due to a bug/quirk/feature in how shiny handles file changes, only changes to the main `app.R` file are registered. Setting up an external file watcher to trigger updates may be beneficial for an extended development workflow, but is left as an exercise for the reader.

## Project Structure
The `dashboard/` folder contains the source code for a shiny frontend to the `pacheck` package (which is referenced as a submodule in the `pacheck/` folder). 

The `explore/` folder contains scripts and snippets used while developing and testing.

Application logic is contained in `dashboard/app.R`, with handler functions for the specific tabs located in `dashboard/R/`, and various helper functions in `dashboard/functions/`. Most of the core functionality underlying the dashboard is provided by the [`pacheck`](https://github.com/Xa4P/pacheck) package.
