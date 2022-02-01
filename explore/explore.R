# install.packages("devtools")
# devtools::install_github("Xa4P/pacheck")

library(pacheck)
library(knitr)
library(plotly)
library(ggplot2)
data(df_pa)

df_pa <- calculate_nb(df = df_pa,
                      e_int = "QALY_int",
                      e_comp = "QALY_comp",
                      c_int = "Costs_int",
                      c_comp = "Costs_comp",
                      wtp = 80000)# calculate net benefits

df <- generate_sum_stats(df_pa)
kable(df)
rm(df)


generate_cor(df_pa)
