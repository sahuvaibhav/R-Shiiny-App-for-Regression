install.packages("shinydashboard")

library(shiny)
library("shinydashboard",lib.loc = 'C:/Users/Vaibhav/Documents/R/win-library/3.1')

if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shinyapps")





shinyApp(ui,server)