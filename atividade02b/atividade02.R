# Deploy feito em https://pcecilio.shinyapps.io/atv2b/
if (!require('shiny')) install.packages('shiny')
library(shiny)

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#options(shiny.reactlog=TRUE)
#options(warning.expression=quote(recover()))
runApp('atv2')
