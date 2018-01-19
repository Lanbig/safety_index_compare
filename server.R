require(shiny)
require(plotly)
require(DT)
require(broom)
require(psych)
require(sqldf)
require(here)
require(dplyr)

source(here("module_correlation/correlation.R"))
source(here("module_ranking/rankingtable.R"))
source(here("module_dataquery/dataquery.R"))
source(here("module_timeseries/timeseries.R"))

options(warn =-1)

shinyServer(function(input, output, session) {
  
  callModule(correlationScatter, "corr")
  callModule(rankingTable, "rank")
  callModule(dataQuery, "dataquery")
  callModule(timeSeries, "timeseries")
  
})
