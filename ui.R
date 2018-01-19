library(shiny)
library(plotly)
library(DT)
library(shinythemes)
require(here)
require(dplyr)

source(here("module_correlation/correlation.R"))
source(here("module_ranking/rankingtable.R"))
source(here("module_dataquery/dataquery.R"))
source(here("module_timeseries/timeseries.R"))

shinyUI(
  
  navbarPage("UL Safety Index", 
      
      theme = shinytheme("lumen"),
             
      tabPanel("Correlation", correlationScatterUI("corr")),
      tabPanel("Rank Compare", rankingTableUI("rank")),
      tabPanel("Data Query Tool", dataQueryUI("dataquery")),
      tabPanel("Time Series", timeSeriesUI("timeseries"))

  )
  
)
