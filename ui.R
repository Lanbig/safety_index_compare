library(shiny)
library(plotly)
library(DT)
library(shinythemes)

safetydata <- read.csv("combined2015.csv")

shinyUI(
  
  navbarPage("UL Safety Index",
             
      tabPanel("Correlation",
      
               theme = shinytheme("lumen"),
               
               sidebarPanel(
                 selectInput('IndicatorX', 'X: Indicator', c( names(safetydata)), selected = 'transport_injuries'),
                 selectInput('IndicatorY', 'Y: Indicator', c( names(safetydata)), selected = 'ul_safety_index')
               ),
               
               mainPanel(
                 
                 tabsetPanel(
                   tabPanel( "Scatter Plot", fluidRow(plotlyOutput('distPlot', height = 500), DT::dataTableOutput('dispData') )),
                   tabPanel( "Residule Plot", plotlyOutput('residualPlot', height = 500), p("... information here ...")  ),
                   tabPanel( "Summary", verbatimTextOutput("summary"))
                 )   
                 
               )
                      
      ),
      
      tabPanel("Help",
               p("Nothing but blank page")
      )

          

  )
  
)
