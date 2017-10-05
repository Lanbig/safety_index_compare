library(shiny)
library(plotly)
library(DT)
library(shinythemes)

safetydata <- read.csv("combined2015.csv")

shinyUI(
  
  navbarPage("UL Safety Index", 
      
      theme = shinytheme("lumen"),
             
      tabPanel("Correlation",
      
               fluidPage(
                 fluidRow(
                   column(3,
                          sidebarPanel( width=12, tags$b("Choose: Year and Indicators"),
                                        selectInput('year', 'Year', c('2000', '2005', '2010', '2015'), selected = '2015'),
                                        selectInput('IndicatorX', 'X: Indicator', c( names(safetydata)), selected = 'transport_injuries'),
                                        selectInput('IndicatorY', 'Y: Indicator', c( names(safetydata)), selected = 'ul_safety_index')
                          ),
                          
                          sidebarPanel( width=12, 
                                        tags$b("Result: Spearman's rank correlation rho "),
                                        tags$br(),
                                        htmlOutput("dispCor")
                                       
                          ),
                          
                          sidebarPanel( width=12, 
                                        tags$b("Download Dataset .CSV"),
                                        tags$br(),
                                        htmlOutput("dispDL")
                                        
                          )
                          
                   ),
                   
                   column(8,
                          tabsetPanel(
                            tabPanel( "Scatter Plot", fluidRow(plotlyOutput('distPlot', height = 500), DT::dataTableOutput('dispData') )),
                            tabPanel( "Residual Plot", plotlyOutput('residualPlot', height = 500), p("")  ),
                            tabPanel( "Summary", 
                                      tags$br(),
                                      tags$b("Spearman's rank correlation rho"),
                                      verbatimTextOutput("summary_cor"), 
                                      tags$b("Linear Regression"),
                                      verbatimTextOutput("summary_lm")),
                            tabPanel( "Dataset", 
                                      tags$br(),
                                      htmlOutput("dispDataTitle"),
                                      tags$br(),
                                      dataTableOutput("view"))
                          ) 
                   )
                 )
               )
               
               

               
               
           
                      
      ),
      
      tabPanel("Time Series",
               
               fluidPage(
                 fluidRow(
                      p("Soon... Nothing but blank page...")
                 )
               )
      ),

      tabPanel("Help",
               p("Nothing but blank page....")
      )
          

  )
  
)
