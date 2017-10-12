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
                                        selectizeInput('year', 'Year', c('2000', '2005', '2010', '2015'), selected = '2015'),
                                        selectizeInput('IndicatorX', 'X: Indicator', c( names(safetydata)[4:40]), selected = 'transport_injuries'),
                                        selectizeInput('IndicatorY', 'Y: Indicator', c( names(safetydata)[4:40]), selected = 'ul_safety_index')
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
                            tabPanel( "Scatter Plot", fluidRow(plotlyOutput('distPlot', height = 500),dataTableOutput('dispData') )),
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
      
      #navbarMenu("Time Series",
      tabPanel("Time Series",         
               #tabPanel("By Country",
                        
                fluidPage(
                  column(3,
                    
                    sidebarPanel( width=12, selectizeInput('ts_countries', label= "Select: Country", choices = safetydata$country_slug, 
                                                           selected = NULL, multiple = FALSE)),
                    sidebarPanel( width=12, 
                                  tags$b("Download Dataset .CSV"),
                                  tags$br(),
                                  htmlOutput("dispDL2")
                                  
                    )
                    
                  ),
                  
                  column(8,
                    
                    tabsetPanel(
                      tabPanel( "Index & Drivers", tags$br(), 
                                plotlyOutput('ts_idx'), 
                                tags$br(),
                                tags$h4("Ranking"),
                                dataTableOutput('dispTStableIdx')),
                      
                      tabPanel( "Instution and Resources & Indicators", tags$br(), 
                                plotlyOutput('ts_institutions_resources'), 
                                tags$br(),
                                tags$h4("Ranking"),
                                dataTableOutput('dispTStableIR')),
                      
                      tabPanel( "Safety Outcome & Indicators",  tags$br(), 
                                plotlyOutput('ts_outcome') ,
                                tags$br(),
                                tags$h4("Ranking"),
                                dataTableOutput('dispTStableSO')                           
                                ),
                      tabPanel( "Safety Framework & Indicators",   tags$br(), 
                                plotlyOutput('ts_safety_frameworks'), 
                                tags$br(),
                                tags$h4("Ranking"),
                                dataTableOutput('dispTStableSF')
                                )
                    )   
                    
                  )
                 
               #  )
               )

               
      ),

      tabPanel("Help",
               p("Nothing but blank page....")
      )
          

  )
  
)
