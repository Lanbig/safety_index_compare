library(shiny)
library(plotly)
library(DT)
library(shinythemes)
require(here)
require(dplyr)

source(here("module_correlation/correlation.R"))
source(here("module_ranking/rankingtable.R"))

shinyUI(
  
  navbarPage("UL Safety Index", 
      
      theme = shinytheme("lumen"),
             
      tabPanel("Correlation", correlationScatterUI("corr")),
      tabPanel("Ranking Compare", rankingTableUI("rank")),
      
      tabPanel("Data Query Tool",
               fluidPage(
                 fluidRow(
                   column(3,
                          sidebarPanel( width=12, tags$b("Choose: Year"),
                                        selectizeInput('qtyear', 'Year', c('2000', '2005', '2010', '2015'), selected = '2015')
                          ),
                          
                          sidebarPanel( width=12, 
                                        tags$b("Download full Dataset .CSV"),
                                        tags$br(),
                                        htmlOutput("dispDL3")
                                        
                          )
                   ),
                   column(8,
                          tabPanel( "Dataset", 
                                    tags$br(),
                                    htmlOutput("dispDataTitle"),
                                    tags$br(),
                                    dataTableOutput("view"))
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

               
      )
          

  )
  
)
