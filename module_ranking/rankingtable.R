require(shiny)
require(plotly)
require(DT)
require(broom)
require(psych)
require(sqldf)
require(here)
require(dplyr)

# Load dataset for listing indicators 
safetydata <- read.csv(here('asset/datasets/UL_safety_index_data_2017.csv'))

# ranking User Interface
rankingTableUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    # Compare by Peer Group UI
    tabsetPanel(  
      
      tabPanel( "Peer Groups", tags$br(), 
      column(12,
        wellPanel(
          fluidRow(
            
            column( width = 2, offset = 0, style='padding:10px;', selectizeInput(ns('year'), 'Year', c('2000', '2005', '2010', '2016','2017'), selected = '2017')),
            column( width = 5, offset = 0, style='padding:10px;', selectizeInput(ns('country'), 'Country', safetydata$country_slug, selected = 'Canada', multiple = FALSE)),
            column( width = 5, offset = 0, style='padding:10px;', selectizeInput(ns('peergroup'), 'Peer Groups', c(
                                                                                                            'population_decile',
                                                                                                            'youth_population_group',
                                                                                                            'older_population_group',
                                                                                                            'gdp_decile',
                                                                                                            'oecd','g7','g20',
                                                                                                            'un_region',
                                                                                                            'un_sub_region',
                                                                                                            'un_development_status',
                                                                                                            'who_region',
                                                                                                            'iso_membership',
                                                                                                            'iec_membership'
                                                                                                          ), selected = 'population_decile')) 
          )
        )
      ),
      column( width = 12, dataTableOutput(ns('rankingResult')))
        
      ),
      
      # Compare by custom selection - Choose individual country
      tabPanel( "Custom selection", tags$br(), 
      column(12,
         wellPanel(
           fluidRow(
             
             column( width = 2, offset = 0, style='padding:10px;',  selectizeInput(ns('year'), 'Year', c('2000', '2005', '2010', '2016','2017'), selected = '2017')),
             column( width = 10, offset = 0, style='padding:10px;', selectizeInput(ns('countries'), 'Country', safetydata$country_slug, selected = c('Japan','Canada'), multiple = TRUE))
           )
          )
      ))
    
    ) # fluidPage
      
)}

rankingTable <- function(input, output, session){
  
  #Reactive - react from user selection - year
  dataFile <- reactive({
    year <- input$year
    country <- input$country
    peergroup <- input$peergroup
    
    inputfile <- paste(here('asset/datasets/UL_safety_index_data_'),year,'.csv', sep = "")
    print(inputfile)
    data <- read.csv(inputfile, na.strings = "NULL")
    
    filterValue <- data[data$country_slug==country, eval(peergroup)]
    cat(peergroup,"-" ,filterValue)
    
    data <- data[,c(eval(peergroup),'country_slug','ul_safety_index','institutions_resources','safety_frameworks','safety_outcomes')]
    data <- data[which(data[[1]] == filterValue),]
    
    rownames(data) <- data[,'country_slug']
    tdata <- t(data[c(-1,-2)])
    
    rownames(tdata) <- c("UL Safety Index", "~   Institutions Resources", "*Safety Frameworks", "Safety Outcomes")
    tdata
    
  })  
  
  
  output$rankingResult <- DT::renderDataTable(
    dataFile(),
    
    options=list(dom = 'Bfrtlip', buttons = list( 
      I('colvis'),
      
      list(
        extend = 'collection',
        buttons = c('csv', 'excel'),
        text = 'Download')), 
      
      scrollX = TRUE,  
      colReorder = TRUE, 
      pageLength = 50, 
      lengthMenu = c(10, 25 ,50, 100,200)
    ),
    extensions = c('Buttons','FixedColumns','ColReorder')
    
  )
  
  
}
