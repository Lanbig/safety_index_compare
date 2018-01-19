require(shiny)
require(plotly)
require(DT)
require(broom)
require(psych)
require(sqldf)
require(here)
require(dplyr)


dataQueryUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(3,
             sidebarPanel( width=12, tags$b("Choose: Year"),
                           selectizeInput(ns('qtyear'), 'Year', c('2000', '2005', '2010', '2016','2017'), selected = '2017')
             )

      ),
      column(8,
             tabPanel( "Dataset", 
                       tags$br(),
                       htmlOutput(ns("dispDataTitle")),
                       tags$br(),
                       dataTableOutput(ns("view"))
                    )
      )  
    )
  )
   
}

dataQuery <- function(input, output, session){
  
  qtdata <- reactive({
    year <- input$qtyear
    cat(year)
    inputfile <- paste(here('asset/datasets/UL_safety_index_data_'),year,'.csv', sep = "")
    print(inputfile)
    data <- read.csv(inputfile, na.strings = "NULL")
  })
  
  output$view <- DT::renderDataTable(
    qtdata(),
    rownames = FALSE,
    filter = 'top',
    options=list(dom = 'Bfrtlip', buttons = list( 
      I('colvis'),
      
      list(
        extend = 'collection',
        buttons = c('csv', 'excel'),
        text = 'Download')), 
      
      scrollX = TRUE,  
      colReorder = TRUE, 
      pageLength = 50, 
      lengthMenu = c(10, 25 ,50, 100,200),
      
      columnDefs = list(
        list(targets = {c(1,5:43,45:72)}, visible = FALSE)
      )
      
    ),
    
    extensions = c('Buttons','FixedColumns','ColReorder')
  )
  
  output$dispDataTitle <- renderText({
    HTML(paste0("<b>Displaying : ",input$qtyear," Data </b>"))
  })
  

}