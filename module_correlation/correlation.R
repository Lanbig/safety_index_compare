require(shiny)
require(plotly)
require(DT)
require(broom)
require(psych)
require(sqldf)
require(here)

# Load dataset for listing indicators 
safetydata <- read.csv(here('asset/datasets/UL_safety_index_data_2017.csv'))

# Correlation User Interface
correlationScatterUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      
      # Left side menu
      column(3,
             
             # Selection box - Indicators
             sidebarPanel( width = 12, tags$b("Choose: Year and Indicators"),
                           selectizeInput(ns('year'), 'Year', c('2000', '2005', '2010', '2016','2017'), selected = '2017'),
                           selectizeInput(ns('IndicatorX'), 'X: Indicator', c( names(safetydata)[3:41]), selected = 'transport_injuries'),
                           selectizeInput(ns('IndicatorY'), 'Y: Indicator', c( names(safetydata)[3:41]), selected = 'ul_safety_index')
             ),
             
             # Corelation 
             sidebarPanel( width = 12, htmlOutput(ns('dispCor'))
             ),
             
             # Download box
             sidebarPanel( width = 12, tags$b("Download Dataset .CSV"),
                           tags$br(),
                           htmlOutput("dispDL")
             )
             
      ),
      
      # Main 
      column(8,
             tabsetPanel(
               tabPanel( "Scatter Plot", fluidRow(plotlyOutput(ns('distPlot'), height = 500),dataTableOutput(ns('dispData')))),
               tabPanel( "Residual Plot", plotlyOutput(ns('residualPlot'), height = 500), p("")  ),
               tabPanel( "Summary", 
                         tags$br(),
                         tags$b("Correlation"),
                         verbatimTextOutput(ns('summary_cor')), 
                         tags$b("Linear Regression"),
                         verbatimTextOutput(ns('summary_lm')))
               
             ) 
      )
    )
  )
}

# Logic and Calculation
correlationScatter <- function(input, output, session) {
  
  dataFile <- reactive({
    year <- input$year
    cat(year)
    inputfile <- paste(here('asset/datasets/UL_safety_index_data_'),year,'.csv', sep = "")
    print(inputfile)
    data <- read.csv(inputfile, na.strings = "NULL")
  })
  
  # Selected Indicator
  safetydata <- reactive( {
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    countryName <- 'country_slug'
    
    cat(input$IndicatorX)
    safetydata <- dataFile()[, c(countryName, columnY, columnX)]
    safetydata <- safetydata[complete.cases(safetydata), ]
  })

  safetydata_m <- reactive({
    safetydata_m <- lm(safetydata()[-1])
  })
  
  safetydata_cook <- reactive({
    cooks = cooks.distance(safetydata_m())
  })
  
  safetydata_cor <- reactive({
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    if(columnX == 'gdp_per_capita' | columnY == 'gdp_per_capita' | columnX == 'gdp_per_capita_rating' | columnY == 'gdp_per_capita_rating' )
      cor <- cor.test(safetydata()[,columnX], safetydata()[,columnY], method = "pearson")
    else
      cor <- cor.test(safetydata()[,columnX], safetydata()[,columnY], method = "spearman")
  })
  
  output$distPlot <- renderPlotly({
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    countryName <- 'country_slug'
  
    plot_ly(x = safetydata()[,columnX],y = safetydata()[,columnY], text = safetydata()[,countryName]) %>%
      
      add_text(text = safetydata()[,countryName], color= I("black"),
               textposition = "top right", name ="Country Name", visible = "legendonly")  %>%
      
      add_markers(showlegend = TRUE, name = "Country",
                  marker = list(size = 10,
                                color = 'rgba(0, 0, 0, .0)',
                                line = list(color = ifelse(safetydata_cook() > quantile(safetydata_cook(),.90),'rgba(152, 0, 0, .5)','rgba(0, 0, 0, .5)'),
                                            width = 2)))  %>%
      
      add_lines(y = ~fitted(lm(safetydata()[,columnY] ~ safetydata()[,columnX])),
                line = list(color = 'rgba(7, 16 181, 1)'),
                name = "Linear Line") %>%
      
      add_ribbons(data = augment(safetydata_m()),
                  ymin = ~.fitted - 1.96 * .se.fit,
                  ymax = ~.fitted + 1.96 * .se.fit,
                  line = list(color = 'rgba(7, 164, 181, 0.05)'),
                  fillcolor = 'rgba(7, 164, 181, 0.2)',
                  name = "Standard Error", visible = "legendonly") %>%
      
      layout(xaxis = list(title = columnX),
             yaxis = list(title = columnY),
             legend = list(x = 0.80, y = ifelse(safetydata_cor()$estimate > 0, 0.10, 0.90) ))   
  })
  
  output$dispData <- DT::renderDataTable(
    {safetydata <- safetydata()
    
    
    safetydata$Residuals <- safetydata_m()$residuals
    safetydata$Cooks <- safetydata_cook()
    
    safetydata},
    
    rownames = FALSE,
    options=list(dom = 'frtlip', scrollX = TRUE, colReorder = TRUE, pageLength = 10, lengthMenu = c(10, 25 ,50, 100,200)),
    extensions = c('ColReorder')
    
  )

  
  output$dispCor <- renderText({
    HTML(paste0("<b>",safetydata_cor()['method'],"</b>",
                "<br /><p>Selected Year : ",input$year,
                "
                </p><p>Selected Indicators : </p>
                <ul>
                <li>",input$IndicatorX,"</li>
                <li>",input$IndicatorY,"</li>
                </ul>", 
                
                "<b>Correlation Coefficient:</b> ",
                round(safetydata_cor()$estimate, digits = 4),                 
                "<br/> <b>P-Value:</b> ",
                round(safetydata_cor()$p.value, digits = 3), 
                "<br/> <b>Tests of Significance:</b> ",
                
                if(round(safetydata_cor()$p.value, digits = 4) <= 0.05)
                  "Passed"
                else
                  "Failed",
                
                "<br/> <b>Number of Obs:</b> ",
                nrow(safetydata())
                
    ))
  })
  
  output$residualPlot <- renderPlotly({
    countryName <- 'country_slug'
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    
    
    plot_ly(x = safetydata_m()$fitted.values,y = safetydata_m()$residuals, text = safetydata()[,countryName]) %>%    
      
      
      add_markers(showlegend = TRUE, name = "Country",
                  marker = list(size = 10,
                                color = 'rgba(0, 0, 0, .0)',
                                line = list(color = ifelse(safetydata_cook() > quantile(safetydata_cook(),.90),'rgba(152, 0, 0, .5)','rgba(0, 0, 0, .5)'),
                                            width = 2)))  %>%
      
      add_text(text = safetydata()[,countryName], color= I("black"),
               textposition = "top right", name ="Country Name", visible = "legendonly")  %>%
      
      layout(xaxis = list(title = 'Fitted Value - Linear Line'),
             yaxis = list(title = 'Residuals'),
             legend = list(x = 0.80, y = 0.90)) 
    
  })
  
  output$summary_lm <- renderPrint({ 
    summary(safetydata_m()) 
  }) 
  
  output$summary_cor <- renderPrint({ 
    print(safetydata_cor())
  }) 
}
