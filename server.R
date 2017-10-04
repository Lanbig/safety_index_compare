library(shiny)
library(plotly)
library(DT)
library(broom)
library(psych)

shinyServer(function(input, output) {
  
  ####### Reactive ##########
  data <- reactive({
    year <- input$year
    inputfile <- paste('combined',year,'.csv', sep = "")
    print(inputfile)
    data <- read.csv(inputfile, na.strings = "NULL")
  })
  
  safetydata <- reactive( {
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    countryName <- 'country_slug'
    
    safetydata <- data()[, c(countryName, columnY, columnX)]
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
    cor <- cor.test(safetydata()[,columnX], safetydata()[,columnY], method = "spearman")
    
  })
  
  ####### Output ##########
  
  output$summary_lm <- renderPrint({ 
    summary(safetydata_m()) 
  }) 
  
  output$summary_cor <- renderPrint({ 
    print(safetydata_cor())
  }) 
  
  output$view <- DT::renderDataTable({ data() })
  
   
  output$distPlot <- renderPlotly({
    
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    countryName <- 'country_slug'
    
    plot_ly(x = safetydata()[,columnX],y = safetydata()[,columnY], text = safetydata()[,countryName]) %>%  
      
      add_text(text = safetydata()[,countryName], color= I("black"),
               textposition = "top right", name ="country Name", visible = "legendonly")  %>%
      
      add_markers(showlegend = FALSE, name = "",
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
             legend = list(x = 0.80, y = 0.90)) 

  })
  
  output$residualPlot <- renderPlotly({
    countryName <- 'country_slug'
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    
    plot_ly(x =  safetydata_m()$fitted.values,y =  safetydata_m()$residuals, color = I("black"), type = 'scatter', 
            mode = 'text',  text = safetydata()[,countryName],
            textposition = "top right", name ="country Name") %>%  
      
      add_markers(showlegend = FALSE, 
                  marker = list(size = 10,
                                color = 'rgba(0, 0, 0, .0)',
                                line = list(color = ifelse(safetydata_cook() > quantile(safetydata_cook(),.90),'rgba(152, 0, 0, .5)','rgba(0, 0, 0, .5)'),
                                            width = 2)))  %>%
      
      layout(xaxis = list(title = 'Fitted Value - Linear Line'),
             yaxis = list(title = 'Residuals'),
             legend = list(x = 0.80, y = 0.90)) 
    
  })
  
  output$dispData <- DT::renderDataTable({
    
    safetydata <- safetydata()
    
    safetydata$Residules <- safetydata_m()$residuals
    safetydata$Cooks <- safetydata_cook()
    
    safetydata

  })
 
  output$dispDataTitle <- renderText({
    HTML(paste0("<b>Displaying : ",input$year," Data </b>"))
  })

  output$dispCor <- renderText({
    HTML(paste0("<br />
                  <p>Selected Year : ",input$year,
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
                    "Failed"
                ))
  })
  
  output$dispDL <- renderText({
    HTML(paste0('<a href="combined2000.csv" class="btn btn-primary btn-sm">2000</a>',
                ' <a href="combined2005.csv" class="btn btn-primary btn-sm">2005</a>',
                ' <a href="combined2010.csv" class="btn btn-primary btn-sm">2010</a>',
                ' <a href="combined2015.csv" class="btn btn-primary btn-sm">2015</a>'
                
    ))
  })
})
