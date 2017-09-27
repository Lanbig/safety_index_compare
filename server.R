library(shiny)
library(plotly)
library(DT)
library(broom)

data <- read.csv("combined2015.csv")

shinyServer(function(input, output) {
   
  output$distPlot <- renderPlotly({
    
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    countryName <- 'country_slug_map'
    
    safetydata <- data [, c(countryName, columnX, columnY)]
    safetydata <- safetydata[complete.cases(safetydata), ]
    
    m <- lm(safetydata[,columnY] ~ safetydata[,columnX])
    cooks = cooks.distance(m)
    
    
    plot_ly(x = safetydata[,columnX],y = safetydata[,columnY], name="") %>%  
      
      add_text(text = safetydata[,countryName], color= I("black"),
               textposition = "top right", name ="country Name", visible = "legendonly")  %>%
      
      add_markers(showlegend = FALSE, 
                  marker = list(size = 10,
                                color = 'rgba(0, 0, 0, .0)',
                                line = list(color = ifelse(cooks > quantile(cooks,.90),'rgba(152, 0, 0, .5)','rgba(0, 0, 0, .5)'),
                                            width = 2)))  %>%
      
      add_lines(y = ~fitted(lm(safetydata[,columnY] ~ safetydata[,columnX])),
                line = list(color = 'rgba(7, 16 181, 1)'),
                name = "Linear Line") %>%
      
      add_ribbons(data = augment(m),
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
    
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    countryName <- 'country_slug_map'
    
    safetydata <- data [, c(countryName, columnX, columnY)]
    safetydata <- safetydata[complete.cases(safetydata), ]
    
    m <- lm(safetydata[,columnY] ~ safetydata[,columnX])
    cooks = cooks.distance(m)
    
    
    plot_ly(x = m$fitted.values,y = m$residuals, color = I("black"), type = 'scatter', 
            mode = 'text',  text = safetydata[,countryName],
            textposition = "top right", name ="country Name") %>%  
      
      add_markers(showlegend = FALSE, 
                  marker = list(size = 10,
                                color = 'rgba(0, 0, 0, .0)',
                                line = list(color = ifelse(cooks > quantile(cooks,.90),'rgba(152, 0, 0, .5)','rgba(0, 0, 0, .5)'),
                                            width = 2)))  %>%
      
      layout(xaxis = list(title = columnX),
             yaxis = list(title = columnY),
             legend = list(x = 0.80, y = 0.90)) 
    
  })
  
  output$dispData <- DT::renderDataTable({
    
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    
    #columnX <- 'safety_outcomes'
    #columnY <- 'ul_safety_index'
    countryName <- 'country_slug_map'
    
    safetydata <- data [, c(countryName, columnX, columnY)]
    
     
    
    safetydata <- safetydata[complete.cases(safetydata), ]
    
    m <- lm(safetydata[,columnY] ~ safetydata[,columnX])
    cooks = cooks.distance(m)
    
    safetydata$Residule <- m$residuals
    safetydata$Cooks <- cooks
    
    safetydata
  })
 
  
  output$summary <- renderPrint({
    
    columnX <- input$IndicatorX
    columnY <- input$IndicatorY
    
    #columnX <- 'safety_outcomes'
    #columnY <- 'ul_safety_index'
    countryName <- 'country_slug_map'
    
    safetydata <- data [, c(countryName, columnX, columnY)]
    
    m <- lm(safetydata[,columnY] ~ safetydata[,columnX])
    summary(m)
  }) 
  
})
