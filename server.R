library(shiny)
library(plotly)
library(DT)
library(broom)
library(psych)
library(sqldf)

options(warn =-1)

shinyServer(function(input, output) {
  
  
  ####### Output for correlation page ##########
  
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
    if(columnX == 'gdp_per_capita' | columnY == 'gdp_per_capita' | columnX == 'gdp_per_capita_rating' | columnY == 'gdp_per_capita_rating' )
      cor <- cor.test(safetydata()[,columnX], safetydata()[,columnY], method = "pearson")
    else
      cor <- cor.test(safetydata()[,columnX], safetydata()[,columnY], method = "spearman")
    
    
  })
  
  ####### Output ##########
  
  output$summary_lm <- renderPrint({ 
    summary(safetydata_m()) 
  }) 
  
  output$summary_cor <- renderPrint({ 
    print(safetydata_cor())
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
                    "Failed"
                ))
  })
  
  output$dispDL <- renderText({
    HTML(paste0('<a href="data/combined2000.csv" class="btn btn-primary btn-sm">2000</a>',
                ' <a href="data/combined2005.csv" class="btn btn-primary btn-sm">2005</a>',
                ' <a href="data/combined2010.csv" class="btn btn-primary btn-sm">2010</a>',
                ' <a href="data/combined2015.csv" class="btn btn-primary btn-sm">2015</a>'
                
    ))
  })
  
  
  
  
  
  ####### Output for time series page ##########
  ####### Reactive ##########
  tsdata <- reactive({
    ts_countries <- input$ts_countries
    print(ts_countries)
    
    data2000 <- read.csv("combined2000.csv", na.strings = "NULL")
    data2005 <- read.csv("combined2005.csv", na.strings = "NULL")
    data2010 <- read.csv("combined2010.csv", na.strings = "NULL")
    data2015 <- read.csv("combined2015.csv", na.strings = "NULL")
    
    data2000$year <- 2000
    data2005$year <- 2005
    data2010$year <- 2010
    data2015$year <- 2015
    
    data_all <- rbind(data2000,data2005,data2010,data2015)
    data_all$year <- as.factor(data_all$year)
    
    sql_stm <- paste("SELECT * FROM data_all WHERE country_slug == '" , ts_countries, "'" , sep ="")
    ts_data <- sqldf(sql_stm)
    
    ts_data
  })
  
  output$ts_idx <- renderPlotly({
    plot_ly(tsdata(), x = ~year, y = ~ul_safety_index, name = 'ul_safety_index', line = list(width = 4), type = 'scatter', mode = 'lines+markers') %>%
      add_trace(y = ~institutions_resources, name = 'institutions_resources', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~safety_frameworks, name = 'safety_frameworks', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~safety_outcomes, name = 'safety_outcomes', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      
      layout(title = paste0("Safety Index Score for ", input$ts_countries),
             yaxis = list(title = "Safety Index - Indicators", range = c(0,105)),
             xaxis = list (title = "", type='category'),
             legend = list(orientation = 'h'))
  })
  
  output$ts_outcome <- renderPlotly({
    plot_ly(tsdata(), x = ~year, y = ~safety_outcomes, name = 'safety_outcomes',  line = list(width = 4), type = 'scatter', mode = 'lines+markers') %>%
      add_trace(y = ~transport_injuries_rating*100, name = 'transport_injuries_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~falls_rating*100, name = 'falls_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~drowning_rating*100, name = 'drowning_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~fires_heat_hot_substances_rating*100, name = 'fires_heat_hot_substances_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~poisonings_rating*100, name = 'poisonings_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~exposure_to_mechanical_forces_rating*100, name = 'exposure_to_mechanical_forces_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~foreign_body_rating*100, name = 'foreign_body_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~other_unintentional_injuries_rating*100, name = 'other_unintentional_injuries_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~exposure_to_forces_of_nature_disaster_rating*100, name = 'nature_disaster_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
  
    layout(title = paste0("Safety Outcomes Score for ", input$ts_countries),
           yaxis = list(title = "Safety Outcomes - Indicators", range = c(0,105)),
           xaxis = list (title = "", type='category'),
           legend = list(orientation = 'h'))
  })

  output$ts_safety_frameworks <- renderPlotly({
    plot_ly(tsdata(), x = ~year, y = ~safety_frameworks, name = 'safety_frameworks', line = list(width = 4), type = 'scatter', mode = 'lines+markers') %>%
      add_trace(y = ~ul_standards_index_rating*100, name = 'ul_standards_index_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~consumer_protection_survey_rating*100, name = 'consumer_protection_survey_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~ul_labor_rights_index_rating*100, name = 'ul_labor_rights_index_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~road_safety_rating*100, name = 'road_safety_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      
    layout(title = paste0("Safety Frameworks Score for ", input$ts_countries),
           yaxis = list(title = "Safety Frameworks - Indicators", range = c(0,105)),
           xaxis = list (title = "", type='category'),
           legend = list(orientation = 'h'))
  })

  output$ts_institutions_resources <- renderPlotly({
    plot_ly(tsdata(), x = ~year, y = ~institutions_resources, name = 'institutions_resources', line = list(width = 4),  type = 'scatter', mode = 'lines+markers') %>%
      add_trace(y = ~gdp_per_capita_rating*100, name = 'gdp_per_capita_rating',  line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~government_effectiveness_rating*100, name = 'government_effectiveness_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~education_rating*100, name = 'education_rating', line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
      add_trace(y = ~network_readiness_rating*100, name = 'network_readiness_rating',  line = list(width = 2, dash = 'dot'), mode='lines+markers') %>%
    
    layout(title = paste0("Institutions Resources Score for ", input$ts_countries),
           yaxis = list(title = "Institutions Resources - Indicators", range = c(0,105)),
           xaxis = list (title = "", type='category'),
           legend = list(orientation = 'h')
    )
  })
  
  output$dispTStableIdx <- DT::renderDataTable(
    tsdata()[,c('year','country_slug','ul_safety_index_rank','institutions_resources_rank','safety_outcomes_rank',
                'safety_frameworks_rank')],
    rownames = FALSE,
    options=list(lengthChange = FALSE,  dom = 'Bfrtip', buttons = list( 
      I('colvis'),
      list(
        extend = 'collection',
        buttons = c('csv', 'excel'),
        text = 'Download')), scrollX = TRUE,  colReorder = TRUE),
    extensions = c('Buttons','FixedColumns','ColReorder')
  )
 
  output$dispTStableSO <- DT::renderDataTable(
    
    tsdata()[,c('year','country_slug','safety_outcomes_rank', 'transport_injuries_rating_rank','falls_rating_rank','drowning_rating_rank',
                'fires_heat_hot_substances_rating_rank','poisonings_rating_rank','exposure_to_mechanical_forces_rating_rank',
                'foreign_body_rating_rank','other_unintentional_injuries_rating_rank','exposure_to_forces_of_nature_disaster_rating_rank')],
    rownames = FALSE,
    options=list(lengthChange = FALSE,  dom = 'Bfrtip', buttons = list( 
                                                                  I('colvis'),
                                                                  list(
                                                                    extend = 'collection',
                                                                    buttons = c('csv', 'excel'),
                                                                    text = 'Download')), scrollX = TRUE, colReorder = TRUE),
    extensions = c('Buttons','FixedColumns','ColReorder')
  )
  
  output$dispTStableSF <- DT::renderDataTable(
    tsdata()[,c('year','country_slug','safety_frameworks_rank','ul_standards_index_rating_rank',
                'consumer_protection_survey_rating_rank','ul_labor_rights_index_rating_rank','road_safety_rating_rank')],
    rownames = FALSE,
    options=list(lengthChange = FALSE,  dom = 'Bfrtip', buttons = list( 
                                          I('colvis'),
                                          list(
                                            extend = 'collection',
                                            buttons = c('csv', 'excel'),
                                            text = 'Download')), scrollX = TRUE, colReorder = TRUE),
                                        extensions = c('Buttons','FixedColumns','ColReorder')
                                      )
  
  output$dispTStableIR <- DT::renderDataTable(
    tsdata()[,c('year','country_slug','institutions_resources_rank','gdp_per_capita_rating_rank',
                'government_effectiveness_rating_rank','education_rating_rank','network_readiness_rating_rank')],
    rownames = FALSE,
    options=list(lengthChange = FALSE,  dom = 'Bfrtip', buttons = list( 
                                                                    I('colvis'),
                                                                    list(
                                                                      extend = 'collection',
                                                                      buttons = c('csv', 'excel'),
                                                                      text = 'Download')), scrollX = TRUE, colReorder = TRUE),
    extensions = c('Buttons','FixedColumns','ColReorder')
  )
  
  output$dispDL2 <- renderText({
    HTML(paste0('<a href="data/combined2000.csv" class="btn btn-primary btn-sm">2000</a>',
                ' <a href="data/combined2005.csv" class="btn btn-primary btn-sm">2005</a>',
                ' <a href="data/combined2010.csv" class="btn btn-primary btn-sm">2010</a>',
                ' <a href="data/combined2015.csv" class="btn btn-primary btn-sm">2015</a>'
                
    ))
  })

  
  ####### Reactive ##########
  qtdata <- reactive({
    year <- input$qtyear
    inputfile <- paste('combined',year,'.csv', sep = "")
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
        list(targets = {c(0,1,7:43,45:72)}, visible = FALSE)
      )
      
      ),
    
    extensions = c('Buttons','FixedColumns','ColReorder')
  )
  
  output$dispDataTitle <- renderText({
    HTML(paste0("<b>Displaying : ",input$qtyear," Data </b>"))
  })
  
  output$dispDL3 <- renderText({
    HTML(paste0('<a href="data/combined2000.csv" class="btn btn-primary btn-sm">2000</a>',
                ' <a href="data/combined2005.csv" class="btn btn-primary btn-sm">2005</a>',
                ' <a href="data/combined2010.csv" class="btn btn-primary btn-sm">2010</a>',
                ' <a href="data/combined2015.csv" class="btn btn-primary btn-sm">2015</a>'
                
    ))
  })
  
})
