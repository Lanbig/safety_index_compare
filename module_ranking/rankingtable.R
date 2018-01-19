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
      
      # Compare the country over the time
      tabPanel( "Rank over Time", tags$br(), 
                column(12,
                       wellPanel(
                         fluidRow(
                           column( width = 5, offset = 0, style='padding:10px;', selectInput(ns('countryOT'), 'Country', safetydata['country_slug'], selected = c('Vietnam'), multiple = FALSE))
                         )
                       )
                ),
                column(12, dataTableOutput(ns('rankingOverTime')),tags$br())
      ),
      
      tabPanel( "Peer Groups", tags$br(), 
      column(12,
        wellPanel(
          fluidRow(
            
            column( width = 2, offset = 0, style='padding:10px;', selectizeInput(ns('year'), 'Year', c('2000', '2005', '2010', '2016','2017'), selected = '2017')),
            column( width = 5, offset = 0, style='padding:10px;', selectizeInput(ns('country'), 'Country', safetydata$country_slug, selected = 'Vietnam', multiple = FALSE)),
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
                                                                                                          ), selected = 'un_sub_region')) 
          )
        )
      ),
      column(12, dataTableOutput(ns('rankingResult')), tags$br())
        
      ),
      
      # Compare by custom selection - Choose individual country
      tabPanel( "Custom selection", tags$br(), 
        column(12,
           wellPanel(
             fluidRow(
               column( width = 2, offset = 0, style='padding:10px;',  selectizeInput(ns('years'), 'Year', c('2000', '2005', '2010', '2016','2017'), selected = '2017')),
               column( width = 10, offset = 0, style='padding:10px;', selectInput(ns('countries'), 'Country', safetydata['country_slug'], selected = c('Japan', 'Canada'), multiple = TRUE))
             )
            )
        ),
      column(12, dataTableOutput(ns('rankingCustomResult')),tags$br())
      )
    
    ) # fluidPage
      
)}

rankingTable <- function(input, output, session){
  
  ComputeRanking <- function(df, order = 'desc'){
    if(order == 'desc')
      return(rank(desc(df), na.last = "keep", ties.method = "min"))
    else
      return(rank((df), na.last = "keep", ties.method = "min"))
  
    }
  
  autoComputeRanking <- function(data){
    
    data_rank <- data['country_slug']
    data_rank$ul_safety_index_rank                   <- ComputeRanking(data['ul_safety_index'])
    
    # I&R Ranking
    data_rank$institutions_resources_rank            <- ComputeRanking(data['institutions_resources'])
    data_rank$education_rating_rank                  <- ComputeRanking(data['education_rating'])
    data_rank$gdp_per_capita_rating_rank             <- ComputeRanking(data['gdp_per_capita_rating'])
    data_rank$government_effectiveness_rating_rank   <- ComputeRanking(data['government_effectiveness_rating'])
    data_rank$Technology_rank                        <- ComputeRanking(data['network_readiness_rating'])
    
    # Safety Frameworks rank
    data_rank$safety_frameworks_rank                 <- ComputeRanking(data['safety_frameworks'])
    data_rank$ul_standards_index_rating_rank         <- ComputeRanking(data['ul_standards_index_rating'])
    data_rank$consumer_protection_survey_rating_rank <- ComputeRanking(data['consumer_protection_survey_rating'])
    data_rank$ul_labor_rights_index_rating_rank      <- ComputeRanking(data['ul_labor_rights_index_rating'])
    data_rank$road_safety_rating_rank                <- ComputeRanking(data['road_safety_rating'])
    
    # Safety Outcome Ranking
    data_rank$safety_outcomes_rank              <- ComputeRanking(data['safety_outcomes'])
    data_rank$drowning_rating_rank              <- ComputeRanking(data['drowning_rating'])
    data_rank$exposure_to_forces_of_nature_disaster_rating_rank <- ComputeRanking(data['exposure_to_forces_of_nature_disaster_rating'])
    data_rank$exposure_to_mechanical_forces_rating_rank <- ComputeRanking(data['exposure_to_mechanical_forces_rating'])
    data_rank$falls_rating_rank_rank                    <- ComputeRanking(data['falls_rating'])
    data_rank$fires_heat_hot_substances_rating_rank     <- ComputeRanking(data['fires_heat_hot_substances_rating'])
    data_rank$foreign_body_rating_rank          <- ComputeRanking(data['foreign_body_rating'])
    data_rank$other_unintentional_injuries_rating_rank  <- ComputeRanking(data['other_unintentional_injuries_rating'])
    data_rank$poisonings_rating_rank            <- ComputeRanking(data['poisonings_rating'])
    data_rank$transport_injuries_rating_rank    <- ComputeRanking(data['transport_injuries_rating'])
    
    return(data_rank)
  }
  
  autoRenameIndicators <- function(data){
    row.names(data) <- c( 'UL Safety Index',
                                '__Institutions & resources',
                                '____Education',
                                '____GDP per Capita',
                                '____Government Effectiveness',
                                '____Technology',
                                
                                '__Safety Frameworks', 
                                '____Code & Standard',
                                '____Consumer Protection',
                                '____Labor Protections',
                                '____Road Safety Framework',
                                
                                '__Safety Outcomes',
                                '____Drowning',
                                '____Exposure to forces of Nature, Disaster',
                                '____Exposure to mechanical forces',
                                '____Falls',
                                '____Fires heat hot substances',
                                '____Foreign Body',
                                '____Other Unintentional Injuries',
                                '____Poisonings',
                                '____Transport Injuries')
    
    return(data)
  }
  
  indicatorRatingList <- c( 'country_slug','ul_safety_index',
                            'institutions_resources',
                            'education_rating',
                            'gdp_per_capita_rating',
                            'government_effectiveness_rating',
                            'network_readiness_rating',
                            
                            'safety_frameworks', 
                            'ul_standards_index_rating',
                            'consumer_protection_survey_rating',
                            'ul_labor_rights_index_rating',
                            'road_safety_rating',
                            
                            'safety_outcomes',
                            'drowning_rating',
                            'exposure_to_forces_of_nature_disaster_rating',
                            'exposure_to_mechanical_forces_rating',
                            'falls_rating',
                            'fires_heat_hot_substances_rating',
                            'foreign_body_rating',
                            'other_unintentional_injuries_rating',
                            'poisonings_rating',
                            'transport_injuries_rating')

  indicatorRankingList <- c( 'country_slug','ul_safety_index_rank',
                            'institutions_resources_rank',
                            'education_rating_rank',
                            'gdp_per_capita_rating_rank',
                            'government_effectiveness_rating_rank',
                            'network_readiness_rating_rank',
                            
                            'safety_frameworks_rank', 
                            'ul_standards_index_rating_rank',
                            'consumer_protection_survey_rating_rank',
                            'ul_labor_rights_index_rating_rank',
                            'road_safety_rating_rank',
                            
                            'safety_outcomes_rank',
                            'drowning_rating_rank',
                            'exposure_to_forces_of_nature_disaster_rating_rank',
                            'exposure_to_mechanical_forces_rating_rank',
                            'falls_rating_rank',
                            'fires_heat_hot_substances_rating_rank',
                            'foreign_body_rating_rank',
                            'other_unintentional_injuries_rating_rank',
                            'poisonings_rating_rank',
                            'transport_injuries_rating_rank')
  
  
  
  #Reactive - react from user selection - year
  dataPeerGroup <- reactive({
    # Get input fron User Interface
    year <- input$year
    country <- input$country
    peergroup <- input$peergroup
    
    # Load data and filter data
    inputfile <- paste(here('asset/datasets/UL_safety_index_data_'),year,'.csv', sep = "")
    print(inputfile)
    data <- read.csv(inputfile, na.strings = "NULL")
    
    # Load data and filter data
    filterValue <- data[data$country_slug==country, eval(peergroup)]
    data <- data[,c(eval(peergroup),indicatorRatingList)]
    data <- data[which(data[[1]] == filterValue),]
    
    # Compute ranking on the data subset
    data_rank <- autoComputeRanking(data)
    row.names(data_rank) <- data_rank[,'country_slug']
    tdata <- t(data_rank[,-1])
    tdata <- autoRenameIndicators(tdata)
    
  })  
  
  output$rankingResult <- DT::renderDataTable(
    dataPeerGroup(),
    options=list(dom = 'Bfrt', buttons = list( 
      #I('colvis'),
      
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

  # Filter data by country
  dataCustomCompare <- reactive({
    
    # Get input fron User Interface
    year <- input$years
    countries <- unlist(strsplit(input$countries, " "))
    
    # Load data and filter data
    inputfile <- paste(here('asset/datasets/UL_safety_index_data_'),year,'.csv', sep = "")
    print(inputfile)
    data <- read.csv(inputfile, na.strings = "NULL")
    
    data <-  subset(data, country_slug %in% countries)
    data <- data[,indicatorRatingList] 
    
    # Compute ranking on the data subset
    data_rank <- autoComputeRanking(data)
    
    row.names(data_rank) <- data_rank[,'country_slug']
    tdata <- t(data_rank[,-1])
    
    tdata <- autoRenameIndicators(tdata)
    
  })  
  
  output$rankingCustomResult <- DT::renderDataTable(
    dataCustomCompare(),
    options=list(dom = 'Bfrt', buttons = list( 
      #I('colvis'),
      
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

  dataOverTime <- reactive({
    # Get input fron User Interface
    country <- input$countryOT
    cat(country)

    data2017 <- read.csv(here('asset/datasets/UL_safety_index_data_2017.csv'), na.strings = "NULL")
    dataRating <-  data2017[which(data2017['country_slug']==country),c(indicatorRatingList)]
    dataRanking <-  data2017[which(data2017['country_slug']==country),c(indicatorRankingList)]
    
    dataResult          <- as.data.frame(t(dataRating))
    colnames(dataResult)[1] <- '2017 Score'
    dataResult[,'2017 Rank'] <- as.data.frame(t(dataRanking))[,1]
    
    
    for(i in c('2016','2010','2005','2000')){
      inputfile <- paste(here('asset/datasets/UL_safety_index_data_'),i,'.csv', sep = "")
      data <- read.csv(inputfile, na.strings = "NULL")
      
      dataRating <-  data[which(data2017['country_slug']==country),c(indicatorRatingList)]
      dataRanking <-  data[which(data2017['country_slug']==country),c(indicatorRankingList)]
      
      dataResult[,eval(i)] <- as.data.frame(t(dataRating))[,1]
      colnames(dataResult)[which(colnames(dataResult) == i)] <- paste(i,'Score',sep = " ")
      
      dataResult[,eval(i)] <- as.data.frame(t(dataRanking))[,1]
      colnames(dataResult)[which(colnames(dataResult) == i)] <- paste(i,'Rank',sep = " ")
    }
  
    autoRenameIndicators(dataResult[-1,])
    
  })  
  
  output$rankingOverTime <- DT::renderDataTable(
    dataOverTime(),
    options=list(dom = 'Bfrt', buttons = list( 
      #I('colvis'),
      
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
