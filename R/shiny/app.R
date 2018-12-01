setwd("/home/hsc0526/datamining")

require('shiny')
require('shinydashboard')
require('leaflet')
require('dplyr')
require('FNN')

load('train.df.RData')

ui <- dashboardPage(
  skin = 'yellow', 
  dashboardHeader(title = "Newyork Taxi"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fare Calculator", tabName = "dashboard", icon = icon("taxi")),
      menuItem("Fare Range", tabName = 'FR', icon = icon("street-view"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow( 
                column(width = 12,
                       box(width = NULL, title = 'Newyork Map', status = "primary", solidHeader = TRUE,
                           leafletOutput("map", height = 600))
                       
                )),
              fluidRow(
                column(width = 4, infoBoxOutput(width = NULL, "dash_lon")),
                column(width = 4, infoBoxOutput(width = NULL, "dash_lat")),
                textOutput("currentTime")
              ),
              fluidRow(
                column(width = 4, infoBoxOutput(width = NULL, "dash_val"))
              )),
      tabItem(tabName = "FR",
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = 'Newyork Map', status = "success", solidHeader = TRUE,
                           leafletOutput("map2", height = 600))
                )),
              fluidRow(
                column(width = 5,
                       sliderInput('fare', "Available fare", min = 3, max = 50, value = 20, round = TRUE))
              ))
    )
    )
  
  
    )


### Server 

server <- function(input, output, session)
{ 
  
  ### Fare Cal 
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles()%>%
      setView(lng = -74.00, lat = 40.71, zoom = 15)
  })
  
  RV <- reactiveValues(Clicks = list())
  observeEvent(input$map_click, {
    click <- input$map_click
    RV$Clicks <- bind_rows(RV$Clicks, click)
    proxy <- leafletProxy("map") %>% clearMarkers() %>% addMarkers(lng = click$lng, lat = click$lat )
    
  })
  
  output$dash_lon <- renderInfoBox({
    if(is.null(nrow(RV$Clicks)))
    {
      infoBox(
        
        "Start Lon", "Click your location", icon = icon("map"), color = 'olive'
        
      )} else if(nrow(RV$Clicks) %% 2  == 1){
        
        infoBox(
          
          "Start Lon", round(RV$Clicks$lng[nrow(RV$Clicks)],4), icon = icon("map"), color = 'olive'
        )
      } else { 
        
        infoBox(
          
          "Desti Lon", round(RV$Clicks$lng[nrow(RV$Clicks)],4), icon = icon("map"), color = 'purple'
        )
        
      }
  })
  output$dash_lat <- renderInfoBox({
    if(is.null(nrow(RV$Clicks)))
    {
      infoBox(
        
        "Start Lat", "Click your location", icon = icon("map"), color = 'olive'
        
      )} else if(nrow(RV$Clicks) %% 2  == 1){
        
        infoBox(
          
          "Start Lat", round(RV$Clicks$lat[nrow(RV$Clicks)],4), icon = icon("map"), color = 'olive'
        )
      } else { 
        
        infoBox(
          
          "Desti Lat", round(RV$Clicks$lat[nrow(RV$Clicks)],4), icon = icon("map"), color = 'purple'
        )
        
      }
  })
  
  ### Fare estimate
  
  output$dash_val <- renderInfoBox({
    if(is.null(nrow(RV$Clicks))){
      infoBox(
        "Fare", "Click Start", color = 'blue', icon = icon('dollar'), fill = TRUE
      )} else if(nrow(RV$Clicks) %% 2  == 1){
        infoBox(
          "Fare", "Click Destination", color = 'blue', icon = icon('dollar'), fill = TRUE
        )
      }
    else {
      newdata <- data.frame(p_lon = RV$Clicks[nrow(RV$Clicks)-1,]$lng, p_lat = RV$Clicks[nrow(RV$Clicks)-1,]$lat,
                            d_lon = RV$Clicks[nrow(RV$Clicks),]$lng, d_lat = RV$Clicks[nrow(RV$Clicks),]$lat)
      h <- as.numeric(substr(Sys.time(), 12, 13))
      
      train <- train.df %>% filter(hour == h)
      
      set.seed(1)
      
      cl <- kmeans(train[,2:3], centers = 100)
      
      cluster <- cl$cluster
      
      train <- cbind(train, cluster)
      
      cluster2 <- knn(train = train[,2:3], test = newdata[,1:2], cl = train$cluster, k = 3)
      
      tmp_train <- train %>% filter(cluster == cluster2)
      
      fare <- KernelKnn(data = tmp_train[,2:5], TEST_data = newdata, y = tmp_train$fare, k = 5, weights_function = 'gaussian', regression = TRUE)
     
      infoBox(
        "Fare", paste0(round(fare,2),'$',' + 15% tax = ',round(round(fare,2) + round(fare,2)*(0.15), 2), '$'), color = 'blue', icon = icon('dollar'), fill = TRUE
      )} 
  }
  )
  
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
  
  
  ### Fare range 
  
  output$map2 <- renderLeaflet({
    leaflet() %>% 
      addTiles()%>%
      setView(lng = -74.00, lat = 40.71, zoom = 15)
  })
  
  RV2 <- reactiveValues(Clicks = list())
  observeEvent(input$map2_click, {
    click2 <- input$map2_click
    RV$Clicks <- bind_rows(RV2$Clicks, click2)
    proxy <- leafletProxy("map2") %>% clearMarkers() %>% addMarkers(lng = click2$lng, lat = click2$lat )
  })
}

shinyApp(ui, server)

