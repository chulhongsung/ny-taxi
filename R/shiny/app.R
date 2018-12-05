require('shiny')
require('shinydashboard')
require('leaflet')
require('dplyr')
require('xgboost') 

load('xgb.fit.RData') ### GitHub/ny-taxi/R/fit/xgb.fit.RData

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
                       box(width = NULL, title = 'Newyork Map', status = "primary", solidHeader = TRUE,
                           leafletOutput("map2", height = 600))
                )),
              fluidRow(
                column(width = 5,
                       sliderInput('fare', "Available fare", min = 3, max = 30, value = 3, round = TRUE)),
                textOutput("currentTime2")
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
  
  icons1 <- awesomeIcons(
    icon = 'taxi',
    iconColor = 'black',
    library = 'fa',
    markerColor = 'green'
  )
  
  icons2 <- awesomeIcons(
    icon = 'taxi',
    iconColor = 'black',
    library = 'fa',
    markerColor = 'yellow'
  )
  
  RV <- reactiveValues(Clicks = list())
  observeEvent(input$map_click, {
    click <- input$map_click
    RV$Clicks <- bind_rows(RV$Clicks, click)
    if(nrow(RV$Clicks) %% 2 == 1)
    {
      proxy <- leafletProxy("map") %>% clearMarkers() %>% addAwesomeMarkers(lng = click$lng, lat = click$lat, icon = icons1, popup  = paste(sep = '<br/>', 'Start',
                                                                                                                                            paste0('lng: ', round(click$lng, 4)),
                                                                                                                                            paste0('lat: ', round(click$lat,4))), popupOptions = popupOptions(keepInView = T))
    } else if(nrow(RV$Clicks) %% 2 == 0) {
      proxy <- leafletProxy("map") %>% addAwesomeMarkers(lng = click$lng, lat = click$lat, icon = icons2, popup  = paste(sep = '<br/>', 'Destination',
                                                                                                                         paste0('lng: ', round(click$lng,4)),
                                                                                                                         paste0('lat: ', round(click$lat,4))), popupOptions = popupOptions(keepInView = T))
    } 
    
  })
  
  output$dash_lon <- renderInfoBox({
    if(is.null(nrow(RV$Clicks)))
    {
      infoBox(
        
        "Start Lon", "Click your location", icon = icon("map"), color = 'olive', fill = TRUE
        
      )} else if(nrow(RV$Clicks) %% 2  == 1){
        
        infoBox(
          
          "Start Lon", round(RV$Clicks$lng[nrow(RV$Clicks)],4), icon = icon("map"), color = 'olive', fill = TRUE
        )
      } else { 
        
        infoBox(
          
          "Desti Lon", round(RV$Clicks$lng[nrow(RV$Clicks)],4), icon = icon("map"), color = 'purple', fill = TRUE
        )
        
      }
  })
  output$dash_lat <- renderInfoBox({
    if(is.null(nrow(RV$Clicks)))
    {
      infoBox(
        
        "Start Lat", "Click your location", icon = icon("map"), color = 'olive', fill = TRUE
        
      )} else if(nrow(RV$Clicks) %% 2  == 1){
        
        infoBox(
          
          "Start Lat", round(RV$Clicks$lat[nrow(RV$Clicks)],4), icon = icon("map"), color = 'olive', fill = TRUE
        )
      } else { 
        
        infoBox(
          
          "Desti Lat", round(RV$Clicks$lat[nrow(RV$Clicks)],4), icon = icon("map"), color = 'purple', fill = TRUE
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
                            d_lon = RV$Clicks[nrow(RV$Clicks),]$lng, d_lat = RV$Clicks[nrow(RV$Clicks),]$lat, month = factor('01', levels = c('01', '02', '03', '04', '05', '06')))
      
      newdata <- model.matrix(~., newdata)
      
      fare <- predict(xgb.fit, newdata)
      
      infoBox(
        "Fare", paste0(round(fare,2),'$',' + 15% tip = ',round(round(fare,2) + round(fare,2)*(0.15), 2), '$'), color = 'blue', icon = icon('dollar'), fill = TRUE
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
  
  output$currentTime2 <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
  
  
  RV2 <- reactiveValues(Clicks = list())
  observeEvent(input$map2_click, {
    click2 <- input$map2_click
    RV$Clicks <- bind_rows(RV2$Clicks, click2)
    proxy <- leafletProxy("map2") %>% clearMarkers() %>% addMarkers(lng = click2$lng, lat = click2$lat )
  })
}

shinyApp(ui, server)

