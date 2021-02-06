library(shiny)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(sp)
library(shinythemes)
library(dashboardthemes)
library(shinydashboard)
library(diffobj)
library(shinydashboardPlus)



#header
header <- dashboardHeader(title = "Sandusky, OH Demographic and Environmental Explorer", titleWidth = "1520px")

#body
    
body <- dashboardBody(
   # shinyDashboardThemes(
   #   theme = "poor_mans_flatly"
   # ),
   box(width= "1210px",height="700px",
        leafletOutput("map", height = "700px", width = "1170px"))
    )

# Sidebar

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    box(width = "15px", background = "black",
      p(strong("This is an interactive visualization made"),
      br(),
      strong("to help visualize the local demographic and"),
      br(),
      strong("environmental landscape."),
      br(),
      br(),
      strong("The main boundaries shown are local Census"),
      br(),
      strong("tracts, over which you can layer all kinds"),
      br(),
      strong("of environmental data, like a map of all"),
      br(),
      strong("the streams that flow through the city."),
      hr())),
      sliderInput("year", "Select Year:", min = min(final$year), max = max(final$year), value = 2010, step = 1, sep = ""),
      selectInput("variable", "US Census Variables:", choices = names(as.data.frame(final) %>% dplyr::select(-c(year,tract,geometry))), multiple = FALSE)
  )
)



ui <- dashboardPage(
  header,
  sidebar,
  body
)

  
server <- function(input, output, session) {
  
  filteredData <- reactive({
    final %>%
      filter(year == input$year) %>%
      dplyr::select(input$variable, tract)
      }) 
  
  colorpal <- reactive({
    colorNumeric("YlGnBu", filteredData()$input$variable, na.color = "#808080", reverse = TRUE)
    })
  
  
  output$map <- renderLeaflet({
      pal <- colorFactor(rainbow(229), domain = values(landcover2016)) %>%
    
      leaflet() %>%
    
      addTiles() %>%
      
      addProviderTiles(providers$OpenStreetMap) %>%
      
      fitBounds(-82.976990,41.253205,-82.364502,41.662607) %>%
      
      addPolygons(data = Wetlands3, group = "Wetlands", stroke = TRUE, weight=.4, opacity = 1) %>%
      
      addMiniMap() %>%
        
      addPolylines(data=Neighborhoods, col= "Black", weight = 1.3, opacity = 1, group = "Downtown Neighborhood Lines") %>%
      
      addPolylines(data=GLRI, col= "Blue", weight = 1.3, opacity = 1, group = "Sandusky Bay Pathway") %>%
        
      addMarkers(data = parks, icon = leafIcons, group = "Parks", popup = ~(FEATURE_NAME)) %>%
        
        addRasterImage(x, pal1, opacity = 1, group = "Land Cover 2016") %>%
        addRasterImage(xx, pal2, opacity = 1, group = "Land Cover 2014") %>%
        addRasterImage(xxx, pal3, opacity = 1, group = "Land Cover 2012") %>%
        addRasterImage(xxxx, pal4, opacity = 1, group = "Land Cover 2010") %>%
        addRasterImage(xxxxx, pal5, opacity = 1, group = "Land Cover 2008") %>%
        addRasterImage(xxxxxx, pal6, opacity = 1, group = "Land Cover 2001")
      
  })
  
  
  
  
  observe({
    pal <- colorpal()
    proxy <- leafletProxy("map", data = filteredData()) %>%
      clearControls() %>%
      # addLegend(position ="bottomleft", pal = pal, group = "Land Cover", na.label = "N/A for Current Year", labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)), values = ~filteredData()[[1]], opacity=1, title = "Land Cover Legend") %>%
      addPolygons(label = ~paste ("Tract:", tract, paste0(input$variable,":"),as.numeric(eval(as.symbol(input$variable)))),
                  fillColor = ~pal(as.numeric(eval(as.symbol(input$variable)))),
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1,
                  group = "Display Census Data",
                  fillOpacity = .88,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLegend(pal = pal4, opacity = 1, values = values(xxxx), labFormat = function(type, cuts, p) {paste0(labels2)}, group = "Land Cover Legend", title = "Land Cover Legend") %>%
      addLegend(position ="bottomleft", pal = pal, group = "Display Census Data", na.label = "N/A for Current Year", labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)), values = ~filteredData()[[1]], opacity=1, title = "Census Data Legend") %>%
      addLayersControl(
        overlayGroups = c("Display Census Data","Parks","Wetlands","Land Cover Legend", "Downtown Neighborhood Lines","Sandusky Bay Pathway","Land Cover 2001", "Land Cover 2008", "Land Cover 2010", "Land Cover 2012", "Land Cover 2014", "Land Cover 2016"),
        options = layersControlOptions(collapsed = FALSE)
        )
    
      
     
    })
  
}  

shinyApp(ui, server)

