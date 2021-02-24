
#load packages 

library(data.table)
library(shiny)
library(sf)
library(sp)
library(raster)
library(leaflet)
library(dashboardthemes)
library(shinydashboard)

#load necessary data and UI accessories

leafIcons <- icons(
  iconUrl = 
    "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 15, iconHeight = 40,
  iconAnchorX = 14, iconAnchorY = 40
)

labels2<- c("Developed - Medium/High Intensity","Developed - Low/Medium Intensity","Developed - Forested", "Developed - herbaceous or shrubland", "Cropland","Tree Cover 0-50%", "Tree Cover 50-100%","Herb Cover 0-50%", "Herb Cover 50-100%", "NA")
final <- read_sf("data/derived/final.shp")
Wetlands3 <- read_sf("data/derived/Wetlands3.shp")
Neighborhoods <- read_sf("data/source/city/Neighborhoods/City_of_Sandusky_Neighborhoods.shp")
# Neighborhoods <- st_transform(Neighborhoods, 4326)
GLRI <- read_sf("data/source/city/GLRI Pathway/Polygon_layer.shp")
# GLRI <- st_transform(GLRI, 4326)
parks <- fread(file="data/derived/parks.txt")

ras_2001 <- raster::raster("data/derived/ras_2001.gri") 
ras_2008 <- raster::raster("data/derived/ras_2008.gri") 
ras_2010 <- raster::raster("data/derived/ras_2010.gri") 
ras_2012 <- raster::raster("data/derived/ras_2012.gri") 
ras_2014 <- raster::raster("data/derived/ras_2014.gri")
ras_2016<- raster::raster("data/derived/ras_2016.gri")

pal1<- leaflet::colorFactor(palette = "Spectral", domain = values(ras_2016), na.color = "transparent")
pal2 <- leaflet::colorFactor(palette = "Spectral", domain = values(ras_2014), na.color = "transparent")
pal3 <- leaflet::colorFactor(palette = "Spectral", domain = values(ras_2012), na.color = "transparent")
pal4 <- leaflet::colorFactor(palette = "Spectral", domain = values(ras_2010), na.color = "transparent")
pal5 <- leaflet::colorFactor(palette = "Spectral", domain = values(ras_2008), na.color = "transparent")
pal6 <- leaflet::colorFactor(palette = "Spectral", domain = values(ras_2001), na.color = "transparent")


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

#ui call that hamburgers everything - header, sidebar, body - together

ui <- dashboardPage(
  header,
  sidebar,
  body
)

#server side where all of the actual mapping arrangements are made
  
server <- function(input, output, session) {
  #creating a reactive dataframe from the main dataset (final)
  filteredData <- reactive({
    final %>%
      #filtering based on user year selection
      filter(year == input$year) %>%
      #filtering based on user Census variable selection
      dplyr::select(input$variable, tract)
      }) 
  
  #generating reactive color palette
  colorpal <- reactive({
    colorNumeric("YlGnBu", filteredData()$input$variable, na.color = "#808080", reverse = TRUE)
    })
  
  #generating the static part of the map
  #groups enable the checkbox layer display feature once called to addLayersControl
  output$map <- renderLeaflet({
      pal <- colorFactor(rainbow(229), domain = values(ras_2016)) %>%
    
      leaflet() %>%
    
      addTiles() %>%
      
      addProviderTiles(providers$OpenStreetMap) %>%
      
      fitBounds(-82.976990,41.253205,-82.364502,41.662607) %>%
      
      addPolygons(data = Wetlands3, group = "Wetlands", stroke = TRUE, weight=.4, opacity = 1) %>%
      
      addMiniMap() %>%
        
      addPolylines(data=Neighborhoods, col= "Black", weight = 1.3, opacity = 1, group = "Downtown Neighborhood Lines") %>%
      
      addPolylines(data=GLRI, col= "Blue", weight = 1.3, opacity = 1, group = "Sandusky Bay Pathway") %>%
        
      addMarkers(data = parks, icon = leafIcons, group = "Parks", popup = ~(FEATURE_NAME)) %>%
        
        addRasterImage(ras_2016, pal1, opacity = 1, group = "Land Cover 2016") %>%
        addRasterImage(ras_2014, pal2, opacity = 1, group = "Land Cover 2014") %>%
        addRasterImage(ras_2012, pal3, opacity = 1, group = "Land Cover 2012") %>%
        addRasterImage(ras_2010, pal4, opacity = 1, group = "Land Cover 2010") %>%
        addRasterImage(ras_2008, pal5, opacity = 1, group = "Land Cover 2008") %>%
        addRasterImage(ras_2001, pal6, opacity = 1, group = "Land Cover 2001")
      
  })
  
  
  #the reactive part of the map
  
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
      addLegend(pal = pal4, opacity = 1, values = values(ras_2010), labFormat = function(type, cuts, p) {paste0(labels2)}, group = "Land Cover Legend", title = "Land Cover Legend") %>%
      addLegend(position ="bottomleft", pal = pal, group = "Display Census Data", na.label = "N/A for Current Year", labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)), values = ~filteredData()[[1]], opacity=1, title = "Census Data Legend") %>%
      addLayersControl(
        overlayGroups = c("Display Census Data","Parks","Wetlands","Land Cover Legend", "Downtown Neighborhood Lines","Sandusky Bay Pathway","Land Cover 2001", "Land Cover 2008", "Land Cover 2010", "Land Cover 2012", "Land Cover 2014", "Land Cover 2016"),
        options = layersControlOptions(collapsed = FALSE)
        )
    
      
     
    })
  
}  

#call the shiny app to function

shinyApp(ui, server)

