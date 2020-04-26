# EO mapmaker
A shiny app to download a satellite image and combine it with an EO outreach poster. 

## Background
Welcome! This project hosts the script to the compagnion site to the outreach project *We can see our oceans from space - a tailored illustrated guide to promote engagement with Earth Observation* of the [National Oceangraphic Centre (NOC)](https://www.noc.ac.uk). The project was supported by a generous grant from [AGU](https://www.agu.org). In the project, we made a two-paged leaflet that explains on one page the basic principles of earth observation and features on the other page a recent satelite image of a specified area. The website is run by a shiny application and will enable you to assemble your own printable two-page pdf with a satellite image of an area of your choosing. It uses up to date images from the Sentinel satellites of the European Space agency through the [Sentinel Hub](https://www.sentinel-hub.com) service via a WMS request.

## The script 
The script is a shiny application and written in R. In case you want to you use [the complete shiny script](../EOmapmaker/app.R) or parts of it in your own project here are some explanations to the different parts of the script. 


### Choosing a place on a map
This uses the leaflet package and openstreetmaps to generate a map on which the user can indcate a place using a mouse click. When clicked the script saves the location data to a variable. When there is a new click on the map, these values get updated.

```R 
ui <- fluidPage(
    leafletOutput("MyMap", width = "70%")
)
server <- function(input, output, session) {
    output$MyMap <- renderLeaflet({
        leaflet() %>% 
            addTiles(options = providerTileOptions(noWrap = TRUE)) %>% 
            addTiles() %>% 
            addMarkers(lng = -1.394479, lat = 50.893765)
    })
    
    # Create reactive boolean value that indicates a click on the leaflet widget
    react_list <- reactiveValues(doubleClick = FALSE, lastClick = NA)
    observeEvent(input$MyMap_click$.nonce, {
        react_list$doubleClick <- identical(react_list$lastClick, input$MyMap_click[1:2])
        react_list$lastClick <- input$MyMap_click[1:2]
    })
    
    observeEvent(input$MyMap_click, {
        click = input$MyMap_click
        leafletProxy('MyMap') %>% 
            clearMarkers() %>% 
            addMarkers(lng = input$MyMap_click$lng, 
                       lat = input$MyMap_click$lat)
    }) 
}
```

## Calculating the bounding box for which the satellite image should be downloaded
Based on the previous location data this part calculates 4 coordinates marking the corner of a rectancle with a DIN A4 aspect ratio.

```R
lichtenbergratio = sqrt(2)/1
zoomfac = 0.2
 
if(is.na(react_list$lastClick[1])){
  upper.lat = 50.893765 + zoomfac 
  upper.lon = -1.394479 - zoomfac*lichtenbergratio
  lower.lat = 50.893765 - zoomfac 
  lower.lon = -1.394479 + zoomfac*lichtenbergratio
  } else {
  upper.lat = input$MyMap_click$lat + zoomfac 
  upper.lon = input$MyMap_click$lng - zoomfac*lichtenbergratio
  lower.lat = input$MyMap_click$lat - zoomfac 
  lower.lon = input$MyMap_click$lng + zoomfac*lichtenbergratio
  }
```

## Assembling the web request

```R
ui <- fluidPage(
    textInput("caption", "Your Sentinel Hub INSTANCE ID here", "", width = "60%"),
    selectInput("band", "Choose the satellite band you want to download",
                choices = c("Natural", "Vegetation", "MoistureIndex"), width = "60%")
                )


trara <- reactive({
  switch(input$band, "Natural" = "TRUE_COLOR", "Vegetation" = "FALSE_COLOR", "MoistureIndex" = "MOISTURE_INDEX")
        })
address = "https://services.sentinel-hub.com/ogc/wms/"
identifier = input$caption
requeststart = "?REQUEST=GetMap&BBOX="
areacoords = paste(coordbox, sep=",", collapse=",")
crs = "&CRS=CRS:84"
priority = "&PRIORITY=leastCC"
layers  = "&LAYERS="
layername = paste(trara())
today = Sys.Date()
lastpart = "&MAXCC10&WIDTH=2500&HEIGHT=1768&FORMAT=image/jpeg&TIME=2019-01-01/"
coordbox = c( upper.lon,upper.lat,lower.lon,lower.lat)
            
hereyougo = paste(address,identifier,requeststart,areacoords,crs,priority,layers,layername,lastpart,today,sep = "", collapse = "")
```

## Downloading the satellite image 
```R
tryCatch({
  download.file(hereyougo,'satimage.jpg', mode = 'wb')
  },
  warning = function(warn){
  showNotification(paste0(warn), type = 'warning')
  },
  error = function(err){
  showNotification(paste0(err), type = 'err')
  })
```

## Adding an overlay to the satellite image and combining it with the explanatory illustration.
This part using the R magick package to handle and manipulate the image files
```R
eobs <- image_read("NOC_english_map.jpg")
overlay <- image_read("NOC_english_frame.png")
satimage <- image_read("satimage.jpg")
            
img <- c(satimage,overlay)
img = image_flatten(img)

img = image_flatten(img)
all_images = c(eobs, img)
            
image_write(all_images, path = "YourMap.pdf", format = "pdf", density = 72)  
```

# Adding a download link 
```R
ui <-fluidpage(downloadLink("downloadData","Click here for your map."))
server <- function(input, output, session) {
  output$downloadData <- downloadHandler(
                filename = "YourMap.pdf",
                content = function(file) {
                file.copy("YourMap.pdf", file)
                }
}
```
