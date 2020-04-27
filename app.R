
library(shiny)
library(shinyWidgets)
library(leaflet)
library(ggplot2)
library(magick)
library(leaflet.extras)

ui <- fluidPage(
    setBackgroundColor("ghostwhite"),
    h1("Build your own Earth observation poster"),
    p("Welcome! This website is a compagnion site to the outreach project", tags$strong("We can see our oceans from space - a tailored illustrated guide to promote engagement with Earth Observation"), "funded by the", tags$a(href="https://www.agu.org", "AGU"), "as part of celebrating 100 years of Earth and Space science."),
    
    p(tags$a(href="https://heuschele.com", "Heuschele.com"),"and the UK", tags$a(href="https://www.noc.ac.uk", "National Oceangraphic Centre"), "worked in partnership to make and share a two-paged poster that explains on one page the basic principles of earth observation and features on the other page a recent satellite image of a specified area."),
    
    p("By running this app you will be able to assemble your own printable two-page pdf version with a satellite image of an area of your choosing. It uses up to date images from the Sentinel satellites of the European Space Agency. This website uses the services provided by", tags$a(href="https://www.sentinel-hub.com", "Sentinel Hub.")),
    
 
    h2("Step 1"),
    
   
    p("Obtain a free trial account from", tags$a(href="https://services.sentinel-hub.com/oauth/subscription", "their website."), "Once you have an account, log in, and go to the Configuration utility to copy the ID given in the WMS template. You will also find the correct ID in user settings under OGC requests. To be able to download the satellite image you need to enter this ID. The script does not save any data, but to be able to make the pdf it will make a temporary copy of the satellite image you download."),
    
    textInput("caption", "Your Sentinel Hub INSTANCE ID here", "", width = "60%"),
    
    h2("Step 2"),
    
    p("Drag the map around and zoom into it, and click on the place you are interested in. You can change the place several times."),
    
    leafletOutput("MyMap", width = "70%"),
    
    
    selectInput("band", "Choose the satellite band you want to download",
                choices = c("Natural", "Vegetation", "MoistureIndex"), width = "60%"),
    
    p("(1) Natural will download a natural color image, (2) this image will indicate where vegetation is based on a colorized infrared image, (3) MoistureIndex gives you an indication about the water content of the ground"),
    
    h2("Step 3"),
    
    p("If you want to add a title onto your satellite image, just change the text below. You can also remove it if you want."),
    textInput("maptext", "", "My favorite place", width = "60%"),
    p("When you are happy with the place, title, and you have entered your ID then press the", tags$strong("go button"), "to generate your personal EO poster. This process might take a while, as the program will download your chosen satellite image and combine it with the explanatory poster, and then assemble a two-sided pdf."),
    
    actionButton("go", "Go"),
    br(),
    p("Once the progress infobox ",tags$strong("has closed"), "you can click the download link below, and download a file called MyMap.pdf."),
    downloadLink("downloadData","Click here for your map."),
    br(),
    br(),
    p("This script/shiny app was written by Jan Heuschele", tags$a(href="http://heuschele.com/","heuschele.com."))
)

server <- function(input, output, session) {
    
    output$MyMap <- renderLeaflet({
        leaflet() %>% 
            addTiles(options = providerTileOptions(noWrap = TRUE)) %>% 
            addTiles() %>% 
            addMarkers(lng = -1.394479, lat = 50.893765)
    })
    
    # Create reactive boolean value that indicates a double-click on the leaflet widget
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
    
    trara <- reactive({
        switch(input$band,
               "Natural" = "TRUE_COLOR",
               "Vegetation" = "FALSE_COLOR",
               "MoistureIndex" = "MOISTURE_INDEX")
        })
    
    
    observeEvent(input$go, {
        withProgress(message = 'Making plot', value = 0, {
            n = 10 #for the progress bar
            
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
            
            
            # Calculate the area 
            coordbox = c( upper.lon,
                          upper.lat,
                          lower.lon,
                          lower.lat)
            
            incProgress(1/n, detail = paste("Calculating the coordinates"))
            incProgress(2/n, detail = paste("Assembling the server request"))
            
            #Website address components
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
        
            hereyougo = paste(address,identifier,
                              requeststart,areacoords,
                              crs,priority,layers,
                              layername,lastpart,today,
                              sep = "", collapse = "")
            
            incProgress(5/n, detail = paste("Summoning the servers"))
            
            tryCatch({
                download.file(hereyougo,'satimage.jpg', mode = 'wb')
            },
            warning = function(warn){
                showNotification(paste0(warn), type = 'warning')
            },
            error = function(err){
                showNotification(paste0(err), type = 'err')
            })
            
            incProgress(6/n, detail = paste("Downloading the satellite image"))
            eobs <- image_read("NOC_english_map.jpg")
            incProgress(7/n, detail = paste("Reading the images"))
            overlay <- image_read("NOC_english_frame.png")
            satimage <- image_read("satimage.jpg")
            incProgress(8/n, detail = paste("Combining the images"))
            
            img <- c(satimage,overlay)
            img = image_flatten(img)
            
            map.t = input$maptext
            img = image_annotate(img, map.t, 
                                      size = 60, 
                                      color = "white", 
                                      location = "+200+200", 
                                      font = "Helvetica", 
                                      weight = 700)
            img = image_flatten(img)
            all_images = c(eobs, img)
            
            image_write(all_images, 
                        path = "YourMap.pdf", 
                        format = "pdf", 
                        density = 72)  
            
            incProgress(9/n, detail = paste("Preparing the pdf"))
            
            output$downloadData <- downloadHandler(
                filename = "YourMap.pdf",
                content = function(file) {
                file.copy("YourMap.pdf", file)
                }
            )
            incProgress(10/n, detail = paste("You can download your map now"))
            
        }) #end progress bar
    })
    
    
}


shinyApp(ui, server)
