library(shiny)
library(rgdal)
library(leaflet)

# read map data from shapefile, store in data frame
df <- readOGR(".", "TM_WORLD_BORDERS_SIMPL-0.3")
df$POP2005 <- as.numeric(as.character(df$POP2005))
# define color scale to use for the map (shades of blue)
pal <- scales::seq_gradient_pal(low = "#132B43", 
                                high = "#56B1F7", 
                                space = "Lab")
(seq(0, 1, length.out = 255))

# shiny server function listens for input/output 
function(input, output, session) {
  # listen for selection of index to use for map generation
  # currently, user can choose 2005 population or
  # upload their own csv file
  index <- reactive({
    if (is.null(input$index)) return(NULL) # no file selected
    else if (input$index == "pop") index <- df$POP2005 # 2005 population
    else {
      file <- input$file1
      if (is.null(file)) return(NULL)
      df$index <- read.csv(file$datapath, header=TRUE, sep=",")$parameter 
      index <- df$index # read index from csv
    }
    index # the reactive function returns the selected index
  })
  
  # render the left (clickable) map
  output$map <- renderLeaflet({
    # if no selection from sidebar, don't generate the map
    if (is.null(input$index) || (input$index == "other" && is.null(input$file1))) return(NULL)
    
    # generate a leaflet plot
    leaflet() %>% setView(0, 0, zoom=1) %>%
      addPolygons(
        # set dataframe to use
        data = df,
        # set index to group polygons by country
        layerId = ~UN,
        # set color
        color = "#000", weight = 1, opacity = 0.5,
        fillColor = ~colorNumeric(pal, index())(index()), fillOpacity = 1, 
        # set highlight options
        highlightOptions = highlightOptions(color = "white", fill=TRUE, fillColor="white", weight = 2,
                                            bringToFront = TRUE),
        # set data to display in the tooltip popup 
        # that appears when a country is clicked on
        popup = with(df@data, htmltools::htmlEscape(sprintf("%s: %s", NAME, index())))
      )
  })
  
  # render the right (non-interactive) map
  output$map2 <- renderLeaflet({
    if (is.null(input$index) || (input$index == "other" && is.null(input$file1))) return(NULL)
    
    leaflet() %>% setView(0, 0, zoom=1) %>%
      addPolygons(
        data = df,
        layerId = ~UN,
        color = "#000", weight = 1, opacity = 0.5,
        fillColor = ~colorNumeric(pal, index())(index()), fillOpacity = 1, 
        popup = with(df@data, htmltools::htmlEscape(sprintf("%s: %s", NAME, index())))
      )
  })
  
  # listen for user click on country in left map
  observeEvent(input$map_shape_click, {
    # listen for clicks
    click <- input$map_shape_click
    
    if(is.null(click))
      return()   
    
    # create 2 proxies since both maps need modifying
    proxy <- leafletProxy("map")
    proxy2 <- leafletProxy("map2")
    
    # action to perform if currently highlighted item
    # is clicked again
    if(click$id == "Selected"){
      proxy %>% removeShape(layerId = "Selected")
      proxy2 %>% removeShape(layerId = "Selected")
    } 
    # action to perform is regular polygon is clicked
    else {
      name <- click$id
      # create a new data frame of polygon data only
      # for the selected country
      newdf <- df[df$UN==name,]
      # draw the selected country's polygons on an
      # additional layer above the base map layer (left)
      proxy %>% addPolygons(data = newdf, 
                            fillColor = "green",
                            fillOpacity = 1, 
                            color = "green",
                            weight = 3, 
                            stroke = T,
                            layerId = "Selected")
      # do the same for the right map
      proxy2 %>% addPolygons(data = newdf, 
                             fillColor = "red",
                             fillOpacity = 1, 
                             color = "red",
                             weight = 3, 
                             stroke = T,
                             layerId = "Selected")
    }
  })
  
}