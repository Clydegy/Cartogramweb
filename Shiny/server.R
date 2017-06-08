# Packages used.
library(shiny)
library(rgdal)
library(leaflet)
library(ggplot2)
library(sp)

# Convert Normal map .gen into dataframe
if (exists("gendf"))
  rm(gendf)
gen <- readLines("low48splitMElo_conic.gen")
poly.number <- 1
line.number <- 2
while (line.number < length(gen)) {
  if (line.number %% 1000 == 0)
    cat("working on line ", line.number, " of the .gen file (out of ",
        length(gen), ")\n", sep = "")
  line <- gen[line.number]
  if (line != "END") {
    coord <- as.numeric(strsplit(line, " ")[[1]])
    tmp <- data.frame(poly = poly.number, 
                      x = coord[1], y = coord[2])
    if (exists("gendf")) {
      gendf <- rbind(gendf, tmp)
    } else {
      gendf <- tmp
    }
  } else {
    line.number <- line.number + 1
    line <- gen[line.number]
    if (line == "END")
      break
    poly.number <- poly.number + 1
  }
  line.number <- line.number + 1
}

max_lat_map <- max(gendf[2])
min_lat_map <- min(gendf[2])
cen_lat_map <- (max_lat_map + min_lat_map) / 2
max_lng_map <- max(gendf[3])
min_lng_map <- min(gendf[3])
cen_lng_map <- (max_lng_map + min_lng_map) / 2
# make a list
Map_df <- gendf[1:3]
Map_list <- split(Map_df, Map_df$poly)
# only want lon-lats in the list, not the names
Map_list <- lapply(Map_list, function(x) { x["poly"] <- NULL; x })
ps <- lapply(Map_list, Polygon)
# add id variable
p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
                                                 ID = names(Map_list)[i]))
# create SpatialPolygons object
my_Map_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84")) 

my_Map_polys_df <- SpatialPolygonsDataFrame(my_Map_polys, 
                                            data.frame(id = unique(Map_df$poly), 
                                                       row.names = unique(Map_df$poly)))
plot(my_Map_polys_df)

# Convert Cartogram .gen file into data frame.
if (exists("gendf")){
  rm(gendf)
}
if (exists("idpoly")){
  rm(idpoly)
}
gen <- readLines("low48_elector_cart.gen")
poly.number <- 1
poly.id <- as.numeric(gen[1])
line.number <- 2
while (line.number < length(gen)) {
  if (line.number %% 1000 == 0)
    cat("working on line ", line.number, " of the .gen file (out of ",
        length(gen), ")\n", sep = "")
  line <- gen[line.number]
  if (line != "END") {
    coord <- as.numeric(strsplit(line, " ")[[1]])
    tmp <- data.frame(id = poly.id, poly = poly.number, 
                      x = coord[1], y = coord[2])
    if (exists("gendf")) {
      gendf <- rbind(gendf, tmp)
    } else {
      gendf <- tmp
    }
  } else {
    line.number <- line.number + 1
    line <- gen[line.number]
    if (line == "END")
      break
    poly.id <- as.numeric(line)
    poly.number <- poly.number + 1
  }
  line.number <- line.number + 1
  tmp1 <- data.frame(id = poly.id, poly = poly.number)
  if (! exists("idpoly")) {
    idpoly <- tmp1
  }
  if ((exists("idpoly")) && (tmp1[2]!=idpoly[nrow(idpoly),2])) {
    idpoly <- rbind(idpoly, tmp1)
  }
}
max_lat_carto <- max(gendf[3])
min_lat_carto <- min(gendf[3])
cen_lat_carto <- (max_lat_carto + min_lat_carto) / 2
max_lng_carto <- max(gendf[4])
min_lng_carto <- min(gendf[4])
cen_lng_carto <- (max_lng_carto + min_lng_carto) / 2
lat_ratio <- (max_lat_carto - min_lat_carto) / (max_lat_map - min_lat_map)
lng_ratio <- (max_lng_carto - min_lng_carto) / (max_lng_map - min_lng_map)
gendf[3] <- gendf[3] / lat_ratio
gendf[4] <- gendf[4] / lng_ratio
max_lat_carto <- max(gendf[3])
min_lat_carto <- min(gendf[3])
cen_lat_carto <- (max_lat_carto + min_lat_carto) / 2
max_lng_carto <- max(gendf[4])
min_lng_carto <- min(gendf[4])
cen_lng_carto <- (max_lng_carto + min_lng_carto) / 2
## From this data.frame, I then convert it to a list, then to polygons, then to a SpatialPolygons object
# make a list
Carto_df <- gendf[2:4]
Carto_list <- split(Carto_df, Carto_df$poly)
# only want lon-lats in the list, not the names
Carto_list <- lapply(Carto_list, function(x) { x["poly"] <- NULL; x })

#  make data.frame into spatial polygon, cf. http://jwhollister.com/iale_open_science/2015/07/05/03-Spatial-Data-In-R/

# create SpatialPolygons Object, convert coords to polygon
ps <- lapply(Carto_list, Polygon)

# add id variable
p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]), 
                                                 ID = names(Carto_list)[i]  ))

# create SpatialPolygons object
my_Carto_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84") ) 

my_Carto_polys_df <- SpatialPolygonsDataFrame(my_Carto_polys, 
                                                data.frame(id = unique(Carto_df$poly), 
                                                           row.names = unique(Carto_df$poly)))
my_Carto_polys_df$NUMID <- idpoly[1]
my_Map_polys_df$NUMID <- idpoly[1]
plot(my_Carto_polys_df)



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
    leaflet() %>% setView(cen_lat_map, cen_lng_map, zoom=10) %>%
      addPolygons(
        # set dataframe to use
        data = my_Map_polys_df,
        # set index to group polygons by country
        layerId = ~id,
        # set color
        color = "#000", weight = 1, opacity = 0.5,
        fillColor = ~colorNumeric(pal, index())(index()), fillOpacity = 1, 
        # set highlight options
        highlightOptions = highlightOptions(color = "white", fill=TRUE, fillColor="white", weight = 2,
                                            bringToFront = TRUE),
        # set data to display in the tooltip popup 
        # that appears when a country is clicked on
        popup = with(my_Map_polys_df@data, htmltools::htmlEscape(sprintf("%s: %s", id, id)))
      )
  })
  
  # render the right (non-interactive) map
  output$map2 <- renderLeaflet({
    if (is.null(input$index) || (input$index == "other" && is.null(input$file1))) return(NULL)
    
    leaflet() %>% setView(cen_lat_carto, cen_lng_carto, zoom=10) %>%
      addPolygons(
        data = my_Carto_polys_df,
        layerId = ~id,
        color = "#000", weight = 1, opacity = 0.5,
        fillColor = ~colorNumeric(pal, index())(index()), fillOpacity = 1, 
        popup = with(my_Carto_polys_df@data, htmltools::htmlEscape(sprintf("%s: %s", id, id)))
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
      newMapdf <- my_Map_polys_df[my_Map_polys_df$id==name,]
      # draw the selected country's polygons on an
      # additional layer above the base map layer (left)
      proxy %>% addPolygons(data = newMapdf, 
                            fillColor = "green",
                            fillOpacity = 1, 
                            color = "green",
                            weight = 3, 
                            stroke = T,
                            layerId = "Selected")
      newCartodf <- my_Carto_polys_df[my_Carto_polys_df$id==name,]
      # do the same for the right map
      proxy2 %>% addPolygons(data = newCartodf, 
                             fillColor = "red",
                             fillOpacity = 1, 
                             color = "red",
                             weight = 3, 
                             stroke = T,
                             layerId = "Selected")
    }
  })
  
}