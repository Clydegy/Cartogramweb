#! /usr/bin/env Rscript
# Packages used.
library(shiny)
library(rgdal)
library(leaflet)
library(sp)
library(rgeos)

# Setting up USA Data.
# Read data frames.
gendf <- read.csv("USADF.csv", header = TRUE, sep = ',')

# Add a pairwise data for RegionID and PolyID.
pw_dat <- unique(gendf[, 1:2])
pw_dat_reg <- unique(pw_dat[,1])

# Make a list
Map_df <- gendf[, c(2:4)]
Map_list <- split(Map_df, Map_df$poly)

# Getting names and data.frames needed
datdf <- read.csv("USAData.csv", header = TRUE, sep = ',')
codedf <- datdf[, c(1:2)] 
pw_dat_code <- data.frame(Region.ID=pw_dat_reg, Region.NM=codedf[match(pw_dat_reg, codedf$Region.ID), 2])

# only want lng-lats in the list, not the names
Map_list <- lapply(Map_list, function(x) { x["poly"] <- NULL; x })
ps <- lapply(Map_list, Polygon)

# add id variable
p1 <- lapply(seq_along(pw_dat_reg), function(i) Polygons(ps[pw_dat[,1] == pw_dat_reg[i]], 
                                                         ID = i-1))


# create SpatialPolygons object
USA_map <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84")) 

USA_map_df  <- SpatialPolygonsDataFrame(USA_map,
                                            data.frame(ID = pw_dat_code$Region.ID,
                                                       NAME = pw_dat_code$Region.NM,
                                                       row.names = 0:(nrow(pw_dat_code) - 1)))
plot(USA_map_df)
USA_map_df <- spTransform(USA_map_df,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

# Setting up China Data.
# Read data frames.
gendf <- read.csv("ChinaDF.csv", header = TRUE, sep = ',')

# Add a pairwise data for RegionID and PolyID.
pw_dat <- unique(gendf[, 1:2])
pw_dat_reg <- unique(pw_dat[,1])

# Make a list
Map_df <- gendf[, c(2:4)]
Map_list <- split(Map_df, Map_df$poly)

# Getting names and data.frames needed
datdf <- read.csv("ChinaData.csv", header = TRUE, sep = ',')
codedf <- datdf[, c(1:2)] 
pw_dat_code <- data.frame(Region.ID=pw_dat_reg, Region.NM=codedf[match(pw_dat_reg, codedf$Region.ID), 2])

# only want lng-lats in the list, not the names
Map_list <- lapply(Map_list, function(x) { x["poly"] <- NULL; x })
ps <- lapply(Map_list, Polygon)

# add id variable
p1 <- lapply(seq_along(pw_dat_reg), function(i) Polygons(ps[pw_dat[,1] == pw_dat_reg[i]], 
                                                         ID = i-1))


# create SpatialPolygons object
China_map <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84")) 

China_map_df  <- SpatialPolygonsDataFrame(China_map,
                                        data.frame(ID = pw_dat_code$Region.ID,
                                                   NAME = pw_dat_code$Region.NM,
                                                   row.names = 0:(nrow(pw_dat_code) - 1)))
plot(China_map_df)
China_map_df <- spTransform(China_map_df,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))


# Setting up India Data.
# Read data frames.
gendf <- read.csv("IndiaDF.csv", header = TRUE, sep = ',')

# Add a pairwise data for RegionID and PolyID.
pw_dat <- unique(gendf[, 1:2])
pw_dat_reg <- unique(pw_dat[,1])

# Make a list
Map_df <- gendf[, c(2:4)]
Map_list <- split(Map_df, Map_df$poly)

# Getting names and data.frames needed
datdf <- read.csv("IndiaData.csv", header = TRUE, sep = ',')
codedf <- datdf[, c(1:2)] 
pw_dat_code <- data.frame(Region.ID=pw_dat_reg, Region.NM=codedf[match(pw_dat_reg, codedf$Region.ID), 2])

# only want lng-lats in the list, not the names
Map_list <- lapply(Map_list, function(x) { x["poly"] <- NULL; x })
ps <- lapply(Map_list, Polygon)

# add id variable
p1 <- lapply(seq_along(pw_dat_reg), function(i) Polygons(ps[pw_dat[,1] == pw_dat_reg[i]], 
                                                         ID = i-1))


# create SpatialPolygons object
India_map <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84")) 

India_map_df  <- SpatialPolygonsDataFrame(India_map,
                                          data.frame(ID = pw_dat_code$Region.ID,
                                                     NAME = pw_dat_code$Region.NM,
                                                     row.names = 0:(nrow(pw_dat_code) - 1)))
plot(India_map_df)
India_map_df <- spTransform(India_map_df,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

#Using US Cartogram.
gendf <- read.csv("USADF.csv", header = TRUE, sep = ',')

# Getting the max/min data for the original map.
max_lat_map <- max(gendf[3])
min_lat_map <- min(gendf[3])
cen_lat_map <- (max_lat_map + min_lat_map) / 2
max_lng_map <- max(gendf[4])
min_lng_map <- min(gendf[4])
cen_lng_map <- (max_lng_map + min_lng_map) / 2

gendf <- read.csv("USACarto.csv", header = TRUE, sep = ',')
# Getting the max/min data for the Cartogram map.
max_lat_carto <- max(gendf[3])
min_lat_carto <- min(gendf[4])
cen_lat_carto <- (max_lat_carto + min_lat_carto) / 2
max_lng_carto <- max(gendf[3])
min_lng_carto <- min(gendf[4])
cen_lng_carto <- (max_lng_carto + min_lng_carto) / 2

# Convert the coords.
lat_ratio <- (max_lat_carto - min_lat_carto) / (max_lat_map - min_lat_map)
lng_ratio <- (max_lng_carto - min_lng_carto) / (max_lng_map - min_lng_map)
gendf[3] <- gendf[3] / lat_ratio
gendf[4] <- gendf[4] / lng_ratio

# Getting the center for the new cartogram coords.
cen_lat_carto <- cen_lat_carto / lat_ratio
cen_lng_carto <- cen_lng_carto / lng_ratio

# Add a pairwise data for RegionID and PolyID.
pw_dat <- unique(gendf[, 1:2])
pw_dat_reg <- unique(pw_dat[,1])

# Make a list
Map_df <- gendf[, c(2:4)]
Map_list <- split(Map_df, Map_df$poly)

# Getting names and data.frames needed
datdf <- read.csv("USAData.csv", header = TRUE, sep = ',')
codedf <- datdf[, c(1:2)] 
pw_dat_code <- data.frame(Region.ID=pw_dat_reg, Region.NM=codedf[match(pw_dat_reg, codedf$Region.ID), 2])

# only want lng-lats in the list, not the names
Map_list <- lapply(Map_list, function(x) { x["poly"] <- NULL; x })
ps <- lapply(Map_list, Polygon)

# add id variable
p1 <- lapply(seq_along(pw_dat_reg), function(i) Polygons(ps[pw_dat[,1] == pw_dat_reg[i]], 
                                                         ID = i-1))


# create SpatialPolygons object
Carto_map <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84")) 

Carto_df  <- SpatialPolygonsDataFrame(Carto_map,
                                      data.frame(ID = pw_dat_code$Region.ID,
                                                 NAME = pw_dat_code$Region.NM,
                                                 row.names = 0:(nrow(pw_dat_code) - 1)))
plot(Carto_df)
Carto_df <- spTransform(Carto_df,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

########################

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
    else if (input$index == "USA") index <- df$POP2005 # 2005 population
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
    if (input$index == "USA") Map_df <- USA_map_df
    if (input$index == "CHN") 
    {
      Map_df <- China_map_df
      gendf <- read.csv("ChinaDF.csv", header = TRUE, sep = ',')
      # Getting the max/min data for the original map.
      max_lat_map <- max(gendf[3])
      min_lat_map <- min(gendf[3])
      cen_lat_map <- (max_lat_map + min_lat_map) / 2
      max_lng_map <- max(gendf[4])
      min_lng_map <- min(gendf[4])
      cen_lng_map <- (max_lng_map + min_lng_map) / 2
    }
    if (input$index == "IND")
    {
      Map_df <- India_map_df
      gendf <- read.csv("IndiaDF.csv", header = TRUE, sep = ',')
      # Getting the max/min data for the original map.
      max_lat_map <- max(gendf[3])
      min_lat_map <- min(gendf[3])
      cen_lat_map <- (max_lat_map + min_lat_map) / 2
      max_lng_map <- max(gendf[4])
      min_lng_map <- min(gendf[4])
      cen_lng_map <- (max_lng_map + min_lng_map) / 2
    }
    # generate a leaflet plot
    leaflet() %>% setView(cen_lat_map, cen_lng_map, zoom=10) %>%
      addPolygons(
        # set dataframe to use
        data = Map_df,
        # set index to group polygons by country
        layerId = ~ID,
        # set color
        color = "#000", weight = 1, opacity = 0.5,
        fillColor = ~colorNumeric(pal, index())(index()), fillOpacity = 1, 
        # set highlight options
        highlightOptions = highlightOptions(color = "white", fill=TRUE, fillColor="white", weight = 2,
                                            bringToFront = TRUE),
        # set data to display in the tooltip popup 
        # that appears when a country is clicked on
        popup = with(Map_df@data, htmltools::htmlEscape(sprintf("%s: %d", NAME, ID)))
      )
  })
  
  # render the right (non-interactive) map
  output$map2 <- renderLeaflet({
    if (is.null(input$index) || (input$index == "other" && is.null(input$file1))) return(NULL)
    if (input$index == "CHN") return(NULL)
    if (input$index == "IND") return(NULL)
    
    leaflet() %>% setView(cen_lat_carto, cen_lng_carto, zoom=10) %>%
      addPolygons(
        data = Carto_df,
        layerId = ~ID,
        color = "#000", weight = 1, opacity = 0.5,
        fillColor = ~colorNumeric(pal, index())(index()), fillOpacity = 1, 
        popup = with(Carto_df@data, htmltools::htmlEscape(sprintf("%s: %d", NAME, ID)))
      )
  })
  
  # listen for user click on country in left map
  observeEvent(input$map_shape_click, {
    # listen for clicks
    if (input$index == "USA") Map_df <- USA_map_df
    if (input$index == "CHN") Map_df <- China_map_df
    if (input$index == "IND") Map_df <- India_map_df
    
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
      newdf <- Map_df[Map_df$ID==name,]
      newdf2 <- Carto_df[Carto_df$ID==name,]
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
      proxy2 %>% addPolygons(data = newdf2, 
                             fillColor = "green",
                             fillOpacity = 1, 
                             color = "green",
                             weight = 3, 
                             stroke = T,
                             layerId = "Selected")
    }
  })
  
}