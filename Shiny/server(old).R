#! /usr/bin/env Rscript
# Packages used.
library(shiny)
library(rgdal)
library(leaflet)
library(sp)
library(rgeos)

# Read data frame. Should be from Rcpp then
gendf <- read.csv("Electors.csv", header = TRUE, sep = ',')

datdf <- read.csv("Electors-Code.csv", header = TRUE, sep = ',')
codedf <- datdf[, c(1,5)] 
# Getting the max/min data for the original map.
max_lat_map <- max(gendf[5])
min_lat_map <- min(gendf[5])
cen_lat_map <- (max_lat_map + min_lat_map) / 2
max_lng_map <- max(gendf[6])
min_lng_map <- min(gendf[6])
cen_lng_map <- (max_lng_map + min_lng_map) / 2

# Getting the max/min data for the cartogram.
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

# make a list
Map_df <- gendf[, c(2,5,6)]
Map_list <- split(Map_df, Map_df$poly)
pw_dat_code <- data.frame(FIPS.Code=pw_dat_reg, State_nm=codedf[match(pw_dat_reg, codedf$FIPS.Code), 1])

# only want lng-lats in the list, not the names
Map_list <- lapply(Map_list, function(x) { x["poly"] <- NULL; x })
ps <- lapply(Map_list, Polygon)

# add id variable
p1 <- lapply(seq_along(pw_dat_reg), function(i) Polygons(ps[pw_dat[,1] == pw_dat_reg[i]], 
                                                         ID = i-1))

# create SpatialPolygons object
my_Map_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84")) 

my_Map_polys_df <- SpatialPolygonsDataFrame(my_Map_polys,
                                            data.frame(ID = pw_dat_code$FIPS.Code,
                                                       NAME = pw_dat_code$State_nm,
                                                       row.names = 0:49))
#plot(my_Map_polys_df)

# make a Carto list
Carto_df <- gendf[2:4]
Carto_list <- split(Carto_df, Carto_df$poly)

# only want lng-lats in the list, not the names
Carto_list <- lapply(Carto_list, function(x) { x["poly"] <- NULL; x })
ps <- lapply(Carto_list, Polygon)

# add id variable
p1 <- lapply(seq_along(pw_dat_reg), function(i) Polygons(ps[pw_dat[,1] == pw_dat_reg[i]], 
                                                         ID = i-1))


# create SpatialPolygons object
my_Carto_polys <- SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84")) 

my_Carto_polys_df <- SpatialPolygonsDataFrame(my_Carto_polys,
                                              data.frame(ID = pw_dat_code$FIPS.Code,
                                                         NAME = pw_dat_code$State_nm,
                                                         row.names = 0:49))
#plot(my_Carto_polys_df)
my_Map_polys_df <- spTransform(my_Map_polys_df,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
my_Carto_polys_df <- spTransform(my_Carto_polys_df,  CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

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
        layerId = ~ID,
        # set color
        color = "#000", weight = 1, opacity = 0.5,
        fillColor = ~colorNumeric(pal, index())(index()), fillOpacity = 1, 
        # set highlight options
        highlightOptions = highlightOptions(color = "white", fill=TRUE, fillColor="white", weight = 2,
                                           bringToFront = TRUE),
        # set data to display in the tooltip popup 
        # that appears when a country is clicked on
        popup = with(my_Carto_polys_df@data, htmltools::htmlEscape(sprintf("%s: %d", NAME, ID)))
      )
  })
  
  # render the right (non-interactive) map
  output$map2 <- renderLeaflet({
    if (is.null(input$index) || (input$index == "other" && is.null(input$file1))) return(NULL)
    
    leaflet() %>% setView(cen_lat_carto, cen_lng_carto, zoom=10) %>%
      addPolygons(
        data = my_Carto_polys_df,
        layerId = ~ID,
        color = "#000", weight = 1, opacity = 0.5,
        fillColor = ~colorNumeric(pal, index())(index()), fillOpacity = 1, 
        popup = with(my_Carto_polys_df@data, htmltools::htmlEscape(sprintf("%s: %d", NAME, ID)))
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
      newdf <- my_Map_polys_df[my_Map_polys_df$ID==name,]
      newdf2 <- my_Carto_polys_df[my_Carto_polys_df$ID==name,]
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
                             fillColor = "red",
                             fillOpacity = 1, 
                             color = "red",
                             weight = 3, 
                             stroke = T,
                             layerId = "Selected")
    }
  })
  
}