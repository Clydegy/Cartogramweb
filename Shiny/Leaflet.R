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
plot(my_Map_polys_df)

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
plot(my_Carto_polys_df)