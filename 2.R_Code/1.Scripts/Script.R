#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2021-02-01 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(sf)
library(sp)
library(raster)
library(terra)
library(mapview)
library(ggplot2)
library(rayshader)

#      Functions                                                            ####

#      Data                                                                 ####

# Datapoints

Datapoints <- read.csv("1.Data\\Datapoints.csv")


# Raster Data
raster_raster <- raster("1.Data\\ASTGTMV003_N00E010_dem.tif")
plot(raster_raster)

raster_terra  <- rast("1.Data\\ASTGTMV003_N00E010_dem.tif")
plot(raster_terra)
###############################################################################
#   Point Data / Polygons                                                   ####
#      sp                                                                   ####

# SpatialPoints

# Coordinates
coords <- data.frame(
  lat = Datapoints$lat,
  lon = Datapoints$lon
)

# Spatial Points 
sp <- SpatialPoints(coords,
                   proj4string = crs("+init=epsg:4326"))

# Spatial Points Data Frame
spdf <- SpatialPointsDataFrame(coords = coords,
                               data = Datapoints,
                               proj4string = crs("+init=epsg:4326"))

# Plotting
mapview(sp)
mapview(spdf)

#      sf                                                                   ####
#        Setup                                                              ####
# Coordinates
coords <- data.frame(
  lat = Datapoints$lat,
  lon = Datapoints$lon
)

# Sf points
sf_points <- st_as_sf(Datapoints,
                      coords = c('lat',"lon"),
                      crs = crs("+init=epsg:4326"))

#        Reprojection                                                       ####

reproj <- st_transform(sf_points,
                       crs = crs("+init=epsg:32632"))


#        Buffer Example                                                     ####

buffer_lonlat <- st_buffer(sf_points, dist = .1)
mapview(buffer_lonlat)

buffer_UTM <- st_buffer(reproj, dist = 1000)
mapview(buffer_UTM)


###############################################################################
#   Raster Data                                                             ####

# Note: EPSG codes can be found at https://spatialreference.org/

#      Raster                                                               ####
#        Aggregate                                                          ####

raster_ag_2fact <- raster::aggregate(raster_raster,
                                    fact = 2,
                                    fun = mean)

raster_ag_10fact <- raster::aggregate(raster_raster,
                                     fact = 10,
                                     fun = mean)

raster_ag_100fact <- raster::aggregate(raster_raster,
                                      fact = 100,
                                      fun = mean)
#        RasterBricks                                                       ####

# Creating Different Layers
raster_slope <- raster::terrain(raster_raster, opt = "slope")
raster_tri <- terrain(raster_raster, opt = "TRI")
raster_aspect <- terrain(raster_raster, opt = "aspect")


# Creating a Raster Stack 
raster_stack <- stack(raster_raster,raster_slope,terra_slope,raster_tri,raster_aspect)
plot(raster_stack)

#        Reprojecting                                                       ####

proj_string <- crs("+init=epsg:32632")

projected_raster <- projectRaster(raster_raster, crs = proj_string)

#      Terra                                                                ####
#        Aggregate                                                          ####

terra_ag_2fact <- terra::aggregate(raster_terra,
                                    fact = 2,
                                    fun = mean)

terra_ag_10fact <- terra::aggregate(raster_terra,
                                     fact = 10,
                                     fun = mean)

terra_ag_100fact <- terra::aggregate(raster_terra,
                                      fact = 100,
                                      fun = mean)
#        RasterBricks                                                       ####

# Creating Different Layers
terra_slope <- terrain(raster_terra, v = "slope")
terra_tri <- terrain(raster_terra, v = "TRI")
terra_aspect <- terrain(raster_terra, v = "aspect")


# Creating a Raster Stack 
terra_stack <- c(raster_terra,terra_slope,terra_tri,terra_aspect)
plot(terra_stack)

#        Reprojecting                                                       ####

proj_string <- crs("+init=epsg:32632")

projected_terraraster <- project(raster_terra, y = proj_string)

###############################################################################
#      Rayshader                                                            ####

landscape <- raster_to_matrix(raster_raster)

landscape <- landscape[c(1:300),c(1:300)]
#We use another one of rayshader's built-in textures:
landscape %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(landscape, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(landscape), 0) %>%
  plot_3d(landscape, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

###############################################################################
#   Resources                                                               ####

# great sf book : https://geocompr.robinlovelace.net/index.html
# rayshader     : https://github.com/tylermorganwall/rayshader
# epsg codes    : https://spatialreference.org/
# colorbrewer   : https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
