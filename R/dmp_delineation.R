#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Watershed Delineations:
#   Talladega

#First attempt; 20211115
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#OUTLINE --

#Step 0: Download and import data, setup workspace
# - DEM (preferably 3m or 1m if possible)
# - STIC and LTM points
# - load necessary packages
# - DEFINE DIRECTORIES FOR WBT
#Step 1: prep DEM to delineate, and DELINEATE
# - also export spatial files for later
#Step 2: prep sensor data and add to map

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 0: Download and import data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#0.1 clear workspace and install packages
#0.2 define directories
#0.3 Load in DEM
#0.4 import sensor location file
#0.5 write (?) outlet point

#0.1 -----
remove(list=ls())

library(tidyverse)
library(raster)
library(sf)
#if using spatial points,
#library(sp)
library(mapview)
library(stars)
#library(rayshader)
library(whitebox)
#library(rgdal)

#reinstall whitebox
#install.packages("whitebox", repos="http://R-Forge.R-project.org")
#IF YOU HAVE NEVER USED WHITEBOX --> whitebox::install_whitebox()


#0.2 -----
data_dir<-"/Users/Delaney/R/data_dir/RESEARCH/AIMS_mapping/AIMS_WSdelin/all_data/" 
temp_dir<-"/Users/Delaney/R/temp_dir/" 

#0.3 -----
DEM <- raster(paste0(data_dir,"TalladegaDEMfiles_usda/elevation_lidar01m33085g5.tif"))

#0.4 -----
#this is for mapping our AIMS sensors, you don't have to do this! BUT you will need to 
#tell it where your outlet it -- mine is included in tal_sensors, but you can import yours
#separately!
tal_sensors <- read_csv(paste0(data_dir, "AIMSsiteCoords_tent_TAL.csv")) 
tal_supersensor <- tal_sensors %>% 
  filter(SiteID == "TLM01_SENS") 

#0.5 -----
#convert it into barebones sf
#tell it where your data is, what the coords are in the df, and the crs (FOR LAT LONG, WGS84)
outlet <- st_as_sf(tal_supersensor, coords = c("Long", "Lat"), 
                   crs = '+proj=longlat +datum=WGS84 +no_defs')
sites <- st_as_sf(tal_sensors, coords = c("Long", "Lat"),
                  crs = '+proj=longlat +datum=WGS84 +no_defs')
#reproject to utm 16
outlet <- st_transform(outlet, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()
sites <- st_transform(sites, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()

#plot with mapview to check
mapview(DEM) + mapview(sites)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: prep DEM and delineate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.1 write DEM and outlet to temp
#1.2 fill cell pits
#1.3 breach depressions
#1.4 write flow direction raster
#1.5.2 write flow accumulation raster
#1.5.2 write stream layer
#1.6 snap pour point
#1.7 delineate
#1.8 read back into main
#1.9 convert to polygons

#A NOTE TO THOSE WHO MAY BE READING THIS (Stella <3);
# there are some redundancies here because the DEM I downloaded from USDA is
# STUPIDLY large. But the steps are overall the same regardless, the only part 
# that changes is the input rasters (the first round, I used the large one and 
# DID NOT export the stream layer, and the second round I used the WS extent 
# values to crop the raster, run it again, and then delineate the streams so it
# didn't take forever). If there's any confusion, I can walk you through, but I 
#left all these steps so you could see them!!!

#1.1 -----
writeRaster(DEM, paste0(temp_dir, "dem_tal.tif"), overwrite = T)
st_write(outlet, paste0(temp_dir, "outlet_tal.shp"), delete_layer = T)

#1.2 -----
wbt_fill_single_cell_pits(
  dem = "dem_tal.tif",
  output = "dem_tal_fill.tif",
  wd = temp_dir)

#1.3 -----
wbt_breach_depressions(
  dem = "dem_tal_fill.tif",
  output = "dem_tal_breach.tif",
  wd = temp_dir)

#1.4 -----
wbt_d8_pointer(
  dem = "dem_tal_breach.tif",
  output = "flowdir_tal.tif",
  wd = temp_dir)

#1.5.1 -----
wbt_d8_flow_accumulation(
  input = "dem_tal_breach.tif",
  #you have to use the DEM not the flow direction for some reason
  output = "flowaccum_tal.tif",
  wd = temp_dir
)

#1.5.2 -----
#DO THIS AGAIN LATER ON THE SMALLER RASTER
#streams <- raster(paste0(temp_dir, "flowaccum_tal.tif"))
#streams[streams<10000] <- NA
#writeRaster(streams, paste0(temp_dir, "streams_tal.tif"), overwrite=T)

#1.6 -----
wbt_snap_pour_points(
  pour_pts = "outlet_tal.shp",
  flow_accum = "flowaccum_tal.tif",
  snap_dist = 100,
  output = "snap_pour_tal.shp",
  wd = temp_dir
)

#1.7 -----
wbt_watershed(
  d8_pntr = "flowdir_tal.tif",
  pour_pts = "snap_pour_tal.shp",
  output = "shed_tal.tif",
  wd = temp_dir
)

#1.8 -----
#be sure your watershed shapefile is pulled in so we can use polygon area to get WS area
tal_ws <- raster(paste0(temp_dir, "shed_tal.tif"))
#tal_streams <- raster(paste0(temp_dir, "streams_tal.tif"))
#will not work this way
tal_sensor <- st_read(paste0(temp_dir, "snap_pour_tal.shp")) %>% st_geometry()

#1.9 -----
tal_ws <- st_as_stars(tal_ws) %>% st_as_sf()
#tal_streams <- st_as_stars(tal_streams) %>% st_as_sf(merge=T)
#also will not work
st_write(tal_ws, paste0(data_dir, "WS_tal.shp"), delete_layer = T)

mapview(tal_ws) + mapview(tal_sensor) + mapview(DEM)

#crop the DEM to run again
crop_extent <- readOGR(paste0(data_dir, "WS_tal.shp"))
cropped_DEM <- crop(DEM, crop_extent)
plot(cropped_DEM)
plot(tal_ws, add = T)

#this all worked, so run the entire WBT series again to try making streams off of the smaller DEM
writeRaster(cropped_DEM, paste0(temp_dir, "cropped_dem_tal.tif"), overwrite=T)
#st_write(outlet, paste0(temp_dir, "outlet_tal.shp"), delete_layer = T)
#didn't change

wbt_fill_single_cell_pits(
  dem = "cropped_dem_tal.tif",
  output = "cropped_dem_tal_fill.tif",
  wd = temp_dir)

wbt_breach_depressions(
  dem = "cropped_dem_tal_fill.tif",
  output = "cropped_dem_tal_breach.tif",
  wd = temp_dir)

wbt_d8_pointer(
  dem = "cropped_dem_tal_breach.tif",
  output = "cropped_flowdir_tal.tif",
  wd = temp_dir)

wbt_d8_flow_accumulation(
  input = "cropped_dem_tal_breach.tif",
  output = "cropped_flowaccum_tal.tif",
  wd = temp_dir
)

streams <- raster(paste0(temp_dir, "cropped_flowaccum_tal.tif"))
streams[streams<50000] <- NA #THIS VALUE IS SOMETHING YOU PLAY AROUND WITH; there's no one answer
writeRaster(streams, paste0(temp_dir, "cropped_streams_tal.tif"), overwrite=T)

#now use the new WBT functions to extract the streams!
wbt_extract_streams(
  flow_accum = "cropped_flowaccum_tal.tif",
  output = "streams_tal.tif",
  threshold = 50000,
  wd = temp_dir
)

wbt_raster_streams_to_vector(
  streams = "streams_tal.tif",
  d8_pntr = "cropped_flowdir_tal.tif",
  output = "streams_tal.shp",
  wd = temp_dir
)

#input into R
streams <- st_read(paste0(temp_dir, "streams_tal.shp"))
st_crs(streams) <- st_crs(DEM)
#crop to the watershed
streams <- streams[tal_ws,]

mapview(cropped_DEM) + mapview(tal_ws) + mapview(streams)

#export all of these so we have them!
st_write(tal_ws, paste0(data_dir, "/20211115_spatial_files/Talladega/watershed.shp"), delete_layer = T)
st_write(streams, paste0(data_dir, "/20211115_spatial_files/Talladega/stream_network.shp"), delete_layer = T)
writeRaster(cropped_DEM, paste0(data_dir, "/20211115_spatial_files/Talladega/croppedDEM.tif"), overwrite=T)


#TO GET THE AREA OF YOUR WATERSHED POLYGONS it has to be in sf format

st_area(tal_ws)
#spits out 925712 m^2 which is 92 hectares WHICH IS RIGHT

#shout out to Michelle for saving our lives hehe

#--------

#EVERYTHING FROM HERE IS FOR MAPPING -- if you want it, it's yours, but this is definitely
#not the best way to do all of this; it's just what worked for me this first try

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: prep sensor data and add to map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 edit data for better analysis
#2.2 convert to spatial data
#0.3 map it

#2.1 -----
#separate into stic, sensor, and ltm points
tal_sensors <- tal_sensors %>% 
  mutate(ID = NA,
         Type = NA) %>% 
  separate("SiteID", into = c("ID", "Type"), sep = "_")

tal_stics <- tal_sensors %>% 
  dplyr::filter(Type == "STIC") %>% 
  dplyr::select(ID, Long, Lat)
tal_ltms <- tal_sensors %>% 
  dplyr::filter(Type == "LTM") %>% 
  dplyr::select(ID, Long, Lat)

#convert to sf
tal_stics <- st_as_sf(tal_stics, coords = c("Long", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')
tal_stics <- st_transform(tal_stics, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()

tal_ltms <- st_as_sf(tal_ltms, coords = c("Long", "Lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')
tal_ltms <- st_transform(tal_ltms, crs = '+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs') %>% 
  st_geometry()

mapview(cropped_DEM) + mapview(streams, color = "blue") +
  mapview(tal_ws) + 
  mapview(tal_stics, color = "lightblue", col.regions = "lightblue") +
  mapview(tal_ltms, color = "white", col.regions = "indianred") +
  mapview(tal_sensor)


#now map
dem <- raster(paste0(data_dir,"/TalladegaDEMfiles_usda/elevation_lidar01m33085g5.tif"))
extent_ws <- extent(628600, 630600, 3735800, 3738000) #from the cropped dem
crop_ws <- raster::crop(dem, extent_ws)


bgc <- tm_shape(crop_ws) +
  tm_raster(col = "white") +
  tm_shape(tal_ws) +
  tm_polygons(col = "gray90", border.col = "black", lwd = 3) +
  #add the stream network shapefile and its aesthetics as well
  tm_shape(streams) +
  tm_lines(col = "darkblue", lwd = 2.5) +
  tm_shape(tal_stics) +
  tm_dots(size = 1.5, shape = 21, col = "steelblue2", border.col = "black") +
  tm_shape(tal_ltms) +
  tm_dots(size = 1.5, shape = 21, col = "indianred3", border.col = "black") +
  tm_shape(outlet) +
  tm_dots(size = 1.5, shape = 23, col = "gold1", border.col = "black") +
  tm_add_legend(title = "Sensor Type",
                type = "symbol", 
                col = c("steelblue2", "indianred3", "gold1"),
                border.col = c("black", "black", "black"),
                labels = c("STIC", "LTM", "Supersensor")) + 
  tm_layout(legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.frame = TRUE,
            legend.position = c(0.63, 0.65),
            legend.bg.color = "white",
            legend.bg.alpha = 0.25,
            legend.text.size = 1.75,
            title = "Talladega Sensor Locations",
            title.size = 3,
            title.fontface = "bold") +
  #add compass and scale for spatial context 
  tm_compass(text.size = 2) +
  tm_scale_bar(text.size = 1.5)

dem <- tm_shape(crop_ws) +
  tm_raster(palette = "-Greys", n = 5, alpha = 0.75, title = "Elevation [m]", legend.reverse = TRUE) +
  tm_shape(tal_ws) +
  tm_borders(col = "black", lwd = 4) +
  #add the stream network shapefile and its aesthetics as well
  tm_shape(streams) +
  tm_lines(col = "darkblue", lwd = 3) +
  tm_shape(tal_stics) +
  tm_dots(size = 1.5, shape = 21, col = "#b2abd2", border.col = "black") +
  tm_shape(tal_ltms) +
  tm_dots(size = 1.5, shape = 21, col = "#fdb863", border.col = "black") +
  tm_shape(outlet) +
  tm_dots(size = 1.5, shape = 23, col = "#ffffbf", border.col = "black") +
  tm_add_legend(title = "Sensor Type",
                type = "symbol", 
                col = c("#b2abd2", "#fdb863", "#ffffbf"),
                border.col = c("black", "black", "black"),
                labels = c("STIC", "LTM", "Supersensor")) + 
  tm_layout(legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.frame = TRUE,
            legend.position = c(0.77, 0.3),
            legend.bg.color = "white",
            legend.bg.alpha = 0.75,
            legend.text.size = 1.75,
            title = "Talladega Sensor Locations",
            title.size = 3,
            title.fontface = "bold") +
  #add compass and scale for spatial context 
  tm_compass(text.size = 2) +
  tm_scale_bar(text.size = 1.5)

masked_dem <- mask(crop_ws, tal_ws)
dem_greymask <- tm_shape(masked_dem) +
  tm_raster(palette = "-Greys", n = 8, alpha = 0.75, title = "Elevation [m]", legend.reverse = TRUE) +
  tm_shape(tal_ws) +
  tm_borders(col = "black", lwd = 4) +
  #add the stream network shapefile and its aesthetics as well
  tm_shape(streams) +
  tm_lines(col = "darkblue", lwd = 3) +
  tm_shape(tal_stics) +
  tm_dots(size = 1.5, shape = 21, col = "gold2", border.col = "black") +
  tm_shape(tal_ltms) +
  tm_dots(size = 1.5, shape = 21, col = "purple1", border.col = "black") +
  tm_shape(outlet) +
  tm_dots(size = 1.5, shape = 23, col = "springgreen3", border.col = "black") +
  tm_add_legend(title = "Sensor Type",
                type = "symbol", 
                col = c("gold2", "purple1", "springgreen3"),
                border.col = c("black", "black", "black"),
                labels = c("STIC", "LTM", "Supersensor")) + 
  tm_layout(legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.frame = TRUE,
            legend.position = c(0.77, 0.3),
            legend.bg.color = "white",
            legend.bg.alpha = 0.75,
            legend.text.size = 1.75,
            title = "Talladega Sensor Locations",
            title.size = 3,
            title.fontface = "bold") +
  #add compass and scale for spatial context 
  tm_compass(text.size = 2) +
  tm_scale_bar(text.size = 1.5)


iso <- rasterToContour(masked_dem)

dem_contour <- tm_shape(crop_ws) +
  tm_raster(col = "white") +
  tm_shape(iso) +
  tm_lines(col = "darkgrey")+
  tm_shape(tal_ws) +
  tm_borders(col = "black", lwd = 4) +
  #add the stream network shapefile and its aesthetics as well
  tm_shape(streams) +
  tm_lines(col = "darkblue", lwd = 3) +
  tm_shape(tal_stics) +
  tm_dots(size = 1.5, shape = 21, col = "gold2", border.col = "black") +
  tm_text(tal_stics$ID)
tm_shape(tal_ltms) +
  tm_dots(size = 1.5, shape = 21, col = "purple1", border.col = "black") +
  tm_shape(outlet) +
  tm_dots(size = 1.5, shape = 23, col = "springgreen3", border.col = "black") +
  tm_add_legend(title = "Sensor Type",
                type = "symbol", 
                col = c("gold2", "purple1", "springgreen3"),
                border.col = c("black", "black", "black"),
                labels = c("STIC", "LTM", "Supersensor")) + 
  tm_layout(legend.title.fontface = "bold",
            legend.title.size = 2,
            legend.frame = TRUE,
            legend.position = c(0.69, 0.6),
            legend.bg.color = "white",
            legend.bg.alpha = 0.75,
            legend.text.size = 1.75,
            title = "Talladega Sensor Locations",
            title.size = 3,
            title.fontface = "bold") +
  #add compass and scale for spatial context 
  tm_compass(text.size = 2) +
  tm_scale_bar(text.size = 1.5)



















