# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
setwd("C:/Users/Zita/Documents/Ecology &  Conservation/APCE2024/QGIS")


# restore the libraries of the project 
renv::restore()


# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))
grey.colors()
mycolors <- c("red", "white", "blue" )
mycolors
barplot(rep(1,30), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10))) # rev means reverse the palette 
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = rev(viridis::viridis(10))) # rainfall
barplot(rep(1,10), col = viridis::plasma(10)) # heat
viridis::plasma(10)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem, you take different layers from the geopackage 
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)
protected_areas
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                              layer="my_study_area")


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
plot(protected_areas)
plot(elevation)
plot(protected_areas, add = T)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
#xlimits<-c(550000,900000)
#ylimits<-c(9600000,9950000)

# set the limits of my own area 
xlimits <- c(720000,770000)
ylimits <- c(9760000,9790000)


# plot the woody biomass map that you want to predict
woody_map <- ggplot() + 
 tidyterra::geom_spatraster(data = woodybiom) +
  scale_fill_gradientn(colours = rev(terrain.colors(6)),
                       limits= c(0.77,6.55),
                       oob = squish, 
                       name = "Tree basal area/ha") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = lakes, fill = "lightblue") +
  tidyterra::geom_spatvector(data = studyarea, linewidth = 0.8, fill = NA, col = "red") +
   # title of the plot
  coord_sf(xlimits, ylimits, datum = sf::st_crs(32736)) + # set the limits of the map)
  labs(title = "Woody biomass") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) # add a scale bar
                         
woody_map

# plot the rainfall map
rainfall_map <- ggplot() + 
 tidyterra::geom_spatraster(data = rainfall) +
  scale_fill_gradientn(colours = pal_zissou1,,
                       limits= c(364,2450),
                       oob = squish, 
                       name = "mm/year") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, 
                             col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = lakes, 
                             fill = "lightblue") +
  tidyterra::geom_spatvector(data = studyarea, 
                             linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, datum = sf::st_crs(32736)) + # set the limits of the map)
  labs(title = "Rainfall") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) # add a scale bar

rainfall_map


# plot the elevation map
elevation_map <- ggplot() + 
 tidyterra::geom_spatraster(data = elevation) +
  scale_fill_gradientn(colours = terrain.colors(10),
                       limits= c(500,2100),
                       oob = squish, 
                       name = "Meters") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, 
                             col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = lakes, 
                             fill = "lightblue") +
  tidyterra::geom_spatvector(data = studyarea, 
                             linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, datum = sf::st_crs(32736)) + # set the limits of the map)
  labs(title = "Elevation") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) # add a scale bar

elevation_map

# combine the different maps  into one composite map using the patchwork library
# and save it to a high resolution png

composite_map_study_area <-woody_map + elevation_map + rainfall_map + plot_layout(ncol = 2)
composite_map_study_area

# save the map to a high resolution png in the plots folder


ggsave("./figures/composite_map_study_area.png", width = 20, height = 20, units = "cm", dpi = 300)

############################
### explore your study area


# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)

# ---- woody biomass map ---- 
woodybiom_sa <- terra::crop(woodybiom, saExt)

woody_map_sa <- ggplot() + 
 tidyterra::geom_spatraster(data = woodybiom_sa) +
  scale_fill_gradientn(colours = rev(terrain.colors(6)),
                       limits= c(0.77,6.55),
                       oob = squish, 
                       name = "Tree basal area/ha") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = lakes, fill = "lightblue") +
  tidyterra::geom_spatvector(data = studyarea, linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = F,
           datum = sf::st_crs(32736)) + # set the limits of the map, expand = F so is is exactly the size of the study area
  labs(title = "Woody biomass") + # title of the plot 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) # add a scale bar

# ---- end of woody biomass map ----
    
# preview of map                      
woody_map_sa

# create 500 random points in your study area
set.seed(123)
npoints <- 500

# and add them to the previous map
studyarea_sf <- sf::st_as_sf(studyarea)

# Set a seed for reproducibility and specify the number of points
set.seed(123)
npoints <- 500

# Generate 500 random points within 'studyarea'
random_points <- sf::st_sample(studyarea_sf, size = npoints)

# Add random points to your existing map
woody_map_random_point <- woody_map_sa +
  geom_sf(data = random_points, color = "red", size = 1.5) +
  labs(title = "Woody Map with Random Sample Points") +
  coord_sf(xlimits, ylimits, expand = F,
           datum = sf::st_crs(32736))

ggsave("./figures/woody_map_random_point.png", woody_map_random_point, width = 20, height = 20, units = "cm", dpi = 300)  

#########################
### mkaing more maps with data relevant for predicting woody biomass
#########################

# ---- Elevation map ----
elevation_sa <- terra::crop(elevation, saExt)

elevation_map_sa <- ggplot() + 
 tidyterra::geom_spatraster(data = elevation_sa) +
  scale_fill_gradientn(colors = c("#7f9f7f", "#c2b280", "#b03a2e", "#2980b9", "#ffffff"),
                       limits= c(500,2100),
                       oob = squish, 
                       name = "Meters") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, 
                             col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = lakes, 
                             fill = "lightblue") +
  tidyterra::geom_spatvector(data = studyarea, 
                             linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = F, datum = sf::st_crs(32736)) + # set the limits of the map)
  labs(title = "Elevation") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) # add a scale bar

# end of elevation map ----

# preview of the map 
elevation_map_sa


# ---- Rainfall map ----
rainfall_sa <- terra::crop(rainfall, saExt)


rainfall_map_sa <- ggplot() + 
 tidyterra::geom_spatraster(data = rainfall_sa) +
  scale_fill_gradientn(colors = rev(viridis::viridis(10)),
                       limits= c(364,2450),
                       oob = squish, 
                       name = "mm/year") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, 
                             col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = lakes, 
                             fill = "lightblue") +
  tidyterra::geom_spatvector(data = studyarea, 
                             linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = F, datum = sf::st_crs(32736)) + # set the limits of the map)
  labs(title = "Rainfall") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) # add a scale bar

# ---- end of rainfall map ----

# preview of the map 
rainfall_map_sa


# ----  Distance to River Map ----

distance_to_river <- terra::rast("./2022_rivers/DistanceToRiver.tif")
distance_to_river_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data = distance_to_river) +
  scale_fill_gradientn(colours = rev(pal_zissou2),
                       limits = c(0, 16238),
                       oob = squish, 
                       name = "Distance to river (m)") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = FALSE,
           datum = sf::st_crs(32736)) + 
  labs(title = "Distance to river") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# ---- End of Distance to River Map ----

# Preview the map
distance_to_river_map_sa

# ---- Burn Frequency Map ----

burn_frequency <- terra::rast("./fires/BurnFreq.tif")
burn_frequency_map <- ggplot() + 
  tidyterra::geom_spatraster(data = burn_frequency) +
  scale_fill_gradientn(colours = rev(pal_zissou2),
                       limits = c(0, 23),  # Adjust limits as appropriate for your data
                       oob = squish, 
                       name = "Burn Frequency") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = FALSE,
           datum = sf::st_crs(32736)) + 
  labs(title = "Burn Frequency") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# ---- End of Burn Frequency Map ----

# Preview the map
burn_frequency_map

# ---- CEC map ----

CEC <- terra::rast("./CEC_5_15cm.tif")
CEC_map <- ggplot() + 
  tidyterra::geom_spatraster(data = CEC) +
  scale_fill_gradientn(colors = c("#8B4513", "#F0E68C", "#228B22"),
                       limits = c(156, 357),  # Adjust limits as appropriate for your data
                       oob = squish, 
                       name = "CEC (soil fertility)") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = FALSE,
           datum = sf::st_crs(32736)) + 
  labs(title = "CEC") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# --- end of CEC map ----

# preview of map 
CEC_map


# --- NDVI map ----

NDVI <- terra::rast("./NDVI.tif")

NDVI_map <- ggplot() + 
  tidyterra::geom_spatraster(data = NDVI, aes(fill = NDVI)) +
  scale_fill_gradientn(colors = c("#A8E6A1", "#4CAF50", "#006400"),
                       limits = c(0.09, 0.47),  # Adjust limits as appropriate for your data
                       oob = squish, 
                       name = "NDVI") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = FALSE,
           datum = sf::st_crs(32736)) + 
  labs(title = "NDVI") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# --- end of NDVI map ----  

# preview of map 
NDVI_map





### put all maps together

composite_map_my_study_area <-woody_map_sa + distance_to_river_map_sa + rainfall_map_sa + elevation_map_sa + burn_frequency_map + CEC_map + NDVI_map + plot_layout(ncol = 2)
composite_map_my_study_area

ggsave("./figures/composite_map_my_study_area.png", width = 20, height = 20, units = "cm", dpi = 300)





# extract your the values of the different raster layers to the points

# make long format

# plot how woody cover is predicted by different variables


