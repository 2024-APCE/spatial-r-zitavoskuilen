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
grey.colors(7)
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

sf::st_layers("./2022_roads/gsme_roads.gpkg")
roads <-terra::vect("./2022_roads/gsme_roads.gpkg",
                    layer="gsme_roads")
           
buildings <- terra::vect("./studyarea/drive-download-20241113T080356Z-001/buildings_study_area.shp")

plot(buildings)



# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
plot(protected_areas)
plot(roads)
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
                             fill = NA, linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers, col = "blue", linewidth = 0.1) +
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
                             fill = NA, linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers, 
                             col = "blue", linewidth = 0.1) +
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
                             fill = NA, linewidth = 0.3) +
  tidyterra::geom_spatvector(data = rivers, 
                             col = "blue", linewidth = 0.1) +
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

composite_map_study_area <-elevation_map+ woody_map + rainfall_map + plot_layout(nrow = 1)
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
        axis.ticks = element_blank()) 

# ---- end of woody biomass map ----
    
# preview of map                      
woody_map_sa

# create 500 random points in your study area
set.seed(123)
npoints <- 250

# and add them to the previous map
studyarea_sf <- sf::st_as_sf(studyarea)

# Set a seed for reproducibility and specify the number of points
set.seed(123)
npoints <- 250

# Generate 500 random points within 'studyarea'
random_points <- sf::st_sample(studyarea_sf, size = npoints)

# Add random points to your existing map
woody_map_random_point <- woody_map_sa +
  geom_sf(data = random_points, color = "red", size = 0.5) +
  labs(title = "Woody Map with Random Sample Points") +
  coord_sf(xlimits, ylimits, expand = F)

plot(woody_map_random_point)

ggsave("./figures/woody_map_random_point.png", woody_map_random_point, width = 20, height = 20, units = "cm", dpi = 300)  

#########################
### making more maps with data relevant for predicting woody biomass
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

rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_sa<-terra::crop(rainfall_30m,saExt) # crop to study area
rainfall_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits= c(364,2450),,
                       oob=squish,
                       name="mm/yr") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Rainfall") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rainfall_map_sa  

## my old version (not with 30 m grid )
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
  scale_fill_gradientn(colours=topo.colors(6),
                       limits=c(0,12000),
                       oob=squish,
                       name="Meters") + 
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
  scale_fill_gradientn(colours = (pal_zissou2),
                       limits = c(0, 23),  # Adjust limits as appropriate for your data
                       oob = squish, 
                       name = "n years burned") + 
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

# ---- rainfall wet season ----
rainfall_wet_season <- terra::rast("./rainfall/ChirpsAnnualRainfall2001_2020_wet_season.tif")

rainfall_wet_season_map <- ggplot() + 
  tidyterra::geom_spatraster(data = rainfall_wet_season) +
  scale_fill_gradientn(colors = rev(viridis::viridis(10)),
                       limits= c(738,1055),
                       oob = squish, 
                       name = "mm/year") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, 
                             col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, 
                             linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = F, datum = sf::st_crs(32736)) + # set the limits of the map)
  labs(title = "Rainfall wet season") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) # add a scale bar

# ---- end of rainfall wet season map ----

# preview of the map
rainfall_wet_season_map

# ---- rainfall dry season ----
rainfall_dry_season <- terra::rast("./rainfall/ChirpsAnnualRainfall2001_2020_dry_season.tif")

rainfall_dry_season_map <- ggplot() + 
  tidyterra::geom_spatraster(data = rainfall_dry_season) +
  scale_fill_gradientn(colors = rev(viridis::viridis(10)),
                       limits= c(481, 660),
                       oob = squish, 
                       name = "mm/year") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, 
                             col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, 
                             linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = F, datum = sf::st_crs(32736)) + # set the limits of the map)
  labs(title = "Rainfall dry season") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2) # add a scale bar

# ---- end of rainfall dry season map ----

# preview of the map
rainfall_dry_season_map

# ---- distance to buildings ----
distance_to_buildings <- terra::rast("./2022_towns/DistanceToBuilding_01.tif")

distance_to_buildings_map <- ggplot() + 
  tidyterra::geom_spatraster(data = distance_to_buildings) +
  scale_fill_gradientn(colors = rev(pal_zissou1),
                       limits = c(0, 18000),  # Adjust limits as appropriate for your data
                       oob = squish, 
                       name = "Distance to buildings (m)") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = FALSE,
           datum = sf::st_crs(32736)) + 
  labs(title = "Distance to buildings") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# ---- End of Distance to Buildings Map ----

# Preview the map
distance_to_buildings_map


# ---- distance to cropland ----
distance_to_cropland <- terra::rast("./DistanceToCropland (1).tif")

distance_to_cropland_map <- ggplot() + 
  tidyterra::geom_spatraster(data = distance_to_cropland) +
  scale_fill_gradientn(colors = rev(pal_zissou1),
                       limits = c(0, 1600),  # Adjust limits as appropriate for your data
                       oob = squish, 
                       name = "Distance to cropland (m)") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = FALSE,
           datum = sf::st_crs(32736)) + 
  labs(title = "Distance to cropland") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# ---- End of Distance to Cropland Map ----

# preview of the map
distance_to_cropland_map


# ---- landform map ----
landform_sa<-terra::rast("./landform/hills.tif")
landform_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("valleys\nand\nplains","hills" )) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="green") +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Landform") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

# ---- End of landform map ---- 

# preview of the map
landform_map_sa

# ---- core protected ares map ----

r<-terra::rast("./2022_protected_areas/CoreProtectedAreas.tif") 
CoreProtectedAreas_sa <- r |> #  replace NA by 0
  is.na() |>
  terra::ifel(0,r) 
  
CoreProtectedAreas_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(CoreProtectedAreas_sa)) +
  scale_fill_manual(values=c("grey","lightgreen"),
                    labels=c("no","yes")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Core protected areas") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)

# ---- end of core protected areas map ----

# preview of the map
CoreProtectedAreas_map_sa

# ---- Slope map ----
slope_sa <- terra::rast("./Slope_Map.tif")

slope_map_sa <- ggplot() + 
  tidyterra::geom_spatraster(data = slope_sa) +
  scale_fill_gradientn(colors = rev(viridis::viridis(10)),
                       limits = c(0, 30),  # Adjust limits as appropriate for your data
                       oob = squish, 
                       name = "Slope (degrees)") + 
  tidyterra::geom_spatvector(data = protected_areas, 
                             fill = NA, linewidth = 0.5) +
  tidyterra::geom_spatvector(data = rivers, 
                             col = "blue", linewidth = 0.5) +
  tidyterra::geom_spatvector(data = studyarea, 
                             linewidth = 0.8, fill = NA, col = "red") +
  coord_sf(xlimits, ylimits, expand = FALSE,
           datum = sf::st_crs(32736)) + 
  labs(title = "Slope") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2)

# ---- end of slope map ----

# preview of map 
slope_map_sa


### put all maps together
composite_map_my_study_area <-woody_map_sa + distance_to_river_map_sa + rainfall_map_sa + elevation_map_sa + burn_frequency_map + CEC_map + NDVI_map + slope_map_sa + rainfall_dry_season_map + rainfall_wet_season_map + distance_to_buildings_map + CoreProtectedAreas_map_sa + distance_to_cropland_map + landform_map_sa + plot_layout(ncol = 3)
composite_map_my_study_area

ggsave("./figures/composite_map_my_study_area.png", width = 20, height = 20, units = "cm", dpi = 300)


##### end of map making 

# create 250 random points
set.seed(123)
rpoints <- terra::spatSample(studyarea, size = 250, 
                             method = "random")
rpoints_map_sa<-ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="250 random points") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rpoints_map_sa


# add this map to the patchwork 

all_maps <- woody_map_sa + distance_to_river_map_sa + rainfall_map_sa + elevation_map_sa + burn_frequency_map + CEC_map + NDVI_map + rainfall_dry_season_map + rainfall_wet_season_map + distance_to_buildings_map + distance_to_cropland_map + landform_map_sa + CoreProtectedAreas_map_sa + slope_map_sa + rpoints_map_sa + plot_layout(nrow = 5 )

all_maps


ggsave("./figures/all_maps_sa.png", width = 297, height = 210, units = "mm",dpi=300)

# extract your the values of the different raster layers to the points
woody_points <- terra::extract(woodybiom_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(woody=TBA_gam_utm36s)

woody_points

dist2river_points <- terra::extract(distance_to_river, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2river=distance)

dist2river_points

elevation_points <- terra::extract(elevation, rpoints) |> 
  as_tibble() 

elevation_points

CorProtAr_points <- terra::extract(CoreProtectedAreas_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(CorProtAr=CoreProtectedAreas)
CorProtAr_points

rainfall_points <- terra::extract(rainfall_sa, rpoints) |> 
  as_tibble() |> 
  dplyr::rename(rainfall=CHIRPS_MeanAnnualRainfall)
rainfall_points

cec_points <- terra::extract(CEC, rpoints) |> 
  as_tibble() |>
  dplyr::rename(cec='cec_5-15cm_mean')
cec_points

burnfreq_points <- terra::extract(burn_frequency, rpoints) |> 
  as_tibble() |>
  dplyr::rename(burnfreq=burned_sum)
burnfreq_points

landform_points <- terra::extract(landform_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(hills=remapped)
landform_points

distance_to_buildings_points <- terra::extract(distance_to_buildings, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2buildings=distance)

disatnce_to_cropland_points <- terra::extract(distance_to_cropland, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2cropland=distance)

NDVI_points <- terra::extract(NDVI$NDVI, rpoints) |> 
  as_tibble() 

slope_points <- terra::extract(slope_sa, rpoints) |> 
  as_tibble()

slope_points

# merge the different variable into a single table
# use woody biomass as the last variable
pointdata<-cbind(dist2river_points[,2],elevation_points[,2],
                 rainfall_points[,2], CorProtAr_points[,2],
                 cec_points[,2],burnfreq_points[,2],
                 landform_points[,2],woody_points[,2], slope_points[,2],
                 disatnce_to_cropland_points[,2],NDVI_points[,2],
                 distance_to_buildings_points[,2])|>
  as_tibble()

pointdata

# made a csv file from all points to use in google sheeets 
getwd()
readr::write_csv(pointdata,"pointdata.csv")


# plot how woody cover is predicted by different variables
# Create a correlation panel plot
library(psych)
correlation_panel_plot <- psych::pairs.panels(
  pointdata ,
  method = "pearson",     # Correlation method (use "spearman" for rank correlation)
  hist.col = "lightblue",  # Color for histograms
  density = TRUE,          # Add density plots
  ellipses = F,         # Add correlation ellipses
  lm = TRUE,                # Add linear regression lines
  stars=T
)

# export the panels plot
ggsave("./figures/correlation_panel_plot.png", width = 20, height = 20, units = "cm", dpi = 300)

# make long format
names(pointdata)
pointdata_long<-pivot_longer(data=pointdata,
                             cols = dist2river:hills, # all except woody
                             names_to ="pred_var",
                             values_to = "pred_val")
pointdata_long

# panel plot
ggplot(data=pointdata_long, mapping=aes(x=pred_val,y=woody,group=pred_var)) +
  geom_point() +
  geom_smooth() +
  ylim(0,40) +
  facet_wrap(~pred_var,scales="free") 

# do a pca
# Load the vegan package
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::rda(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)

# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", pch=pointdata$CorProtAr+1, col = pointdata$hills+1, bg = "blue", cex = 1)
# Add arrows for variables
arrows(0, 0, scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
       length = 0.1, col = "red")
# Label the variables with arrows
text(scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
     labels = colnames(pointdata), col = "red", cex = 0.8, pos = 4)
# Add axis labels and a title
title(main = "PCA Biplot")
xlabel <- paste("PC1 (", round(pca_result$CA$eig[1] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(pca_result$CA$eig[2] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
title(xlab=xlabel)
title(ylab=ylabel)
# add contours for woody cover
vegan::ordisurf(pca_result, pointdata$woody, add = TRUE, col = "green4")
  

