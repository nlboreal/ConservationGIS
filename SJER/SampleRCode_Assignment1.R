### SPATIAL ANALYSIS IN R FOR BIOL 4651/7794 CONSERVATION IN PRACTICE ###

##code to complete assignment (based on https://datacarpentry.org/semester-biology/assignments/r-spatial-1/)

library(stars)
library(sf)
library(ggplot2)
library(dplyr)

setwd("C:/Users/ywiersma/Documents/BIOL4651/GIS_R/neon-geospatial-data/SJER")

#map the digital terrain model for SJER

dtm_sjer <- read_stars("sjer_dtmCrop.tif")  #reads in the file. Type object name to get metadata

#to make map of the digital terrain model with default aesthetic
ggplot() + geom_stars(data = dtm_sjer)

#to make map with viridis

ggplot() +
  geom_stars(data = dtm_sjer) + 
  scale_fill_viridis_c()


#to make map of canopy height with viridis colour ramp by substracting dtm values from digital surface model

dsm_sjer <- read_stars("SJER_dsmCrop.tif")

canopy_sjer = dsm_sjer - dtm_sjer

ggplot() +
  geom_stars(data = canopy_sjer) + 
  scale_fill_viridis_c()

#map boundary and plot locations, and colour plot locations by type

boundary_sjer <- st_read("sjer_boundary.shp")
plots_sjer <- st_read("sjer_plots.shp")
ggplot() +
  geom_sf(data = boundary_sjer) + 
  geom_sf(data = plots_sjer, mapping = aes(color = plot_type))


#transform the plot data to have the same CRS as the CHM and so vector data on top of raster

#to see what it looks like without transforming
ggplot() +
  geom_stars(data = canopy_sjer) + 
  geom_sf(data = boundary_sjer) + 
  geom_sf(data = plots_sjer, mapping = aes(color = plot_type))

#to look up coordinate reference system (CRS) 

st_crs(canopy_sjer)  #it's UTM Zone 11N
st_crs(plots_sjer)


#to transform projections to match plot data TO the CRS for the canopy height model

plots_sjr_utm <- st_transform(plots_sjer, st_crs(canopy_sjer))

#canopy_lat_lon <- st_transform(canopy_sjer, 4326)  #this would transform canopy to projection type 4326 which is WGS (lat/lon), matches plots
#plots_sjer_utm <- st_transform(plots_sjer, st_crs(dtm_sjer)) #this transforms plots to match CRS of dtm (which is in UTM)

ggplot() +
  geom_stars(data = canopy_sjer) + 
  geom_sf(data = plots_sjer_utm, mapping = aes(color = plot_type))



#Extract the mean elevation at the plot points and display values

plot_canopy_height <- aggregate(canopy_sjer, plots_sjer_utm, mean, as_points = FALSE)
plot_canopy_height$SJER_dSCrop.tif

mutate(plots_sjer_utm, canopy = plot_canopy_height$SJER_dsmCrop.tif)

#plot canopy with boundary and plot locations (NB, need to project bondary first)

boundary_sjer_utm <- st_transform(boundary_sjer, st_crs(canopy_sjer))

ggplot() +
  geom_stars(data = canopy_sjer) + 
  geom_sf(data = boundary_sjer_utm, alpha = 0.5) +
  geom_sf(data = plots_sjer_utm, mapping = aes(color = plot_type))


#note alpha = 0.5 sets the transparency of the boundary so you can see the raster underneath

#or re-order it:

ggplot() +
  geom_sf(data = boundary_sjer_utm) +
  geom_stars(data = canopy_sjer) + 
  geom_sf(data = plots_sjer_utm, mapping = aes(color = plot_type))

#canopy height in cm
canopy_sjer_cm = canopy_sjer * 100

ggplot() +
  geom_stars(data = canopy_sjer_cm)
 

#create map of digital terrain raster, plot locatoins, and
ggplot() +
  geom_sf(data = boundary_sjer_utm) +
  geom_stars(data = dtm_sjer) + 
  geom_sf(data = plots_sjer_utm, mapping = aes(color = plot_type))


#conduct an analysis of relationship between elevation and canopy height at the plots (make the grad students do this)

plot_elevations <- aggregate(dtm_sjer, plots_sjer_utm, mean, as_points = FALSE)
plot_elevations$sjer_dtmCrop.tif

mutate(plots_sjer_utm, elevation = plot_elevations$sjer_dtmCrop.tif, canopy = plot_canopy_height$SJER_dsmCrop.tif)

plots_for_analysis <- mutate(plots_sjer_utm, elevation = plot_elevations$sjer_dtmCrop.tif, canopy = plot_canopy_height$SJER_dsmCrop.tif)


plot(plots_for_analysis$elevation, plots_for_analysis$canopy, xlab = "elevation (m)", ylab = "canopy height (m)")

#modify above to colour code points by plot type (tower vs. distributed) and fit leanear model through all points. Then us dplyr to acluate average canopy height and elevation for two plot types


#do with ggplot

ggplot(data = plots_for_analysis, mapping = aes(x = elevation, y = canopy)) +
  geom_point(aes(colour = factor(plot_type))) +
  labs(x = "elevation (m)", y = "canopy (m)") + 
  geom_smooth(method = "lm")

 #use dplyr to calculate average canopy height and average elevatin for the two plot types

plot_summary_byType <- (group_by(plots_for_analysis, plot_type))
summarize(plot_summary_byType, average_canopy_height = mean(canopy))
summarize(plot_summary_byType, average_elevation = mean(elevation))

################################### Try with Harvard set#############################################

setwd("C:/Users/ywiersma/Documents/BIOL4651/GIS_R/neon-geospatial-data/HARV")

#extrat raster data with polygon


dtm_harv <- read_stars("HARV_dtmCrop.tif")
plots_harv <- st_read("harv_plots.shp")

ggplot() +
    geom_stars(data = dtm_harv)

ggplot() +
  geom_sf(data = plots_harv)  #visualize the above two to see that they have different coordinate system

#look at CRF
st_crs(dtm_harv) #it's UTM Zone 18N
st_crs(plots_harv) #it's WGS 1984

#to project

#to convert the raster data from UTM to lat-lon:
dtm_harv_lat_lon <- st_transform(dtm_harv, 4326)

#test plot
ggplot() +
  geom_stars(data = dtm_harv_lat_lon) + 
  geom_sf(data = plots_harv)

plots_harv_utm <- st_transform(plots_harv, st_crs(dtm_harv))


#easier way to do, is match all objects to one objects' coordinate system
#do it to match plots to raster

plots_harv_utm <- st_transform(plots_harv, st_crs(dtm_harv)) #tells it to look up CRS for raster and transform vector to match it. 

ggplot() +
  geom_stars(data = dtm_harv) +
  geom_sf(data = plots_harv_utm)

