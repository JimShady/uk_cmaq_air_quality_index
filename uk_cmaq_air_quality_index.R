rm(list=ls())

### Loading libraries

library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(sf)
library(rmarkdown)

## Copy maps over from cluster2
system('sshpass -f "../cluster2password.txt" scp james@10.0.4.225:/mnt/modelling2/UKmaps20m/* .')

latlong                   <- "+init=epsg:4326"
ukgrid                    <- "+init=epsg:27700"
google                    <- "+init=epsg:3857"

## read in the raster files and UK shapefile
pm25                      <- raster('Annual_CMAQUrban_PM25_2012.grd')
no2                       <- raster('Annual_CMAQUrban_NO2_2012.grd')
eng_scot_wales            <- st_read('https://raw.githubusercontent.com/JimShady/useful_geography/master/eng_scot_wales.geojson')

## Convert to same crs as the raster files. Just use pm25 one, could have used no2 instead.
eng_scot_wales            <- st_transform(eng_scot_wales, proj4string(pm25))

## Extract pm2.5 and no2 concentrations by polygon
eng_scot_wales$mean_pm25  <- extract(pm25, eng_scot_wales, fun=mean, method='simple')
eng_scot_wales$mean_no2   <- extract(no2,  eng_scot_wales, fun=mean, method='simple')

## Get countrywide means
country_mean_pm25         <- mean(eng_scot_wales$mean_pm25)
country_mean_no2          <- mean(eng_scot_wales$mean_no2)

## Index value (Add new layer to the raster, turn into stack)
pm25                      <- stack(pm25, pm25/country_mean_pm25)
no2                       <- stack(no2, no2/country_mean_no2)

names(pm25)               <- c('pm25', 'relative_pm25')
names(no2)                <- c('no2', 'relative_no2')

uk_air_quality_index      <- stack(pm25, no2)
uk_air_quality_index      <- uk_air_quality_index[[c(2,4)]]

## Crop to areas of concern
example_areas             <- as(eng_scot_wales[eng_scot_wales$name %in% c('Leicester', 'Southwark'),], 'Spatial')
uk_air_quality_index      <- crop(uk_air_quality_index, example_areas)
uk_air_quality_index      <- mask(uk_air_quality_index, example_areas)

writeRaster(uk_air_quality_index, filename="uk_air_quality_index.grd", overwrite=TRUE)






                               