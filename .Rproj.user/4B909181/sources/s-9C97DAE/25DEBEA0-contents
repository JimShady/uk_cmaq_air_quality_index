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
st_write(eng_scot_wales, 'eng_scot_wales_conc_averages.geojson')

## Our two examples
leicester                 <- as(eng_scot_wales[eng_scot_wales$name == 'Leicester',], 'Spatial')

westminster               <- as(eng_scot_wales[eng_scot_wales$name == 'Westminster',], 'Spatial')

pm25_leicester            <- crop(pm25, leicester)
pm25_leicester            <- mask(pm25_leicester, leicester)

pm25_westminster          <- crop(pm25, westminster) 
pm25_westminster          <- mask(pm25_westminster, westminster) 

no2_leicester             <- crop(no2, leicester)
no2_leicester             <- mask(no2_leicester, leicester)

no2_westminster           <- crop(no2, westminster) 
no2_westminster           <- mask(no2_westminster, westminster) 

## Get countrywide means
country_mean_pm25         <- mean(eng_scot_wales$mean_pm25)
country_mean_no2          <- mean(eng_scot_wales$mean_no2)

# Local and national scaled concentrations
pm25_leicester            <- stack(pm25_leicester, 
                                   pm25_leicester/eng_scot_wales[eng_scot_wales$name == 'Leicester',]$mean_pm25,
                                   pm25_leicester/country_mean_pm25)
pm25_westminster          <- stack(pm25_westminster, 
                                   pm25_westminster/eng_scot_wales[eng_scot_wales$name == 'Westminster',]$mean_pm25,
                                   pm25_westminster/country_mean_pm25)

no2_leicester             <- stack(no2_leicester, 
                                   no2_leicester/eng_scot_wales[eng_scot_wales$name == 'Leicester',]$mean_no2,
                                   no2_leicester/country_mean_no2)
no2_westminster           <- stack(no2_westminster, 
                                   no2_westminster/eng_scot_wales[eng_scot_wales$name == 'Westminster',]$mean_no2,
                                   no2_westminster/country_mean_no2)

names(pm25_leicester)     <- c('pm25', 'relative_local_pm25', 'relative_country_pm25')
names(pm25_westminster)   <- c('pm25', 'relative_local_pm25', 'relative_country_pm25')
names(no2_leicester)      <- c('no2', 'relative_local_no2', 'relative_country_no2')
names(no2_westminster)    <- c('no2', 'relative_local_no2', 'relative_country_no2')

# Get LAEI for comparison
concentrations_url    <- 'https://files.datapress.com/london/dataset/london-atmospheric-emissions-inventory-2013/2017-01-26T18:50:00/4.1.%20Concentrations%20LAEI%202013%20Update.zip'
temp = tempfile()
download.file(concentrations_url, temp, method = 'curl')
unzip(temp, exdir = ".")

no2_2013                      <- raster('4.1. Concentrations LAEI 2013 Update/2013/ASCII/PostLAEI2013_2013_NO2.asc')
proj4string(no2_2013)         <- CRS(ukgrid)

pm25_2013                      <- raster('4.1. Concentrations LAEI 2013 Update/2013/ASCII/PostLAEI2013_2013_PM25.asc')
proj4string(pm25_2013)         <- CRS(ukgrid)

london                        <- st_read('https://raw.githubusercontent.com/KCL-ERG/useful_geography/master/london_boroughs.geojson')
london                        <- as(london, 'Spatial')
london                        <- spTransform(london, ukgrid)
westminster                   <- london[london$NAME == 'Westminster',]
rm(london)

pm25_westminster_laei          <- crop(pm25_2013, westminster)
pm25_westminster_laei         <- mask(pm25_westminster_laei, westminster)
no2_westminster_laei          <- crop(no2_2013,  westminster)
no2_westminster_laei          <- mask(pm25_westminster_laei,  westminster)

## Make 1km versions of these rasters

no2_leicester_1km                 <- aggregate(no2_leicester,    fact = 50, method="bilinear")
pm25_leicester_1km                <- aggregate(pm25_leicester,   fact = 50, method="bilinear")

no2_westminster_1km               <- aggregate(no2_westminster,  fact = 50, method="bilinear")
pm25_westminster_1km              <- aggregate(pm25_westminster, fact = 50, method="bilinear")

## Make outputs

writeRaster(pm25_leicester,        filename="gis_results/pm25_leicester", overwrite=TRUE, format='EHdr')
writeRaster(pm25_westminster,      filename="gis_results/pm25_westminster", overwrite=TRUE, format='EHdr')

writeRaster(no2_leicester,         filename="gis_results/no2_leicester", overwrite=TRUE, format='EHdr')
writeRaster(no2_westminster,       filename="gis_results/no2_westminster", overwrite=TRUE, format='EHdr')

writeRaster(pm25_leicester_1km,    filename="gis_results/pm25_leicester_1km", overwrite=TRUE, format='EHdr')
writeRaster(pm25_westminster_1km,  filename="gis_results/pm25_westminster_1km", overwrite=TRUE, format='EHdr')

writeRaster(no2_leicester_1km,     filename="gis_results/no2_leicester_1km", overwrite=TRUE, format='EHdr')
writeRaster(no2_westminster_1km,   filename="gis_results/no2_westminster_1km", overwrite=TRUE, format='EHdr')

writeRaster(pm25_westminster_laei,   filename="gis_results/pm25_westminster_laei", overwrite=TRUE, format='EHdr')

## Make PNG outputs

#no2_colours
source('https://raw.githubusercontent.com/KCL-ERG/colour_schemes/master/no2_laei2013_colours_breaks.R')

no2_laei2013_breaks  <- c(cellStats(no2_leicester$no2, stat=min)-1,
                          format(round(quantile(no2_leicester$no2, seq(0,1,length.out = 15)),4), scientific=F),
                          cellStats(no2_leicester$no2, stat=min)+1) #17

no2_laei2013_labels  <- c("", paste(no2_laei2013_breaks[1:length(no2_laei2013_breaks)-1], "-", 
                                    no2_laei2013_breaks[2:length(no2_laei2013_breaks)])) #17 (same as breaks)

levelplot(no2_leicester$no2,
          maxpixels = no2_leicester$no2@ncols/2 * no2_leicester$no2@nrows/2,
          margin = FALSE,
          colorkey = list(
            at = seq(min(no2_laei2013_breaks), max(no2_laei2013_breaks), length = 17),
            space = 'right',
            labels = list(at=seq(min(no2_laei2013_breaks), max(no2_laei2013_breaks), length = 17), 
                          labels = paste(" \n \n ",no2_laei2013_labels), 
                          font = 1,
                          cex = 1.5)
          ),
          par.settings = list(
            axis.line =list( col = 'transparent')
          ),
          scales = list(draw = FALSE),
          col.regions = no2_laei2013_colours,
          at = no2_laei2013_breaks)














                               
