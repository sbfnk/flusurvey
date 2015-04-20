library(RgoogleMaps)
library(rgdal)
library(RSAGA)
library(R.utils)

UKregions <- readOGR(path.expand("~/Research/FluSurvey/Shapefiles/uk_regions.shp"), "uk_regions")
mzoom <- MaxZoom(latrange=UKregions@bbox[2,], lonrange=UKregions@bbox[1,], size=c(640, 640))[[1]]
mzoom
# Center of the study area:
lonc <- mean(UKregions@bbox[1,])
latc <- mean(UKregions@bbox[2,])
lonc; latc
# Get a satellite image of the Netherlands:
MyMap <- GetMap.bbox(center=c(latc, lonc), zoom=mzoom, maptype ="roadmap")
tmp <- PlotOnStaticMap(MyMap, UKregions, add=F, lwd=.5)
