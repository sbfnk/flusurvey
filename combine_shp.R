library(maptools)
library(rgdal)

wgs.84 <- CRS(paste("+init=epsg:", 27700, sep=""))

columns.to.keep <- c('label')

files <- list.files(pattern="*.shp$", recursive=TRUE, full.names=TRUE) 

uid <- 1 

poly.data <- readOGR(files[1], gsub("^.*/(.*).shp$", "\\1", files[1]))
poly.data <- spTransform(poly.data, wgs.84)
n <- length(slot(poly.data, "polygons"))
poly.data <- spChFIDs(poly.data, as.character(uid:(uid+n-1))) 
uid <- uid + n
if (ncol(poly.data) == length(columns.to.keep)) {
  names(poly.data) <- columns.to.keep
}
poly.data <- poly.data[columns.to.keep]

for (i in 2:length(files)) {
    temp.data <- readOGR(files[i], gsub("^.*/(.*).shp$", "\\1",files[i]))
    temp.data <- spTransform(temp.data, wgs.84)
    n <- length(slot(temp.data, "polygons")) 
    temp.data <- spChFIDs(temp.data, as.character(uid:(uid+n-1))) 
    if (ncol(temp.data) == length(columns.to.keep)) {
      names(temp.data) <- columns.to.keep
    }
    temp.data <- temp.data[columns.to.keep]
    uid <- uid + n 
    poly.data <- spRbind(poly.data,temp.data) 
}

plot(poly.data)

# save new shapefile

combined.shp <- 'uk_pcd_1998.shp'
writeOGR(poly.data, dsn=combined.shp, layer='combined1', driver='ESRI Shapefile')
