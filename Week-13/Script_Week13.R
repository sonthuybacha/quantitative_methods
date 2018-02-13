library(ggplot2)
library(tidyverse)
library(raster)

ras <- raster(ncol = 100, nrow = 100, xmn = 0, xmx = 100, ymn = 0, ymx = 100)

ras

m <- rnorm(n = ncell(ras), mean = 0, sd = 1)

values(ras) <- m

plot(ras)

plot(ras)

ras2 <- ras + 10

ras3 <- ras * ras2

ras4 <- ras^2

ras5 <- (ras - cellStats(ras, stat = mean)) / 
                                cellStats(ras, stat = sd)

par(mfrow=c(2, 2), mar=c(2.5, 2.5, 3, 3))
plot(ras2, main = "ras + 10")
plot(ras3, main = "ras * ras2")
plot(ras4, main = "ras^2")
plot(ras5, main = "z(ras)")

ras.na <- ras

ras.na[ras.na > 0.5] <- NA

plot(ras.na)

k <- matrix(c(  0, 0.5, 1,
              0.5, 1.5, 2,
              1.5, 6.0, 3), 
            ncol = 3, byrow = TRUE)
k

ras.reclass <- reclassify(x = ras, rcl = k)

plot(ras.reclass, col = c("black", "blue", "red", "green"))

ras[10, 20]

ras[10, 20] <- 10

plot(ras)

ras.stack <- stack(ras, ras2)

ras.stack

layer1 <- subset(x = ras.stack, subset = 1)

layer1

data <- as.data.frame(ras.stack)

head(data)

elevation <- raster("Data/elevation.envi")

elevation

projection(elevation)

plot(elevation)

plot(elevation)

climate <- stack("Data/climate.envi")

nlayers(climate)

names(climate)

names(climate) <- c("Tmax", "Tmin", "Tmean", "Prec")

writeRaster(elevation, 
            filename= "Data/elev.envi",
            overwrite = TRUE)

library(sp)

coords_srs <- data.frame(x = runif(100, 17, 27), 
                         y = runif(100, 45, 50))

pts_srs <- SpatialPoints(coords = coords_srs)
pts_srs

proj <- projection(elevation)

CRS(proj)

pts_srs <- SpatialPoints(coords = coords_srs,
                    proj4string = CRS(proj))

plot(elevation)
plot(pts_srs, add = TRUE)

library(mapview)

mapview(climate)

df.climate <- extract(climate, pts_srs, df = TRUE)

class(df.climate)
head(df.climate)

spdf.climate <- extract(climate, pts_srs, sp = TRUE)

spdf.climate

head(spdf.climate)

spdf.climate$Tmax[1]

library(rgdal)

pts.sample <- readOGR('Data', 'pts_sample')

pts.sample

writeOGR(spdf.climate, "Data", "pts_climate", driver = "ESRI Shapefile", overwrite_layer = TRUE)
