library(ggplot2)
library(tidyverse)
library(sp)
library(raster)

abies <- read.csv("data/abiesalba.csv")

head(abies)

abies_sp <- SpatialPoints(abies, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

abies_sp <- spTransform(abies_sp, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

mapview(abies_sp)

coords <- abies_sp@coords
coords <- as.data.frame(coords)

xmean <- mean(coords[, 1])
ymean <- mean(coords[, 2])

ggplot(coords, aes(x = long, y = lat)) + 
  geom_point(alpha = 0.3) +
  geom_point(aes(x = xmean, y = ymean), col = "red", shape = 2)

xmedian <- median(coords[, 1])
ymedian <- median(coords[, 2])

ggplot(coords, aes(x = long, y = lat)) + 
  geom_point(alpha = 0.3) +
  geom_point(aes(x = xmean, y = ymean), col = "red", shape = 2) +
  geom_point(aes(x = xmedian, y = ymedian), col = "blue", shape = 2)

dist <- raster::pointDistance(abies_sp, longlat = FALSE)
dist[1:3, 1:3]

dist <- dist[lower.tri(dist)]

c(min(dist), mean(dist), max(dist)) / 10000

ext <- extent(abies_sp)
ext

ras <- raster(ext)
ras

dim(ras) <- c(1000, 1000, 1)
ras

projection(ras) <- projection(abies_sp)

countgrid <- rasterize(abies_sp, ras, fun = "count", background = 0)

mapview(countgrid)

abies_ras <- aggregate(countgrid, fact = 10, fun = sum)

mapview(abies_ras)

abies_ras[abies_ras == 0] <- NA

distance_abies <- distance(abies_ras)

mapview(distance_abies)

library(spatialEco)

kernel_ras <- sp.kde(abies_sp, bw = 10000, newdata = ras)

mapview(kernel_ras)

library(ggmap)

map <- get_map(location = c(lon = mean(abies$long), lat = mean(abies$lat)), zoom = 5)

ggmap(map) + 
  geom_density2d(data = abies, aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = abies, 
                 aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 50, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0.1, 0.5), guide = FALSE)
