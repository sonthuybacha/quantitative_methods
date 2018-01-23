library(raster)
library(sf)
library(tidyverse)

picea <- list.files("Week-12/occurrences/", pattern = glob2rx("Picea*shp"), full.names = TRUE) %>%
  map(~ sf::read_sf(.)) %>%
  map(~ do.call(rbind, st_geometry(.))) %>%
  map(~ as.data.frame(.)) %>%
  set_names(unlist(lapply(strsplit(list.files("Week-12/occurrences/", pattern = glob2rx("Picea*shp")), "\\."), function(x)x[1]))) %>%
  bind_rows(.id = "species") %>%
    dplyr::rename(x = V1, y = V2)

pinus <- list.files("Week-12/occurrences/", pattern = glob2rx("Pinus*shp"), full.names = TRUE) %>%
  map(~ sf::read_sf(.)) %>%
  map(~ do.call(rbind, st_geometry(.))) %>%
  map(~ as.data.frame(.)) %>%
  set_names(unlist(lapply(strsplit(list.files("Week-12/occurrences/", pattern = glob2rx("Pinus*shp")), "\\."), function(x)x[1]))) %>%
  bind_rows(.id = "species") %>%
  dplyr::rename(x = V1, y = V2)

abies <- list.files("Week-12/occurrences/", pattern = glob2rx("Abies*shp"), full.names = TRUE) %>%
  map(~ sf::read_sf(.)) %>%
  map(~ do.call(rbind, st_geometry(.))) %>%
  map(~ as.data.frame(.)) %>%
  set_names(unlist(lapply(strsplit(list.files("Week-12/occurrences/", pattern = glob2rx("Abies*shp")), "\\."), function(x)x[1]))) %>%
  bind_rows(.id = "species") %>%
  dplyr::rename(x = V1, y = V2)

fagus <- list.files("Week-12/occurrences/", pattern = glob2rx("Fagus*shp"), full.names = TRUE) %>%
  map(~ sf::read_sf(.)) %>%
  map(~ do.call(rbind, st_geometry(.))) %>%
  map(~ as.data.frame(.)) %>%
  set_names(unlist(lapply(strsplit(list.files("Week-12/occurrences/", pattern = glob2rx("Fagus*shp")), "\\."), function(x)x[1]))) %>%
  bind_rows(.id = "species") %>%
  dplyr::rename(x = V1, y = V2)

acer <- list.files("Week-12/occurrences/", pattern = glob2rx("Acer*shp"), full.names = TRUE) %>%
  map(~ sf::read_sf(.)) %>%
  map(~ do.call(rbind, st_geometry(.))) %>%
  map(~ as.data.frame(.)) %>%
  set_names(unlist(lapply(strsplit(list.files("Week-12/occurrences/", pattern = glob2rx("Acer*shp")), "\\."), function(x)x[1]))) %>%
  bind_rows(.id = "species") %>%
  dplyr::rename(x = V1, y = V2)

quercus <- list.files("Week-12/occurrences/", pattern = glob2rx("Quercus*shp"), full.names = TRUE) %>%
  map(~ sf::read_sf(.)) %>%
  map(~ do.call(rbind, st_geometry(.))) %>%
  map(~ as.data.frame(.)) %>%
  set_names(unlist(lapply(strsplit(list.files("Week-12/occurrences/", pattern = glob2rx("Quercus*shp")), "\\."), function(x)x[1]))) %>%
  bind_rows(.id = "species") %>%
  dplyr::rename(x = V1, y = V2)

dat <- rbind(picea, pinus, abies, fagus, acer, quercus)
dat <- sample_n(dat, 10000)

ggplot(dat, aes(x = x, y = y, col = species)) +
  geom_point()

# bioclim <- stack("Week-11/Data/bioclim_global10m.tif")
# eu <- shapefile("../../../Desktop/europe.shp")
# plot(eu)
# bioclim <- crop(bioclim, eu)
# bioclim <- mask(bioclim, eu)
# writeRaster(bioclim, "Week-12/bioclim.tif", datatype = "INT2S")

bioclim <- stack("Week-12/bioclim.tif")

bioclim_sample <- sampleRandom(bioclim, 50000)
bioclim_sample <- as.data.frame(bioclim_sample)

pca <- prcomp(bioclim_sample, scale. = TRUE)

cumsum(pca$sdev^2 / sum(pca$sdev^2))

pca_stack <- predict(bioclim, pca, index = 1:5)

plot(pca_stack, 3)

pca_extract <- raster::extract(pca_stack, dat[,c("x", "y")])
pca_extract <- as.data.frame(pca_extract)
names(pca_extract) <- c(paste0("PC", 1:5))
pca_extract$species <- dat$species
pca_extract <- separate(pca_extract, "species", c("genus", "species"), " ")
pca_extract$response <- 0
pca_extract[pca_extract$genus %in% c("Picea", "Pinus", "Abies"), "response"] <- 1

ggplot(gather(pca_extract, key = pca, value = value, -genus, -species, -response), 
       aes(x = pca, y = value, fill = response)) +
  geom_boxplot()

fit <- glm(response ~ PC1 + PC2 + PC3 + PC4 + PC5, data = pca_extract, family = binomial(link = "logit"))

summary(fit)

names(pca_stack) <- paste0("PC", 1:5)

predict_raster <- predict(pca_stack, fit, type = "response")

plot(predict_raster)

write_csv(dat, "Week-12/species.csv")
