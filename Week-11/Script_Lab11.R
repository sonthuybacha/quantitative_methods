
library(ggplot2)
library(tidyverse)

data(iris)

iris_kmeans <- kmeans(x = iris[, c(1:4)], centers = 3)

iris_kmeans

iris$cluster3 <- iris_kmeans$cluster

ggplot(iris, aes(Sepal.Length, y = Sepal.Width, col = factor(cluster3))) +
  geom_point()

table(iris$cluster3, iris$Species)

iris$cluster2 <- kmeans(x = iris[, c(1:4)], centers = 2)$cluster
iris$cluster4 <- kmeans(x = iris[, c(1:4)], centers = 4)$cluster
iris$cluster5 <- kmeans(x = iris[, c(1:4)], centers = 5)$cluster
iris$cluster6 <- kmeans(x = iris[, c(1:4)], centers = 6)$cluster

iris %>%
  gather(key = k, value = cluster, -(Sepal.Length:Species)) %>%
  ggplot(., aes(Sepal.Length, y = Sepal.Width, col = factor(cluster))) +
  geom_point() +
  facet_wrap(~k, ncol = 6)

dist <- dist(iris[, 1:4])

iris_hc <- hclust(dist)

library(ggdendro)

ggdendrogram(iris_hc)

library(raster)

clim <- stack("Data/climate.envi")

clim

names(clim) <- c("Tmin", "Tmean", "Tmax", "Precip")

plot(clim, y = 1)

clim_samp <- sampleRandom(clim, size = 10000)
clim_samp <- as.data.frame(clim_samp) # sampleRandom returns a matrix

clim_samp[1:5, ]

library(GGally)

clim_samp %>%
  sample_n(., size = 100) %>%
  ggpairs(.)

pca <- prcomp(clim_samp, scale. = TRUE)

pca$rotation

pca$sdev

var_exp <- data.frame(pc = 1:4,
                      var_exp = pca$sdev^2 / sum(pca$sdev^2))

var_exp$var_exp_cumsum <- cumsum(var_exp$var_exp)

ggplot(var_exp) +
  geom_bar(aes(x = pc, y = var_exp), stat = "identity") +
  geom_line(aes(x = pc, y = var_exp_cumsum))

clim_pca <- raster::predict(clim, pca, index = 1:4)

plot(clim_pca, 1)

plot(clim_pca, 2)

plot(clim_pca, 3)

plot(clim_pca, 4)

pca_values <- as.data.frame(clim_pca)

pca_kmeans <- kmeans(pca_values[, 1:3], centers = 5)

kmean_raster <- raster(clim_pca)

values(kmean_raster) <- pca_kmeans$cluster

plot(kmean_raster)


