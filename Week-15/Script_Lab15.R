## ----echo=FALSE, hide=TRUE, message=FALSE, warning=FALSE-----------------
library(ggplot2)
library(tidyverse)
library(sp)
library(mapview)

## ------------------------------------------------------------------------
data("meuse")

head(meuse)

## ---- eval=FALSE, warning=FALSE, message=FALSE---------------------------
## library(sp)
## 
## coordinates(meuse) <- ~x+y
## proj4string(meuse) <- CRS("+init=epsg:28992")
## 
## mapview(meuse)

## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
library(sp)
library(mapview)

coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")

mapview(meuse)

## ------------------------------------------------------------------------
library(gstat)

g <- gstat(formula = log(zinc) ~ 1, data = meuse)
var <- variogram(g)

head(var)

## ------------------------------------------------------------------------
p <- ggplot(var, aes(x = dist, y = gamma, size = np)) + 
  geom_point()

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=5, fig.width=7, fig.align='center'----
print(p)

## ------------------------------------------------------------------------
var_fit <- fit.variogram(var, model = vgm(c("Exp", "Sph", "Gau")))

var_fit

## ------------------------------------------------------------------------
vari_fit_line <- variogramLine(var_fit, maxdist = max(var$dist))

p <- ggplot(var, aes(x = dist, y = gamma)) + 
  geom_point() +
  geom_line(data = vari_fit_line, aes(x = dist, y = gamma))

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=5, fig.width=7, fig.align='center'----
print(p)

## ------------------------------------------------------------------------
xnew <- seq(meuse@bbox[1, 1], meuse@bbox[1, 2], length.out = 50)
ynew <- seq(meuse@bbox[2, 1], meuse@bbox[2, 2], length.out = 50)

gridnew <- expand.grid(xnew, ynew)

head(gridnew)

## ---- eval=FALSE, warning=FALSE, message=FALSE---------------------------
## names(gridnew) <- c("x", "y")
## coordinates(gridnew) <- ~x+y
## proj4string(gridnew) <- CRS("+init=epsg:28992")
## gridded(gridnew) <- TRUE
## 
## mapview(gridnew)

## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
names(gridnew) <- c("x", "y")
coordinates(gridnew) <- ~x+y
proj4string(gridnew) <- CRS("+init=epsg:28992")
gridded(gridnew) <- TRUE

mapview(gridnew)

## ---- message=FALSE, warning=FALSE---------------------------------------
kriging <- krige(log(zinc) ~ 1, meuse, gridnew, model = var_fit)
kriging <- as.data.frame(kriging)

p <- ggplot(kriging, aes(x = x, y = y)) + 
  geom_point(aes(col = var1.pred)) +
  scale_color_gradient(low = "yellow", high = "red") +
  geom_point(data = as.data.frame(meuse@coords), aes(x = x, y = y)) +
  coord_equal()

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=5, fig.width=7, fig.align='center'----
print(p)

## ------------------------------------------------------------------------
p <- ggplot(kriging, aes(x = x, y = y)) + 
  geom_tile(aes(fill = var1.pred)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_point(data = as.data.frame(meuse@coords), aes(x = x, y = y)) +
  coord_equal()

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=5, fig.width=7, fig.align='center'----
print(p)

## ---- warning=FALSE, message=FALSE---------------------------------------
library(spdep)

knn <- knearneigh(meuse@coords, k = 1)
knn <- knn2nb(knn)
knn

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=6, fig.width=10, fig.align='center'----
knn2 <- knn2nb(knearneigh(meuse@coords, k = 2))
knn3 <- knn2nb(knearneigh(meuse@coords, k = 3))
knn4 <- knn2nb(knearneigh(meuse@coords, k = 4))

par(mfrow = c(1, 4))
plot(knn, meuse@coords, col = "red")
plot(knn2, meuse@coords, col = "red")
plot(knn3, meuse@coords, col = "red")
plot(knn4, meuse@coords, col = "red")

## ------------------------------------------------------------------------
dnn <- dnearneigh(meuse, d1 = 0, d2 = 200)
dnn

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=6, fig.width=10, fig.align='center'----
dnn2 <- dnearneigh(meuse, d1 = 0, d2 = 300)
dnn3 <- dnearneigh(meuse, d1 = 0, d2 = 400)
dnn4 <- dnearneigh(meuse, d1 = 0, d2 = 500)

par(mfrow = c(1, 4))
plot(dnn, meuse@coords, col = "red")
plot(dnn2, meuse@coords, col = "red")
plot(dnn3, meuse@coords, col = "red")
plot(dnn4, meuse@coords, col = "red")

## ---- warning=FALSE, message=FALSE---------------------------------------
shp <- raster::shapefile("data/poverty_vietnam.shp")

#mapview(shp)

## ------------------------------------------------------------------------
nb <- poly2nb(shp, queen = TRUE)
nb

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=6, fig.width=4, fig.align='center'----
plot(shp)
plot(nb, coordinates(shp), add = TRUE, col = "red")

## ------------------------------------------------------------------------
neighw <- nb2listw(nb, style = "W", zero.policy = TRUE)

## ---- fig.show='hide'----------------------------------------------------
moransI <- moran(shp$FGT0, neighw, n = length(nb), S0 = Szero(neighw), zero.policy = TRUE)

moran.plot(shp$FGT0, neighw, zero.policy = TRUE)

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=5, fig.width=5, fig.align='center'----
moran.plot(shp$FGT0, neighw, zero.policy = TRUE)

## ------------------------------------------------------------------------
moran.mc(shp$FGT0, neighw, nsim = 999, zero.policy = TRUE)

## ------------------------------------------------------------------------
localmoran <- localmoran(shp$FGT0, neighw, zero.policy = TRUE)

head(localmoran)

## ------------------------------------------------------------------------
shp@data$id <- rownames(shp@data)
shp@data$localmoran <- localmoran[, 1]

shp_df <- dplyr::left_join(fortify(shp, region = "id"), # convert to a data.frame
                           shp@data,
                           by = "id")

p <- ggplot(shp_df, aes(x = long, y = lat, group = group, fill = localmoran)) + 
  geom_polygon() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  coord_equal()

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.height=6, fig.width=4, fig.align='center'----
print(p)

