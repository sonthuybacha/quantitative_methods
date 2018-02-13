library(ggplot2)
library(tidyverse)
library(sp)
library(mapview)

data("meuse")

fit <- lm(log(zinc) ~ elev + sqrt(dist), data = meuse)

summary(fit)

resid <- data.frame(resid = residuals(fit),
                    fitted = fitted(fit))

ggplot(resid, aes(x = resid)) + geom_histogram()

ggplot(resid, aes(x = 1:nrow(meuse), y = resid)) + geom_point()

ggplot(resid, aes(x = fitted, y = resid)) + geom_point()

meuse$residuals <- fit$residuals

ggplot(meuse, aes(x = x, y = y, col = residuals, size = residuals)) +
  geom_point() +
  scale_color_gradient2()

library(spdep)

dnn <- dnearneigh(as.matrix(meuse[,c("x", "y")]), d1 = 0, d2 = 200)
neighw <- nb2listw(dnn, style = "W", zero.policy = TRUE)
moran <- moran.mc(fit$residuals, neighw, nsim = 999, zero.policy = TRUE)

moran

moran_I <- c()

for (d in seq(50, 2000, 50)) {
  dnn <- dnearneigh(as.matrix(meuse[,c("x", "y")]), d1 = 0, d2 = d)
  neighw <- nb2listw(dnn, style = "W", zero.policy = TRUE)
  moran <- moran.mc(fit$residuals, neighw, nsim = 999, zero.policy = TRUE)
  moran_I <- c(moran_I, moran$statistic)
}

moran_I <- data.frame(moran = moran_I, 
                      distance = seq(50, 2000, 50))

ggplot(moran_I, aes(x = distance, y = moran)) + 
  geom_point() +
  geom_line()

dnn <- dnearneigh(as.matrix(meuse[,c("x", "y")]), d1 = 0, d2 = 400)
neighw <- nb2listw(dnn, style = "W", zero.policy = TRUE)

fit_sp1 <- lagsarlm(log(zinc) ~ elev + sqrt(dist), data = meuse, listw = neighw, zero.policy = TRUE)

summary(fit_sp1)

meuse$residuals_sp1 <- fit_sp1$residuals

ggplot(meuse, aes(x = x, y = y, col = residuals_sp1, size = residuals)) +
  geom_point() +
  scale_color_gradient2()

dnn <- dnearneigh(as.matrix(meuse[,c("x", "y")]), d1 = 0, d2 = 400)
neighw <- nb2listw(dnn, style = "W", zero.policy = TRUE)

fit_sp2 <- errorsarlm(log(zinc) ~ elev + sqrt(dist), data = meuse, listw = neighw, zero.policy = TRUE)

summary(fit_sp2)

meuse$residuals_sp2 <- fit_sp2$residuals

ggplot(meuse, aes(x = x, y = y, col = residuals_sp2, size = residuals)) +
  geom_point() +
  scale_color_gradient2()

moran_I <- c()
moran_I_sp1 <- c()
moran_I_sp2 <- c()

for (d in seq(50, 2000, 50)) {
  dnn <- dnearneigh(as.matrix(meuse[,c("x", "y")]), d1 = 0, d2 = d)
  neighw <- nb2listw(dnn, style = "W", zero.policy = TRUE)
  
  moran <- moran.mc(fit$residuals, neighw, nsim = 999, zero.policy = TRUE)
  moran_I <- c(moran_I, moran$statistic)
  
  moran_sp1 <- moran.mc(fit_sp1$residuals, neighw, nsim = 999, zero.policy = TRUE)
  moran_I_sp1 <- c(moran_I_sp1, moran_sp1$statistic)
  
  moran_sp2 <- moran.mc(fit_sp2$residuals, neighw, nsim = 999, zero.policy = TRUE)
  moran_I_sp2 <- c(moran_I_sp2, moran_sp2$statistic)
}

moran_I <- data.frame(moran = moran_I, 
                      moran_sp1 = moran_I_sp1, 
                      moran_sp2 = moran_I_sp2, 
                      distance = seq(50, 2000, 50))

ggplot(moran_I) + 
  geom_point(aes(x = distance, y = moran)) +
  geom_line(aes(x = distance, y = moran)) + 
  geom_point(aes(x = distance, y = moran_sp1), col = "red") +
  geom_line(aes(x = distance, y = moran_sp1), col = "red") + 
  geom_point(aes(x = distance, y = moran_sp2), col = "blue") +
  geom_line(aes(x = distance, y = moran_sp2), col = "blue")

AIC(fit, fit_sp1, fit_sp2)

