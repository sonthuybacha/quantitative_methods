library(ggplot2)
library(tidyverse)

dat <- read.csv("Data/infestation.csv")

ggplot(dat, aes(x = temp_anomaly, y = infested)) +
  geom_point()

ggplot(dat, aes(x = temp_anomaly, y = infested)) +
  geom_point() +
  geom_smooth(method = "lm")

fit <- glm(infested ~ temp_anomaly, data = dat, family = binomial(link = "logit"))

prediction <- data.frame(temp_anomaly = seq(-2, 2, length.out = 100))
prediction$fitted <- predict(object = fit, newdata = prediction, type = "response")

ggplot() +
  geom_point(data = dat, aes(x = temp_anomaly, y = infested)) +
  geom_line(data = prediction, aes(x = temp_anomaly, y = fitted))

pred <- predict(object = fit, newdata = prediction, 
                type = "link", se.fit = TRUE)

prediction$fitted_link <- pred$fit
prediction$lower_link <- prediction$fitted_link - 2 * pred$se.fit
prediction$upper_link <- prediction$fitted_link + 2 * pred$se.fit

library(boot) ## Boot package for the inv.logit function

prediction$link <- inv.logit(prediction$fitted_link)
prediction$lower <- inv.logit(prediction$lower_link)
prediction$upper <- inv.logit(prediction$upper_link)

ggplot() +
  geom_point(data = dat, aes(x = temp_anomaly, y = infested)) +
  geom_ribbon(data = prediction, aes(x = temp_anomaly, ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = prediction, aes(x = temp_anomaly, y = fitted))

library(effects)

efct <- effect("temp_anomaly", 
               mod = fit, 
               xlevels = list(temp_anomaly = seq(-2, 2, length.out = 100)))
efct <- as.data.frame(efct)

ggplot() +
  geom_point(data = dat, aes(x = temp_anomaly, y = infested)) +
  geom_ribbon(data = efct, aes(x = temp_anomaly, ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = efct, aes(x = temp_anomaly, y = fit))

dat <- read.table("Data/bird_counts.txt", sep = ";", header = TRUE)

ggplot(dat, aes(x = relief, y = Y)) +
  geom_point()

fit <- glm(Y ~ relief, data = dat, family = poisson(link = "log"))

efct <- effect("relief", 
               mod = fit, 
               xlevels = list(relief = seq(0, 4, length.out = 100)))
efct <- as.data.frame(efct)

ggplot() +
  geom_point(data = dat, aes(x = relief, y = Y)) +
  geom_ribbon(data = efct, aes(x = relief, ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = efct, aes(x = relief, y = fit))
