
library(ggplot2)
library(tidyverse)


fit.lm <- lm(Ozone ~ Temp * Wind, data = na.omit(airquality))

fit.glm <- glm(Ozone ~ Temp * Wind, data = na.omit(airquality), family = gaussian(link = "identity"))

coef(fit.lm)

coef(fit.glm)

summary(fit.glm)

fit.glm <- glm(Ozone ~ Temp, data = na.omit(airquality), family = gaussian(link = "log"))

fit.lm <- lm(log(Ozone) ~ Temp, data = na.omit(airquality))

prediction <- data.frame(Temp = na.omit(airquality)$Temp,
                         lm = predict(fit.lm, type = "response"),
                         lm_se = predict(fit.lm, type = "response", se = TRUE)$se.fit,
                         glm = predict(fit.glm, type = "response"),
                         glm_se = predict(fit.glm, type = "response", se = TRUE)$se.fit)

ggplot(na.omit(airquality), aes(x = Temp)) +
  geom_point(aes(y = Ozone)) +
  #geom_ribbon(data = prediction, aes(ymin = lm_lwr, ymax = lm_upr), fill = "red", alpha = 0.1) +
  geom_ribbon(data = prediction, aes(ymin = exp(lm - lm_se * 2), ymax = exp(lm + lm_se * 2)), fill = "red", alpha = 0.2) +
  geom_line(data = prediction, aes(y = exp(lm)), col = "red") +
  geom_ribbon(data = prediction, aes(ymin = glm - glm_se * 2, ymax = glm + glm_se * 2), fill = "blue", alpha = 0.3) +
  geom_line(data = prediction, aes(y = glm), col = "blue")

dat <- read.table("Data/logistic.txt", sep = "\t", header = TRUE)

head(dat)

ggplot(dat, aes(x = logdose, y = dead / n, col = product)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") # See colorbrewer.org for palettes

fit <- glm(cbind(dead, n - dead) ~ logdose, 
           data = dat, 
           family = binomial(link = "logit"))

summary(fit)

predictions <- data.frame(logdose = dat$logdose,
                          prediction = predict(fit, type = "response")) # Alternatively type = "link"

ggplot(dat, aes(x = logdose, y = dead/n)) +
  geom_point() +
  geom_line(data = predictions, aes(x = logdose, y = prediction))

fit <- glm(cbind(dead, n - dead) ~ logdose * product, 
           data = dat, 
           family = binomial(link = "logit"))

summary(fit)

library(effects)

predictions <- effect("logdose:product", mod = fit, xlevels = list(logdose = seq(0, 2, length.out = 100)))
predictions <- as.data.frame(predictions)

ggplot() +
  geom_point(data = dat, aes(x = logdose, y = dead/n, col = product)) +
  geom_ribbon(data = predictions, aes(x = logdose, ymin = lower, ymax = upper, fill = product), alpha = 0.2) +
  geom_line(data = predictions, aes(x = logdose, y = fit, col = product)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")

resid <- data.frame(fitted = fitted(fit),
                    residuals = residuals(fit, type = "pearson")) # Pearson residuals

ggplot(resid, aes(x = fitted, y = residuals)) +
  geom_point()

ggplot(resid, aes(sample = scale(residuals))) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1)
