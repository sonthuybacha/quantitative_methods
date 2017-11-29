library(ggplot2)
library(tidyverse)
library(knitr)

airquality_naomit <- na.omit(airquality)

fit1 <- lm(log(Ozone) ~ Temp + Solar.R + Wind, data = airquality_naomit)

summary(fit1)

cor(airquality_naomit[, 1:4])

library(GGally)

ggpairs(data = airquality_naomit, columns = 1:4)

fit2 <- lm(log(Ozone) ~ Temp + Solar.R + I(Solar.R^2) + Wind, data = na.omit(airquality))

summary(fit2)

fit3 <- update(fit2, . ~ . + I(Temp^2)) # add a term from model fit2

fit2 <- update(fit3, . ~ . - I(Temp^2)) # remove a term from model fit3

airquality_naomit$Month <- factor(airquality_naomit$Month, 
                                  labels = c("May", "June", "July", "August", "September"))

fit <- lm(log(Ozone) ~ Temp * Month, data = airquality_naomit)

summary(fit)

p <- ggplot(airquality_naomit, aes(x = Temp, y = Ozone)) +
  geom_point(aes(col = Month)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"))

p

x <- seq(57, 97, length.out = 100) # Simulated temperature values from 57 to 97 Fahrenheit

coefs <- coef(fit) # Model coefficients (log-linear model!)

regressions <- data.frame(Temp = x,
                          May = exp(coefs["(Intercept)"] + coefs["Temp"] * x)) # Backtransformed from log-scale!

p +
  geom_line(data = regressions, aes(x = Temp, y = May), col = "#E41A1C") # Regression line for May

regressions <- regressions %>% 
  mutate(June = exp((coefs["(Intercept)"] + coefs["MonthJune"] + (coefs["Temp"] + coefs["Temp:MonthJune"]) * x)))

p +
  geom_line(data = regressions, aes(x = Temp, y = June), col = "#377EB8") # Regression line for June

regressions <- regressions %>% 
  mutate(July = exp((coefs["(Intercept)"] + coefs["MonthJuly"] + (coefs["Temp"] + coefs["Temp:MonthJuly"]) * x)),
         August = exp((coefs["(Intercept)"] + coefs["MonthAugust"] + (coefs["Temp"] + coefs["Temp:MonthAugust"]) * x)),
         September = exp((coefs["(Intercept)"] + coefs["MonthSeptember"] + (coefs["Temp"] + coefs["Temp:MonthSeptember"]) * x)))

regressions_long <- regressions %>% gather(., key = Month, value = Ozone, -Temp)

ggplot(airquality_naomit, aes(x = Temp, y = Ozone, col = Month)) +
  geom_point() +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")) +
  geom_line(data = regressions_long)

library(effects)

efct <- effect(term = "Temp:Month", mod = fit)

efct <- as.data.frame(efct)

efct$Month <- factor(efct$Month, levels = c("May", "June", "July", "August", "September"))

head(efct)

ggplot(airquality_naomit) +
  geom_point(aes(x = Temp, y = log(Ozone), col = Month)) +
  geom_line(data = efct, aes(x = Temp, y = fit, col = Month)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"))

efct <- effect(term = "Temp:Month", mod = fit, 
               confint = list(compute = TRUE)) # Calculate confidence intervals

efct <- as.data.frame(efct)

efct$Month <- factor(efct$Month, levels = c("May", "June", "July", "August", "September"))

head(efct)

ggplot(airquality_naomit) +
  geom_point(aes(x = Temp, y = log(Ozone), col = Month)) +
  geom_ribbon(data = efct, aes(x = Temp, ymin = lower, ymax = upper, fill = Month), alpha = 0.2) +
  geom_line(data = efct, aes(x = Temp, y = fit, col = Month)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"))

efct <- effect(term = "Temp:Month", mod = fit, 
               confint = list(compute = TRUE),  # Calculate confidence intervals
               transformation = list(link = log, inverse = exp)) # Transforming into non-log space

efct <- as.data.frame(efct)

efct$Month <- factor(efct$Month, levels = c("May", "June", "July", "August", "September"))

ggplot(airquality_naomit, aes(x = Temp, y = Ozone, col = Month)) +
  geom_point() +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")) +
  geom_line(data = efct, aes(y = fit))

efct <- effect(term = "Temp:Month", mod = fit, 
               confint = list(compute = TRUE), # Calculate confidence intervals
               transformation = list(link = log, inverse = exp), # Transforming into non-log space
               xlevels = list(Temp = seq(57, 97, length.out = 100))) # Changing the x-levels

efct <- as.data.frame(efct)

efct$Month <- factor(efct$Month, levels = c("May", "June", "July", "August", "September"))

ggplot(airquality_naomit, aes(x = Temp, y = Ozone, col = Month)) +
  geom_point() +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")) +
  geom_line(data = efct, aes(y = fit))

ggplot(airquality_naomit) +
  geom_point(aes(x = Temp, y = Ozone, col = Month)) +
  geom_ribbon(data = efct, aes(x = Temp, ymin = lower, ymax = upper, fill = Month), alpha = 0.2) +
  geom_line(data = efct, aes(x = Temp, y = fit, col = Month)) +
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"))

fit <- lm(log(Ozone) ~ Temp * Wind, data = airquality_naomit)

summary(fit)

efct <- effect("Temp:Wind", mod = fit, 
               xlevels = list(Temp = seq(57, 97, length.out = 100)), 
               transformation = list(link = log, inverse = exp))

efct <- as.data.frame(efct)

ggplot(airquality_naomit) +
  geom_point(aes(x = Temp, y = Ozone, col = Wind)) +
  geom_line(data = efct, aes(x = Temp, y = fit, col = Wind, group = Wind))

airquality_naomit[, 2:4] <- scale(airquality_naomit[, 2:4]) # z-transforing Solar, Wind, Temp

fit <- lm(log(Ozone) ~ Temp * Wind, data = airquality_naomit)

summary(fit)

efct <- effect("Temp:Wind", mod = fit, 
               xlevels = list(Temp = seq(-2, 2, length.out = 100)), # Temp nor ranges from -2 SD to +2 SD! 
               transformation = list(link = log, inverse = exp))

efct <- as.data.frame(efct)

ggplot(airquality_naomit) +
  geom_point(aes(x = Temp, y = Ozone, col = Wind)) +
  geom_line(data = efct, aes(x = Temp, y = fit, col = Wind, group = Wind))

AIC(fit1, fit2, fit3)

c(summary(fit1)$r.squared, summary(fit2)$r.squared, summary(fit3)$r.squared)

c(summary(fit1)$adj.r.squared, summary(fit2)$adj.r.squared, summary(fit3)$adj.r.squared)

library(MuMIn)

options(na.action = "na.fail")  # Prevent fitting models when data set changes due to NAs

fit_full <- lm(log(Ozone) ~ (Temp * Wind + Solar.R) * Month, data = airquality_naomit)

fit_all <- dredge(global.model = fit_full, rank = AIC)

head(fit_all, n = 5)

model_selection <- get.models(fit_all, delta < 2) # Returns a list of models, in our case only one!

model <- model_selection[[1]]

summary(model)
