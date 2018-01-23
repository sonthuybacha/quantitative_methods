library(ggplot2)
library(tidyverse)
library(effects)

dat <- read.table("Data/bird_counts.txt", sep = ";", header = TRUE)
fit <- glm(Y ~ relief, data = dat, family = poisson(link = "log"))

efct <- effect("relief", 
               mod = fit, 
               xlevels = list(relief = seq(0, 4, length.out = 100)))
efct <- as.data.frame(efct)

ggplot() +
  geom_point(data = dat, aes(x = relief, y = Y)) +
  geom_ribbon(data = efct, aes(x = relief, ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = efct, aes(x = relief, y = fit))

params <- coef(fit)

y_hat <- exp(params[1] + params[2] * unique(dat$relief))
y_sim <- rpois(n = length(y_hat), lambda = y_hat)

sim_poisson <- data.frame(relief = unique(dat$relief),
                          y_sim.0 = y_sim)

ggplot() +
  geom_point(data = dat, aes(x = relief, y = Y)) +
  geom_point(data = sim_poisson, aes(x = relief, y = y_sim.0), 
             shape = 17, col = "red")

for (i in 1:1000) {
  sim_poisson[, paste0("y_sim.", i)] <- rpois(n = length(y_hat), lambda = y_hat)
}

sim_poisson_gather <- sim_poisson %>%
  gather(key = simulations_number, value = simulations, -relief)

ggplot() +
  geom_point(data = dat, aes(x = relief, y = Y)) +
  geom_point(data = sim_poisson_gather, aes(x = relief, y = simulations), shape = 17, alpha = 0.1, col = "red")

fit <- glm(Y ~ relief, data = dat, family = quasipoisson(link = "log"))

efct <- effect("relief", 
               mod = fit, 
               xlevels = list(relief = seq(0, 4, length.out = 100)))
efct <- as.data.frame(efct)

ggplot() +
  geom_point(data = dat, aes(x = relief, y = Y)) +
  geom_ribbon(data = efct, aes(x = relief, ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = efct, aes(x = relief, y = fit))

library(MASS)

fit <- glm.nb(Y ~ relief, data = dat, link = "log")

summary(fit)

efct <- effect("relief", 
               mod = fit, 
               xlevels = list(relief = seq(0, 4, length.out = 100)))
efct <- as.data.frame(efct)

ggplot() +
  geom_point(data = dat, aes(x = relief, y = Y)) +
  geom_ribbon(data = efct, aes(x = relief, ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(data = efct, aes(x = relief, y = fit))

dat_logit <- read.table("Data/logistic.txt", sep = "\t", header = TRUE)

fit <- glm(cbind(dead, n - dead) ~ logdose, 
           data = dat_logit, 
           family = binomial(link = "logit"))

params <- coef(fit)

y_hat <- boot::inv.logit(params[1] + params[2] * dat_logit$logdose)
sim_logit <- data.frame(logdose = dat_logit$logdose)

for (i in 1:1000) {
  sim_logit[, paste0("y_sim.", i)] <- rbinom(n = length(y_hat), size = dat_logit$n, prob = y_hat)
}

sim_logit_gather <- sim_logit %>%
  gather(key = simulations_number, value = simulations, -logdose)

ggplot() +
  geom_point(data = sim_logit_gather, aes(x = logdose, y = simulations), 
             shape = 17, alpha = 0.1, col = "red") +
  geom_point(data = dat_logit, aes(x = logdose, y = dead))

fit_ozone <- glm(Ozone ~ Temp, data = airquality, gaussian(link = "log"))

params <- coef(fit_ozone)

y_hat <- exp(params[1] + params[2] * airquality$Temp)
sim_ozone <- data.frame(Temp = airquality$Temp)

for (i in 1:1000) {
  sim_ozone[, paste0("y_sim.", i)] <- rnorm(n = length(y_hat), 
                                            mean = y_hat, 
                                            sd = sqrt(summary(fit_ozone)$dispersion))
}

sim_ozone_gather <- sim_ozone %>%
  gather(key = simulations_number, value = simulations, -Temp)

ggplot() +
  geom_point(data = sim_ozone_gather, aes(x = Temp, y = simulations), 
             shape = 17, alpha = 0.1, col = "red") +
  geom_point(data = airquality, aes(x = Temp, y = Ozone))

library(DHARMa)

fit_poisson <- glm(Y ~ relief, data = dat, family = poisson(link = "log"))

sim_poisson <- simulateResiduals(fittedModel = fit_poisson, n = 250)

# Create QQ-plot data
sim_poisson <- data.frame(observed = quantile(sim_poisson$scaledResiduals, p = seq(0, 1, 0.1)),
                          expected = punif(q = seq(0, 1, 0.1),
                                           min(sim_poisson$scaledResiduals),
                                           max(sim_poisson$scaledResiduals)))

ggplot(sim_poisson, aes(x = expected, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

fit_negbin <- glm.nb(Y ~ relief, data = dat, link = "log")

sim_negbin <- simulateResiduals(fittedModel = fit_negbin, n = 250)

# Create QQ-plot data
sim_negbin_resid <- data.frame(observed = quantile(sim_negbin$scaledResiduals, p = seq(0, 1, 0.1)),
                               expected = punif(q = seq(0, 1, 0.1),
                                                min(sim_negbin$scaledResiduals),
                                                max(sim_negbin$scaledResiduals)))

ggplot(sim_negbin_resid, aes(x = expected, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

dat <- read.csv("Data/infestation.csv")
fit_logit <- glm(infested ~ temp_anomaly, data = dat, family = binomial(link = "logit"))

sim_logit <- simulateResiduals(fittedModel = fit_logit, n = 250)

# Create QQ-plot data
sim_logit_resid <- data.frame(observed = quantile(sim_logit$scaledResiduals, p = seq(0, 1, 0.1)),
                              expected = punif(q = seq(0, 1, 0.1),
                                               min(sim_logit$scaledResiduals),
                                               max(sim_logit$scaledResiduals)))

ggplot(sim_logit_resid, aes(x = expected, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

dat <- read.table("Data/logistic.txt", sep = "\t", header = TRUE)

fit_logit <- glm(cbind(dead, n - dead) ~ logdose,
                 data = dat, 
                 family = binomial(link = "logit"))

sim_logit <- simulateResiduals(fittedModel = fit_logit, n = 250)

# Create QQ-plot data
sim_logit_resid <- data.frame(observed = quantile(sim_logit$scaledResiduals, p = seq(0, 1, 0.1)),
                              expected = punif(q = seq(0, 1, 0.1),
                                               min(sim_logit$scaledResiduals),
                                               max(sim_logit$scaledResiduals)))

ggplot(sim_logit_resid, aes(x = expected, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
