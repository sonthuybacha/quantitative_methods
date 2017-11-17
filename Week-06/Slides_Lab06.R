library(ggplot2)
library(tidyverse)

yields <- read.table("Data/yields.txt", sep = ";", header = TRUE)

summary(yields)

yields_tidy <- yields %>% gather(., key = soiltype, value = yield, -plotid)

yields_tidy_summary <- yields_tidy %>% 
  group_by(soiltype) %>%
  summarize(mean = mean(yield))

ggplot(yields_tidy, aes(x = soiltype, y = yield)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(yields_tidy$yield), linetype = "dashed") +
  geom_point(data = yields_tidy_summary, aes(x = soiltype, y = mean), shape = 2)

bartlett.test(x = yields_tidy$yield, g = yields_tidy$soiltype)

fit <- aov(yield ~ soiltype, data = yields_tidy)

summary(fit)

summary.lm(fit)

yields_residuals_fitted <- data.frame(residuals = residuals(fit),
                                      fitted = fitted(fit))

ggplot(data = yields_residuals_fitted,
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0)

yields_residuals_fitted <- yields_residuals_fitted %>% 
  mutate(residuals_ztrans = (residuals - mean(residuals)) / sd(residuals))

ggplot(yields_residuals_fitted, aes()) +
  stat_qq(aes(sample = residuals_ztrans)) +
  geom_abline(intercept = 0, slope = 1)

model.tables(fit, type = "means")

model.tables(fit, type = "effects")

TukeyHSD(fit)

tukey <- as.data.frame(TukeyHSD(fit)$soiltype)
tukey$soiltype <- rownames(tukey)

ggplot(tukey, aes(x = soiltype, y = diff)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.25) +
  geom_hline(yintercept = 0) +
  labs(x = "Soiltype", y = "Differnece in means")
