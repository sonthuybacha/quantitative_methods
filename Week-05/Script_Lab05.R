library(ggplot2)
library(tidyverse)

dat <- read.csv("Data/anscombe/anscombe01.txt")

lm.anscombe1 <- lm(y ~ x, data = dat)


lm.anscombe1


summary(lm.anscombe1)

anova(lm.anscombe1)

fitted(lm.anscombe1)

residuals(lm.anscombe1)

coefficients(lm.anscombe1)

dat$fitted <- fitted(lm.anscombe1)

ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(x = x, y = fitted), col = "red")

parameters <- coefficients(lm.anscombe1)

ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = parameters[1], slope = parameters[2], col="red")

predict(lm.anscombe1) # Similar to fitted(lm.anscombe1)

new.dat <- data.frame(x = c(1, 4, 6, 19))

predict(lm.anscombe1, newdata = new.dat)

predict(lm.anscombe1, interval = 'confidence')

lm.anscombe1 %>% 
  predict(., interval = 'confidence')

prediction.anscombe1 <- lm.anscombe1 %>%
  predict(., interval = 'confidence') %>%
  as.data.frame() %>%
  mutate(x = dat$x)

p <- ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = prediction.anscombe1, aes(x = x, y = fit)) +
  geom_line(data = prediction.anscombe1, aes(x = x, y = upr), linetype = "dashed" ) +
  geom_line(data = prediction.anscombe1, aes(x = x, y = lwr), linetype = "dashed")

print(p)

prediction.anscombe1 <- lm.anscombe1 %>%
  predict(., interval = 'prediction') %>%
  as.data.frame() %>%
  mutate(x = dat$x)

p <- ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = prediction.anscombe1, aes(x = x, y = fit)) +
  geom_line(data = prediction.anscombe1, aes(x = x, y = upr), linetype = "dashed" ) +
  geom_line(data = prediction.anscombe1, aes(x = x, y = lwr), linetype = "dashed")

print(p)

random.data <- data.frame(x = rnorm(10000, mean = mean(dat$x), sd = sd(dat$x)))

prediction <- predict(lm.anscombe1, newdata = random.data, se.fit = TRUE)

head(prediction$fit) # Fitted values

head(prediction$se.fit) # Standard error (parameter uncertainty)

prediction$df # Degrees of freedom: df = (n - 2)

prediction$residual.scale # Residual variance sigma^2: y = b0 + b1 * x + N(0, sigma^2)

random.data$predicted <- prediction$fit + rnorm(10000, mean = 0, sd = prediction$residual.scale)

p <- ggplot(prediction.anscombe1) +
  geom_line(aes(x = x, y = upr)) +
  geom_line(aes(x = x, y = lwr)) +
  geom_point(data = random.data, aes(x = x, y = predicted), col = "red", alpha = 0.2) +
  labs(x = "x", y = "prediction")

print(p)

p <- ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.95)

print(p)

anscombe.all <- read.csv("Data/anscombe_all.csv")

p <- ggplot(anscombe.all, aes(x = x, y = y)) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = "lm", se = FALSE, col = 'red') +      
            facet_wrap(~ dataset)

print(p)

ggplot(anscombe.all, aes(x = dataset, y = x)) +
            geom_boxplot()

lm.anscombe1 <- anscombe.all %>% filter(dataset == "Anscombe 1") %>% lm(y ~ x, data = .)

diagnostics.anscombe1 <- data.frame(residuals = residuals(lm.anscombe1),
                                    fitted = fitted(lm.anscombe1))

p1 <- ggplot(data = diagnostics.anscombe1, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "Anscombe 1")

lm.anscombe2 <- anscombe.all %>% filter(dataset == "Anscombe 2") %>% lm(y ~ x, data = .)

diagnostics.anscombe2 <- data.frame(residuals = residuals(lm.anscombe2),
                                    fitted = fitted(lm.anscombe2))

p2 <- ggplot(data = diagnostics.anscombe2, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "Anscombe 2")

gridExtra::grid.arrange(p1, p2, ncol = 2)

diagnostics.anscombe1 <- diagnostics.anscombe1 %>% mutate(observation = 1:11)

p1 <- ggplot(data = diagnostics.anscombe1, aes(x = observation, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "Anscombe 1")

diagnostics.anscombe2 <- diagnostics.anscombe2 %>% mutate(observation = 1:11)

p2 <- ggplot(data = diagnostics.anscombe2, aes(x = observation, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red") +
  labs(title = "Anscombe 2")

gridExtra::grid.arrange(p1, p2, ncol = 2)

p1 <- ggplot(data = diagnostics.anscombe1, aes(x = residuals)) +
  geom_histogram() +
  labs(title = "Anscombe 1")

p2 <- ggplot(data = diagnostics.anscombe2, aes(x = residuals)) +
  geom_histogram() +
  labs(title = "Anscombe 2")

gridExtra::grid.arrange(p1, p2, ncol = 2)

p1 <- ggplot(diagnostics.anscombe1, aes(sample = residuals)) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1)

diagnostics.anscombe3 <- anscombe.all %>%
  filter(dataset == "Anscombe 3") %>%
  lm(y ~ x, data = .) %>% 
  residuals(.) %>%
  data.frame(residuals = .)

p3 <- ggplot(diagnostics.anscombe3, aes(sample = residuals)) +
  stat_qq() +
  geom_abline(intercept = 0, slope = 1)

gridExtra::grid.arrange(p1, p3, ncol = 2)

ggplot(anscombe.all, aes(x = x, y = y)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm", se = FALSE, col = 'red', lwd = 0.5) +
  facet_wrap(~dataset, scales = "free")

