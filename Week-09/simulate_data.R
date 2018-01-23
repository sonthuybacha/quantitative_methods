
### Simulate data

x1 <- runif(350, -2, 2)
x2 <- runif(350, -2, 2)
x3 <- sample(c(0, 1), 350, replace = TRUE)
y <- 1.5 + 0.15 * x1 - 0.5 * x2 - 0.2 * x1 * x2 - 0.35 * x3 + 0.39 * x2 * x3 + runif(350, 0, 2)
y <- ifelse(y > 2, 1, 0)

write.csv(data.frame(infested = y, 
                     temp_anomaly = x1,
                     prec_anomaly = x2,
                     thinned = factor(x3, labels = c("No", "Yes"))), "Week-09/Data/infestation2.csv", row.names = FALSE)

### Solution to the assignment

dat <- read.csv("/Users/corneliussenf/Dropbox/Teching/Quantitative Methods/Week-09/Data/infestation2.csv")

fit <- glm(infested ~ (temp_anomaly * prec_anomaly) * thinned, data = dat, family = binomial(link = "logit"))

summary(fit)

options(na.action = "na.fail") 

fit_all <- MuMIn::dredge(fit, rank = AIC)

head(fit_all)

fit <- MuMIn::get.models(fit_all, delta < 1)[[1]]

efct <- effects::effect("prec_anomaly:temp_anomaly", fit,
                        xlevels = list(temp_anomaly = c(-2, -1, 0, 1, 2),
                                       prec_anomaly = seq(-2, 2, length.out = 100)))

efct <- as.data.frame(efct)

ggplot(efct, aes(x = prec_anomaly, y = fit, col = temp_anomaly, group = temp_anomaly)) +
  geom_line()

efct <- effects::effect("prec_anomaly:thinned", fit,
                        xlevels = list(prec_anomaly = seq(-2, 2, length.out = 100)))

efct <- as.data.frame(efct)


ggplot(efct, aes(x = prec_anomaly, y = fit, col = thinned)) +
  geom_line()


