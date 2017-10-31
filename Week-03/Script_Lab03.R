
hist(airquality[ , c("Ozone")])

plot(airquality[ , c("Ozone")], airquality[ , c("Temp")])

boxplot(airquality[ , c("Ozone")] ~ airquality[ , c("Month")])

library(ggplot2)

ggplot(data = airquality, aes(x = Ozone)) +
  geom_histogram()

ggplot(data = airquality, aes(x = Ozone, y = Temp)) + 
  geom_point()

ggplot(data = airquality, aes(x = factor(Month), y = Ozone)) +
  geom_boxplot()

ggplot(data = airquality, aes(x = Ozone, y = Temp, col = factor(Month))) +
  geom_point()

ggplot(data = airquality, aes(x = Ozone, y = Temp, shape  = factor(Month))) +
  geom_point()

ggplot(data = airquality, aes(x = Day, y = Temp, linetype = factor(Month))) +
  geom_line()

ggplot(data = airquality, aes(x = Ozone, fill = factor(Month))) +
  geom_histogram()

ggplot(data = airquality, aes(x = Ozone, y = Temp, col = Wind, size = Solar.R)) +
  geom_point()

ggplot(data = airquality, aes(x = Ozone, y = Temp)) +
  geom_point() +
  facet_wrap(~Month)

ggplot(data = airquality, aes(x = Ozone, y = Temp)) +
  geom_point() +
  facet_wrap(~Month, scales = "free")

ggplot(data = airquality, aes(x = Day, y = Temp)) +
  geom_point(aes(size = Ozone)) +
  geom_line(aes(linetype = factor(Month)))

ggplot(data = airquality, aes(x = Ozone, y = Temp, col = factor(Month))) +
  geom_point() +
  labs(x = "Ozone concentration [ppm]", 
       y = "Temperature [°F]", 
       title = "Relationship between Ozone und Temperture in New York")

plot1 <- ggplot(data = airquality, aes(x = Ozone, y = Temp)) + geom_point()
plot2 <- ggplot(data = airquality, aes(x = Ozone, y = Wind)) + geom_point()

library(gridExtra)

grid.arrange(plot1, plot2, ncol = 2)

pdf(file = "scatterplot01.pdf", width = 7, height = 5)

ggplot(data = airquality, aes(x = Ozone, y = Temp, col = factor(Month))) +
  geom_point()

dev.off()

p <- ggplot(data = airquality, aes(x = Ozone, y = Temp, col = factor(Month))) +
  geom_point()

ggsave(filename = "plot.pdf", plot = p, path = "")

yields <- read.table("Data/yields.txt", sep = ";", header = TRUE)

yields[1:5, ]

library(tidyr)

yields_tidy <- gather(yields, key = soiltype, value = yield, -plotid)

yields_tidy[c(1:3, 11:13), ]

ggplot(yields_tidy, aes(x = soiltype, y = yield)) +
  geom_boxplot()

yields <- spread(yields_tidy, key = soiltype, value = yield)

yields[1:3, ]

yields2 <- read.table("Data/yields2.txt", sep = ";", header = TRUE)

yields2[1:3, ]

yields2_tidy <- gather(yields2, key = soiltype, value = yield, -plotid)

yields2_tidy[c(1:3, 11:13), ]

yields2_tidy <- separate(yields2_tidy, col = soiltype, into = c("soiltype", "grain"), sep = "_")

yields2_tidy[c(1:3, 11:13), ]

ggplot(yields2_tidy, aes(x = soiltype, y = yield, fill = grain)) +
  geom_boxplot()

library(dplyr)

yields2_tidy <- yields2 %>%
  gather(., key = soiltype, value = yield, -plotid) %>%
  separate(., col = soiltype, into = c("soiltype", "grain"), sep = "_")

yields2_tidy <- yields2_tidy %>%
  mutate(yield_log = log(yield),
         yield_log10 = log10(yield))

yields2_tidy[1:3, ]

yields2_tidy_summary <- yields2_tidy %>%
  summarize(yield_mean = mean(yield),
            yield_median = median(yield))

yields2_tidy_summary

yields2_tidy_summary <- yields2_tidy %>%
  group_by(soiltype, grain) %>%
  summarize(yield_mean = mean(yield),
            yield_median = median(yield))

yields2_tidy_summary

yields2_tidy <- yields2_tidy %>%
  group_by(soiltype, grain) %>%
  mutate(yield_centered = yield - mean(yield))

yields2_tidy

yields2_tidy_subset <- yields2_tidy %>%
  filter(grain == "fine")

yields2_tidy_subset <- yields2_tidy %>%
  select(-yield_log10, -yield_log)

