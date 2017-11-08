library(ggplot2)
library(tidyverse)

x <- seq(-4, 4, length = 100)

normal_dist <- data.frame(x = x,
                          d1 = dnorm(x = x, mean = 0, sd = 1),
                          d2 = dnorm(x = x, mean = 0.5, sd = 2),
                          d3 = dnorm(x = x, mean = -1, sd = 0.5))

ggplot(normal_dist) +
  geom_line(aes(x = x, y = d1), col = "blue") +
  geom_line(aes(x = x, y = d2), col = "red") +
  geom_line(aes(x = x, y = d3), col = "green") +
  labs(x = "x", y = "N(x)")

normal_dist <- normal_dist %>%
  mutate(p1 = pnorm(q = x, mean = 0, sd = 1),
         p2 = pnorm(q = x, mean = 0.5, sd = 2),
         p3 = pnorm(q = x, mean = -1, sd = 0.5))

ggplot(normal_dist) +
  geom_line(aes(x = x, y = p1), col = "blue") +
  geom_line(aes(x = x, y = p2), col = "red") +
  geom_line(aes(x = x, y = p3), col = "green") +
  labs(x = "x", y = "Cummulative N(x)")

z_95 <- qnorm(p = 0.95, mean = 0, sd = 1)

ggplot(normal_dist) +
  geom_line(aes(x = x, y = p1), col = "blue") +
  geom_vline(xintercept = z_95) +
  labs(x = "x", y = "density")

random_normal <- data.frame(r1 = rnorm(n = 100, mean = 0, sd = 1),
                            r2 = rnorm(n = 100, mean = 0, sd = 1))

ggplot(random_normal, aes(x = r1, y = r2)) +
  geom_point() +
  labs(x = "random draws", y = "random draws")

yields <- read.table("Data/yields.txt", sep = ";", header = TRUE)

yields %>% 
  gather(., key = soiltype, value = yield, -plotid) %>%
  ggplot(., aes(x = soiltype, y = yield)) +
  geom_boxplot()

t.test(x = yields$loam, y = yields$sand, alternative = "two.sided", var.equal = TRUE)

t <- seq(-4, 4, length.out = 100) # Sequence of t-values
df <- nrow(yields) * 2 - 2 # Degrees of freedom: n1 + n2 - 2

t_dist <- data.frame(t = t,
                     p = pt(q = t, df = df))

t_crit <- qt(p = 0.95, df = df) # Critical t-value with p<0.05
t_emp <- 3.1375 # Empirical t-value extracted from t.test()

ggplot(t_dist, aes(x = t, y = p)) +
  geom_line() +
  geom_vline(xintercept = t_crit, col = "red") +
  geom_vline(xintercept = t_emp, col = "blue") +
  labs(x = "t", y = "Cummulative STUT(t)")

df <- nrow(yields) * 2 - 2 # Degrees of freedom: n1 + n2 - 2
t_emp <- 3.1375 # Empirical t-value extracted from t.test()

p_value <- (1 - pt(q = t_emp, df = df)) * 2 # Calculate p-value
p_value

var.test(x = yields$loam, y = yields$sand, alternative = "two.sided")

t.test(x = yields$loam, y = yields$sand, alternative = "two.sided", var.equal = FALSE)

x <- c(0.142774142092094, 0.316857467638329, 0.0738065412733704, 0.0719271108973771, 
    0.0980032859370112, 0.151790785603225, 0.84363564895466, 0.402990009170026,     
    0.938224496087059, 0.248315626755357, 0.628750984324142, 0.950565603096038,     
    0.81698462064378, 0.730103035923094, 0.712243484565988, 0.177004434866831,      
    0.330815308261663, 0.337264955043793, 0.443428652128205, 0.268835727125406,     
    0.406888382975012, 0.884146957425401, 0.532588079338893, 0.178504134062678,     
    0.232083402574062) 


y <- c(-0.440492311511667, -0.0735368749994293, -0.859960344171456, -0.743824458596828, 
       -0.64070219748058, -1.07285402921956, 0.501539926105283,
      -0.312940382649315, 1.23244773284018, -0.134928197208915, 0.448658220494093,    
      1.00549448775604, 1.09018988458665, 0.450785089596242, 0.71501013756271,        
      -0.431530236485397, -0.390310491026423, -0.339787719167435, -0.161792394783762, 
      -0.35297130612154, -0.153988389472749, 0.938049434968332, 0.521399142740638,    
      -0.62642913830775, -0.167525585447361)  

dat <- data.frame(x=x, y=y)

ggplot(dat, aes(x = x, y = y)) +
  geom_point(size=2)

f <- seq(0, 250, length.out = 100)
f_dist <- data.frame(f = f,
                     pf = pf(f, df1 = 1, df2 = 23))

t_emp <- 212.8974
t_crit <- qf(0.95, df1 = 1, df2 = 23)

ggplot(f_dist, aes(x = f, y = pf)) +
  geom_line() +
  geom_vline(xintercept = t_emp, col = "red") +
  geom_vline(xintercept = t_crit, col = "blue") +
  labs(x = "f", y = "Cummulative F(f)")

p_value <- 1 - pf(t_emp, df1 = 1, df2 = 23)
p_value

ggplot(dat, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(intercept = -0.9262, slope = 2.1206, col = "red")
