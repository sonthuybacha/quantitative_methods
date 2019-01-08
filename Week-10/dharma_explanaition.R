
x <- runif(100, -10, 10)
a <- 0.5
b <- 3.5
y <- a + b * x + rnorm(100, 0, 2.5)

plot(x, y)

sim <- replicate(100, expr = rnorm(1, a, 0.5) + rnorm(1, b, 0.5) * x + rnorm(100, 0, 2.5))

s <- sort(sim[1,])
plot(s, cumsum(s / sum(s)))
abline(v = y[1], col = "red")

#

p <- pnorm(rnorm(1000, 0, 2.5), 0, 2.5)
plot(quantile(p, seq(0, 1, 0.2)), quantile(runif(1000), seq(0, 1, 0.2)))
abline(0, 1)

p <- ppois(rpois(1000, 5), 5)
plot(quantile(p, seq(0, 1, 0.2)), quantile(runif(1000), seq(0, 1, 0.2)))
abline(0, 1)

p <- pbinom(rbinom(1000, 100, 0.25), 100, 0.25)
plot(quantile(p, seq(0, 1, 0.2)), quantile(runif(1000), seq(0, 1, 0.2)))
abline(0, 1)


