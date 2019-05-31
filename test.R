rm(list = ls())
library(dplyr)
library(ggplot2)
n = 100
x = runif(n, -2, 2)
mat = cbind(rep(1, n), x)
beta = c(-1, 3)
error = rnorm(n, 0, 1)

y = cbind(rep(1, n), x) %*% beta + error

plot(x,y)

# lm
m1 = lm(y ~ x)
coefs = summary(m1)$coef[,1]
coefs.sd = summary(m1)$coef[,2]
y.est = mat %*% coefs
y.est.var = mat %*% diag(coefs.sd^2) %*% t(mat)
y.est.sd = sqrt(diag(y.est.var))

y.est.l = y.est - 1.96 * y.est.sd
y.est.u = y.est + 1.96 * y.est.sd

# empirical
y.mean = mean(y)
y.sd = sqrt(mean((y-y.est)^2))

y.l = y.est - 1.96 * y.sd
y.u = y.est + 1.96 * y.sd

y.est.l - y.l

dat = data.frame(y, x, y.est, y.est.l, y.est.u, y.l, y.u)

dat %>% ggplot(aes(x = x)) +
      geom_ribbon(aes(ymin=y.est.l, ymax = y.est.u)) +
      geom_point(aes(y = y)) +
      theme_bw()

dat %>% ggplot(aes(x = x)) +
      geom_ribbon(aes(ymin=y.l, ymax = y.u)) +
      geom_point(aes(y = y)) +
      theme_bw()

dat2 = predict(m1, data.frame(x =x), interval="predict")
dat2 = data.frame(x = x, dat2, y=y)
dat2 %>% ggplot(aes(x = x)) +
      geom_ribbon(aes(ymin=lwr, ymax = upr)) +
      geom_point(aes(y = y)) +
      theme_bw()
