rm(list = ls())

#-------------- 3*3---------------------
dat = read_csv("ThresSize3x3.csv", col_names = FALSE)
names(dat) = c("measure", "age", "id", "dist")
dat = dat %>% filter(dist != 5)
dat$dist.f = as.factor(dat$dist)

dat_axis = read_csv("ThresMinorAxis3x3.csv", col_names = FALSE)
names(dat_axis) = c("measure", "age", "id", "dist")
dat_axis = dat_axis %>% filter(dist != 5)
dat_axis$dist.f = as.factor(dat_axis$dist)

dat2 = dat
dat2$dist.f = as.factor(dat2$dist)

dat2_axis = dat_axis
dat2_axis$dist.f = as.factor(dat2_axis$dist)

geelm = geeglm(measure ~ age + dist.f, id = id, data = dat2, corstr = "independence", family = "gaussian")
save(geelm, file = "data/area-gee-model.RData")

geelm_axis = geeglm(measure ~ age + dist.f, id = id, data = dat2_axis, corstr = "independence", family = "gaussian")
save(geelm_axis, file = "data/axis-gee-model.RData" )