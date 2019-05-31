rm(list = ls())

library(gee)
library(readr)
library(dplyr)
library(pander)
library(kableExtra)
library(data.table)
library(DT)
library(ggplot2)

dat = read_csv("ThresSize6x6.csv", col_names = FALSE)

Junk  <- read_csv("AgeR6.csv", col_names = FALSE)

names(dat) = c("measure", "age", "id", "dist")

geelm = gee(measure ~ age + dist, id = id, data = dat, corstr = "independence", family = "gaussian")

Zval = summary(geelm)$coefficients[,c("Robust z")]

Page = 2 * (1 - pnorm(abs(Zval[2])))

Pdist = 2 * (1 - pnorm(abs(Zval[3])))


se = summary(geelm)$coefficients[,c("Robust S.E.")]
gee_tab = data.frame(Coef = coef(geelm), SE = se, "L" = coef(geelm) - se * qnorm(0.975), "U" = coef(geelm) + se * qnorm(0.975)) 

kable(gee_tab, caption = "GEE Results with Robust S.E") %>%
  
  kable_styling()

sd_vec = summary(geelm)$coefficients[, "Robust S.E."]
coefs = coef(geelm)

var_mat = diag(sd_vec^2)

# the following shows how to compute the ci for measure
# we assume measure ~ normal distribution
# therefore, all the estimated coefs beta (a vector) also follow multinormal distribution
# once we predict measures based on new impute (age, distance), the linear transform of multinormal distribution is also normal distributed.

ci_measure = function(age, dist){
  
  predict_vec= c(1, age, dist)
  
  mean = sum(predict_vec * coefs)
  
  sd = sqrt(t(predict_vec) %*% var_mat %*% predict_vec)
  
  return(c(mean - 1.96 * sd, mean + 1.96 * sd))
}

# let's see the ci of the collected data
# unique_age_dist = dat %>% select(id, age, dist) %>% distinct()

ci_dat = t(apply(Junk, 1, function(x) ci_measure(x[1],x[2])))

colnames(ci_dat) = c("2.5%", "97.5%")

#predicted_ci_dat = unique_age_dist %>% full_join(mean_measure_per_person, by = c("id", "dist"))

# predicted_ci_dat = cbind(predicted_ci_dat, ci_dat)

predicted_ci_dat = cbind(Junk, ci_dat)

write.csv(predicted_ci_dat, "CI_for_Each_Age6x6Size.csv")