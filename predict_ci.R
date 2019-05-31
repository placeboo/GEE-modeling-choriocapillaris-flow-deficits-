rm(list = ls())

library(gee)
library(readr)
library(dplyr)


dat = read_csv("ThresSize3x3.csv", col_names = FALSE)

names(dat) = c("measure", "age", "id", "dist")

# for each patient, display the number of measures. 
mean_measure_per_person = dat %>% group_by(id, dist) %>% summarise(n = n(), mean = mean(measure))

# assumption: 
# 1. missing at completely random
# 2. there is no interaction between age and distance
geelm = gee(measure ~ age + dist, id = id, data = dat, corstr = "independence", family = "gaussian")

summary(geelm)

sd_vec = summary(geelm)$coefficients[, "Robust S.E."]
coefs = coef(geelm)

var_mat = diag(sd_vec^2)

# the following shows how to compute the ci for measure
# we assume measure ~ normal distribution
# therefore, all the estimated coefs beta (a vector) also follow multinormal distribution
# once we predict measures based on new impute (age, distance), the linear transform of multinormal distribution is also normal distributed.

ci_measure = function(age, dist, object){
        
        predict_vec= c(1, age, dist)
        
        mean = sum(predict_vec * coefs)
        
        sd = sqrt(t(predict_vec) %*% var_mat %*% predict_vec)
        
        return(c(mean - 1.96 * sd, mean + 1.96 * sd))
}

# let's see the ci of the collected data
unique_age_dist = dat %>% select(id, age, dist) %>% distinct()

ci_dat = t(apply(unique_age_dist, 1, function(x) ci_measure(x[2],x[3])))

colnames(ci_dat) = c("2.5%", "97.5%")

predicted_ci_dat = unique_age_dist %>% full_join(mean_measure_per_person, by = c("id", "dist"))

predicted_ci_dat = cbind(predicted_ci_dat, ci_dat)

write.csv(predicted_ci_dat, "CI_for_Each_Person.csv")
