rm(list = ls())

library(geepack)
library(readr)
library(dplyr)
library(ggplot2)

# fig setting
x = scale_x_continuous(breaks = seq(15, 90, 10)) 
thm = theme(strip.text.x = element_text(size=10, face="bold"),
            axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
            axis.title.x = element_text(color = "grey20", size =10, face = "bold"),
            axis.title.y = element_text(color = "grey20", size = 10, face = "bold"),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.position="bottom")

dat = read_csv("ThresSize3x3.csv", col_names = FALSE)
names(dat) = c("measure", "age", "id", "dist")
dat = dat %>% filter(dist != 5)
dat$dist.f = as.factor(dat$dist)

dat_axis = read_csv("ThresMinorAxis3x3.csv", col_names = FALSE)
names(dat_axis) = c("measure", "age", "id", "dist")
dat_axis = dat_axis %>% filter(dist != 5)
dat_axis$dist.f = as.factor(dat_axis$dist)

#---------Area-----------

## add age^2 term
dat2 = dat
dat2$age_sq = (dat$age)^2

geelm = geeglm(measure ~ age + age_sq + dist.f, id = id, data = dat2, corstr = "independence", family = "gaussian")
save(geelm, file = "data/area-age_sq-gee-model.RData")

cov = geelm$geese$vbeta
se = summary(geelm)$coefficients[,2]
gee_tab = data.frame(Coef = coef(geelm), SE = se, "L" = coef(geelm) - se * qnorm(0.975), "U" = coef(geelm) + se * qnorm(0.975), pvalue = summary(geelm)$coefficients[,4]) 
save(gee_tab, file = "data/gee-age_sq-tab.RData")

## prediction
dist_level = 4
# change dist.f as dummy variable
dist.mat = matrix(NA, ncol = 3, nrow = nrow(dat2))
for (i in 1: nrow(dat2)) {
        if(dat2$dist[i]==1) {
                dist.mat[i,] = c(0,0,0)
        } else if(dat2$dist[i]==2) {
                dist.mat[i,] = c(1,0,0)
        } else if(dat2$dist[i]==3) {
                dist.mat[i,] = c(0,1,0)
        } else {
                dist.mat[i,] = c(0,0,1)
        }
}
mydata = cbind(rep(1, nrow(dat2)), dat2[, c(2, 6)], dist.mat)
mydata = as.matrix(mydata)
y_area_pred = predict(geelm, newdata = dat2)
y_area_var = apply(mydata, 1, function(x) x %*% cov %*% as.matrix(x))
y_area_bias_sq = mean((geelm$residuals)^2)
y_area_sd = sqrt(y_area_var + y_area_bias_sq)
L = y_area_pred - 1.96 * y_area_sd
# negtive in L, change to 0
L[L < 0] = 0
U = y_area_pred + 1.96 * y_area_sd

mydata_predict = data.frame(dat2, area_size = y_area_pred, L = L, U=U, included = dat2$measure < U)

area_cover.df = mydata_predict %>% 
        group_by(id, dist.f,age) %>%
        summarise(n = n(), cover_num = sum(included), cover_pr = cover_num / n)

save(area_cover.df, file = "data/area-age_sq-coverage-pr.RData")

area_cover.df %>% 
        ggplot(aes(x = age, y=cover_pr)) + 
        geom_point(size = 2, alpha = 0.5) +
        geom_hline(yintercept = 0.95, color = "red", size = 2) + 
        x + 
        facet_wrap(~ dist.f)  +
        xlab("Age") + 
        ylab("Coverage Probability") + 
        theme_bw() + 
        thm
mydata_predict %>% ggplot(aes(x = age)) + 
        #geom_point(aes(y = measure), size = 1, color = "gray", alpha = 0.5) + 
        geom_ribbon(aes(ymin=L, ymax = U), alpha = 0.3, color = "red") + 
        geom_line(aes(y = area_size), size = 1) + 
        x + 
        facet_wrap(~ dist.f)  +
        xlab("Age") + 
        ylab("Measure") + 
        theme_bw() + 
        thm
