---
title: "Zhongdi's project 6*6"
author: "Jiaqi Yin"
date: "November 29, 2018"
output: html_document
---

---
title: "Zhongdi's Proejct"
author: "Jiaq Yin"
date: "November 29, 2018"
output: 
  bookdown::html_document2:
    toc: no
    number_sections: yes
    fig_caption: yes
    fig_width: 7
    fig_height: 4
header-includes: 
- \usepackage{graphicx}
- \usepackage{float}
- \usepackage{amsmath}
- \usepackage{dsfont}
- \usepackage{booktabs}
- \usepackage{longtable}
- \usepackage{array}
- \usepackage{multirow}
- \usepackage[table]{xcolor}
- \usepackage{wrapfig}
- \usepackage{colortbl}
- \usepackage{pdflscape}
- \usepackage{tabu}
- \usepackage{threeparttable}
- \usepackage{threeparttablex}
- \usepackage[normalem]{ulem}
- \usepackage{makecell}
---



```r
library(gee)
library(readr)
library(dplyr)
library(pander)
library(kableExtra)
library(data.table)
library(DT)
library(ggplot2)
```
# Data Exploration

```r
dat = read_csv("ThresSize6x6.csv", col_names = FALSE)
names(dat) = c("measure", "age", "id", "dist")
dat = dat %>% filter(dist < 9)
dat$dist.f = as.factor(dat$dist)

dat_axis = read_csv("ThresMinorAxis6x6.csv")
names(dat_axis) = c("measure", "age", "id", "dist")
dat_axis = dat_axis %>% filter(dist < 9)
dat_axis$dist.f = as.factor(dat_axis$dist)
```

## Area
For each participants, we collected the age, and measured the size of cells multiple times with respect to different distance. In the following, we display the number of measurements, mean of size, and its standard deviation. 



```r
measure_per_person = dat %>% group_by(id, dist, age) %>% 
        summarise(n = n(), mean = mean(measure), sd = sd(measure))
datatable(measure_per_person,  class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```
In Fig.(\@ref(fig:SizeVsAge)), each point represents the mean size of one participant w.r.t different distance types. 


```r
measure_per_person$dist.f = as.factor(measure_per_person$dist)
measure_per_person %>% ggplot(aes(x = age, y = mean, color = dist.f, linetype = dist.f)) +
        geom_point() + 
        geom_smooth(aes(linetype=dist.f, color=dist.f)) + # Add a loess smoothed fit curve with confidence region 
        ylab("Mean Size") +
        scale_x_continuous(breaks = seq(15, 90, 5)) +
        scale_color_discrete(name = "Distance") + 
        scale_linetype_discrete(name = "Distance") + 
        theme_bw()
```

![Size Vs. age](figures/SizeVsAge-1.png)

### Empirical Results
Without considering the clustering issue.


```r
area_tmp = dat %>% group_by(age, dist.f) %>% summarise(n = n(), mean_area = mean(measure), sd_area = sd(measure)) %>% mutate(L = mean_area - sd_area, U = mean_area + sd_area)
area_tmp %>%
        ggplot(aes(x = age, fill=dist.f)) + 
        geom_ribbon(aes(ymin=L, ymax = U), alpha = 0.3) + 
        geom_line(aes(y = mean_area, color = dist.f)) + 
        scale_x_continuous(breaks = seq(15, 90, 5)) +
        theme_bw()
```

![Area: Empirical area vs. age](figures/AreaEmp-1.png)
From the plot, we realize there are outliers when collecting data, which are shown in the following table. The number of measurements for such errors are very small. We further delete them from our original dataset. 

```r
measure_per_person %>% filter(dist == 3, mean > 3000) %>%
        kable(caption = "Errors When Collectiong Data") %>%
        kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Errors When Collectiong Data</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> id </th>
   <th style="text-align:right;"> dist </th>
   <th style="text-align:right;"> age </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:left;"> dist.f </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 3064.936 </td>
   <td style="text-align:right;"> 1392.379 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 3364.571 </td>
   <td style="text-align:right;"> 1793.460 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 3861.864 </td>
   <td style="text-align:right;"> 4171.376 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 3006.188 </td>
   <td style="text-align:right;"> 1934.754 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 3373.162 </td>
   <td style="text-align:right;"> 2160.135 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 3125.219 </td>
   <td style="text-align:right;"> 1445.712 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 3379.640 </td>
   <td style="text-align:right;"> 1581.148 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 3497.065 </td>
   <td style="text-align:right;"> 2475.506 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 5100.588 </td>
   <td style="text-align:right;"> 4884.687 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 38 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 3402.485 </td>
   <td style="text-align:right;"> 2743.124 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 3926.994 </td>
   <td style="text-align:right;"> 3796.845 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 33 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 3399.422 </td>
   <td style="text-align:right;"> 1805.144 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 3423.131 </td>
   <td style="text-align:right;"> 2104.247 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 36 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 3337.090 </td>
   <td style="text-align:right;"> 2537.209 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 3688.685 </td>
   <td style="text-align:right;"> 2291.911 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 105 </td>
   <td style="text-align:right;"> 4385.380 </td>
   <td style="text-align:right;"> 3673.867 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 3497.760 </td>
   <td style="text-align:right;"> 1924.095 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 4029.433 </td>
   <td style="text-align:right;"> 2905.276 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:right;"> 3791.804 </td>
   <td style="text-align:right;"> 2845.459 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 43 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 3329.755 </td>
   <td style="text-align:right;"> 1874.548 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 3394.000 </td>
   <td style="text-align:right;"> 2117.997 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 3166.426 </td>
   <td style="text-align:right;"> 1799.686 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 3316.361 </td>
   <td style="text-align:right;"> 2553.979 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 3807.448 </td>
   <td style="text-align:right;"> 4469.530 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 3122.066 </td>
   <td style="text-align:right;"> 2404.252 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 3288.912 </td>
   <td style="text-align:right;"> 2895.079 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 4473.242 </td>
   <td style="text-align:right;"> 4464.132 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 3522.347 </td>
   <td style="text-align:right;"> 2416.476 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 3093.518 </td>
   <td style="text-align:right;"> 1633.394 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 3091.928 </td>
   <td style="text-align:right;"> 2201.028 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 3668.927 </td>
   <td style="text-align:right;"> 2429.200 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 3231.817 </td>
   <td style="text-align:right;"> 1915.861 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 3975.758 </td>
   <td style="text-align:right;"> 3019.044 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 3572.343 </td>
   <td style="text-align:right;"> 2498.950 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 3874.835 </td>
   <td style="text-align:right;"> 3195.932 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 3554.393 </td>
   <td style="text-align:right;"> 2634.213 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 3703.450 </td>
   <td style="text-align:right;"> 3322.204 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 3711.847 </td>
   <td style="text-align:right;"> 2517.152 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 6324.636 </td>
   <td style="text-align:right;"> 14881.678 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 4503.594 </td>
   <td style="text-align:right;"> 5374.472 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 58 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 4363.700 </td>
   <td style="text-align:right;"> 3528.762 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 3774.785 </td>
   <td style="text-align:right;"> 3223.399 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 3750.973 </td>
   <td style="text-align:right;"> 2749.550 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 4240.933 </td>
   <td style="text-align:right;"> 3408.986 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 3852.445 </td>
   <td style="text-align:right;"> 3110.363 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 3806.944 </td>
   <td style="text-align:right;"> 2960.604 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 87 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 3667.061 </td>
   <td style="text-align:right;"> 2422.811 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 3103.256 </td>
   <td style="text-align:right;"> 1528.302 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 3123.873 </td>
   <td style="text-align:right;"> 1701.947 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 3023.053 </td>
   <td style="text-align:right;"> 1374.478 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 3670.114 </td>
   <td style="text-align:right;"> 2740.015 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 92 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 3996.279 </td>
   <td style="text-align:right;"> 4036.022 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 93 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 3748.079 </td>
   <td style="text-align:right;"> 2601.670 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 3192.460 </td>
   <td style="text-align:right;"> 1483.797 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 3119.333 </td>
   <td style="text-align:right;"> 1809.760 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:right;"> 3837.248 </td>
   <td style="text-align:right;"> 3544.394 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 3330.235 </td>
   <td style="text-align:right;"> 2309.577 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 3043.213 </td>
   <td style="text-align:right;"> 1464.885 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 4538.808 </td>
   <td style="text-align:right;"> 6405.877 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 3413.863 </td>
   <td style="text-align:right;"> 3258.706 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 101 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 3477.103 </td>
   <td style="text-align:right;"> 2975.519 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:right;"> 87 </td>
   <td style="text-align:right;"> 3938.360 </td>
   <td style="text-align:right;"> 2811.912 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 103 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 3583.860 </td>
   <td style="text-align:right;"> 5996.555 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 104 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 3472.471 </td>
   <td style="text-align:right;"> 2363.935 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 106 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 3740.191 </td>
   <td style="text-align:right;"> 3498.906 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 107 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 99 </td>
   <td style="text-align:right;"> 3528.953 </td>
   <td style="text-align:right;"> 2260.879 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 107 </td>
   <td style="text-align:right;"> 3589.816 </td>
   <td style="text-align:right;"> 2560.366 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 110 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 12694.239 </td>
   <td style="text-align:right;"> 61008.129 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 3175.745 </td>
   <td style="text-align:right;"> 1587.097 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 112 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 3439.378 </td>
   <td style="text-align:right;"> 2789.260 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 114 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 5162.455 </td>
   <td style="text-align:right;"> 4459.399 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 115 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 3646.860 </td>
   <td style="text-align:right;"> 2354.825 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 116 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 66 </td>
   <td style="text-align:right;"> 3535.188 </td>
   <td style="text-align:right;"> 2378.283 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 117 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:right;"> 3779.606 </td>
   <td style="text-align:right;"> 2870.036 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 118 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 3850.291 </td>
   <td style="text-align:right;"> 3089.687 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 119 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 4360.700 </td>
   <td style="text-align:right;"> 3696.308 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 120 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 4244.899 </td>
   <td style="text-align:right;"> 3635.530 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 121 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 4143.317 </td>
   <td style="text-align:right;"> 6370.588 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 4232.388 </td>
   <td style="text-align:right;"> 3021.260 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 123 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 3199.261 </td>
   <td style="text-align:right;"> 2267.952 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 124 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 3591.811 </td>
   <td style="text-align:right;"> 2773.394 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 125 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 3294.149 </td>
   <td style="text-align:right;"> 2260.290 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 126 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 4255.533 </td>
   <td style="text-align:right;"> 7336.475 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 127 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 123 </td>
   <td style="text-align:right;"> 4199.152 </td>
   <td style="text-align:right;"> 4169.252 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 3150.834 </td>
   <td style="text-align:right;"> 1703.241 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 129 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 73 </td>
   <td style="text-align:right;"> 92 </td>
   <td style="text-align:right;"> 3997.460 </td>
   <td style="text-align:right;"> 4377.692 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 130 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 5025.741 </td>
   <td style="text-align:right;"> 5148.575 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 131 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 3351.703 </td>
   <td style="text-align:right;"> 2062.439 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 132 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 4167.712 </td>
   <td style="text-align:right;"> 5721.390 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 3996.893 </td>
   <td style="text-align:right;"> 4395.228 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 134 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 77 </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 3111.634 </td>
   <td style="text-align:right;"> 1781.042 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 135 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 3110.800 </td>
   <td style="text-align:right;"> 1856.181 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 136 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 3909.186 </td>
   <td style="text-align:right;"> 2603.839 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 137 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 3084.625 </td>
   <td style="text-align:right;"> 1797.764 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 138 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 3210.883 </td>
   <td style="text-align:right;"> 1402.445 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 139 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 3530.422 </td>
   <td style="text-align:right;"> 2252.469 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 140 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 3839.252 </td>
   <td style="text-align:right;"> 2324.426 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 142 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 90 </td>
   <td style="text-align:right;"> 3491.210 </td>
   <td style="text-align:right;"> 2392.982 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 144 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 54 </td>
   <td style="text-align:right;"> 3466.296 </td>
   <td style="text-align:right;"> 1992.607 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 145 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 3102.772 </td>
   <td style="text-align:right;"> 1644.565 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 146 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 87 </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 3684.004 </td>
   <td style="text-align:right;"> 2349.072 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 147 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 3857.234 </td>
   <td style="text-align:right;"> 2746.008 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 148 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 3593.847 </td>
   <td style="text-align:right;"> 2738.453 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 149 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 3179.304 </td>
   <td style="text-align:right;"> 1772.929 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 150 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 4114.449 </td>
   <td style="text-align:right;"> 4050.600 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 151 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 5076.879 </td>
   <td style="text-align:right;"> 6882.788 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 152 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 87 </td>
   <td style="text-align:right;"> 76 </td>
   <td style="text-align:right;"> 4349.363 </td>
   <td style="text-align:right;"> 3541.117 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 153 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 5329.988 </td>
   <td style="text-align:right;"> 7875.937 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 154 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 5354.398 </td>
   <td style="text-align:right;"> 8665.310 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 155 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 3773.279 </td>
   <td style="text-align:right;"> 3602.857 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 156 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 119 </td>
   <td style="text-align:right;"> 3893.679 </td>
   <td style="text-align:right;"> 2885.029 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 157 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 84 </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 3604.247 </td>
   <td style="text-align:right;"> 2795.930 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 158 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 134 </td>
   <td style="text-align:right;"> 6985.342 </td>
   <td style="text-align:right;"> 10530.852 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 159 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 87 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> 4285.468 </td>
   <td style="text-align:right;"> 3953.857 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 160 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 3117.370 </td>
   <td style="text-align:right;"> 1836.866 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 161 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 82 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 4356.766 </td>
   <td style="text-align:right;"> 5386.830 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 162 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> 4672.966 </td>
   <td style="text-align:right;"> 4814.351 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 163 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 3107.626 </td>
   <td style="text-align:right;"> 1359.998 </td>
   <td style="text-align:left;"> 3 </td>
  </tr>
</tbody>
</table>

```r
# dat2: remove errors
dat2 = dat %>% filter(id != 95 | dist != 3) %>% filter(id != 161 | dist != 3)
```
Before we start regression, we check the distribution of predictors. In Fig.(\@ref(fig:countAge)), the number of measurements for distance as 3 is rather small compared with the other two types. It can causes large variation when we preform regression methods.

```r
dat2$dist.f = as.factor(dat2$dist)
dat2 %>% group_by(age, dist.f) %>% summarise(n = n()) %>%
        ggplot(aes(x=age,y=n, fill = dist.f)) + 
        geom_bar(stat="identity",position=position_dodge()) + 
        scale_x_continuous(breaks = seq(15, 90, 5)) +
        theme_bw()
```

![The number of measurements w.r.t ages and distance](figures/countAge-1.png)
In the follwowing, size varies with age w.r.t different distance.

Distance as 1

```r
tmp1 = dat2 %>% filter(dist == 1)
tmp1 %>%
        ggplot(aes(x = age, y = measure)) + 
        geom_point() +
        theme_bw()
```

![Distance = 1 Measure vs. Age](figures/Dist1MeasureVsAge-1.png)
Distance as 2.

```r
tmp2 = dat2 %>% filter(dist == 2)
tmp2 %>%
        ggplot(aes(x = age, y = measure)) + 
        geom_point() +
        theme_bw()
```

![Distance = 2 Measure vs. Age](figures/Dist2MeasureVsAge-1.png)
Distance as 3.


```r
tmp3 = dat2 %>% filter(dist == 3)
tmp3 %>%
        ggplot(aes(x = age, y = measure)) + 
        geom_point() +
        theme_bw()
```

![Distance = 3 Measure vs. Age](figures/Dist3MeasureVsAge-1.png)
## Axis


```r
measure_per_person_axis = dat_axis %>% group_by(id, dist, age) %>% 
        summarise(n = n(), mean = mean(measure), sd = sd(measure))
datatable(measure_per_person_axis,  class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```



```r
measure_per_person_axis$dist.f = as.factor(measure_per_person_axis$dist)
measure_per_person_axis %>% ggplot(aes(x = age, y = mean, color = dist.f, linetype = dist.f)) +
        geom_point() + 
        geom_smooth(aes(linetype=dist.f, color=dist.f)) + # Add a loess smoothed fit curve with confidence region 
        ylab("Mean Size") +
        scale_x_continuous(breaks = seq(15, 90, 5)) +
        scale_color_discrete(name = "Distance") + 
        scale_linetype_discrete(name = "Distance") + 
        theme_bw()
```

![Axis: Size Vs. age](figures/AxisSizeVsAge-1.png)
### Empirical Results
Without considering the clustering issue.


```r
axis_tmp = dat_axis %>% group_by(age, dist.f) %>% summarise(n = n(), mean_axis = mean(measure), sd_area = sd(measure)) %>% mutate(L = mean_axis - sd_area, U = mean_axis + sd_area)
axis_tmp %>%
        ggplot(aes(x = age, fill=dist.f)) + 
        geom_ribbon(aes(ymin=L, ymax = U), alpha = 0.3) + 
        geom_line(aes(y = mean_axis, color = dist.f)) + 
        scale_x_continuous(breaks = seq(15, 90, 5)) +
        theme_bw()
```

![Area: Empirical area vs. age](figures/AxisEmp-1.png)

From the plot, we realize there are outliers when collecting data, which are shown in the following table. The number of measurements for such errors are very small. We further delete them from our original dataset. 

```r
# dat2: remove errors
dat2_axis = dat_axis 
```


```r
dat2_axis$dist.f = as.factor(dat2_axis$dist)
dat2_axis %>% group_by(age, dist.f) %>% summarise(n = n()) %>%
        ggplot(aes(x=age,y=n, fill = dist.f)) + 
        geom_bar(stat="identity",position=position_dodge()) + 
        scale_x_continuous(breaks = seq(15, 90, 5)) +
        theme_bw()
```

![Axis: The number of measurements w.r.t ages and distance](figures/AxiscountAge-1.png)

# Regression

## Area
Consider of multiple measurements for each participants, we perform General Estimated Equation (GEE). Treat each participants as one cluster. The outcome is size, and the predictors are age and distance type. Age are treated as continouse variables, and distance type as category variables.  We assign independence working matrix to each cluster. Robust standard erros are used and 95% confidnece interval is reported in the following table

```r
#geelm = gee(measure ~ age + dist.f, id = id, data = dat2, corstr = "independence", family = "gaussian")
geelm = gee(measure ~ age + dist.f, id = id, data = dat2, corstr = "independence", family = "gaussian")
```

```
## (Intercept)         age     dist.f2     dist.f3     dist.f4     dist.f5 
##  3795.68354    22.56936  -990.80094 -1383.44894 -1778.15798 -1641.44966 
##     dist.f6     dist.f7     dist.f8 
## -1548.63587 -1610.50352 -1359.14509
```

```r
se = summary(geelm)$coefficients[,c("Robust S.E.")]
gee_tab = data.frame(Coef = coef(geelm), SE = se, "L" = coef(geelm) - se * qnorm(0.975), "U" = coef(geelm) + se * qnorm(0.975), pvalue = 2 * (1 - pnorm(abs(summary(geelm)$coefficients[,5])))) 

kable(gee_tab, caption = "GEE Results with Robust S.E") %>%
        kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>GEE Results with Robust S.E</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Coef </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> U </th>
   <th style="text-align:right;"> pvalue </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 3795.68354 </td>
   <td style="text-align:right;"> 280.662897 </td>
   <td style="text-align:right;"> 3245.59437 </td>
   <td style="text-align:right;"> 4345.77271 </td>
   <td style="text-align:right;"> 0.00e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:right;"> 22.56936 </td>
   <td style="text-align:right;"> 4.107425 </td>
   <td style="text-align:right;"> 14.51896 </td>
   <td style="text-align:right;"> 30.61977 </td>
   <td style="text-align:right;"> 0.00e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f2 </td>
   <td style="text-align:right;"> -990.80094 </td>
   <td style="text-align:right;"> 224.427606 </td>
   <td style="text-align:right;"> -1430.67096 </td>
   <td style="text-align:right;"> -550.93091 </td>
   <td style="text-align:right;"> 1.01e-05 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f3 </td>
   <td style="text-align:right;"> -1383.44894 </td>
   <td style="text-align:right;"> 248.482072 </td>
   <td style="text-align:right;"> -1870.46486 </td>
   <td style="text-align:right;"> -896.43303 </td>
   <td style="text-align:right;"> 0.00e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f4 </td>
   <td style="text-align:right;"> -1778.15798 </td>
   <td style="text-align:right;"> 245.721304 </td>
   <td style="text-align:right;"> -2259.76289 </td>
   <td style="text-align:right;"> -1296.55308 </td>
   <td style="text-align:right;"> 0.00e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f5 </td>
   <td style="text-align:right;"> -1641.44966 </td>
   <td style="text-align:right;"> 267.026577 </td>
   <td style="text-align:right;"> -2164.81213 </td>
   <td style="text-align:right;"> -1118.08718 </td>
   <td style="text-align:right;"> 0.00e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f6 </td>
   <td style="text-align:right;"> -1548.63587 </td>
   <td style="text-align:right;"> 287.582610 </td>
   <td style="text-align:right;"> -2112.28743 </td>
   <td style="text-align:right;"> -984.98431 </td>
   <td style="text-align:right;"> 1.00e-07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f7 </td>
   <td style="text-align:right;"> -1610.50352 </td>
   <td style="text-align:right;"> 240.884215 </td>
   <td style="text-align:right;"> -2082.62791 </td>
   <td style="text-align:right;"> -1138.37914 </td>
   <td style="text-align:right;"> 0.00e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f8 </td>
   <td style="text-align:right;"> -1359.14509 </td>
   <td style="text-align:right;"> 259.288838 </td>
   <td style="text-align:right;"> -1867.34188 </td>
   <td style="text-align:right;"> -850.94831 </td>
   <td style="text-align:right;"> 2.00e-07 </td>
  </tr>
</tbody>
</table>
## Axis
interval is reported in the following table

```r
geelm_axis = gee(measure ~ age + dist.f, id = id, data = dat2_axis, corstr = "independence", family = "gaussian")
```

```
## (Intercept)         age     dist.f2     dist.f3     dist.f4     dist.f5 
##  55.1383567   0.1456852  -4.7064720  -7.8090461 -10.4190434 -10.7427427 
##     dist.f6     dist.f7     dist.f8 
## -10.5968082 -10.8932209 -10.2004005
```

```r
se = summary(geelm_axis)$coefficients[,c("Robust S.E.")]
gee_tab = data.frame(Coef = coef(geelm_axis), SE = se, "L" = coef(geelm_axis) - se * qnorm(0.975), "U" = coef(geelm_axis) + se * qnorm(0.975), pvalue = 2 * (1 - pnorm(abs(summary(geelm_axis)$coefficients[,5])))) 

kable(gee_tab, caption = "GEE Results with Robust S.E") %>%
        kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>GEE Results with Robust S.E</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Coef </th>
   <th style="text-align:right;"> SE </th>
   <th style="text-align:right;"> L </th>
   <th style="text-align:right;"> U </th>
   <th style="text-align:right;"> pvalue </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 55.1383567 </td>
   <td style="text-align:right;"> 1.4333230 </td>
   <td style="text-align:right;"> 52.3290952 </td>
   <td style="text-align:right;"> 57.9476182 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age </td>
   <td style="text-align:right;"> 0.1456852 </td>
   <td style="text-align:right;"> 0.0208742 </td>
   <td style="text-align:right;"> 0.1047725 </td>
   <td style="text-align:right;"> 0.1865979 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f2 </td>
   <td style="text-align:right;"> -4.7064720 </td>
   <td style="text-align:right;"> 1.0106998 </td>
   <td style="text-align:right;"> -6.6874071 </td>
   <td style="text-align:right;"> -2.7255369 </td>
   <td style="text-align:right;"> 3.2e-06 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f3 </td>
   <td style="text-align:right;"> -7.8090461 </td>
   <td style="text-align:right;"> 1.0562658 </td>
   <td style="text-align:right;"> -9.8792891 </td>
   <td style="text-align:right;"> -5.7388031 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f4 </td>
   <td style="text-align:right;"> -10.4190434 </td>
   <td style="text-align:right;"> 1.1154857 </td>
   <td style="text-align:right;"> -12.6053552 </td>
   <td style="text-align:right;"> -8.2327315 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f5 </td>
   <td style="text-align:right;"> -10.7427427 </td>
   <td style="text-align:right;"> 1.0869382 </td>
   <td style="text-align:right;"> -12.8731024 </td>
   <td style="text-align:right;"> -8.6123829 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f6 </td>
   <td style="text-align:right;"> -10.5968082 </td>
   <td style="text-align:right;"> 1.1645731 </td>
   <td style="text-align:right;"> -12.8793295 </td>
   <td style="text-align:right;"> -8.3142868 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f7 </td>
   <td style="text-align:right;"> -10.8932209 </td>
   <td style="text-align:right;"> 1.1518206 </td>
   <td style="text-align:right;"> -13.1507479 </td>
   <td style="text-align:right;"> -8.6356940 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dist.f8 </td>
   <td style="text-align:right;"> -10.2004005 </td>
   <td style="text-align:right;"> 1.2297767 </td>
   <td style="text-align:right;"> -12.6107185 </td>
   <td style="text-align:right;"> -7.7900825 </td>
   <td style="text-align:right;"> 0.0e+00 </td>
  </tr>
</tbody>
</table>
### prediction

```r
dist_level = 8
mydata_axis = data.frame(intercept = rep(1,dist_level*100), age = rep(1:100, dist_level))
dist.f = as.factor(rep(c(1:dist_level), each = 100))
dist_tmp = model.matrix(~ dist.f)[,-1]
colnames(dist_tmp) = paste("dist.f", c(2:dist_level), sep = "")
        
mydata_axis = as.matrix(cbind(mydata_axis, dist_tmp))

mydata_y = mydata_axis %*% as.matrix(coef(geelm_axis))
sd_vec = summary(geelm_axis)$coefficients[, "Robust S.E."]
var_mat = diag(sd_vec^2)
mydata_sd = sqrt(apply(mydata_axis, 1, function(x) t(x) %*% var_mat %*% x))

L = mydata_y - 1.96 * mydata_sd
U = mydata_y + 1.96 * mydata_sd

mydata_predict = data.frame(age = rep(1:100, dist_level), dist.f = as.factor(rep(c(1:dist_level), each = 100)), axis_size = mydata_y, L = L, U=U)

mydata_predict %>% ggplot(aes(x = age, fill=dist.f)) + 
        geom_ribbon(aes(ymin=L, ymax = U), alpha = 0.3) + 
        geom_line(aes(y = mydata_y, color = dist.f)) + 
        scale_x_continuous(breaks = seq(0, 100, 5)) +
        #scale_y_continuous(breaks = seq(0,80,5)) +
        theme_bw()
```

![prediction with 95% CI](figures/axisMydata-1.png)
## Diagnostics

```r
# dat2$fittedValue = fitted(geelm)
# 
# tmp1 = dat2 %>% filter(dist == 1)
# tmp1 %>%
#         ggplot(aes(x = age, y = measure)) + 
#         geom_point() + 
#         geom_line(color = "red", data = tmp1, aes(x = age, y = fittedValue))
```
