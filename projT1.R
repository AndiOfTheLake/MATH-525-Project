## ---- pkg ----
library(survey)
library(srvyr)
library(sampling)
library(tidyverse)
library(knitr)
library(kableExtra)

## ---- load data ----
dt<-read.csv("Employee_A_data.csv")

## ---- sub task 1----
# This is an SRS of size 6000, not a stratified sample
# drv: Disney Review
N<-40041
n<-6000
drv_srs_design = svydesign(id=~1, data=dt, fpc=rep(N, n))

# estimate the mean
svymean(x=~Rating, design=drv_srs_design)

# 95% CI for the mean
svymean(x=~Rating, design=drv_srs_design) %>% confint()

## ---- sub task 2 ----
