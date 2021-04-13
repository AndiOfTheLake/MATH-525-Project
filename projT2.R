## ---- pkg ----
library(survey)
library(srvyr)
library(sampling)
library(tidyverse)
library(knitr)
library(kableExtra)

## ---- load the data ----
dt<-read.csv("Employee_A_data.csv")
dim(dt)
variable.names(dt)
head(dt)
sum(is.na(dt)) # no missing data

## ---- sub-task1-cont ----
# continent variable
# We are given that
N_h_cont<-c(446, 17175, 7011, 12550, 5472)

cont<-c("Africa", "Americas", "Asia", "Europe", "Oceania")

ybar_hu_hat<-c()
for (h in 1:length(cont)){
dt.h <- dt %>% filter(continent == cont[h]) %>% select(Rating) %>% pull()
ybar_hu_hat[h]<-mean(dt.h)
}

ybar_hu_hat

(ybar_hat<-mean(dt$Rating))

(SSB_cont_hat<-((ybar_hu_hat-ybar_hat)^2*N_h_cont)%>% sum)


## ---- sub-task1-branch ----
# branch variable
# we are given that
N_h_brch<-c(19406, 9619, 13629)

brch<-c("Disneyland_California",  "Disneyland_HongKong",  "Disneyland_Paris")

ybar_hu_hat<-c()
for (h in 1:length(brch)){
  dt.h <- dt %>% filter(Branch == brch[h]) %>% select(Rating) %>% pull()
  ybar_hu_hat[h]<-mean(dt.h)
}

ybar_hu_hat 

(SSB_brch_hat<-((ybar_hu_hat-ybar_hat)^2*N_h_brch)%>% sum)
## ---- sub-task2 ----
set.seed(0412)