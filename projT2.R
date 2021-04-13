## ---- pkg ----
library(survey)
library(srvyr)
library(sampling)
library(tidyverse)
library(knitr)
library(kableExtra)

## ---- load data ----
dt<-read.csv("Employee_A_data.csv")

## ---- sub-task1-cont ----
# continent variable
# We are given that
N_h_cont<-c(446, 17175, 7011, 12550, 5472)

cont<-c("Africa", "Americas", "Asia", "Europe", "Oceania")

ybar_hu_cont_hat<-c(); ssqr_cont<-c()
for (h in 1:length(cont)){
dt.h <- dt %>% filter(continent == cont[h]) %>% select(Rating) %>% pull()
ybar_hu_cont_hat[h]<-mean(dt.h) # sample stratum mean for "continent"
ssqr_cont[h]<-as.numeric(var(dt.h)) # sample within stratum variance for "continent"
}

(ybar_hat<-mean(dt$Rating)) # sample mean

(SSB_cont_hat<-((ybar_hu_cont_hat-ybar_hat)^2*N_h_cont)%>% sum) # SSB based on "continent"

(Delta_cont_hat<-SSB_cont_hat-sum(ssqr_cont)) # Delta hat based on "continent"

## ---- sub-task1-branch ----
# branch variable
# we are given that
N_h_brch<-c(19406, 9619, 13629)

brch<-c("Disneyland_California",  "Disneyland_HongKong",  "Disneyland_Paris")

ybar_hu_brch_hat<-c(); ssqr_brch<-c()
for (h in 1:length(brch)){
  dt.h <- dt %>% filter(Branch == brch[h]) %>% select(Rating) %>% pull()
  ybar_hu_brch_hat[h]<-mean(dt.h) # sample stratum mean for "Branch"
  ssqr_brch[h]<-as.numeric(var(dt.h)) # sample within-stratum variance for "Branch"
}

(SSB_brch_hat<-((ybar_hu_brch_hat-ybar_hat)^2*N_h_brch)%>% sum) # SSB based on "Branch"
(Delta_brch_hat<-SSB_brch_hat-sum(ssqr_brch)) # Delta hat based on "Branch"

## ---- sub-task2 ----
n<-6000 # total sample size

# optimal sample sizes from each stratum
(n_h<-(n*N_h_brch*sqrt(ssqr_brch)/sum(N_h_brch*sqrt(ssqr_brch))) %>% round)
