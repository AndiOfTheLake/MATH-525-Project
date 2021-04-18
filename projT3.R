## ---- pkg ----
library(survey)
library(srvyr)
library(sampling)
library(tidyverse)
library(knitr)
library(kableExtra)

## ---- load data ----
# I think for the first two data sets, the variable name
# "Total_of_Ratings" should be "Average_of_Ratings" instead

dtB_overall<-read.csv("Employee_B_overall.csv")
dtB_bybrch<-read.csv("Employee_B_by_Branch.csv")
dtB_probs<-as.matrix(read.csv("Employee_B_probs.csv", header=FALSE))

# change variable name
colnames(dtB_overall)[2]<- "Average_of_Ratings"
colnames(dtB_bybrch)[2]<- "Average_of_Ratings"

## ---- sub task 1 ----
t_i<-(dtB_overall$Average_of_Ratings)*(dtB_overall$m_i) # cluster total
t_i

# In the instructions, we are told that there are 112 months (clusters)
# ranging from 2010-10 to 2019-5 with one review month listed
# as missing. We believe that this is a typo since 
# 2010-10 to 2019-5 we have 3+5+8*12=104 months. Subtracting one 
# missing month yields 103 months.

# We have 103 clusters of which we sample 15 according to SRS
(t_hat<-sum(t_i)*103/15) # estimated population total


# estimated population mean. This value seems quite large, perhaps
# it's because we are using dividing by the complete observations.
(y_bar_hat<-t_hat/40041) # estimated population mean 

# If we divide by the total number of observations (complete or not)
# we get a smaller value
# (y_bar_hat<-t_hat/(19406+9619+13629)) # estimated population mean

(ssqr_t<-var(t_i)) # s^2_t

# estimated standard error for t_hat
(SE_t_hat<-((1-15/103)*ssqr_t/15) %>% sqrt()*103) 

# estimated standard error for y_bar_hat, assuming the population
# consists only of complete observations
(SE_y_bar_hat<-SE_t_hat/40041 )

# 95% CI for population mean, which is not informative
y_bar_hat + qnorm(c(0.025, 0.975))*SE_y_bar_hat

## ---- sub task 2 ----


## ---- sub task 3 ----

