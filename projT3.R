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
p<-as.matrix(read.csv("Employee_B_probs.csv", header=FALSE))

# change variable name
colnames(dtB_overall)[2]<- "Average_of_Ratings"
colnames(dtB_bybrch)[3]<- "Average_of_Ratings"


## ---- sub task 1 ----
# we are using the number of observations as given in the tables
# i.e. N_pop=19406+9619+13629=42654
(N_pop<-19406+9619+13629)

dtB_overall$t_i<-(dtB_overall$Average_of_Ratings)*(dtB_overall$m_i) # cluster total

(pi<-diag(p)) # first order inclusion probabilities

# Now we estimate the population total using H-T estimator
sum(dtB_overall$t_i/pi)

# Hence the population mean is estimated to be 
# (we are using the number of observations as given in the tables)
(y_bar_HT<-sum(dtB_overall$t_i/pi)/(N_pop))

# Now we find the variance of the estimator for the total
tmp<-matrix(NA, 15, 15)
for (i in (1:15)){
  for (k in (1:15)){
    tmp[i,k]<-(p[i,k]-p[i,i]*p[k,k])/p[i,k]
  }
}

# H-T estimator of variance of H-T estimator for population total
(var_y_HT<-(t(dtB_overall$t_i/pi) %*% tmp %*% (dtB_overall$t_i/pi)) %>%
  as.numeric)

# hence we obtain estimated SE for population mean
(SE_y_bar_HT<-sqrt(var_y_HT)/N_pop)

# hence the 95% CI for the population mean is
y_bar_HT + qnorm(c(0.025, 0.975))*SE_y_bar_HT





## ---- sub task 2 ----
N_h_brch<-c(19406, 9619, 13629) # population branch sizes


brch<-c("Disneyland_California",  
        "Disneyland_HongKong",  
        "Disneyland_Paris") # Branch names

# add a column of cluster totals to data frame
dtB_bybrch$t_i<-(dtB_bybrch$Average_of_Ratings)*(dtB_bybrch$m_i)

ssqr<-c(); t_hat<-c(); y_bar_hat<-c(); SE_y_bar_hat<-c()
for (h in 1:length(brch)){
  
  dtB.h <- dtB_bybrch %>% filter(Branch == brch[h]) # subsetting by Branch
  
  ssqr[h]<-t(dtB.h$t_i) %*% tmp %*% (dtB.h$t_i) # estimated variance of branch-specific total
  
  t_hat[h]<-sum(dtB.h$t_i/pi) # estimated branch total
  
  y_bar_hat[h]<-t_hat[h]/N_h_brch[h] # estimated branch average
  
  SE_y_bar_hat[h]<-sqrt(ssqr[h])/N_h_brch[h] # estimated SE
}

CI<-cbind(brch, 
          y_bar_hat %>% round(.,3),
          (y_bar_hat + qnorm(0.025))%>% round(.,3),
          (y_bar_hat + qnorm(0.975))%>% round(.,3)
)  %>% 
  data.frame() %>%
  format(., justify="left") 

colnames(CI)<-c("BRANCH",
                "Estimated average",
                "2.5%",
                "97.5%")
CI # 95% CI

# The confidence intervals are very, very wide and they overlap.

# domain estimation (Ratio estimator) (I cannot figure out what to do)
dtB_bybrch$fpc<-15
onestage_drv_design = svydesign(id=~Year_Month,fpc=~fpc,
                                data=dtB_bybrch)
onestage_drv_design

svyby(~Average_of_Ratings, ~Branch, design = onestage_drv_design, svymean)



## ---- sub task 3 ----







## ---- sub task 1 script----
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

## ---- sub task 2 script----
N_h_brch<-c(19406, 9619, 13629) # population branch sizes


brch<-c("Disneyland_California",  
        "Disneyland_HongKong",  
        "Disneyland_Paris") # Branch names

# add a column of cluster totals to data frame
dtB_bybrch$t_i<-(dtB_bybrch$Average_of_Ratings)*(dtB_bybrch$m_i)

ssqr<-c(); t_hat<-c(); y_bar_hat<-c(); SE_y_bar_hat<-c()
for (h in 1:length(brch)){
  
  dtB.h <- dtB_bybrch %>% filter(Branch == brch[h]) # subsetting by Branch
  
  ssqr[h]<-t(dtB.h$t_i)%*% tmp %*% (dtB.h$t_i) # estimated variance of branch totals by Branch
  
  t_hat[h]<-sum(dtB.h$t_i/pi) # estimated branch total
  
  y_bar_hat[h]<-t_hat[h]/N_h_brch[h] # estimated branch average
  
  SE_y_bar_hat[h]<-sqrt(ssqr[h])/N_h_brch[h] # estimated SE
}

CI<-cbind(brch, 
          y_bar_hat %>% round(.,3),
          (y_bar_hat + qnorm(0.025))%>% round(.,3),
          (y_bar_hat + qnorm(0.975))%>% round(.,3)
          )  %>% 
  data.frame() %>%
  format(., justify="left") 

colnames(CI)<-c("BRANCH",
                "Estimated average",
                "2.5%",
                "97.5%")
CI # 95% CI

# The confidence intervals are very, very wide and they overlap.

# domain estimation (Ratio estimator) (I cannot figure out what to do)
dtB_bybrch$fpc<-15
onestage_drv_design = svydesign(id=~Year_Month,fpc=~fpc,
                                data=dtB_bybrch)
onestage_drv_design

svyby(~Average_of_Ratings, ~Branch, design = onestage_drv_design, svymean)

## ---- sub task 3 script ----


