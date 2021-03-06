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

# domain estimation (Ratio estimator) 
# (This is where we run into the issue of zero standard errors)
dtB_bybrch$fpc1<-15
onestage_drv_design = svydesign(id=~Year_Month,fpc=~fpc1,
                                data=dtB_bybrch)

onestage_drv_design


svyby(~Average_of_Ratings, ~Branch, design = onestage_drv_design, svymean)

# The standard errors are zero.







