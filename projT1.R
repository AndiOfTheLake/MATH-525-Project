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
#N<-40041
N<-19406 + 9619 + 13629
n<-6000
drv_srs_design = svydesign(id=~1, data=dt, fpc=rep(N, n))

# estimate the mean (two different ways)
svytotal(x=~Rating, design=drv_srs_design)[[1]]/N
svymean(x=~Rating, design=drv_srs_design)


# 95% CI for the mean (two different ways)
svytotal(x=~Rating, design=drv_srs_design) %>% confint()*(1/N)
svymean(x=~Rating, design=drv_srs_design) %>% confint()




## ---- sub task 2 pt1----
N_h_brch<-c(19406, 9619, 13629) # population branch sizes

brch<-c("Disneyland_California",  
        "Disneyland_HongKong",  
        "Disneyland_Paris") # Branch names
fpc<-n/N
ssqr<-c(); t_hat<-c(); y_bar_hat<-c(); SE_y_bar_hat<-c()

for (h in 1:length(brch)){
  
  dt.h <- dt %>% filter(Branch == brch[h]) %>% select(Rating) %>% pull # subset by Branch
  ssqr[h]<-var(dt.h) # estimated variance of branch totals by Branch
  t_hat[h]<-(N/n)*sum(dt.h)  # estimated branch total
  y_bar_hat[h]<-t_hat[h]/N_h_brch[h] # estimated branch average
  SE_y_bar_hat[h]<-sqrt(fpc*ssqr[h]/n)/N_h_brch[h] # estimated branch SE
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



## ---- sub task 2 pt2 ----
# Domain estimation
svyby(~Rating, ~Branch, design = drv_srs_design, svymean)

# 95% CI
svyby(~Rating, ~Branch, design = drv_srs_design, svymean) %>% confint() 
# The CI's do not seem to overlap


## ---- sub task 2 pt ----
# Domain estimation
svyby(~Rating, ~Branch, design = drv_srs_design, svymean)

# 95% CI
(CI_dom<-svyby(~Rating, ~Branch, design = drv_srs_design, svymean) %>% 
  confint() )
# The CI's do not seem to overlap







'
## ---- sub task 2 domain estimation with Post strat ----
# First create a population table. We use the number of observations
# given in tables for each branch. Although the total number of 
# these observations is greater than the number of complete
# observations N=40,041 , we have no choice but
# but to use them since we do not know the 40,041 observations split
# in each branch


# create post stratification design
poptable = data.frame(Branch = c("Disneyland_California", "Disneyland_HongKong",  "Disneyland_Paris"), 
                      total = c(19406, 9619, 13629)) # name has to match o.w. does not work
poptable

postdesign = postStratify(design = drv_srs_design, 
                          strata = ~Branch, 
                          population = poptable)

# domain estimation with post stratification
svyby(~Rating, ~Branch, design = postdesign, svymean)

# 95% CI
(CI_dom_post<-svyby(~Rating, ~Branch, design = postdesign, svymean) %>% 
  confint() )
# The CIs do not seem to overlap

# compare
CI_dom
CI_dom_post

# I do not know why I get the same answer



