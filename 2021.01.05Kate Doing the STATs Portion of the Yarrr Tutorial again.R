##Hello World

#*********************************
## Version Check
#********************************* 
R.version



## Author: OA Lab, NWFSC
## Title: Kate Doing the STATs Portion of the Yarrr Tutorial again 
## Date: January 2021


#*********************************
##Libraries
#********************************* 
library(shiny)
library(tidyverse)
library(stringr)
library(readxl)
library(readr)
library(tidyr)
library(data.table)
library(lubridate)
library(violinmplot)
library(vioplot)
library(yarrr)
library(datapasta)
library(reprex)
library(miniUI)
library(gridExtra)



#*********************************
## Overview: t-test
#********************************* 

# 1 sample t-test
# Are pirate ages different than 30 on average?
t.test(x = pirates$age, 
       mu = 30)
## Results
# data:  pirates$age
# t = -14.427, df = 999, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 30
# 95 percent confidence interval:
#   27.00092 27.71908
# sample estimates:
#   mean of x 
# 27.36
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |


# 2 sample t-test
# Do females and males have different numbers of  tattoos?
sex.ttest <- t.test(formula = tattoos ~ sex,
                    data = pirates, 
                    subset = sex %in% c("male", "female"))
sex.ttest # Print result

## Results
# Welch Two Sample t-test
# 
# data:  tattoos by sex
# t = -0.016792, df = 952, p-value = 0.9866
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4313270  0.4240083
# sample estimates:
#   mean in group female   mean in group male 
# 9.431034             9.434694 
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |


## Access specific values from test
sex.ttest$statistic
sex.ttest$p.value
sex.ttest$conf.int

## Results
# > ## Access specific values from test
#   > sex.ttest$statistic
# t 
# -0.01679204 
# > sex.ttest$p.value
# [1] 0.986606
# > sex.ttest$conf.int
# [1] -0.4313270  0.4240083
# attr(,"conf.level")
# [1] 0.95
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |


#*********************************
## Overview: Correlation
#********************************* 

# Correlation test
# Is there a relationship between age and height?
cor.test(formula = ~ age + height,
         data = pirates)

# Pearson's product-moment correlation
# 
# data:  age and height
# t = -6.9204, df = 998, p-value = 8.042e-12
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.2723684 -0.1540389
# sample estimates:
#        cor 
# -0.2139885 


# Chi-Square test
# Is there a relationship between college and favorite pirate?
chisq.test(x = pirates$college,
           y = pirates$favorite.pirate)

# Pearson's Chi-squared test
# 
# data:  pirates$college and pirates$favorite.pirate
# X-squared = 52.138, df = 5, p-value = 5.054e-10



#**************E*N*D*************# 
#*********************************
## END OF SCRIPT | END OF DOCUMENT 
#*********************************







#*********************************
## next section (placeholder)
#********************************* 

# _________________________________________________________________________ |
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |

#*********************************
## next section (placeholder)
#********************************* 






