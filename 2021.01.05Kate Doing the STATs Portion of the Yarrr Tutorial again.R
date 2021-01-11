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
## Introduction Hypothesis Tests
#********************************* 

# creating two datasets and running a hypothesis test with first the concatenate tool  

# Body piercing data
american.bp <- c(3, 5, 2, 1, 4, 4, 6, 3, 5, 4)
european.bp <- c(6, 5, 7, 7, 6, 3, 4, 6, 5, 4)

# Creating a dataframe from this information

# Store data in a dataframe
## --Dataframe-- function 
## --Concatenate-- function
## --Replicate Elements of Vectors and Lists-- function

bp.survey <- data.frame("bp" = c(american.bp, european.bp),
                        "group" = rep(c("American", "European"), each = 10),
                        stringsAsFactors = FALSE)


# Creating a plot for the bp.survey dataframe
## Pirateplot function creates a Raw Data Descriptive and Inferential Statistic 
## special plot for 1 to 3 categorical independent variables and 1 continuous dependent variables

yarrr::pirateplot(bp ~ group,
                  data = bp.survey,
                  main = "Body Piercing Survey",
                  ylab = "Number of Body Piercings",
                  xlab = "Group", 
                  theme = 2, point.o = .8, cap.beans = TRUE)



#*********************************
## Null v Alternative Hypothesis
#********************************* 
#*
#*Yarr example about Pirates and it's piecring examples:

# In null hypothesis tests, you always start with a null hypothesis. 
# The specific null hypothesis you choose will depend on the type of question you are asking, 
#         but in general, the null hypothesis states that nothing is going on and everything is the same. 

# For example, in our body piercing study, 
#         our null hypothesis is that American and European pirates have the same 
#         number of body piercings on average.

# The alternative hypothesis is the opposite of the null hypothesis. 
# In this case, our alternative hypothesis is that American and European 
#         pirates do not have the same number of piercings on average. 

# ----- Example of a One-Tailed Test -----#

# We can have different types of alternative hypotheses depending on how specific 
#         we want to be about our prediction. We can make a 1-sided (also called 1-tailed) hypothesis, 
#         by predicting the direction of the difference between American and European pirates. 

# For example, our alternative hypothesis could be that European pirates have more piercings 
#         on average than American pirates.


# ----- Example of a Two-Tailed Test -----#

# Alternatively, we could make a 2-sided (also called 2-tailed) 
#         alternative hypothesis that American and European pirates 
#         simply differ in their average number of piercings, 
#         without stating which group has more piercings than the other.

# Once we’ve stated our null and alternative hypotheses, we collect data and then calculate descriptive statistics.



#*********************************
## Descriptive statistics
#********************************* 

# Descriptive statistics (also called sample statistics) 
#         describes samples of data. 

# For example, a mean, median, or standard deviation of a dataset 
#         is a descriptive statistic of that dataset. 

#Let’s calculate some descriptive statistics on our body piercing 
#         survey American and European pirates using the summary() function:


# Descriptive statistics of the piercing data
summary(american.bp)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0     3.0     4.0     3.7     4.8     6.0
summary(european.bp)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     3.0     4.2     5.5     5.3     6.0     7.0



##### NARR: 

# Sample of 10 American pirates had 3.7 body piercings on average, 
#         while our sample of 10 European pirates had 5.3 piercings on average. 

# Is this difference large or small? 
#         Are we justified in concluding that American and European pirates in general differ
#         about how many body piercings they have? 

# To answer this, we need to calculate a test statistic
# test statistic to determine if differenes are significant



#*********************************
## Test Statistics
#********************************* 


# An test statistic compares descriptive statistics, 
#         and determines how different they are. 
#         The formula you use to calculate a test statistics depends the type of test you are conducting, 
#         which depends on many factors, 
#         from the scale of the data (i.e.; is it nominal or interval?), 
#         to how it was collected (i.e.; was the data collected from the same person over time 
#         or were they all different people?), 
#         to how its distributed (i.e.; is it bell-shaped or highly skewed?).


# For now, I can tell you that the type of data we are analyzing calls for a two-sample T-test. 
# This test will take the descriptive statistics from our study, 
#         and return a test-statistic we can then use to make a decision about whether 
#         American and European pirates really differ. 
#         To calculate a test statistic from a two-sample t-test, 
#         we can use the t.test() function in R. 
#         Don’t worry if it’s confusing for now, we’ll go through it in detail shortly.

# Conduct a two-sided t-test comparing the vectors american.bp and european.bp
#  and save the results in an object called bp.test
bp.test <- t.test(x = american.bp,
                  y = european.bp,
                  alternative = "two.sided")

# Print the main results
bp.test
## 
##  Welch Two Sample t-test
## 
## data:  american.bp and european.bp
## t = -3, df = 20, p-value = 0.02
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.93 -0.27
## sample estimates:
## mean of x mean of y 
##       3.7       5.3



#*********************************
## p-value
#*********************************

# The p-value is a probability that reflects how consistent the test statistic is with the hypothesis
#         that the groups are actually the same. 

# Definition of a p-value: 
#         Assuming that there the null hypothesis is true (i.e.; that there is no difference between the groups), 
#         what is the probability that we would have gotten a test statistic as far away from 0 as the one we actually got?


# What is the p-value from our t-test?
bp.test$p.value
## [1] 0.021



# The p-value we got was 0.02, 
# this means that, assuming the two populations of American and European pirates 
#         have the same number of body piercings on average, 

# The probability that we would obtain a test statistic as large as -2.52 is 2.1% . 
# This is very small, but is it small enough to decide that the null hypothesis is not true? 
# It’s hard to say and there is no definitive answer. 

# However, most pirates use a decision threshold of p < 0.05 to determine 
#         if we should reject the null hypothesis or not. 

# In other words, if you obtain a p-value less than 0.05, then you reject the null hypothesis. 
# Because our p-value of 0.02 is less than 0.05, 
#         we would reject the null hypothesis and conclude that the two populations are not be the same.



# p-values are bullshit detectors against the null hypothesis


#*********************************
## p-value, probability that the null hypothesis is true?
#********************************* 

# No!!! The p-value does not tell you the probability that the null hypothesis is true. 
# In other words, if you calculate a p-value of .04, 
#           this does not mean that the probability that the null hypothesis is true is 4%. 

# Rather, it means that if the null hypothesis was true, 
#           the probability of obtaining the result you got is 4%. 

# Now, this does indeed set off our bullshit detector, but again, 
#           it does not mean that the probability that the null hypothesis is true is 4%.




#*********************************
## Overview: t-test
#********************************* 


# R stores hypothesis tests in special object classes called htest. 
# htest objects contain all the major results from a hypothesis test, 
#           from the test statistic (e.g.; a t-statistic for a t-test, 
#           or a correlation coefficient for a correlation test), 
#           to the p-value, to a confidence interval. 


# To show you how this works, let’s create an h.test object called height.htest containing the results
#           from a two-sample t-test comparing the heights of male and female pirates:

# T-test comparing male and female heights
#  stored in a new htest object called height.htest
height.htest <- t.test(formula = height ~ sex,
                       data = pirates,
                       subset = sex %in% c("male", "female"))


height.htest
## 
##  Welch Two Sample t-test
## 
## data:  height by sex
## t = -20, df = 1000, p-value <2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -15 -13
## sample estimates:
## mean in group female   mean in group male 
##                  163                  177


# Show me all the elements in the height.htest object

names(height.htest)
## [1] "statistic"   "parameter"   "p.value"     "conf.int"    "estimate"   
## [6] "null.value"  "alternative" "method"      "data.name"


height.htest$statistic
##   t 
## -21

# Get the p-value
height.htest$p.value
## [1] 1.4e-78

# Get a confidence interval for the mean
height.htest$conf.int
## [1] -15 -13
## attr(,"conf.level")
## [1] 0.95



#*********************************
## T-test: t.test()
#********************************* 

# To compare the mean of 1 group to a specific value, 
#        or to compare the means of 2 groups, you do a t-test. 

# The t-test function in R is t.test(). 
# The t.test() function can take several arguments, 
#       here I’ll emphasize a few of them. 

# To see them all, check the help menu for t.test (?t.test).




#*********************************
## # 1-sample t-test
#********************************* 

# Argument	         Description

# x                  A vector of data whose mean you want to compare to the null hypothesis mu

# mu                The population mean under the null hypothesis. 
#                         For example, mu = 0 will test the null hypothesis that the true population mean is 0.

# alternative       A string specifying the alternative hypothesis. 
#                   Can be "two.sided" indicating a two-tailed test, 
#                   or "greater" or “less" for a one-tailed test.


# In a one-sample t-test, you compare the data from one group of data to some hypothesized mean. 
# For example, if someone said that pirates on average have 5 tattoos, 
#             we could conduct a one-sample test comparing the data 
#             from a sample of pirates to a hypothesized mean of 5. 

# To conduct a one-sample t-test in R using t.test(), 
#             enter a vector as the main argument x, 
#             and the null hypothesis as the argument mu



tattoo.ttest <- t.test(x = pirates$tattoos,  # Vector of data
                       mu = 5)               # Null: Mean is 5

# Print the result
tattoo.ttest
## 
##  One Sample t-test
## 
## data:  pirates$tattoos
## t = 40, df = 1000, p-value <2e-16
## alternative hypothesis: true mean is not equal to 5
## 95 percent confidence interval:
##  9.2 9.6
## sample estimates:
## mean of x 
##       9.4



#*********************************
## 2-sample t-test
#********************************* 


# Fomulation of a two-sample t-test

# Method 1: Formula
# t.test(formula = y ~ x,  # Formula
#        data = df) # Dataframe containing the variables


# Alternatively, if the data you want to compare are in individual vectors 
#       (not together in a dataframe), you can use the vector notation:


# Method 2: Vector
# t.test(x = x,  # First vector
#        y = y)  # Second vector


# --- Example Time---- :
  # 

# test a prediction:
#         Pirates who wear eye patches have fewer tattoos on average 
#         than those who don’t wear eye patches. 

# Because the data are in the pirates dataframe, we can do this using the formula method:

# 2-sample t-test
#  IV = eyepatch (0 or 1)
#  DV = tattoos

tat.patch.htest <- t.test(formula = tattoos ~ eyepatch,
                          data = pirates)

tat.patch.htest

# Welch Two Sample t-test
# 
# data:  tattoos by eyepatch
# t = 1.2249, df = 709.3, p-value = 0.221
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1641539  0.7087957
# sample estimates:
#   mean in group 0 mean in group 1 
# 9.608187        9.335866 

# This test gave us a test statistic of 1.22 and a p-value of 0.22. 
#       Because the p-value is greater than 0.05, 
#       we would fail to reject the null hypothesis.



# Show me all of the elements in the htest object
names(tat.patch.htest)
## [1] "statistic"   "parameter"   "p.value"     "conf.int"    "estimate"   
## [6] "null.value"  "alternative" "method"      "data.name"


# Confidence interval for mean differences
tat.patch.htest$conf.int
## [1] -0.16  0.71
## attr(,"conf.level")
## [1] 0.95



#*********************************
## Using subset to select levels 
## of an Independent Variable (IV)
#********************************* 
# 
# If your independent variable has more than two values, the t.test() function will return an error because it doesn’t know which two groups you want to compare. For example, let’s say I want to compare the number of tattoos of pirates of different ages. Now, the age column has many different values, so if I don’t tell t.test() which two values of age I want to compare, I will get an error like this:
#                                                         



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






