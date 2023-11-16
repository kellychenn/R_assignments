##---- R Lab #4 -----

##1. Run a simple regression, and interpret your results.  (Did the results fit your expectations?  Why?  Why not?)  

##2. Add an interaction term to that model that you think might moderate the original relationship between X1 and X2.  Explain why you think an interaction might be present and in what direction it would work.  Explain your results.  Did it work out?  Yes?  No?  


w = read.csv(file.choose()) ## choose WVS survey ##

install.packages("plyr")
library(plyr)

w = rename(w, c("V160"="thirtyyrold")) ## how much you are comfortable working for a 30-year old; look it up here: http://www.worldvaluessurvey.us/WVSOnline.jsp ## ##
table(w$thirtyyrold)
w = rename(w, c("V164"="seventyyrold"))
w = rename(w, c("V242"="age"))
w = rename(w, c("V2"="country"))

##1. Run a simple regression, and interpret your results.  (Did the results fit your expectations?  Why?  Why not?)  

## Does one's age and country (US vs. China) affect how comfortable they are working for a 30-year old? ##
lm1 = lm(thirtyyrold ~ age + as.factor(country), data=w, subset = (country==156 | country==840))
summary(lm1)

## Since lm1 generates surprising results, what about: Does one's age and country (US vs. China) affect how comfortable they are working for a 70-year old? ##
lm2 = lm(seventyyrold ~ age + as.factor(country), data=w, subset = (country==156 | country==840))
summary(lm2)

w = rename(w, c("V10"="happy"))

## Let me control for overall level of happiness differences, too ##
lm3 = lm(thirtyyrold ~ age + as.factor(country) + happy, data=w, subset = (country==156 | country==840))
summary(lm3)

##2. Add an interaction term to that model that you think might moderate the original relationship between X1 and X2.  Explain why you think an interaction might be present and in what direction it would work.  Explain your results.  Did it work out?  Yes?  No?  

lm4 = lm(thirtyyrold ~ age*as.factor(country) + happy, data=w, subset = (country==156 | country==840))
summary(lm4)

anova(lm3, lm4) ## Did adding the interaction improve my model? ##



