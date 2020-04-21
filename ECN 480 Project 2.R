library(readxl)  
library(tidyverse)
library(knitr)
library(mosaic)
library(estimatr)
library(rdrobust)

setwd("C:/Users/kenda/OneDrive/Desktop/UAH Courses/ECN 480/Econ Project")

allHH <- read_excel("Project-9-datafile.xlsx", 
                    sheet = "All households", na = "NA")

gotL <- read_excel("Project-9-datafile.xlsx", 
                   sheet = "Got loan", na = "NA")

DataDict <- read_excel("Project-9-datafile.xlsx", 
                       sheet = "Data dictionary", na = "NA")

gotL$loan_amount_usd <- gotL$loan_amount * .05

####Household descriptive stats################################

#Average age by region--consider confidence interval

mean(~age|region, data = allHHc, na.rm = TRUE)

sd(allHHc$age,na.rm = TRUE)

mean(allHHc$age, na.rm = TRUE)


#Average HHsize by region--consider confidence interval

mean(~hhsize|region, data = allHHc, na.rm = TRUE)

sd(allHHc$hhsize,na.rm = TRUE)

mean(allHHc$hhsize, na.rm = TRUE)

#Average education by region--consider confidence interval

mean(~max_education|region, data = allHHc, na.rm = TRUE)

sd(allHHc$max_education,na.rm = TRUE)

mean(allHHc$max_education, na.rm = TRUE)

summary(allHHc$max_education)


#Average number of Assets by region--consider confidence interval
mean(~number_assets|region, data = allHHc, na.rm = TRUE)

sd(allHHc$number_assets,na.rm = TRUE)

mean(allHHc$number_assets, na.rm = TRUE)
summary(allHHc$number_assets)

summary(gotL2$loan_amount_usd)

#Scatter Plot number_assets and workingadults by region

plot(allHHc$number_assets, allHHc$working_age_adults)

#Scatter Plot number_assets and education by region
plot(allHHc$number_assets, allHHc$max_education)

###Loan application descriptive stats##########################

#Table of approval rate--consider confidence interval

apprate<- droplevels(subset(allHHc, HH_status == 'successful' |  HH_status == 'denied'))

stabx <- table(apprate$region, apprate$HH_status, apprate$gender)
prop.table(stabx,1)

#Table of approval by region--consider confidence interval

apprate<- droplevels(subset(allHHc, HH_status == 'successful' |  HH_status == 'denied'))

stabx <- table(apprate$region, apprate$HH_status)
prop.table(stabx, 1)

#conditional mean of age given loan status--consider confidence interval
mean(~age|HH_status, data = allHHc, na.rm = TRUE)

#conditional mean of Hh size given loan status--consider confidence interval
mean(~hhsize|HH_status, data = allHHc, na.rm = TRUE)

#conditional mean of max eduation given loan status--consider confidence interval
mean(~max_education | HH_status, data = allHHc, na.rm = TRUE)

#tabke showing mean for HH size(hhsize) by loan status(HH_status) and region type(Rural)
stats4 <- allHHc %>% 
  group_by(HH_status, rural) %>% 
  summarize(avg_hhsize = mean(hhsize, na.rm = TRUE)) %>% 
  spread(HH_status, avg_hhsize) %>% 
  print()

###Difference in characteristics for succesfull and denied borrowers--with confidence intervals

sel_var <- c("age", "max_education", "number_assets",
             "hhsize", "young_children", "working_age_adults")

temp_plot <- data.frame(name = sel_var, 
                        dmean = NA, yhigh = NA, ylow = NA)

for (i in sel_var){
  sel_success <- unlist(allHHc[
    allHHc$HH_status == "successful", i])
  sel_denied <- unlist(allHHc[
    allHHc$HH_status == "denied", i])
  temp <- t.test(sel_success, sel_denied, 
                 conf.level = 0.95)
  # Mean difference
  temp_plot$dmean[temp_plot$name == i] <- 
    temp$estimate[1] - temp$estimate[2] 
  # Lower limit of the confidence interval
  temp_plot$ylow[temp_plot$name == i] <- temp$conf.int[1] 
  # Upper limit of the confidence interval
  temp_plot$yhigh[temp_plot$name == i] <- temp$conf.int[2] 
}

ggplot(temp_plot, aes(x = name, y = dmean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ylow, ymax = yhigh), 
                width = .2) +
  ylab("Difference in means") + xlab("Variable") +
  theme_bw() +
  ggtitle("Difference in HH characteristics 
    (successful and denied borrowers)")


##### Linear regression analysis###
#Logisitc Regression-are approval odds a significant statistic considering approval rates are so high?#

rd1 <- lm_robust(data = allHHc, mva ~ Da)
summary(rd1)

#linear regression-showing factors contributing to loan amount upon approval###
hist(gotL$loan_amount)

gotL2<- subset(gotL, loan_amount < 200000)

hist(gotL2$loan_amount_usd, xlim=c(0,1000), breaks = 200, main = "USD Loan amount", xlab = 'Loan amount' )

summary(gotL2)

#Simple linear regression loan amount regreesed by age
lr1 <- lm_robust(data = gotL2, loan_amount_usd ~ age)
summary(rdz)

with(gotL2,plot(age, loan_amount_usd,ylim=c(0,8000), main = 'Loan Amount & Age'))
abline(lr1)

lr2 <- lm_robust(data = gotL2, loan_amount_usd ~ working_age_adults)
summary(lr2)

with(gotL2,plot(working_age_adults, loan_amount_usd,ylim=c(0,8000), main = 'Loan Amount & Working Age Adults'))
abline(lr2)

lr3 <- lm_robust(data = gotL2, loan_amount_usd ~ hhsize)
summary(lr3)

with(gotL2,plot(hhsize, loan_amount_usd,ylim=c(0,8000), main = 'Loan Amount & HHsize'))
abline(lr3)

lr4 <- lm_robust(data = gotL2, loan_amount_usd ~ max_education)
summary(lr4)

with(gotL2,plot(max_education, loan_amount_usd, main = 'Loan Amount & Education'))
abline(lr4)

lr5 <- lm_robust(data = gotL2, loan_amount_usd ~ number_assets)
summary(lr5)

with(gotL2,plot(number_assets, loan_amount_usd, main = 'Loan Amount & Assets'))
abline(lr5)


#multiple regression
mlr1 <- lm_robust(data = gotL2, loan_amount_usd ~ number_assets + max_education + age)
summary(mlr1)

counts <- table(gotL2$loan_purpose)
barplot(counts,las=2, main="Reason for Loan", xlab="Loan Purpose", cex.names = .5, horiz = TRUE)
