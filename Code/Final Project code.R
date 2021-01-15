library(readxl)
library (magrittr)
library (tidyverse)
library (dplyr)
library(ggplot2)
install.packages("binom")
install.packages("lubridate")
install.packages("hms")

dplyr::select

library(readr)

## Bayes
install.packages("MCMCpack")
install.packages("coda")
library(MCMCpack)
library(coda) 

## LASSO
install.packages("glmnet")
library(glmnet)

## tidymodels
install.packages("parsnip")
library(parsnip)

install.packages("MASS")
library(MASS)

## 2018 Data 
df <- read_excel("/Users/EmmaEichelman/Downloads/NSEE_Spring_2018_PUD.xlsm")

view(df)

df$Year <- rep(2018, nrow(df))

df <- df %>% rename(demog_age_list = AgeRecode)
view(df)

state_disaster_2017 <- c("LA", "FL", "IA", "AR", "CA", "MI", "AL", "NE", 
                         "WV", "OK", "MS", "GA", "OR", "NV", "MT", "AZ", "KS")

df$state_emergency <- ifelse(df$demog_state %in% state_disaster_2017, 1, 0)

df <- na_if(df, 99)
view(df)

df_final <- df %>% dplyr::select(., Year, demog_race, demog_age_list, gw_belief, demog_edu, demog_income, state_emergency)
view(df_final)

df_final$demog_race <- factor(df_final$demog_race, levels = c(1,2,5,9,10), labels = c("White", "African American", "Asian", "Latino", "Mixed race"))
view(df_final)

df_final$gw_belief <- ifelse(df_final$gw_belief == 2,1,0)
view(df_final)

df_final <- filter(df_final, gw_belief != 98)
df_final <- filter(df_final, demog_race != 98)
df_final <- filter(df_final, demog_age_list != 98)
df_final <- filter(df_final, demog_edu != 98)
df_final <- filter(df_final, demog_income != 98)
view(df_final)

df_final$demog_race <- as.factor(df_final$demog_race)
df_final$Year <- as.factor(df_final$Year)
class(df_final$demog_race)
view(df_final)

## data import_cleaning 2010 survey

df.beg <- read_excel ("/Users/EmmaEichelman/Downloads/S2010-NSEE_pud_v2.xlsm")
view(df.beg)

df.beg <- na_if(df.beg, 99)
view(df.beg)

state_disaster_2009 <- c("LOUISIANA", "NEBRASKA", "OKLAHOMA", "OREGON", "WEST VIRGINIA", "NEW MEXICO", "KANSAS", "ALABAMA", "CALIFORNIA", "WASHINGTON", "ARKANSAS", 
                         "NEW YORK", "FLORIDA", "IOWA", "GEORGIA", "TEXAS", "NORTH DAKOTA", "INDIANA")

df.beg$state_emergency <- ifelse(df.beg$demog_state %in% state_disaster_2009, 1, 0)

df.beg_final <- df.beg %>% dplyr::select(Year, demog_race, demog_age_list, gw_belief, demog_edu, demog_income, state_emergency)
view(df.beg_final)

df.beg_final$demog_race <- factor(df.beg_final$demog_race, levels = c(1,2,5,9,10), labels = c("White", "African American", "Asian", "Latino", "Mixed race"))
view(df.beg_final)

df.beg_final$gw_belief <- ifelse(df.beg_final$gw_belief == 2,1,0)
view(df.beg_final)

df.beg_final <- filter(df.beg_final, gw_belief != 98)
df.beg_final <- filter(df.beg_final, demog_race != 98)
df.beg_final <- filter(df.beg_final, demog_age_list != 98)
df.beg_final <- filter(df.beg_final, demog_edu != 98)
df.beg_final <- filter(df.beg_final, demog_income != 98)
view(df.beg_final)

df.beg_final$demog_race <- as.factor(df.beg_final$demog_race)
df.beg_final$Year <- as.factor(df.beg_final$Year)
class(df.beg_final$demog_race)
view(df.beg_final)

## data import_cleaning 2014 survey

df.mid <- read_excel("/Users/EmmaEichelman/Downloads/s14-pud_v2.xlsm")
view(df.mid)

df.mid <- na_if(df.mid, 99)
view(df.mid)

state_disaster_2013 <- c("CA", "KS", "MO", "ND", "WI", "CO", 
                         "NH", "MN", "KY", "MA", "LA", "OH", "CT", "NV", "ID", "AZ", "WA")

df.mid$state_emergency <- ifelse(df.mid$demog_state %in% state_disaster_2013, 1, 0)

df.mid$Year <- rep(2014, nrow(df.mid))

df.mid_final <- df.mid %>% dplyr::select(Year, demog_race, demog_age_list, gw_belief, demog_edu, demog_income, state_emergency)
view(df.mid_final)

df.mid_final$demog_race <- factor(df.mid_final$demog_race, levels = c(1,2,5,9,10), labels = c("White", "African American", "Asian", "Latino", "Mixed race"))
view(df.mid_final)

df.mid_final$gw_belief <- ifelse(df.mid_final$gw_belief == 2,1,0)
view(df.mid_final)

df.mid_final <- filter(df.mid_final, gw_belief != 98)
df.mid_final <- filter(df.mid_final, demog_race != 98)
df.mid_final <- filter(df.mid_final, demog_age_list != 98)
df.mid_final <- filter(df.mid_final, demog_edu != 98)
df.mid_final <- filter(df.mid_final, demog_income != 98)
view(df.mid_final)

df.mid_final$demog_race <- as.factor(df.mid_final$demog_race)
df.mid_final$Year <- as.factor(df.mid_final$Year)
class(df.mid_final$demog_race)
view(df.mid_final)

## final combined dataset
df.compfinal <- rbind(df.beg_final, df.mid_final, df_final)
view(df.compfinal)

df.compfinal <- na.omit(df.compfinal)

## show the mean of gw_belief by year
tapply(df.compfinal$gw_belief, df.compfinal$Year, mean, na.rm = TRUE)

## comparing gw_belief and race for year 2018 
tapply(df.compfinal$gw_belief, df.compfinal$demog_race, mean, na.rm = TRUE)

## preliminary proportion tests

## gw_belief over time 
res <- prop.test(x = c(370, 533), n = c(623, 666))
res
res2 <- prop.test(table(df.compfinal$gw_belief, df.compfinal$state_emergency), correct=FALSE)
res2

## binary logistic regressions

## df.beg_final analysis
xtabs(~df.beg_final$gw_belief + df.beg_final$demog_race, data = df.beg_final)

df.beg_final %>% group_by(demog_race, Year) %>% summarise(mean = mean(gw_belief))

logit_b <- glm(gw_belief ~ demog_race + demog_age_list + demog_edu + demog_income + state_emergency, family = binomial, data = df.beg_final)
summary(logit_b)

logit_b1 <- stepAIC(logit_b) 
summary(logit_b1)

summary(logit_b1$fitted.values)

confint(logit_b)

## df.mid_final analysis
xtabs(~df.mid_final$gw_belief + df.mid_final$demog_race, data = df.mid_final)

df.mid_final %>% group_by(demog_race, Year) %>% summarise(mean = mean(gw_belief))

logit_d <- glm(gw_belief ~ demog_race + demog_age_list + demog_edu + demog_income + state_emergency, family = binomial, data = df.mid_final)
summary(logit_d)

logit_d1 <- stepAIC(logit_d) 
summary(logit_d1)

summary(logit_d1$fitted.values)

## df_final analysis
xtabs(~df_final$gw_belief + df_final$demog_race, data = df_final)

df_final %>% group_by(demog_race, Year) %>% summarise(mean = mean(gw_belief))

logit_c <- glm(gw_belief ~ demog_race + demog_age_list + demog_edu + demog_income + state_emergency, family = binomial, data = df_final)
summary(logit_c)

logit_c1 <- stepAIC(logit_c) 
summary(logit_c1)

summary(logit_c1$fitted.values)

## df.compfinal analysis
xtabs(~df.compfinal$gw_belief + df.compfinal$demog_race, data = df.compfinal)

df.compfinal %>% group_by(demog_race, Year) %>% summarise(mean = mean(gw_belief))

logit_1 <- glm(df.compfinal$gw_belief~., family = binomial, data = df.compfinal)
summary(logit_1)

logit_2 <- stepAIC(logit_1) 
summary(logit_2)

summary(logit_2$fitted.values)

table(df_final$demog_race)
table(df.beg_final$demog_race)
table(df.mid_final$demog_race)

table(df.beg_final$state_emergency)
table(df_final$state_emergency)

## graphs

df.compfinal <- na.omit(df.compfinal)
ggplot(df.beg_final, aes(x = gw_belief %>% as.factor)) + 
  geom_bar(aes(color = demog_race %>% as.factor), position='dodge') +
  labs(x = "Belief in Climate Change (Yes/No)", y = NULL, color = "Race Category") +
  facet_wrap(~Year)

ggplot(df.beg_final, aes(x = gw_belief %>% as.factor)) + 
  geom_bar(aes(color = state_emergency %>% as.factor), position='dodge') +
  labs(x = "Belief in Climate Change (Yes/No)", y = NULL, color = "State Emergency") +
  facet_wrap(~Year)

ggplot(df.mid_final, aes(x = gw_belief %>% as.factor)) + 
  geom_bar(aes(color = state_emergency %>% as.factor), position='dodge') +
  labs(x = "Belief in Climate Change (Yes/No)", y = NULL, color = "State Emergency") +
  facet_wrap(~Year)

ggplot(df_final, aes(x = gw_belief %>% as.factor)) + 
  geom_bar(aes(color = state_emergency %>% as.factor), position='dodge') +
  labs(x = "Belief in Climate Change (Yes/No)", y = NULL, color = "State Emergency") +
  facet_wrap(~Year)

