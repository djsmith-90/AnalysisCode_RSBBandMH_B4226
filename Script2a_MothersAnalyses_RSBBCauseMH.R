### Script for paper 'Exploring bidirectional causality between religion and mental health: A longitudinal study using data from the parental generation of a UK birth cohort (ALSPAC)' (ALSPAC B-number B4226)
### Script 2a: Mother's analyses - Assessing whether RSBB causes mental health
### Created 26/6/2023 by Dan Major-Smith
### R version 4.0.4

## The analysis plan for this paper has been pre-registered and is available on the OSF: https://osf.io/qtdze/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4226 - RSBB and MH")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("EValue")
library(EValue)

#install.packages("marginaleffects")
library(marginaleffects)



###########################################################################################
#### Read in the processed data

load("data_mum_processed_B4226.RData")


# Make a complete-case dataset with no missing values for the analyses with mental health at age 6 as the outcome
data_mum_cca_mh <- data_mum %>%
  select(c(belief_preg:attend_age5_bin, dep_preg, anx_preg, dep_age6:anx_age6_bin, age:paternalMH)) %>%
  filter(complete.cases(.))

head(data_mum_cca_mh)

# Percent of complete-cases
nrow(data_mum_cca_mh)
round(nrow(data_mum_cca_mh) / nrow(data_mum) * 100, 2)


## Or to read in the synthetic data (Note that this synthetic dataset only includes complete cases):
#load("./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_mum_mhOutcome_B4226.RData")
#data_mum_cca_mh <- data_mum_cca_mh_syn_df
#data_mum_cca_mh <- data_mum_cca_mh %>%
#  select(-FALSE_DATA) ## Drop the 'FALSE_DATA' column


## Create standardised depression (EPDS) and anxiety (CCEI-A) scores
data_mum_cca_mh <- data_mum_cca_mh %>%
  mutate(dep_age6_z = (dep_age6 - mean(dep_age6)) / sd(dep_age6)) %>%
  mutate(anx_age6_z = (anx_age6 - mean(anx_age6)) / sd(anx_age6))

summary(data_mum_cca_mh$dep_age6_z)
summary(data_mum_cca_mh$anx_age6_z)


#### Descriptive stats

### Religious exposures

## Religious belief (categorical)

# Full sample
sum(is.na(data_mum$belief_age5))
round(sum(is.na(data_mum$belief_age5) / nrow(data_mum) * 100), 2)

table(data_mum$belief_age5)
round(prop.table(table(data_mum$belief_age5)) * 100, 2)

# Complete-cases
table(data_mum_cca_mh$belief_age5)
round(prop.table(table(data_mum_cca_mh$belief_age5)) * 100, 2)


## Religious belief (binary)

# Full sample
table(data_mum$belief_age5_bin)
round(prop.table(table(data_mum$belief_age5_bin)) * 100, 2)

# Complete-cases
table(data_mum_cca_mh$belief_age5_bin)
round(prop.table(table(data_mum_cca_mh$belief_age5_bin)) * 100, 2)


## Religious identity (categorical)

# Full sample
sum(is.na(data_mum$identity_age5))
round(sum(is.na(data_mum$identity_age5) / nrow(data_mum) * 100), 2)

table(data_mum$identity_age5)
round(prop.table(table(data_mum$identity_age5)) * 100, 2)

# Complete-cases
table(data_mum_cca_mh$identity_age5)
round(prop.table(table(data_mum_cca_mh$identity_age5)) * 100, 2)


## Religious identity (binary)

# Full sample
table(data_mum$identity_age5_bin)
round(prop.table(table(data_mum$identity_age5_bin)) * 100, 2)

# Complete-cases
table(data_mum_cca_mh$identity_age5_bin)
round(prop.table(table(data_mum_cca_mh$identity_age5_bin)) * 100, 2)


## Religious attendance (categorical)

# Full sample
sum(is.na(data_mum$attend_age5))
round(sum(is.na(data_mum$attend_age5) / nrow(data_mum) * 100), 2)

table(data_mum$attend_age5)
round(prop.table(table(data_mum$attend_age5)) * 100, 2)

# Complete-cases
table(data_mum_cca_mh$attend_age5)
round(prop.table(table(data_mum_cca_mh$attend_age5)) * 100, 2)


## Religious attendance (binary)

# Full sample
table(data_mum$attend_age5_bin)
round(prop.table(table(data_mum$attend_age5_bin)) * 100, 2)

# Complete-cases
table(data_mum_cca_mh$attend_age5_bin)
round(prop.table(table(data_mum_cca_mh$attend_age5_bin)) * 100, 2)



### Mental health outcomes

## Depression (continuous)

# Full sample
sum(is.na(data_mum$dep_age6))
round(sum(is.na(data_mum$dep_age6) / nrow(data_mum) * 100), 2)

summary(data_mum$dep_age6)

# Complete-cases
summary(data_mum_cca_mh$dep_age6)
sd(data_mum_cca_mh$dep_age6)

hist(data_mum_cca_mh$dep_age6, 
     breaks=rep(min(data_mum_cca_mh$dep_age6, na.rm = TRUE):max(data_mum_cca_mh$dep_age6, na.rm = TRUE), 
                each = 2) + c(-0.4, 0.4), freq = TRUE,
     xlab = "EPDS depression score", main = "")


## Depression (binary)

# Full sample
table(data_mum$dep_age6_bin)
round(prop.table(table(data_mum$dep_age6_bin)) * 100, 2)

# Complete-cases
table(data_mum_cca_mh$dep_age6_bin)
round(prop.table(table(data_mum_cca_mh$dep_age6_bin)) * 100, 2)


## Anxiety (continuous)

# Full sample
sum(is.na(data_mum$anx_age6))
round(sum(is.na(data_mum$anx_age6) / nrow(data_mum) * 100), 2)

summary(data_mum$anx_age6)

# Complete-cases
summary(data_mum_cca_mh$anx_age6)
sd(data_mum_cca_mh$anx_age6)

hist(data_mum_cca_mh$anx_age6, 
     breaks=rep(min(data_mum_cca_mh$anx_age6, na.rm = TRUE):max(data_mum_cca_mh$anx_age6, na.rm = TRUE), 
                each = 2) + c(-0.4, 0.4), freq = TRUE,
     xlab = "CCEI-A anxiety score", main = "")


## Anxiety (binary)

# Full sample
table(data_mum$anx_age6_bin)
round(prop.table(table(data_mum$anx_age6_bin)) * 100, 2)

# Complete-cases
table(data_mum_cca_mh$anx_age6_bin)
round(prop.table(table(data_mum_cca_mh$anx_age6_bin)) * 100, 2)


## Correlation between depression and anxiety
cor.test(data_mum_cca_mh$dep_age6, data_mum_cca_mh$anx_age6)



### Comparing outcomes by exposures

## 1a - Categorical religious belief by continuous depression
data_mum_cca_mh %>%
  group_by(belief_age5) %>%
  summarise(n = n(), mean = mean(dep_age6), sd = sd(dep_age6), min = min(dep_age6), perc25 = quantile(dep_age6, 0.25),
            median = median(dep_age6), perc75 = quantile(dep_age6, 0.75), max = max(dep_age6))

## 1b - Categorical religious identity by continuous depression
data_mum_cca_mh %>%
  group_by(identity_age5) %>%
  summarise(n = n(), mean = mean(dep_age6), sd = sd(dep_age6), min = min(dep_age6), perc25 = quantile(dep_age6, 0.25),
            median = median(dep_age6), perc75 = quantile(dep_age6, 0.75), max = max(dep_age6))

## 1c - Categorical religious attendance by continuous depression
data_mum_cca_mh %>%
  group_by(attend_age5) %>%
  summarise(n = n(), mean = mean(dep_age6), sd = sd(dep_age6), min = min(dep_age6), perc25 = quantile(dep_age6, 0.25),
            median = median(dep_age6), perc75 = quantile(dep_age6, 0.75), max = max(dep_age6))

## 2a - Categorical religious belief by continuous anxiety
data_mum_cca_mh %>%
  group_by(belief_age5) %>%
  summarise(n = n(), mean = mean(anx_age6), sd = sd(anx_age6), min = min(anx_age6), perc25 = quantile(anx_age6, 0.25),
            median = median(anx_age6), perc75 = quantile(anx_age6, 0.75), max = max(anx_age6))

## 2b - Categorical religious identity by continuous anxiety
data_mum_cca_mh %>%
  group_by(identity_age5) %>%
  summarise(n = n(), mean = mean(anx_age6), sd = sd(anx_age6), min = min(anx_age6), perc25 = quantile(anx_age6, 0.25),
            median = median(anx_age6), perc75 = quantile(anx_age6, 0.75), max = max(anx_age6))

## 2c - Categorical religious attendance by continuous anxiety
data_mum_cca_mh %>%
  group_by(attend_age5) %>%
  summarise(n = n(), mean = mean(anx_age6), sd = sd(anx_age6), min = min(anx_age6), perc25 = quantile(anx_age6, 0.25),
            median = median(anx_age6), perc75 = quantile(anx_age6, 0.75), max = max(anx_age6))

## 3a - Binary religious belief by continuous depression
data_mum_cca_mh %>%
  group_by(belief_age5_bin) %>%
  summarise(n = n(), mean = mean(dep_age6), sd = sd(dep_age6), min = min(dep_age6), perc25 = quantile(dep_age6, 0.25),
            median = median(dep_age6), perc75 = quantile(dep_age6, 0.75), max = max(dep_age6))

## 3b - Binary religious identity by continuous depression
data_mum_cca_mh %>%
  group_by(identity_age5_bin) %>%
  summarise(n = n(), mean = mean(dep_age6), sd = sd(dep_age6), min = min(dep_age6), perc25 = quantile(dep_age6, 0.25),
            median = median(dep_age6), perc75 = quantile(dep_age6, 0.75), max = max(dep_age6))

## 3c - Binary religious attendance by continuous depression
data_mum_cca_mh %>%
  group_by(attend_age5_bin) %>%
  summarise(n = n(), mean = mean(dep_age6), sd = sd(dep_age6), min = min(dep_age6), perc25 = quantile(dep_age6, 0.25),
            median = median(dep_age6), perc75 = quantile(dep_age6, 0.75), max = max(dep_age6))

## 4a - Binary religious belief by continuous anxiety
data_mum_cca_mh %>%
  group_by(belief_age5_bin) %>%
  summarise(n = n(), mean = mean(anx_age6), sd = sd(anx_age6), min = min(anx_age6), perc25 = quantile(anx_age6, 0.25),
            median = median(anx_age6), perc75 = quantile(anx_age6, 0.75), max = max(anx_age6))

## 4b - Binary religious identity by continuous anxiety
data_mum_cca_mh %>%
  group_by(identity_age5_bin) %>%
  summarise(n = n(), mean = mean(anx_age6), sd = sd(anx_age6), min = min(anx_age6), perc25 = quantile(anx_age6, 0.25),
            median = median(anx_age6), perc75 = quantile(anx_age6, 0.75), max = max(anx_age6))

## 4c - Binary religious attendance by continuous anxiety
data_mum_cca_mh %>%
  group_by(attend_age5_bin) %>%
  summarise(n = n(), mean = mean(anx_age6), sd = sd(anx_age6), min = min(anx_age6), perc25 = quantile(anx_age6, 0.25),
            median = median(anx_age6), perc75 = quantile(anx_age6, 0.75), max = max(anx_age6))

## 5a - Categorical religious belief by binary depression
table(data_mum_cca_mh$belief_age5, data_mum_cca_mh$dep_age6_bin)
round(prop.table(table(data_mum_cca_mh$belief_age5, data_mum_cca_mh$dep_age6_bin), margin = 1) * 100, 2)

## 5b - Categorical religious identity by binary depression
table(data_mum_cca_mh$identity_age5, data_mum_cca_mh$dep_age6_bin)
round(prop.table(table(data_mum_cca_mh$identity_age5, data_mum_cca_mh$dep_age6_bin), margin = 1) * 100, 2)

## 5c - Categorical religious attendance by binary depression
table(data_mum_cca_mh$attend_age5, data_mum_cca_mh$dep_age6_bin)
round(prop.table(table(data_mum_cca_mh$attend_age5, data_mum_cca_mh$dep_age6_bin), margin = 1) * 100, 2)

## 6a - Categorical religious belief by binary anxiety
table(data_mum_cca_mh$belief_age5, data_mum_cca_mh$anx_age6_bin)
round(prop.table(table(data_mum_cca_mh$belief_age5, data_mum_cca_mh$anx_age6_bin), margin = 1) * 100, 2)

## 6b - Categorical religious identity by binary anxiety
table(data_mum_cca_mh$identity_age5, data_mum_cca_mh$anx_age6_bin)
round(prop.table(table(data_mum_cca_mh$identity_age5, data_mum_cca_mh$anx_age6_bin), margin = 1) * 100, 2)

## 6c - Categorical religious attendance by binary anxiety
table(data_mum_cca_mh$attend_age5, data_mum_cca_mh$anx_age6_bin)
round(prop.table(table(data_mum_cca_mh$attend_age5, data_mum_cca_mh$anx_age6_bin), margin = 1) * 100, 2)

## 7a - Binary religious belief by binary depression
table(data_mum_cca_mh$belief_age5_bin, data_mum_cca_mh$dep_age6_bin)
round(prop.table(table(data_mum_cca_mh$belief_age5_bin, data_mum_cca_mh$dep_age6_bin), margin = 1) * 100, 2)

## 7b - Binary religious identity by binary depression
table(data_mum_cca_mh$identity_age5_bin, data_mum_cca_mh$dep_age6_bin)
round(prop.table(table(data_mum_cca_mh$identity_age5_bin, data_mum_cca_mh$dep_age6_bin), margin = 1) * 100, 2)

## 7c - Binary religious attendance by binary depression
table(data_mum_cca_mh$attend_age5_bin, data_mum_cca_mh$dep_age6_bin)
round(prop.table(table(data_mum_cca_mh$attend_age5_bin, data_mum_cca_mh$dep_age6_bin), margin = 1) * 100, 2)

## 8a - Binary religious belief by binary anxiety
table(data_mum_cca_mh$belief_age5_bin, data_mum_cca_mh$anx_age6_bin)
round(prop.table(table(data_mum_cca_mh$belief_age5_bin, data_mum_cca_mh$anx_age6_bin), margin = 1) * 100, 2)

## 8b - Binary religious identity by binary anxiety
table(data_mum_cca_mh$identity_age5_bin, data_mum_cca_mh$anx_age6_bin)
round(prop.table(table(data_mum_cca_mh$identity_age5_bin, data_mum_cca_mh$anx_age6_bin), margin = 1) * 100, 2)

## 8c - Binary religious attendance by binary anxiety
table(data_mum_cca_mh$attend_age5_bin, data_mum_cca_mh$anx_age6_bin)
round(prop.table(table(data_mum_cca_mh$attend_age5_bin, data_mum_cca_mh$anx_age6_bin), margin = 1) * 100, 2)




## Make a table to save results to
mum_results_mh <- as.data.frame(cbind(model = c("1a", "1a", "1b", "1b", "1c", "1c", "1c", 
                                                "2a", "2a", "2b", "2b", "2c", "2c", "2c",  
                                                "3a", "3b", "3c", "4a", "4b", "4c", 
                                                "5a", "5a", "5b", "5b", "5c", "5c", "5c", 
                                                "6a", "6a", "6b", "6b", "6c", "6c", "6c",
                                                "7a", "7b", "7c", "8a", "8b", "8c"),
                                   exposure = rep(c("Belief (cat)", "Belief (cat)", "Identity (cat)", "Identity (cat)",
                                                    "Attend (cat)", "Attend (cat)", "Attend (cat)",
                                                    "Belief (cat)", "Belief (cat)", "Identity (cat)", "Identity (cat)",
                                                    "Attend (cat)", "Attend (cat)", "Attend (cat)",
                                                    "Belief (bin)", "Identity (bin)", "Attend (bin)",
                                                    "Belief (bin)", "Identity (bin)", "Attend (bin)"), 2),
                                   level = rep(c("Not sure", "Yes", "Christian", "Other", "1/yr", "1/mth", "1/wk",
                                                 "Not sure", "Yes", "Christian", "Other", "1/yr", "1/mth", "1/wk",
                                                 "Yes", "Religious", "Regular", "Yes", "Religious", "Regular"), 2),
                                   outcome = c("Dep (cont)", "Dep (cont)", "Dep (cont)", "Dep (cont)",
                                               "Dep (cont)", "Dep (cont)", "Dep (cont)",
                                               "Anx (cont)", "Anx (cont)", "Anx (cont)", "Anx (cont)", 
                                               "Anx (cont)", "Anx (cont)", "Anx (cont)",
                                               "Dep (cont)", "Dep (cont)", "Dep (cont)",
                                               "Anx (cont)", "Anx (cont)", "Anx (cont)",
                                               "Dep (bin)", "Dep (bin)", "Dep (bin)", "Dep (bin)",
                                               "Dep (bin)", "Dep (bin)", "Dep (bin)",
                                               "Anx (bin)", "Anx (bin)", "Anx (bin)", "Anx (bin)", 
                                               "Anx (bin)", "Anx (bin)", "Anx (bin)",
                                               "Dep (bin)", "Dep (bin)", "Dep (bin)", 
                                               "Anx (bin)", "Anx (bin)", "Anx (bin)"),
                                   est_unadj = rep(NA, 40), 
                                   lci_unadj = rep(NA, 40),
                                   uci_unadj = rep(NA, 40),
                                   est_unadj_z = rep(NA, 40), 
                                   lci_unadj_z = rep(NA, 40),
                                   uci_unadj_z = rep(NA, 40),
                                   p_unadj = rep(NA, 40),
                                   est_adj = rep(NA, 40), 
                                   lci_adj = rep(NA, 40),
                                   uci_adj = rep(NA, 40),
                                   est_adj_z = rep(NA, 40), 
                                   lci_adj_z = rep(NA, 40),
                                   uci_adj_z = rep(NA, 40),
                                   p_adj = rep(NA, 40)
))
mum_results_mh


## Also make a table for the predicted differences in the probability of being depressed/anxious for logistic models
mum_results_mh_probs <- as.data.frame(cbind(model = c("5a", "5a", "5b", "5b", "5c", "5c", "5c", 
                                                      "6a", "6a", "6b", "6b", "6c", "6c", "6c",
                                                      "7a", "7b", "7c", "8a", "8b", "8c"),
                                            exposure = c("Belief (cat)", "Belief (cat)", "Identity (cat)", 
                                                         "Identity (cat)", "Attend (cat)", "Attend (cat)", 
                                                         "Attend (cat)", "Belief (cat)", "Belief (cat)", 
                                                         "Identity (cat)", "Identity (cat)", "Attend (cat)", 
                                                         "Attend (cat)", "Attend (cat)", "Belief (bin)", 
                                                         "Identity (bin)", "Attend (bin)", "Belief (bin)", 
                                                         "Identity (bin)", "Attend (bin)"),
                                            level = c("Not sure", "Yes", "Christian", "Other", "1/yr", 
                                                      "1/mth", "1/wk", "Not sure", "Yes", "Christian", "Other", 
                                                      "1/yr", "1/mth", "1/wk", "Yes", "Religious", 
                                                      "Regular", "Yes", "Religious", "Regular"),
                                            outcome = c("Dep (bin)", "Dep (bin)", "Dep (bin)", "Dep (bin)",
                                                        "Dep (bin)", "Dep (bin)", "Dep (bin)",
                                                        "Anx (bin)", "Anx (bin)", "Anx (bin)", "Anx (bin)", 
                                                        "Anx (bin)", "Anx (bin)", "Anx (bin)",
                                                        "Dep (bin)", "Dep (bin)", "Dep (bin)", 
                                                        "Anx (bin)", "Anx (bin)", "Anx (bin)"),
                                            est_unadj = rep(NA, 20), 
                                            lci_unadj = rep(NA, 20),
                                            uci_unadj = rep(NA, 20),
                                            est_adj = rep(NA, 20), 
                                            lci_adj = rep(NA, 20),
                                            uci_adj = rep(NA, 20)
))
mum_results_mh_probs



#################################################################################
### Analysis 1: Continuous depression as outcome, with categorical RSBB variables

## 1a: Religious belief

# Unadjusted - raw and standardised scales
mod_1a_unadj <- lm(dep_age6 ~ belief_age5, data = data_mum_cca_mh)
summary(mod_1a_unadj)

mod_1a_unadj_z <- lm(dep_age6_z ~ belief_age5, data = data_mum_cca_mh)
summary(mod_1a_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_1a_unadj))["belief_age5Not sure", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_1a_unadj)["belief_age5Not sure", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_1a_unadj)["belief_age5Not sure", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_1a_unadj_z))["belief_age5Not sure", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_1a_unadj_z)["belief_age5Not sure", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_1a_unadj_z)["belief_age5Not sure", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_1a_unadj))["belief_age5Not sure", "Pr(>|t|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_1a_unadj))["belief_age5Yes", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_1a_unadj)["belief_age5Yes", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_1a_unadj)["belief_age5Yes", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_1a_unadj_z))["belief_age5Yes", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_1a_unadj_z)["belief_age5Yes", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_1a_unadj_z)["belief_age5Yes", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_1a_unadj))["belief_age5Yes", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_1a_adj <- lm(dep_age6 ~ belief_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_1a_adj)

mod_1a_adj_z <- lm(dep_age6_z ~ belief_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_1a_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_1a_adj))["belief_age5Not sure", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_1a_adj)["belief_age5Not sure", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_1a_adj)["belief_age5Not sure", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_1a_adj_z))["belief_age5Not sure", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_1a_adj_z)["belief_age5Not sure", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_1a_adj_z)["belief_age5Not sure", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "1a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_1a_adj))["belief_age5Not sure", "Pr(>|t|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_1a_adj))["belief_age5Yes", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_1a_adj)["belief_age5Yes", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_1a_adj)["belief_age5Yes", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_1a_adj_z))["belief_age5Yes", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_1a_adj_z)["belief_age5Yes", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_1a_adj_z)["belief_age5Yes", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "1a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_1a_adj))["belief_age5Yes", "Pr(>|t|)"], 4)



## 1b: Religious identity

# Unadjusted - raw and standardised scales
mod_1b_unadj <- lm(dep_age6 ~ identity_age5, data = data_mum_cca_mh)
summary(mod_1b_unadj)

mod_1b_unadj_z <- lm(dep_age6_z ~ identity_age5, data = data_mum_cca_mh)
summary(mod_1b_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_1b_unadj))["identity_age5Christian", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_1b_unadj)["identity_age5Christian", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_1b_unadj)["identity_age5Christian", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_1b_unadj_z))["identity_age5Christian", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_1b_unadj_z)["identity_age5Christian", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_1b_unadj_z)["identity_age5Christian", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_1b_unadj))["identity_age5Christian", "Pr(>|t|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_1b_unadj))["identity_age5Other", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_1b_unadj)["identity_age5Other", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_1b_unadj)["identity_age5Other", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_1b_unadj_z))["identity_age5Other", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_1b_unadj_z)["identity_age5Other", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_1b_unadj_z)["identity_age5Other", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_1b_unadj))["identity_age5Other", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_1b_adj <- lm(dep_age6 ~ identity_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_1b_adj)

mod_1b_adj_z <- lm(dep_age6_z ~ identity_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_1b_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_1b_adj))["identity_age5Christian", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_1b_adj)["identity_age5Christian", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_1b_adj)["identity_age5Christian", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_1b_adj_z))["identity_age5Christian", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_1b_adj_z)["identity_age5Christian", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_1b_adj_z)["identity_age5Christian", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "1b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_1b_adj))["identity_age5Christian", "Pr(>|t|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_1b_adj))["identity_age5Other", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_1b_adj)["identity_age5Other", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_1b_adj)["identity_age5Other", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_1b_adj_z))["identity_age5Other", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_1b_adj_z)["identity_age5Other", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_1b_adj_z)["identity_age5Other", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "1b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_1b_adj))["identity_age5Other", "Pr(>|t|)"], 4)


## As is a small association here - with religious identities associated with slightly lower depression scores - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from continuous to binary outcomes (see ?evalues.OLS).

## Christian
# First using SD of outcome (may be conservative)
evalues.OLS(est = coef(summary(mod_1b_adj))["identity_age5Christian", "Estimate"], 
            se = coef(summary(mod_1b_adj))["identity_age5Christian", "Std. Error"],
            sd = sd(data_mum_cca_mh$dep_age6), true = 0)

# Or using residual SD from model to avoid conservatism
evalues.OLS(est = coef(summary(mod_1b_adj))["identity_age5Christian", "Estimate"], 
            se = coef(summary(mod_1b_adj))["identity_age5Christian", "Std. Error"],
            sd = summary(mod_1b_adj)$sigma, true = 0)

# For both, E-value is approximately 40% (1.38 using outcome SD; 1.41 using residual model SD), meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by about 40% is necessary to completely remove the observed association. This doesn't seem particularly large, especially as 95% CI already crosses the null - Although have adjusted for a wide range of confounders... 


## OTher religion
# First using SD of outcome (may be conservative)
evalues.OLS(est = coef(summary(mod_1b_adj))["identity_age5Other", "Estimate"], 
            se = coef(summary(mod_1b_adj))["identity_age5Other", "Std. Error"],
            sd = sd(data_mum_cca_mh$dep_age6), true = 0)

# Or using residual SD from model to avoid conservatism
evalues.OLS(est = coef(summary(mod_1b_adj))["identity_age5Other", "Estimate"], 
            se = coef(summary(mod_1b_adj))["identity_age5Other", "Std. Error"],
            sd = summary(mod_1b_adj)$sigma, true = 0)

# For both, E-value is approximately 75% (1.70 using outcome SD; 1.78 using residual model SD), meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by about 75% is necessary to completely remove the observed association. While this is larger than the E-value for Christian affiliation above, the 95% CI already crosses the null - Plus, sample sizes for 'other' religion are smaller, so more prone to random variation...



## 1c: Religious attendance

# Unadjusted - raw and standardised scales
mod_1c_unadj <- lm(dep_age6 ~ attend_age5, data = data_mum_cca_mh)
summary(mod_1c_unadj)

mod_1c_unadj_z <- lm(dep_age6_z ~ attend_age5, data = data_mum_cca_mh)
summary(mod_1c_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_1c_unadj))["attend_age5MIN 1 a YR", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_1c_unadj)["attend_age5MIN 1 a YR", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_1c_unadj)["attend_age5MIN 1 a YR", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_1c_unadj_z))["attend_age5MIN 1 a YR", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_1c_unadj_z)["attend_age5MIN 1 a YR", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_1c_unadj_z)["attend_age5MIN 1 a YR", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_1c_unadj))["attend_age5MIN 1 a YR", "Pr(>|t|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_1c_unadj))["attend_age5MIN 1 a MTH", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_1c_unadj)["attend_age5MIN 1 a MTH", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_1c_unadj)["attend_age5MIN 1 a MTH", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_1c_unadj_z))["attend_age5MIN 1 a MTH", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_1c_unadj_z)["attend_age5MIN 1 a MTH", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_1c_unadj_z)["attend_age5MIN 1 a MTH", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_1c_unadj))["attend_age5MIN 1 a MTH", "Pr(>|t|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_1c_unadj))["attend_age5MIN 1 a WK", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_1c_unadj)["attend_age5MIN 1 a WK", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_1c_unadj)["attend_age5MIN 1 a WK", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_1c_unadj_z))["attend_age5MIN 1 a WK", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_1c_unadj_z)["attend_age5MIN 1 a WK", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_1c_unadj_z)["attend_age5MIN 1 a WK", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_1c_unadj))["attend_age5MIN 1 a WK", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_1c_adj <- lm(dep_age6 ~ attend_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_1c_adj)

mod_1c_adj_z <- lm(dep_age6_z ~ attend_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_1c_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_1c_adj))["attend_age5MIN 1 a YR", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_1c_adj)["attend_age5MIN 1 a YR", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_1c_adj)["attend_age5MIN 1 a YR", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_1c_adj_z))["attend_age5MIN 1 a YR", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_1c_adj_z)["attend_age5MIN 1 a YR", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_1c_adj_z)["attend_age5MIN 1 a YR", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_1c_adj))["attend_age5MIN 1 a YR", "Pr(>|t|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_1c_adj))["attend_age5MIN 1 a MTH", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_1c_adj)["attend_age5MIN 1 a MTH", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_1c_adj)["attend_age5MIN 1 a MTH", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_1c_adj_z))["attend_age5MIN 1 a MTH", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_1c_adj_z)["attend_age5MIN 1 a MTH", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_1c_adj_z)["attend_age5MIN 1 a MTH", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_1c_adj))["attend_age5MIN 1 a MTH", "Pr(>|t|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_1c_adj))["attend_age5MIN 1 a WK", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_1c_adj)["attend_age5MIN 1 a WK", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_1c_adj)["attend_age5MIN 1 a WK", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_1c_adj_z))["attend_age5MIN 1 a WK", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_1c_adj_z)["attend_age5MIN 1 a WK", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_1c_adj_z)["attend_age5MIN 1 a WK", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "1c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_1c_adj))["attend_age5MIN 1 a WK", "Pr(>|t|)"], 4)



#################################################################################
### Analysis 2: Continuous anxiety as outcome, with categorical RSBB variables

## 2a: Religious belief

# Unadjusted - raw and standardised scales
mod_2a_unadj <- lm(anx_age6 ~ belief_age5, data = data_mum_cca_mh)
summary(mod_2a_unadj)

mod_2a_unadj_z <- lm(anx_age6_z ~ belief_age5, data = data_mum_cca_mh)
summary(mod_2a_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_2a_unadj))["belief_age5Not sure", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_2a_unadj)["belief_age5Not sure", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_2a_unadj)["belief_age5Not sure", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_2a_unadj_z))["belief_age5Not sure", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_2a_unadj_z)["belief_age5Not sure", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_2a_unadj_z)["belief_age5Not sure", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_2a_unadj))["belief_age5Not sure", "Pr(>|t|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_2a_unadj))["belief_age5Yes", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_2a_unadj)["belief_age5Yes", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_2a_unadj)["belief_age5Yes", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_2a_unadj_z))["belief_age5Yes", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_2a_unadj_z)["belief_age5Yes", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_2a_unadj_z)["belief_age5Yes", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_2a_unadj))["belief_age5Yes", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_2a_adj <- lm(anx_age6 ~ belief_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_2a_adj)

mod_2a_adj_z <- lm(anx_age6_z ~ belief_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_2a_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_2a_adj))["belief_age5Not sure", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_2a_adj)["belief_age5Not sure", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_2a_adj)["belief_age5Not sure", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_2a_adj_z))["belief_age5Not sure", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_2a_adj_z)["belief_age5Not sure", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(confint(mod_2a_adj_z)["belief_age5Not sure", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "2a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_2a_adj))["belief_age5Not sure", "Pr(>|t|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_2a_adj))["belief_age5Yes", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_2a_adj)["belief_age5Yes", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_2a_adj)["belief_age5Yes", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_2a_adj_z))["belief_age5Yes", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_2a_adj_z)["belief_age5Yes", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_2a_adj_z)["belief_age5Yes", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "2a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_2a_adj))["belief_age5Yes", "Pr(>|t|)"], 4)


## 2b: Religious identity

# Unadjusted - raw and standardised scales
mod_2b_unadj <- lm(anx_age6 ~ identity_age5, data = data_mum_cca_mh)
summary(mod_2b_unadj)

mod_2b_unadj_z <- lm(anx_age6_z ~ identity_age5, data = data_mum_cca_mh)
summary(mod_2b_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_2b_unadj))["identity_age5Christian", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_2b_unadj)["identity_age5Christian", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_2b_unadj)["identity_age5Christian", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_2b_unadj_z))["identity_age5Christian", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_2b_unadj_z)["identity_age5Christian", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_2b_unadj_z)["identity_age5Christian", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_2b_unadj))["identity_age5Christian", "Pr(>|t|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_2b_unadj))["identity_age5Other", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_2b_unadj)["identity_age5Other", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_2b_unadj)["identity_age5Other", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_2b_unadj_z))["identity_age5Other", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_2b_unadj_z)["identity_age5Other", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_2b_unadj_z)["identity_age5Other", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_2b_unadj))["identity_age5Other", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_2b_adj <- lm(anx_age6 ~ identity_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_2b_adj)

mod_2b_adj_z <- lm(anx_age6_z ~ identity_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_2b_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_2b_adj))["identity_age5Christian", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_2b_adj)["identity_age5Christian", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_2b_adj)["identity_age5Christian", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_2b_adj_z))["identity_age5Christian", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_2b_adj_z)["identity_age5Christian", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(confint(mod_2b_adj_z)["identity_age5Christian", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "2b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_2b_adj))["identity_age5Christian", "Pr(>|t|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_2b_adj))["identity_age5Other", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_2b_adj)["identity_age5Other", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_2b_adj)["identity_age5Other", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_2b_adj_z))["identity_age5Other", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_2b_adj_z)["identity_age5Other", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(confint(mod_2b_adj_z)["identity_age5Other", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "2b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_2b_adj))["identity_age5Other", "Pr(>|t|)"], 4)



## 2c: Religious attendance

# Unadjusted - raw and standardised scales
mod_2c_unadj <- lm(anx_age6 ~ attend_age5, data = data_mum_cca_mh)
summary(mod_2c_unadj)

mod_2c_unadj_z <- lm(anx_age6_z ~ attend_age5, data = data_mum_cca_mh)
summary(mod_2c_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_2c_unadj))["attend_age5MIN 1 a YR", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_2c_unadj)["attend_age5MIN 1 a YR", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_2c_unadj)["attend_age5MIN 1 a YR", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_2c_unadj_z))["attend_age5MIN 1 a YR", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_2c_unadj_z)["attend_age5MIN 1 a YR", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_2c_unadj_z)["attend_age5MIN 1 a YR", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_2c_unadj))["attend_age5MIN 1 a YR", "Pr(>|t|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_2c_unadj))["attend_age5MIN 1 a MTH", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_2c_unadj)["attend_age5MIN 1 a MTH", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_2c_unadj)["attend_age5MIN 1 a MTH", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_2c_unadj_z))["attend_age5MIN 1 a MTH", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_2c_unadj_z)["attend_age5MIN 1 a MTH", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_2c_unadj_z)["attend_age5MIN 1 a MTH", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_2c_unadj))["attend_age5MIN 1 a MTH", "Pr(>|t|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_2c_unadj))["attend_age5MIN 1 a WK", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_2c_unadj)["attend_age5MIN 1 a WK", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_2c_unadj)["attend_age5MIN 1 a WK", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_2c_unadj_z))["attend_age5MIN 1 a WK", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_2c_unadj_z)["attend_age5MIN 1 a WK", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_2c_unadj_z)["attend_age5MIN 1 a WK", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_2c_unadj))["attend_age5MIN 1 a WK", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_2c_adj <- lm(anx_age6 ~ attend_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_2c_adj)

mod_2c_adj_z <- lm(anx_age6_z ~ attend_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_2c_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_2c_adj))["attend_age5MIN 1 a YR", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_2c_adj)["attend_age5MIN 1 a YR", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_2c_adj)["attend_age5MIN 1 a YR", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_2c_adj_z))["attend_age5MIN 1 a YR", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_2c_adj_z)["attend_age5MIN 1 a YR", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(confint(mod_2c_adj_z)["attend_age5MIN 1 a YR", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_2c_adj))["attend_age5MIN 1 a YR", "Pr(>|t|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_2c_adj))["attend_age5MIN 1 a MTH", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_2c_adj)["attend_age5MIN 1 a MTH", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_2c_adj)["attend_age5MIN 1 a MTH", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_2c_adj_z))["attend_age5MIN 1 a MTH", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_2c_adj_z)["attend_age5MIN 1 a MTH", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(confint(mod_2c_adj_z)["attend_age5MIN 1 a MTH", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_2c_adj))["attend_age5MIN 1 a MTH", "Pr(>|t|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_2c_adj))["attend_age5MIN 1 a WK", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_2c_adj)["attend_age5MIN 1 a WK", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_2c_adj)["attend_age5MIN 1 a WK", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_2c_adj_z))["attend_age5MIN 1 a WK", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_2c_adj_z)["attend_age5MIN 1 a WK", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(confint(mod_2c_adj_z)["attend_age5MIN 1 a WK", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "2c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_2c_adj))["attend_age5MIN 1 a WK", "Pr(>|t|)"], 4)



#################################################################################
### Analysis 3: Continuous depression as outcome, with binary RSBB variables

## 3a: Religious belief

# Unadjusted - raw and standardised scales
mod_3a_unadj <- lm(dep_age6 ~ belief_age5_bin, data = data_mum_cca_mh)
summary(mod_3a_unadj)

mod_3a_unadj_z <- lm(dep_age6_z ~ belief_age5_bin, data = data_mum_cca_mh)
summary(mod_3a_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_3a_unadj))["belief_age5_binYes", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_3a_unadj)["belief_age5_binYes", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_3a_unadj)["belief_age5_binYes", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_3a_unadj_z))["belief_age5_binYes", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_3a_unadj_z)["belief_age5_binYes", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_3a_unadj_z)["belief_age5_binYes", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_3a_unadj))["belief_age5_binYes", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_3a_adj <- lm(dep_age6 ~ belief_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_3a_adj)

mod_3a_adj_z <- lm(dep_age6_z ~ belief_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                     age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                     carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                     socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_3a_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_3a_adj))["belief_age5_binYes", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_3a_adj)["belief_age5_binYes", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_3a_adj)["belief_age5_binYes", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_3a_adj_z))["belief_age5_binYes", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_3a_adj_z)["belief_age5_binYes", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_3a_adj_z)["belief_age5_binYes", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "3a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_3a_adj))["belief_age5_binYes", "Pr(>|t|)"], 4)


## As is a small association here - with religious believers associated with slightly lower depression scores - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from continuous to binary outcomes (see ?evalues.OLS).

# First using SD of outcome (may be conservative)
evalues.OLS(est = coef(summary(mod_3a_adj))["belief_age5_binYes", "Estimate"], 
            se = coef(summary(mod_3a_adj))["belief_age5_binYes", "Std. Error"],
            sd = sd(data_mum_cca_mh$dep_age6), true = 0)

# Or using residual SD from model to avoid conservatism
evalues.OLS(est = coef(summary(mod_3a_adj))["belief_age5_binYes", "Estimate"], 
            se = coef(summary(mod_3a_adj))["belief_age5_binYes", "Std. Error"],
            sd = summary(mod_3a_adj)$sigma, true = 0)

# For both, E-value is approximately 30% (1.32 using outcome SD; 1.34 using residual model SD), meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by about 30% is necessary to completely remove the observed association. This doesn't seem particularly large, especially as 95% CI already crosses the null - Although have adjusted for a wide range of confounders... 



## 3b: Religious identity

# Unadjusted - raw and standardised scales
mod_3b_unadj <- lm(dep_age6 ~ identity_age5_bin, data = data_mum_cca_mh)
summary(mod_3b_unadj)

mod_3b_unadj_z <- lm(dep_age6_z ~ identity_age5_bin, data = data_mum_cca_mh)
summary(mod_3b_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_3b_unadj))["identity_age5_binReligious", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_3b_unadj)["identity_age5_binReligious", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_3b_unadj)["identity_age5_binReligious", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_3b_unadj_z))["identity_age5_binReligious", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_3b_unadj_z)["identity_age5_binReligious", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_3b_unadj_z)["identity_age5_binReligious", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_3b_unadj))["identity_age5_binReligious", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_3b_adj <- lm(dep_age6 ~ identity_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_3b_adj)

mod_3b_adj_z <- lm(dep_age6_z ~ identity_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                     age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                     carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                     socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_3b_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_3b_adj))["identity_age5_binReligious", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_3b_adj)["identity_age5_binReligious", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_3b_adj)["identity_age5_binReligious", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_3b_adj_z))["identity_age5_binReligious", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_3b_adj_z)["identity_age5_binReligious", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_3b_adj_z)["identity_age5_binReligious", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "3b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_3b_adj))["identity_age5_binReligious", "Pr(>|t|)"], 4)


## As is a small association here - with religious identity associated with slightly lower depression scores - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from continuous to binary outcomes (see ?evalues.OLS).

# First using SD of outcome (may be conservative)
evalues.OLS(est = coef(summary(mod_3b_adj))["identity_age5_binReligious", "Estimate"], 
            se = coef(summary(mod_3b_adj))["identity_age5_binReligious", "Std. Error"],
            sd = sd(data_mum_cca_mh$dep_age6), true = 0)

# Or using residual SD from model to avoid conservatism
evalues.OLS(est = coef(summary(mod_3b_adj))["identity_age5_binReligious", "Estimate"], 
            se = coef(summary(mod_3b_adj))["identity_age5_binReligious", "Std. Error"],
            sd = summary(mod_3b_adj)$sigma, true = 0)

# For both, E-value is approximately 40% (1.41 using outcome SD; 1.44 using residual model SD), meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by about 40% is necessary to completely remove the observed association.



## 3c: Religious attendance

# Unadjusted - raw and standardised scales
mod_3c_unadj <- lm(dep_age6 ~ attend_age5_bin, data = data_mum_cca_mh)
summary(mod_3c_unadj)

mod_3c_unadj_z <- lm(dep_age6_z ~ attend_age5_bin, data = data_mum_cca_mh)
summary(mod_3c_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_3c_unadj))["attend_age5_binRegular", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_3c_unadj)["attend_age5_binRegular", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_3c_unadj)["attend_age5_binRegular", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_3c_unadj_z))["attend_age5_binRegular", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_3c_unadj_z)["attend_age5_binRegular", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_3c_unadj_z)["attend_age5_binRegular", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_3c_unadj))["attend_age5_binRegular", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_3c_adj <- lm(dep_age6 ~ attend_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_3c_adj)

mod_3c_adj_z <- lm(dep_age6_z ~ attend_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                     age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                     carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                     socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_3c_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_3c_adj))["attend_age5_binRegular", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_3c_adj)["attend_age5_binRegular", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_3c_adj)["attend_age5_binRegular", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_3c_adj_z))["attend_age5_binRegular", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_3c_adj_z)["attend_age5_binRegular", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_3c_adj_z)["attend_age5_binRegular", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "3c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_3c_adj))["attend_age5_binRegular", "Pr(>|t|)"], 4)


## As is a small association here - with religious attendance associated with slightly lower depression scores - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from continuous to binary outcomes (see ?evalues.OLS).

# First using SD of outcome (may be conservative)
evalues.OLS(est = coef(summary(mod_3c_adj))["attend_age5_binRegular", "Estimate"], 
            se = coef(summary(mod_3c_adj))["attend_age5_binRegular", "Std. Error"],
            sd = sd(data_mum_cca_mh$dep_age6), true = 0)

# Or using residual SD from model to avoid conservatism
evalues.OLS(est = coef(summary(mod_3c_adj))["attend_age5_binRegular", "Estimate"], 
            se = coef(summary(mod_3c_adj))["attend_age5_binRegular", "Std. Error"],
            sd = summary(mod_3c_adj)$sigma, true = 0)

# For both, E-value is approximately 35% (1.35 using outcome SD; 1.38 using residual model SD), meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by about 35% is necessary to completely remove the observed association.



#################################################################################
### Analysis 4: Continuous anxiety as outcome, with binary RSBB variables

## 4a: Religious belief

# Unadjusted - raw and standardised scales
mod_4a_unadj <- lm(anx_age6 ~ belief_age5_bin, data = data_mum_cca_mh)
summary(mod_4a_unadj)

mod_4a_unadj_z <- lm(anx_age6_z ~ belief_age5_bin, data = data_mum_cca_mh)
summary(mod_4a_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_4a_unadj))["belief_age5_binYes", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_4a_unadj)["belief_age5_binYes", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_4a_unadj)["belief_age5_binYes", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_4a_unadj_z))["belief_age5_binYes", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_4a_unadj_z)["belief_age5_binYes", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_4a_unadj_z)["belief_age5_binYes", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_4a_unadj))["belief_age5_binYes", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_4a_adj <- lm(anx_age6 ~ belief_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_4a_adj)

mod_4a_adj_z <- lm(anx_age6_z ~ belief_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                     age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                     carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                     socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_4a_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_4a_adj))["belief_age5_binYes", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_4a_adj)["belief_age5_binYes", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_4a_adj)["belief_age5_binYes", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_4a_adj_z))["belief_age5_binYes", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_4a_adj_z)["belief_age5_binYes", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(confint(mod_4a_adj_z)["belief_age5_binYes", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "4a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_4a_adj))["belief_age5_binYes", "Pr(>|t|)"], 4)



## 4b: Religious identity

# Unadjusted - raw and standardised scales
mod_4b_unadj <- lm(anx_age6 ~ identity_age5_bin, data = data_mum_cca_mh)
summary(mod_4b_unadj)

mod_4b_unadj_z <- lm(anx_age6_z ~ identity_age5_bin, data = data_mum_cca_mh)
summary(mod_4b_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_4b_unadj))["identity_age5_binReligious", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_4b_unadj)["identity_age5_binReligious", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_4b_unadj)["identity_age5_binReligious", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_4b_unadj_z))["identity_age5_binReligious", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_4b_unadj_z)["identity_age5_binReligious", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_4b_unadj_z)["identity_age5_binReligious", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_4b_unadj))["identity_age5_binReligious", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_4b_adj <- lm(anx_age6 ~ identity_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_4b_adj)

mod_4b_adj_z <- lm(anx_age6_z ~ identity_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                     age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                     carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                     socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_4b_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_4b_adj))["identity_age5_binReligious", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_4b_adj)["identity_age5_binReligious", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_4b_adj)["identity_age5_binReligious", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_4b_adj_z))["identity_age5_binReligious", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_4b_adj_z)["identity_age5_binReligious", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(confint(mod_4b_adj_z)["identity_age5_binReligious", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "4b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_4b_adj))["identity_age5_binReligious", "Pr(>|t|)"], 4)



## 4c: Religious attendance

# Unadjusted - raw and standardised scales
mod_4c_unadj <- lm(anx_age6 ~ attend_age5_bin, data = data_mum_cca_mh)
summary(mod_4c_unadj)

mod_4c_unadj_z <- lm(anx_age6_z ~ attend_age5_bin, data = data_mum_cca_mh)
summary(mod_4c_unadj_z)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_4c_unadj))["attend_age5_binRegular", "Estimate"], 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_4c_unadj)["attend_age5_binRegular", "2.5 %"], 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_4c_unadj)["attend_age5_binRegular", "97.5 %"], 3)

mum_results_mh$est_unadj_z[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_4c_unadj_z))["attend_age5_binRegular", "Estimate"], 3)

mum_results_mh$lci_unadj_z[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_4c_unadj_z)["attend_age5_binRegular", "2.5 %"], 3)

mum_results_mh$uci_unadj_z[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_4c_unadj_z)["attend_age5_binRegular", "97.5 %"], 3)

mum_results_mh$p_unadj[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_4c_unadj))["attend_age5_binRegular", "Pr(>|t|)"], 4)


# Adjusted - raw and standardised scales
mod_4c_adj <- lm(anx_age6 ~ attend_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_4c_adj)

mod_4c_adj_z <- lm(anx_age6_z ~ attend_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                     age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                     carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                     socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh)
summary(mod_4c_adj_z)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_4c_adj))["attend_age5_binRegular", "Estimate"], 3)

mum_results_mh$lci_adj[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_4c_adj)["attend_age5_binRegular", "2.5 %"], 3)

mum_results_mh$uci_adj[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_4c_adj)["attend_age5_binRegular", "97.5 %"], 3)

mum_results_mh$est_adj_z[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_4c_adj_z))["attend_age5_binRegular", "Estimate"], 3)

mum_results_mh$lci_adj_z[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_4c_adj_z)["attend_age5_binRegular", "2.5 %"], 3)

mum_results_mh$uci_adj_z[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(confint(mod_4c_adj_z)["attend_age5_binRegular", "97.5 %"], 3)

mum_results_mh$p_adj[mum_results_mh$model == "4c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_4c_adj))["attend_age5_binRegular", "Pr(>|t|)"], 4)


## As is a small association here - with religious attendance associated with slightly lower depression scores - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from continuous to binary outcomes (see ?evalues.OLS).

# First using SD of outcome (may be conservative)
evalues.OLS(est = coef(summary(mod_4c_adj))["attend_age5_binRegular", "Estimate"], 
            se = coef(summary(mod_4c_adj))["attend_age5_binRegular", "Std. Error"],
            sd = sd(data_mum_cca_mh$anx_age6), true = 0)

# Or using residual SD from model to avoid conservatism
evalues.OLS(est = coef(summary(mod_4c_adj))["attend_age5_binRegular", "Estimate"], 
            se = coef(summary(mod_4c_adj))["attend_age5_binRegular", "Std. Error"],
            sd = summary(mod_4c_adj)$sigma, true = 0)

# For both, E-value is approximately 35% (1.35 using outcome SD; 1.39 using residual model SD), meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by about 35% is necessary to completely remove the observed association.




#################################################################################
### Analysis 5: Binary depression as outcome, with categorical RSBB variables

## 5a: Religious belief

# Unadjusted
mod_5a_unadj <- glm(dep_age6_bin ~ belief_age5, data = data_mum_cca_mh, family = "binomial")
summary(mod_5a_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "5a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(coef(summary(mod_5a_unadj))["belief_age5Not sure", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "5a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(confint(mod_5a_unadj)["belief_age5Not sure", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "5a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(confint(mod_5a_unadj)["belief_age5Not sure", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "5a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_5a_unadj))["belief_age5Not sure", "Pr(>|z|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "5a" & mum_results_mh$level == "Yes"] <- 
  round(exp(coef(summary(mod_5a_unadj))["belief_age5Yes", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "5a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_5a_unadj)["belief_age5Yes", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "5a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_5a_unadj)["belief_age5Yes", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "5a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_5a_unadj))["belief_age5Yes", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_5a_unadj, variable = "belief_age5")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_5a_unadj, variable = "belief_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_5a_unadj, variable = "belief_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_5a_unadj, variable = "belief_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_5a_unadj, variable = "belief_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_5a_unadj, variable = "belief_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_5a_unadj, variable = "belief_age5")$conf.high[2] * 100, 2)


# Adjusted
mod_5a_adj <- glm(dep_age6_bin ~ belief_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_5a_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "5a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(coef(summary(mod_5a_adj))["belief_age5Not sure", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "5a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(confint(mod_5a_adj)["belief_age5Not sure", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "5a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(confint(mod_5a_adj)["belief_age5Not sure", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "5a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_5a_adj))["belief_age5Not sure", "Pr(>|z|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "5a" & mum_results_mh$level == "Yes"] <- 
  round(exp(coef(summary(mod_5a_adj))["belief_age5Yes", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "5a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_5a_adj)["belief_age5Yes", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "5a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_5a_adj)["belief_age5Yes", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "5a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_5a_adj))["belief_age5Yes", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_5a_adj, variable = "belief_age5")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_5a_adj, variable = "belief_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_5a_adj, variable = "belief_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_5a_adj, variable = "belief_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_5a_adj, variable = "belief_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_5a_adj, variable = "belief_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "5a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_5a_adj, variable = "belief_age5")$conf.high[2] * 100, 2)


## As is a small association here - with religious belief associated with slightly lower rates of depression - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from odds ratios to risk ratios - As rates of depression ~11%, will use the 'rare = TRUE' option (see ?evalues.OR).

evalues.OR(est = 0.651, lo = 0.435, hi = 0.978, true = 1, rare = TRUE)

# E-value to null is 2.44, meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by two and a half times is necessary to completely remove the observed association - However, to reduce association so is no longer 'significant' (at 95% CI), requires only 17% increase in risk of both exposure and outcome



## 5b: Religious identity

# Unadjusted
mod_5b_unadj <- glm(dep_age6_bin ~ identity_age5, data = data_mum_cca_mh, family = "binomial")
summary(mod_5b_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "5b" & mum_results_mh$level == "Christian"] <- 
  round(exp(coef(summary(mod_5b_unadj))["identity_age5Christian", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "5b" & mum_results_mh$level == "Christian"] <- 
  round(exp(confint(mod_5b_unadj)["identity_age5Christian", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "5b" & mum_results_mh$level == "Christian"] <- 
  round(exp(confint(mod_5b_unadj)["identity_age5Christian", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "5b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_5b_unadj))["identity_age5Christian", "Pr(>|z|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "5b" & mum_results_mh$level == "Other"] <- 
  round(exp(coef(summary(mod_5b_unadj))["identity_age5Other", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "5b" & mum_results_mh$level == "Other"] <- 
  round(exp(confint(mod_5b_unadj)["identity_age5Other", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "5b" & mum_results_mh$level == "Other"] <- 
  round(exp(confint(mod_5b_unadj)["identity_age5Other", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "5b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_5b_unadj))["identity_age5Other", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_5b_unadj, variable = "identity_age5")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_5b_unadj, variable = "identity_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_5b_unadj, variable = "identity_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_5b_unadj, variable = "identity_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_5b_unadj, variable = "identity_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_5b_unadj, variable = "identity_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_5b_unadj, variable = "identity_age5")$conf.high[2] * 100, 2)


# Adjusted
mod_5b_adj <- glm(dep_age6_bin ~ identity_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_5b_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "5b" & mum_results_mh$level == "Christian"] <- 
  round(exp(coef(summary(mod_5b_adj))["identity_age5Christian", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "5b" & mum_results_mh$level == "Christian"] <- 
  round(exp(confint(mod_5b_adj)["identity_age5Christian", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "5b" & mum_results_mh$level == "Christian"] <- 
  round(exp(confint(mod_5b_adj)["identity_age5Christian", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "5b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_5b_adj))["identity_age5Christian", "Pr(>|z|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "5b" & mum_results_mh$level == "Other"] <- 
  round(exp(coef(summary(mod_5b_adj))["identity_age5Other", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "5b" & mum_results_mh$level == "Other"] <- 
  round(exp(confint(mod_5b_adj)["identity_age5Other", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "5b" & mum_results_mh$level == "Other"] <- 
  round(exp(confint(mod_5b_adj)["identity_age5Other", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "5b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_5b_adj))["identity_age5Other", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_5b_adj, variable = "identity_age5")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_5b_adj, variable = "identity_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_5b_adj, variable = "identity_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_5b_adj, variable = "identity_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_5b_adj, variable = "identity_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_5b_adj, variable = "identity_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "5b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_5b_adj, variable = "identity_age5")$conf.high[2] * 100, 2)


## As is a small association here - with religious identity associated with slightly lower rates of depression - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from odds ratios to risk ratios - As rates of depression ~11%, will use the 'rare = TRUE' option (see ?evalues.OR).

# First for Christian affiliation
evalues.OR(est = 0.673, lo = 0.464, hi = 0.984, true = 1, rare = TRUE)

# E-value to null is 2.34, meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by nearly two and a half times is necessary to completely remove the observed association - However, to reduce association so is no longer 'significant' (at 95% CI), requires only 14% increase in risk of both exposure and outcome

# NExt for for other religious affiliation
evalues.OR(est = 0.527, lo = 0.234, hi = 1.112, true = 1, rare = TRUE)

# E-value to null is 3.20, meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by over three times is necessary to completely remove the observed association - However, upper 95% CI covers the null...



## 5c: Religious attendance

# Unadjusted
mod_5c_unadj <- glm(dep_age6_bin ~ attend_age5, data = data_mum_cca_mh, family = "binomial")
summary(mod_5c_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(coef(summary(mod_5c_unadj))["attend_age5MIN 1 a YR", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(confint(mod_5c_unadj)["attend_age5MIN 1 a YR", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(confint(mod_5c_unadj)["attend_age5MIN 1 a YR", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_5c_unadj))["attend_age5MIN 1 a YR", "Pr(>|z|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(coef(summary(mod_5c_unadj))["attend_age5MIN 1 a MTH", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(confint(mod_5c_unadj)["attend_age5MIN 1 a MTH", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(confint(mod_5c_unadj)["attend_age5MIN 1 a MTH", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_5c_unadj))["attend_age5MIN 1 a MTH", "Pr(>|z|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(coef(summary(mod_5c_unadj))["attend_age5MIN 1 a WK", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(confint(mod_5c_unadj)["attend_age5MIN 1 a WK", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(confint(mod_5c_unadj)["attend_age5MIN 1 a WK", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_5c_unadj))["attend_age5MIN 1 a WK", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_5c_unadj, variable = "attend_age5")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_5c_unadj, variable = "attend_age5")$estimate[3] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_5c_unadj, variable = "attend_age5")$conf.low[3] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_5c_unadj, variable = "attend_age5")$conf.high[3] * 100, 2)

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_5c_unadj, variable = "attend_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_5c_unadj, variable = "attend_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_5c_unadj, variable = "attend_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_5c_unadj, variable = "attend_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_5c_unadj, variable = "attend_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_5c_unadj, variable = "attend_age5")$conf.high[2] * 100, 2)


# Adjusted
mod_5c_adj <- glm(dep_age6_bin ~ attend_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_5c_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(coef(summary(mod_5c_adj))["attend_age5MIN 1 a YR", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(confint(mod_5c_adj)["attend_age5MIN 1 a YR", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(confint(mod_5c_adj)["attend_age5MIN 1 a YR", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_5c_adj))["attend_age5MIN 1 a YR", "Pr(>|z|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(coef(summary(mod_5c_adj))["attend_age5MIN 1 a MTH", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(confint(mod_5c_adj)["attend_age5MIN 1 a MTH", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(confint(mod_5c_adj)["attend_age5MIN 1 a MTH", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_5c_adj))["attend_age5MIN 1 a MTH", "Pr(>|z|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(coef(summary(mod_5c_adj))["attend_age5MIN 1 a WK", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(confint(mod_5c_adj)["attend_age5MIN 1 a WK", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(confint(mod_5c_adj)["attend_age5MIN 1 a WK", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "5c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_5c_adj))["attend_age5MIN 1 a WK", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_5c_adj, variable = "attend_age5")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_5c_adj, variable = "attend_age5")$estimate[3] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_5c_adj, variable = "attend_age5")$conf.low[3] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_5c_adj, variable = "attend_age5")$conf.high[3] * 100, 2)

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_5c_adj, variable = "attend_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_5c_adj, variable = "attend_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_5c_adj, variable = "attend_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_5c_adj, variable = "attend_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_5c_adj, variable = "attend_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "5c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_5c_adj, variable = "attend_age5")$conf.high[2] * 100, 2)



#################################################################################
### Analysis 6: Binary anxiety as outcome, with categorical RSBB variables

## 6a: Religious belief

# Unadjusted
mod_6a_unadj <- glm(anx_age6_bin ~ belief_age5, data = data_mum_cca_mh, family = "binomial")
summary(mod_6a_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "6a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(coef(summary(mod_6a_unadj))["belief_age5Not sure", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "6a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(confint(mod_6a_unadj)["belief_age5Not sure", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "6a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(confint(mod_6a_unadj)["belief_age5Not sure", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "6a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_6a_unadj))["belief_age5Not sure", "Pr(>|z|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "6a" & mum_results_mh$level == "Yes"] <- 
  round(exp(coef(summary(mod_6a_unadj))["belief_age5Yes", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "6a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_6a_unadj)["belief_age5Yes", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "6a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_6a_unadj)["belief_age5Yes", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "6a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_6a_unadj))["belief_age5Yes", "Pr(>|z|)"], 4)


## Probability of difference in probable anxiety diagnosis
avg_comparisons(mod_6a_unadj, variable = "belief_age5")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_6a_unadj, variable = "belief_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_6a_unadj, variable = "belief_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_6a_unadj, variable = "belief_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_6a_unadj, variable = "belief_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_6a_unadj, variable = "belief_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_6a_unadj, variable = "belief_age5")$conf.high[2] * 100, 2)


# Adjusted
mod_6a_adj <- glm(anx_age6_bin ~ belief_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_6a_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "6a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(coef(summary(mod_6a_adj))["belief_age5Not sure", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "6a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(confint(mod_6a_adj)["belief_age5Not sure", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "6a" & mum_results_mh$level == "Not sure"] <- 
  round(exp(confint(mod_6a_adj)["belief_age5Not sure", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "6a" & mum_results_mh$level == "Not sure"] <- 
  round(coef(summary(mod_6a_adj))["belief_age5Not sure", "Pr(>|z|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "6a" & mum_results_mh$level == "Yes"] <- 
  round(exp(coef(summary(mod_6a_adj))["belief_age5Yes", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "6a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_6a_adj)["belief_age5Yes", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "6a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_6a_adj)["belief_age5Yes", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "6a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_6a_adj))["belief_age5Yes", "Pr(>|z|)"], 4)


## Probability of difference in probable anxiety diagnosis
avg_comparisons(mod_6a_adj, variable = "belief_age5")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_6a_adj, variable = "belief_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_6a_adj, variable = "belief_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Not sure"] <-
  round(avg_comparisons(mod_6a_adj, variable = "belief_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_6a_adj, variable = "belief_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_6a_adj, variable = "belief_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "6a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_6a_adj, variable = "belief_age5")$conf.high[2] * 100, 2)


## As is a small association here - with religious belief associated with slightly lower rates of anxiety - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from odds ratios to risk ratios - As rates of anxiety ~15%, will use the 'rare = TRUE' option (see ?evalues.OR).

# First for 'yes' belief
evalues.OR(est = 0.650, lo = 0.450, hi = 0.940, true = 1, rare = TRUE)

# E-value to null is 2.45, meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by two and a half times is necessary to completely remove the observed association - However, to reduce association so is no longer 'significant' (at 95% CI), requires only 32% increase in risk of both exposure and outcome

# Next for for 'not sure' belief
evalues.OR(est = 0.714, lo = 0.521, hi = 0.981, true = 1, rare = TRUE)

# E-value to null is 2.15, meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by over two times is necessary to completely remove the observed association - However, to reduce association so is no longer 'significant' (at 95% CI), requires only 16% increase in risk of both exposure and outcome



## 6b: Religious identity

# Unadjusted
mod_6b_unadj <- glm(anx_age6_bin ~ identity_age5, data = data_mum_cca_mh, family = "binomial")
summary(mod_6b_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "6b" & mum_results_mh$level == "Christian"] <- 
  round(exp(coef(summary(mod_6b_unadj))["identity_age5Christian", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "6b" & mum_results_mh$level == "Christian"] <- 
  round(exp(confint(mod_6b_unadj)["identity_age5Christian", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "6b" & mum_results_mh$level == "Christian"] <- 
  round(exp(confint(mod_6b_unadj)["identity_age5Christian", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "6b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_6b_unadj))["identity_age5Christian", "Pr(>|z|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "6b" & mum_results_mh$level == "Other"] <- 
  round(exp(coef(summary(mod_6b_unadj))["identity_age5Other", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "6b" & mum_results_mh$level == "Other"] <- 
  round(exp(confint(mod_6b_unadj)["identity_age5Other", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "6b" & mum_results_mh$level == "Other"] <- 
  round(exp(confint(mod_6b_unadj)["identity_age5Other", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "6b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_6b_unadj))["identity_age5Other", "Pr(>|z|)"], 4)


## Probability of difference in probable anxiety diagnosis
avg_comparisons(mod_6b_unadj, variable = "identity_age5")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_6b_unadj, variable = "identity_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_6b_unadj, variable = "identity_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_6b_unadj, variable = "identity_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_6b_unadj, variable = "identity_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_6b_unadj, variable = "identity_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_6b_unadj, variable = "identity_age5")$conf.high[2] * 100, 2)


# Adjusted
mod_6b_adj <- glm(anx_age6_bin ~ identity_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_6b_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "6b" & mum_results_mh$level == "Christian"] <- 
  round(exp(coef(summary(mod_6b_adj))["identity_age5Christian", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "6b" & mum_results_mh$level == "Christian"] <- 
  round(exp(confint(mod_6b_adj)["identity_age5Christian", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "6b" & mum_results_mh$level == "Christian"] <- 
  round(exp(confint(mod_6b_adj)["identity_age5Christian", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "6b" & mum_results_mh$level == "Christian"] <- 
  round(coef(summary(mod_6b_adj))["identity_age5Christian", "Pr(>|z|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "6b" & mum_results_mh$level == "Other"] <- 
  round(exp(coef(summary(mod_6b_adj))["identity_age5Other", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "6b" & mum_results_mh$level == "Other"] <- 
  round(exp(confint(mod_6b_adj)["identity_age5Other", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "6b" & mum_results_mh$level == "Other"] <- 
  round(exp(confint(mod_6b_adj)["identity_age5Other", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "6b" & mum_results_mh$level == "Other"] <- 
  round(coef(summary(mod_6b_adj))["identity_age5Other", "Pr(>|z|)"], 4)


## Probability of difference in probable anxiety diagnosis
avg_comparisons(mod_6b_adj, variable = "identity_age5")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_6b_adj, variable = "identity_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_6b_adj, variable = "identity_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Christian"] <-
  round(avg_comparisons(mod_6b_adj, variable = "identity_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_6b_adj, variable = "identity_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_6b_adj, variable = "identity_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "6b" & mum_results_mh_probs $level == "Other"] <-
  round(avg_comparisons(mod_6b_adj, variable = "identity_age5")$conf.high[2] * 100, 2)



## 6c: Religious attendance

# Unadjusted
mod_6c_unadj <- glm(anx_age6_bin ~ attend_age5, data = data_mum_cca_mh, family = "binomial")
summary(mod_6c_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(coef(summary(mod_6c_unadj))["attend_age5MIN 1 a YR", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(confint(mod_6c_unadj)["attend_age5MIN 1 a YR", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(confint(mod_6c_unadj)["attend_age5MIN 1 a YR", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_6c_unadj))["attend_age5MIN 1 a YR", "Pr(>|z|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(coef(summary(mod_6c_unadj))["attend_age5MIN 1 a MTH", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(confint(mod_6c_unadj)["attend_age5MIN 1 a MTH", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(confint(mod_6c_unadj)["attend_age5MIN 1 a MTH", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_6c_unadj))["attend_age5MIN 1 a MTH", "Pr(>|z|)"], 4)


mum_results_mh$est_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(coef(summary(mod_6c_unadj))["attend_age5MIN 1 a WK", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(confint(mod_6c_unadj)["attend_age5MIN 1 a WK", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(confint(mod_6c_unadj)["attend_age5MIN 1 a WK", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_6c_unadj))["attend_age5MIN 1 a WK", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_6c_unadj, variable = "attend_age5")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_6c_unadj, variable = "attend_age5")$estimate[3] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_6c_unadj, variable = "attend_age5")$conf.low[3] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_6c_unadj, variable = "attend_age5")$conf.high[3] * 100, 2)

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_6c_unadj, variable = "attend_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_6c_unadj, variable = "attend_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_6c_unadj, variable = "attend_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_6c_unadj, variable = "attend_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_6c_unadj, variable = "attend_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_6c_unadj, variable = "attend_age5")$conf.high[2] * 100, 2)


# Adjusted
mod_6c_adj <- glm(anx_age6_bin ~ attend_age5 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_6c_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(coef(summary(mod_6c_adj))["attend_age5MIN 1 a YR", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(confint(mod_6c_adj)["attend_age5MIN 1 a YR", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/yr"] <- 
  round(exp(confint(mod_6c_adj)["attend_age5MIN 1 a YR", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/yr"] <- 
  round(coef(summary(mod_6c_adj))["attend_age5MIN 1 a YR", "Pr(>|z|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(coef(summary(mod_6c_adj))["attend_age5MIN 1 a MTH", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(confint(mod_6c_adj)["attend_age5MIN 1 a MTH", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/mth"] <- 
  round(exp(confint(mod_6c_adj)["attend_age5MIN 1 a MTH", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/mth"] <- 
  round(coef(summary(mod_6c_adj))["attend_age5MIN 1 a MTH", "Pr(>|z|)"], 4)


mum_results_mh$est_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(coef(summary(mod_6c_adj))["attend_age5MIN 1 a WK", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(confint(mod_6c_adj)["attend_age5MIN 1 a WK", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/wk"] <- 
  round(exp(confint(mod_6c_adj)["attend_age5MIN 1 a WK", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "6c" & mum_results_mh$level == "1/wk"] <- 
  round(coef(summary(mod_6c_adj))["attend_age5MIN 1 a WK", "Pr(>|z|)"], 4)


## Probability of difference in probable anxiety diagnosis
avg_comparisons(mod_6c_adj, variable = "attend_age5")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_6c_adj, variable = "attend_age5")$estimate[3] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_6c_adj, variable = "attend_age5")$conf.low[3] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/yr"] <-
  round(avg_comparisons(mod_6c_adj, variable = "attend_age5")$conf.high[3] * 100, 2)

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_6c_adj, variable = "attend_age5")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_6c_adj, variable = "attend_age5")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/mth"] <-
  round(avg_comparisons(mod_6c_adj, variable = "attend_age5")$conf.high[1] * 100, 2)

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_6c_adj, variable = "attend_age5")$estimate[2] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_6c_adj, variable = "attend_age5")$conf.low[2] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "6c" & mum_results_mh_probs $level == "1/wk"] <-
  round(avg_comparisons(mod_6c_adj, variable = "attend_age5")$conf.high[2] * 100, 2)



#################################################################################
### Analysis 7: Binary depression as outcome, with binary RSBB variables

## 7a: Religious belief

# Unadjusted
mod_7a_unadj <- glm(dep_age6_bin ~ belief_age5_bin, data = data_mum_cca_mh, family = "binomial")
summary(mod_7a_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "7a" & mum_results_mh$level == "Yes"] <- 
  round(exp(coef(summary(mod_7a_unadj))["belief_age5_binYes", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "7a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_7a_unadj)["belief_age5_binYes", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "7a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_7a_unadj)["belief_age5_binYes", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "7a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_7a_unadj))["belief_age5_binYes", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_7a_unadj, variable = "belief_age5_bin")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "7a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_7a_unadj, variable = "belief_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "7a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_7a_unadj, variable = "belief_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "7a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_7a_unadj, variable = "belief_age5_bin")$conf.high[1] * 100, 2)


# Adjusted
mod_7a_adj <- glm(dep_age6_bin ~ belief_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_7a_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "7a" & mum_results_mh$level == "Yes"] <- 
  round(exp(coef(summary(mod_7a_adj))["belief_age5_binYes", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "7a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_7a_adj)["belief_age5_binYes", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "7a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_7a_adj)["belief_age5_binYes", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "7a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_7a_adj))["belief_age5_binYes", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_7a_adj, variable = "belief_age5_bin")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "7a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_7a_adj, variable = "belief_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "7a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_7a_adj, variable = "belief_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "7a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_7a_adj, variable = "belief_age5_bin")$conf.high[1] * 100, 2)


## As is a small association here - with religious belief associated with slightly lower rates of depression - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from odds ratios to risk ratios - As rates of depression ~11%, will use the 'rare = TRUE' option (see ?evalues.OR).

evalues.OR(est = 0.764, lo = 0.575, hi = 1.015, true = 1, rare = TRUE)

# E-value to null is 1.94, meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by two times is necessary to completely remove the observed association - However, upper 95% CI covers the null...



## 7b: Religious identity

# Unadjusted 
mod_7b_unadj <- glm(dep_age6_bin ~ identity_age5_bin, data = data_mum_cca_mh, family = "binomial")
summary(mod_7b_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "7b" & mum_results_mh$level == "Religious"] <- 
  round(exp(coef(summary(mod_7b_unadj))["identity_age5_binReligious", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "7b" & mum_results_mh$level == "Religious"] <- 
  round(exp(confint(mod_7b_unadj)["identity_age5_binReligious", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "7b" & mum_results_mh$level == "Religious"] <- 
  round(exp(confint(mod_7b_unadj)["identity_age5_binReligious", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "7b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_7b_unadj))["identity_age5_binReligious", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_7b_unadj, variable = "identity_age5_bin")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "7b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_7b_unadj, variable = "identity_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "7b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_7b_unadj, variable = "identity_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "7b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_7b_unadj, variable = "identity_age5_bin")$conf.high[1] * 100, 2)


# Adjusted
mod_7b_adj <- glm(dep_age6_bin ~ identity_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_7b_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "7b" & mum_results_mh$level == "Religious"] <- 
  round(exp(coef(summary(mod_7b_adj))["identity_age5_binReligious", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "7b" & mum_results_mh$level == "Religious"] <- 
  round(exp(confint(mod_7b_adj)["identity_age5_binReligious", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "7b" & mum_results_mh$level == "Religious"] <- 
  round(exp(confint(mod_7b_adj)["identity_age5_binReligious", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "7b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_7b_adj))["identity_age5_binReligious", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_7b_adj, variable = "identity_age5_bin")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "7b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_7b_adj, variable = "identity_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "7b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_7b_adj, variable = "identity_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "7b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_7b_adj, variable = "identity_age5_bin")$conf.high[1] * 100, 2)


## As is a small association here - with religious identity associated with slightly lower rates of depression - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from odds ratios to risk ratios - As rates of depression ~11%, will use the 'rare = TRUE' option (see ?evalues.OR).

evalues.OR(est = 0.660, lo = 0.458, hi = 0.960, true = 1, rare = TRUE)

# E-value to null is 2.40, meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by two and a half times is necessary to completely remove the observed association - However, to reduce association so is no longer 'significant' (at 95% CI), requires only 25% increase in risk of both exposure and outcome


## 7c: Religious attendance

# Unadjusted
mod_7c_unadj <- glm(dep_age6_bin ~ attend_age5_bin, data = data_mum_cca_mh, family = "binomial")
summary(mod_7c_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "7c" & mum_results_mh$level == "Regular"] <- 
  round(exp(coef(summary(mod_7c_unadj))["attend_age5_binRegular", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "7c" & mum_results_mh$level == "Regular"] <- 
  round(exp(confint(mod_7c_unadj)["attend_age5_binRegular", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "7c" & mum_results_mh$level == "Regular"] <- 
  round(exp(confint(mod_7c_unadj)["attend_age5_binRegular", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "7c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_7c_unadj))["attend_age5_binRegular", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_7c_unadj, variable = "attend_age5_bin")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "7c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_7c_unadj, variable = "attend_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "7c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_7c_unadj, variable = "attend_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "7c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_7c_unadj, variable = "attend_age5_bin")$conf.high[1] * 100, 2)


# Adjusted
mod_7c_adj <- glm(dep_age6_bin ~ attend_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_7c_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "7c" & mum_results_mh$level == "Regular"] <- 
  round(exp(coef(summary(mod_7c_adj))["attend_age5_binRegular", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "7c" & mum_results_mh$level == "Regular"] <- 
  round(exp(confint(mod_7c_adj)["attend_age5_binRegular", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "7c" & mum_results_mh$level == "Regular"] <- 
  round(exp(confint(mod_7c_adj)["attend_age5_binRegular", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "7c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_7c_adj))["attend_age5_binRegular", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_7c_adj, variable = "attend_age5_bin")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "7c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_7c_adj, variable = "attend_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "7c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_7c_adj, variable = "attend_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "7c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_7c_adj, variable = "attend_age5_bin")$conf.high[1] * 100, 2)



#################################################################################
### Analysis 8: Binary anxiety as outcome, with binary RSBB variables

## 8a: Religious belief

# Unadjusted
mod_8a_unadj <- glm(anx_age6_bin ~ belief_age5_bin, data = data_mum_cca_mh, family = "binomial")
summary(mod_8a_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "8a" & mum_results_mh$level == "Yes"] <- 
  round(exp(coef(summary(mod_8a_unadj))["belief_age5_binYes", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "8a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_8a_unadj)["belief_age5_binYes", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "8a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_8a_unadj)["belief_age5_binYes", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "8a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_8a_unadj))["belief_age5_binYes", "Pr(>|z|)"], 4)


## Probability of difference in probable anxiety diagnosis
avg_comparisons(mod_8a_unadj, variable = "belief_age5_bin")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "8a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_8a_unadj, variable = "belief_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "8a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_8a_unadj, variable = "belief_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "8a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_8a_unadj, variable = "belief_age5_bin")$conf.high[1] * 100, 2)


# Adjusted
mod_8a_adj <- glm(anx_age6_bin ~ belief_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_8a_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "8a" & mum_results_mh$level == "Yes"] <- 
  round(exp(coef(summary(mod_8a_adj))["belief_age5_binYes", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "8a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_8a_adj)["belief_age5_binYes", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "8a" & mum_results_mh$level == "Yes"] <- 
  round(exp(confint(mod_8a_adj)["belief_age5_binYes", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "8a" & mum_results_mh$level == "Yes"] <- 
  round(coef(summary(mod_8a_adj))["belief_age5_binYes", "Pr(>|z|)"], 4)


## Probability of difference in probable anxiety diagnosis
avg_comparisons(mod_8a_adj, variable = "belief_age5_bin")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "8a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_8a_adj, variable = "belief_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "8a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_8a_adj, variable = "belief_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "8a" & mum_results_mh_probs $level == "Yes"] <-
  round(avg_comparisons(mod_8a_adj, variable = "belief_age5_bin")$conf.high[1] * 100, 2)



## 8b: Religious identity

# Unadjusted
mod_8b_unadj <- glm(anx_age6_bin ~ identity_age5_bin, data = data_mum_cca_mh, family = "binomial")
summary(mod_8b_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "8b" & mum_results_mh$level == "Religious"] <- 
  round(exp(coef(summary(mod_8b_unadj))["identity_age5_binReligious", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "8b" & mum_results_mh$level == "Religious"] <- 
  round(exp(confint(mod_8b_unadj)["identity_age5_binReligious", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "8b" & mum_results_mh$level == "Religious"] <- 
  round(exp(confint(mod_8b_unadj)["identity_age5_binReligious", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "8b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_8b_unadj))["identity_age5_binReligious", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_8b_unadj, variable = "identity_age5_bin")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "8b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_8b_unadj, variable = "identity_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "8b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_8b_unadj, variable = "identity_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "8b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_8b_unadj, variable = "identity_age5_bin")$conf.high[1] * 100, 2)


# Adjusted
mod_8b_adj <- glm(anx_age6_bin ~ identity_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_8b_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "8b" & mum_results_mh$level == "Religious"] <- 
  round(exp(coef(summary(mod_8b_adj))["identity_age5_binReligious", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "8b" & mum_results_mh$level == "Religious"] <- 
  round(exp(confint(mod_8b_adj)["identity_age5_binReligious", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "8b" & mum_results_mh$level == "Religious"] <- 
  round(exp(confint(mod_8b_adj)["identity_age5_binReligious", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "8b" & mum_results_mh$level == "Religious"] <- 
  round(coef(summary(mod_8b_adj))["identity_age5_binReligious", "Pr(>|z|)"], 4)


## Probability of difference in probable anxiety diagnosis
avg_comparisons(mod_8b_adj, variable = "identity_age5_bin")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "8b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_8b_adj, variable = "identity_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "8b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_8b_adj, variable = "identity_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "8b" & mum_results_mh_probs $level == "Religious"] <-
  round(avg_comparisons(mod_8b_adj, variable = "identity_age5_bin")$conf.high[1] * 100, 2)


## As is a small association here - with religious identity associated with slightly lower rates of anxiety - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from odds ratios to risk ratios - As rates of anxiety ~15%, will use the 'rare = TRUE' option (see ?evalues.OR).

evalues.OR(est = 0.742, lo = 0.530, hi = 1.042, true = 1, rare = TRUE)

# E-value to null is 2.03, meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by two times is necessary to completely remove the observed association - However, upper 95% CI covers the null...


## 8c: Religious attendance

# Unadjusted
mod_8c_unadj <- glm(anx_age6_bin ~ attend_age5_bin, data = data_mum_cca_mh, family = "binomial")
summary(mod_8c_unadj)

# Store results in table
mum_results_mh$est_unadj[mum_results_mh$model == "8c" & mum_results_mh$level == "Regular"] <- 
  round(exp(coef(summary(mod_8c_unadj))["attend_age5_binRegular", "Estimate"]), 3)

mum_results_mh$lci_unadj[mum_results_mh$model == "8c" & mum_results_mh$level == "Regular"] <- 
  round(exp(confint(mod_8c_unadj)["attend_age5_binRegular", "2.5 %"]), 3)

mum_results_mh$uci_unadj[mum_results_mh$model == "8c" & mum_results_mh$level == "Regular"] <- 
  round(exp(confint(mod_8c_unadj)["attend_age5_binRegular", "97.5 %"]), 3)

mum_results_mh$p_unadj[mum_results_mh$model == "8c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_8c_unadj))["attend_age5_binRegular", "Pr(>|z|)"], 4)


## Probability of difference in probable depression diagnosis
avg_comparisons(mod_8c_unadj, variable = "attend_age5_bin")

mum_results_mh_probs$est_unadj[mum_results_mh_probs$model == "8c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_8c_unadj, variable = "attend_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_unadj[mum_results_mh_probs$model == "8c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_8c_unadj, variable = "attend_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_unadj[mum_results_mh_probs$model == "8c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_8c_unadj, variable = "attend_age5_bin")$conf.high[1] * 100, 2)


# Adjusted
mod_8c_adj <- glm(anx_age6_bin ~ attend_age5_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_mum_cca_mh, family = "binomial")
summary(mod_8c_adj)

# Store results in table
mum_results_mh$est_adj[mum_results_mh$model == "8c" & mum_results_mh$level == "Regular"] <- 
  round(exp(coef(summary(mod_8c_adj))["attend_age5_binRegular", "Estimate"]), 3)

mum_results_mh$lci_adj[mum_results_mh$model == "8c" & mum_results_mh$level == "Regular"] <- 
  round(exp(confint(mod_8c_adj)["attend_age5_binRegular", "2.5 %"]), 3)

mum_results_mh$uci_adj[mum_results_mh$model == "8c" & mum_results_mh$level == "Regular"] <- 
  round(exp(confint(mod_8c_adj)["attend_age5_binRegular", "97.5 %"]), 3)

mum_results_mh$p_adj[mum_results_mh$model == "8c" & mum_results_mh$level == "Regular"] <- 
  round(coef(summary(mod_8c_adj))["attend_age5_binRegular", "Pr(>|z|)"], 4)


## Probability of difference in probable anxiety diagnosis
avg_comparisons(mod_8c_adj, variable = "attend_age5_bin")

mum_results_mh_probs$est_adj[mum_results_mh_probs$model == "8c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_8c_adj, variable = "attend_age5_bin")$estimate[1] * 100, 2)
mum_results_mh_probs$lci_adj[mum_results_mh_probs$model == "8c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_8c_adj, variable = "attend_age5_bin")$conf.low[1] * 100, 2)
mum_results_mh_probs$uci_adj[mum_results_mh_probs$model == "8c" & mum_results_mh_probs $level == "Regular"] <-
  round(avg_comparisons(mod_8c_adj, variable = "attend_age5_bin")$conf.high[1] * 100, 2)



################################################################################################
#### Make plots to summarise results

mum_results_mh

# Convert results to numeric
mum_results_mh <- mum_results_mh %>%
  mutate(est_unadj = as.numeric(est_unadj)) %>%
  mutate(lci_unadj = as.numeric(lci_unadj)) %>%
  mutate(uci_unadj = as.numeric(uci_unadj)) %>%
  mutate(est_unadj_z = as.numeric(est_unadj_z)) %>%
  mutate(lci_unadj_z = as.numeric(lci_unadj_z)) %>%
  mutate(uci_unadj_z = as.numeric(uci_unadj_z)) %>%
  mutate(p_unadj = as.numeric(p_unadj)) %>%
  mutate(est_adj = as.numeric(est_adj)) %>%
  mutate(lci_adj = as.numeric(lci_adj)) %>%
  mutate(uci_adj = as.numeric(uci_adj)) %>%
  mutate(est_adj_z = as.numeric(est_adj_z)) %>%
  mutate(lci_adj_z = as.numeric(lci_adj_z)) %>%
  mutate(uci_adj_z = as.numeric(uci_adj_z)) %>%
  mutate(p_adj = as.numeric(p_adj))

mum_results_mh
glimpse(mum_results_mh)

# Save these results
write_csv(mum_results_mh, file = "./MotherResults/RSBBCauseMH/mum_results.csv")

#mum_results_mh <- read_csv(file = "./MotherResults/RSBBCauseMH/mum_results.csv")
#head(mum_results_mh)


### Turn these results into plots

## Model 1 - Categorical RSBB exposures and continuous depression outcome

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "1a" | model == "1b" | model == "1c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot1 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    labs(x = "Religious exposure", y = "Difference in EPDS depression score") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
    )

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod1.pdf", height = 5, width = 10)
plot1
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- mum_results_mh %>%
  filter(model == "1a" | model == "1b" | model == "1c") %>%
  select(-c(est_unadj, lci_unadj, uci_unadj, p_unadj, est_adj, lci_adj, uci_adj, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj_z:uci_adj_z)) %>%
  rename(est = est_unadj_z, lci = lci_unadj_z, uci = uci_unadj_z) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj_z:uci_unadj_z)) %>%
  rename(est = est_adj_z, lci = lci_adj_z, uci = uci_adj_z) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot1_z <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    labs(x = "Religious exposure", y = "Standardised difference in EPDS depression score") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod1_std.pdf", height = 5, width = 10)
plot1_z
dev.off()


## Model 2 - Categorical RSBB exposures and continuous anxiety outcome

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "2a" | model == "2b" | model == "2c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot2 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    labs(x = "Religious exposure", y = "Difference in CCEI-A anxiety score") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod2.pdf", height = 5, width = 10)
plot2
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- mum_results_mh %>%
  filter(model == "2a" | model == "2b" | model == "2c") %>%
  select(-c(est_unadj, lci_unadj, uci_unadj, p_unadj, est_adj, lci_adj, uci_adj, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj_z:uci_adj_z)) %>%
  rename(est = est_unadj_z, lci = lci_unadj_z, uci = uci_unadj_z) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj_z:uci_unadj_z)) %>%
  rename(est = est_adj_z, lci = lci_adj_z, uci = uci_adj_z) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot2_z <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    labs(x = "Religious exposure", y = "Standardised difference in CCEI-A anxiety score") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod2_std.pdf", height = 5, width = 10)
plot2_z
dev.off()


## Also combine models 1 and 2 together (depression and anxiety together)

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "1a" | model == "1b" | model == "1c" | model == "2a" | model == "2b" | model == "2c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(outcome = factor(outcome, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot12 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    labs(x = "Religious exposure", y = "Difference in score") +
    scale_y_continuous(breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(outcome ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                                "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod12.pdf", height = 10, width = 10)
plot12
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- mum_results_mh %>%
  filter(model == "1a" | model == "1b" | model == "1c" | model == "2a" | model == "2b" | model == "2c") %>%
  select(-c(est_unadj, lci_unadj, uci_unadj, p_unadj, est_adj, lci_adj, uci_adj, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj_z:uci_adj_z)) %>%
  rename(est = est_unadj_z, lci = lci_unadj_z, uci = uci_unadj_z) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj_z:uci_unadj_z)) %>%
  rename(est = est_adj_z, lci = lci_adj_z, uci = uci_adj_z) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(outcome = factor(outcome, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot12_z <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    scale_y_continuous(breaks = c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2)) +
    labs(x = "Religious exposure", y = "Difference in standardised score") +
    coord_flip() +
    theme_classic() +
    facet_wrap(outcome ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                               "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod12_std.pdf", height = 10, width = 10)
plot12_z
dev.off()




## Model 3 - Binary RSBB exposures and continuous depression outcome

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "3a" | model == "3b" | model == "3c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot3 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious exposure", y = "Difference in EPDS depression score") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod3.pdf", height = 5, width = 10)
plot3
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- mum_results_mh %>%
  filter(model == "3a" | model == "3b" | model == "3c") %>%
  select(-c(est_unadj, lci_unadj, uci_unadj, p_unadj, est_adj, lci_adj, uci_adj, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj_z:uci_adj_z)) %>%
  rename(est = est_unadj_z, lci = lci_unadj_z, uci = uci_unadj_z) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj_z:uci_unadj_z)) %>%
  rename(est = est_adj_z, lci = lci_adj_z, uci = uci_adj_z) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot3_z <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious exposure", y = "Standardised difference in EPDS depression score") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod3_std.pdf", height = 5, width = 10)
plot3_z
dev.off()


## Model 4 - Categorical RSBB exposures and continuous anxiety outcome

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "4a" | model == "4b" | model == "4c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot4 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious exposure", y = "Difference in CCEI-A anxiety score") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod4.pdf", height = 5, width = 10)
plot4
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- mum_results_mh %>%
  filter(model == "4a" | model == "4b" | model == "4c") %>%
  select(-c(est_unadj, lci_unadj, uci_unadj, p_unadj, est_adj, lci_adj, uci_adj, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj_z:uci_adj_z)) %>%
  rename(est = est_unadj_z, lci = lci_unadj_z, uci = uci_unadj_z) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj_z:uci_unadj_z)) %>%
  rename(est = est_adj_z, lci = lci_adj_z, uci = uci_adj_z) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot4_z <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious exposure", y = "Standardised difference in CCEI-A anxiety score") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod4_std.pdf", height = 5, width = 10)
plot4_z
dev.off()


## Also combine models 3 and 4 together (depression and anxiety together)

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "3a" | model == "3b" | model == "3c" | model == "4a" | model == "4b" | model == "4c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(outcome = factor(outcome, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot34 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious exposure", y = "Difference in score") +
    scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(outcome ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                               "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod34.pdf", height = 10, width = 10)
plot34
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- mum_results_mh %>%
  filter(model == "3a" | model == "3b" | model == "3c" | model == "4a" | model == "4b" | model == "4c") %>%
  select(-c(est_unadj, lci_unadj, uci_unadj, p_unadj, est_adj, lci_adj, uci_adj, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj_z:uci_adj_z)) %>%
  rename(est = est_unadj_z, lci = lci_unadj_z, uci = uci_unadj_z) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj_z:uci_unadj_z)) %>%
  rename(est = est_adj_z, lci = lci_adj_z, uci = uci_adj_z) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(outcome = factor(outcome, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot34_z <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    scale_y_continuous(breaks = c(-0.2, -0.15, -0.1, -0.05, 0, 0.05)) +
    labs(x = "Religious exposure", y = "Difference in standardised score") +
    coord_flip() +
    theme_classic() +
    facet_wrap(outcome ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                               "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod34_std.pdf", height = 10, width = 10)
plot34_z
dev.off()



## Model 5 - Categorical RSBB exposures and binary depression outcome

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "5a" | model == "5b" | model == "5c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot5 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    scale_y_continuous(trans = "log", breaks = c(0.33, 0.5, 0.75, 1, 1.5)) +
    labs(x = "Religious exposure", y = "Odds ratio for probable depression") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod5.pdf", height = 5, width = 10)
plot5
dev.off()



## Model 6 - Categorical RSBB exposures and binary anxiety outcome

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "6a" | model == "6b" | model == "6c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot6 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    scale_y_continuous(trans = "log", breaks = c(0.33, 0.5, 0.75, 1, 1.5)) +
    labs(x = "Religious exposure", y = "Odds ratio for probable anxiety") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod6.pdf", height = 5, width = 10)
plot6
dev.off()


## Also combine models 5 and 6 together (depression and anxiety together)

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "5a" | model == "5b" | model == "5c" | model == "6a" | model == "6b" | model == "6c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(outcome = factor(outcome, levels = c("Dep (bin)", "Anx (bin)"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot56 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    labs(x = "Religious exposure", y = "Odds ratio for probable diagnosis") +
    scale_y_continuous(trans = "log", breaks = c(0.33, 0.5, 0.75, 1, 1.5)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(outcome ~ ., ncol = 1, labeller = as_labeller(c("Dep (bin)" = "Depression", 
                                                               "Anx (bin)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod56.pdf", height = 10, width = 10)
plot56
dev.off()



## Model 7 - Binary RSBB exposures and binary depression outcome

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "7a" | model == "7b" | model == "7c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot7 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    scale_y_continuous(trans = "log", breaks = c(0.5, 0.75, 1)) +
    labs(x = "Religious exposure", y = "Odds ratio for probable depression") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod7.pdf", height = 5, width = 10)
plot7
dev.off()



## Model 8 - Categorical RSBB exposures and binary anxiety outcome

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "8a" | model == "8b" | model == "8c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot8 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    scale_y_continuous(trans = "log", breaks = c(0.5, 0.75, 1), limits = c(0.475, 1.2)) +
    labs(x = "Religious exposure", y = "Odds ratio for probable anxiety") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod8.pdf", height = 5, width = 10)
plot8
dev.off()


## Also combine models 7 and 8 together (depression and anxiety together)

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh %>%
  filter(model == "7a" | model == "7b" | model == "7c" | model == "8a" | model == "8b" | model == "8c") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, p_unadj, est_adj_z, lci_adj_z, uci_adj_z, p_adj))

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(outcome = factor(outcome, levels = c("Dep (bin)", "Anx (bin)"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot78 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious exposure", y = "Odds ratio for probable diagnosis") +
    scale_y_continuous(trans = "log", breaks = c(0.5, 0.75, 1)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(outcome ~ ., ncol = 1, labeller = as_labeller(c("Dep (bin)" = "Depression", 
                                                               "Anx (bin)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod78.pdf", height = 10, width = 10)
plot78
dev.off()



########################################################################################
#### Also make plots for predicted differences in depression and anxiety from logistic models

mum_results_mh_probs

# Convert results to numeric
mum_results_mh_probs <- mum_results_mh_probs %>%
  mutate(est_unadj = as.numeric(est_unadj)) %>%
  mutate(lci_unadj = as.numeric(lci_unadj)) %>%
  mutate(uci_unadj = as.numeric(uci_unadj)) %>%
  mutate(est_adj = as.numeric(est_adj)) %>%
  mutate(lci_adj = as.numeric(lci_adj)) %>%
  mutate(uci_adj = as.numeric(uci_adj))

mum_results_mh_probs
glimpse(mum_results_mh_probs)

# Save these results
write_csv(mum_results_mh_probs, file = "./MotherResults/RSBBCauseMH/mum_results_probs.csv")

#mum_results_mh_probs <- read_csv(file = "./MotherResults/RSBBCauseMH/mum_results_probs.csv")
#head(mum_results_mh_probs)


### Turn these results into plots

## Model 5 - Categorical RSBB exposures and binary depression outcome

# Reduce data and convert to long format
res_temp <- mum_results_mh_probs %>%
  filter(model == "5a" | model == "5b" | model == "5c")

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot5_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    scale_y_continuous(breaks = c(-12, -10, -8, -6, -4, -2, 0, 2, 4, 6)) +
    labs(x = "Religious exposure", y = "Predicted difference in probable depression") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod5_probs.pdf", height = 5, width = 10)
plot5_probs
dev.off()


## Model 6 - Categorical RSBB exposures and binary anxiety outcome

# Reduce data and convert to long format
res_temp <- mum_results_mh_probs %>%
  filter(model == "6a" | model == "6b" | model == "6c")

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot6_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    scale_y_continuous(breaks = c(-12, -10, -8, -6, -4, -2, 0, 2, 4, 6)) +
    labs(x = "Religious exposure", y = "Predicted difference in probable anxiety") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod6_probs.pdf", height = 5, width = 10)
plot6_probs
dev.off()


## Also combine models 5 and 6 together (depression and anxiety together)

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh_probs %>%
  filter(model == "5a" | model == "5b" | model == "5c" | model == "6a" | model == "6b" | model == "6c")

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(outcome = factor(outcome, levels = c("Dep (bin)", "Anx (bin)"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot56_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 4, 5, 7, 8, 9), 
                    labels = c("Belief - Not sure \n(ref = No)", "Belief - Yes \n(ref = No)",
                               "Identity - Christian \n(ref = None)", "Identity - Other \n(ref = None)",
                               "Attendance - 1/Yr \n(ref = Not at all)", "Attendance - 1/Mth \n(ref = Not at all)",
                               "Attendance - 1/Wk \n(ref = Not at all)")) +
    labs(x = "Religious exposure", y = "Predicted difference in probable diagnosis") +
    scale_y_continuous(breaks = c(-12, -10, -8, -6, -4, -2, 0, 2, 4, 6)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(outcome ~ ., ncol = 1, labeller = as_labeller(c("Dep (bin)" = "Depression", 
                                                               "Anx (bin)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod56_probs.pdf", height = 10, width = 10)
plot56_probs
dev.off()



## Model 7 - Binary RSBB exposures and binary depression outcome

# Reduce data and convert to long format
res_temp <- mum_results_mh_probs %>%
  filter(model == "7a" | model == "7b" | model == "7c")

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot7_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    scale_y_continuous(breaks = c(-8, -6, -4, -2, 0)) +
    labs(x = "Religious exposure", y = "Predicted difference in probable depression") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod7_probs.pdf", height = 5, width = 10)
plot7_probs
dev.off()


## Model 8 - Binary RSBB exposures and binary anxiety outcome

# Reduce data and convert to long format
res_temp <- mum_results_mh_probs %>%
  filter(model == "8a" | model == "8b" | model == "8c")

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot8_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    scale_y_continuous(breaks = c(-6, -4, -2, 0)) +
    labs(x = "Religious exposure", y = "Predicted difference in probable anxiety") +
    coord_flip() +
    theme_classic() +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod8_probs.pdf", height = 5, width = 10)
plot8_probs
dev.off()


## Also combine models 5 and 6 together (depression and anxiety together)

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- mum_results_mh_probs %>%
  filter(model == "7a" | model == "7b" | model == "7c" | model == "8a" | model == "8b" | model == "8c")

res_temp1 <- res_temp %>%
  select(-c(est_adj:uci_adj)) %>%
  rename(est = est_unadj, lci = lci_unadj, uci = uci_unadj) %>%
  mutate(adj = "Unadjusted")
res_temp1

res_temp2 <- res_temp %>%
  select(-c(est_unadj:uci_unadj)) %>%
  rename(est = est_adj, lci = lci_adj, uci = uci_adj) %>%
  mutate(adj = "Adjusted")
res_temp2

res1 <- rbind(res_temp1, res_temp2)
res1

glimpse(res1)

res1 <- res1 %>%
  mutate(adj = factor(adj, levels = c("Adjusted", "Unadjusted"))) %>%
  mutate(exposure = recode(exposure, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                           "Attend (bin)" = "Attend")) %>%
  mutate(exposure = factor(exposure, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(outcome = factor(outcome, levels = c("Dep (bin)", "Anx (bin)"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot78_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious exposure", y = "Predicted difference in probable diagnosis") +
    scale_y_continuous(breaks = c(-8, -6, -4, -2, 0)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(outcome ~ ., ncol = 1, labeller = as_labeller(c("Dep (bin)" = "Depression", 
                                                               "Anx (bin)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./MotherResults/RSBBCauseMH/mum_results_mod78_probs.pdf", height = 10, width = 10)
plot78_probs
dev.off()


