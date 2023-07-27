### Script for paper 'Exploring bidirectional causality between religion and mental health: A longitudinal study using data from the parental generation of a UK birth cohort (ALSPAC)' (ALSPAC B-number B4226)
### Script 3b: Partner's analyses - Assessing whether mental health causes RSBB
### Created 29/6/2023 by Dan Major-Smith
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

library(nnet)



###########################################################################################
#### Read in the processed data

load("data_partner_processed_B4226.RData")


# Make a complete-case dataset with no missing values for the analyses with religiosity at age 5 as the outcome
data_partner_cca_rsbb <- data_partner %>%
  select(c(belief_preg:attend_age5_bin, dep_preg:anx_age2_bin, age:paternalMH)) %>%
  filter(complete.cases(.))

head(data_partner_cca_rsbb)


# Percent of complete-cases
nrow(data_partner_cca_rsbb)
round(nrow(data_partner_cca_rsbb) / nrow(data_partner) * 100, 2)


## Or to read in the synthetic data (Note that this synthetic dataset only includes complete cases):
#load("./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_partner_rsbbOutcome_B4226.RData")
#data_partner_cca_rsbb <- data_partner_cca_rsbb_syn_df
#data_partner_cca_rsbb <- data_partner_cca_rsbb %>%
#  select(-FALSE_DATA) ## Drop the 'FALSE_DATA' column


## Create standardised depression (EPDS) and anxiety (CCEI-A) scores
data_partner_cca_rsbb <- data_partner_cca_rsbb %>%
  mutate(dep_age2_z = (dep_age2 - mean(dep_age2)) / sd(dep_age2)) %>%
  mutate(anx_age2_z = (anx_age2 - mean(anx_age2)) / sd(anx_age2))

summary(data_partner_cca_rsbb$dep_age2_z)
summary(data_partner_cca_rsbb$anx_age2_z)



#### Descriptive stats

### Mental health exposures

## Depression (continuous)

# Full sample
sum(is.na(data_partner$dep_age2))
round(sum(is.na(data_partner$dep_age2) / nrow(data_partner) * 100), 2)

summary(data_partner$dep_age2)

# Complete-cases
summary(data_partner_cca_rsbb$dep_age2)
sd(data_partner_cca_rsbb$dep_age2)

hist(data_partner_cca_rsbb$dep_age2, 
     breaks=rep(min(data_partner_cca_rsbb$dep_age2, na.rm = TRUE):max(data_partner_cca_rsbb$dep_age2, na.rm = TRUE), 
                each = 2) + c(-0.4, 0.4), freq = TRUE,
     xlab = "EPDS depression score", main = "")


## Depression (binary)

# Full sample
table(data_partner$dep_age2_bin)
round(prop.table(table(data_partner$dep_age2_bin)) * 100, 2)

# Complete-cases
table(data_partner_cca_rsbb$dep_age2_bin)
round(prop.table(table(data_partner_cca_rsbb$dep_age2_bin)) * 100, 2)


## Anxiety (continuous)

# Full sample
sum(is.na(data_partner$anx_age2))
round(sum(is.na(data_partner$anx_age2) / nrow(data_partner) * 100), 2)

summary(data_partner$anx_age2)

# Complete-cases
summary(data_partner_cca_rsbb$anx_age2)
sd(data_partner_cca_rsbb$anx_age2)

hist(data_partner_cca_rsbb$anx_age2, 
     breaks=rep(min(data_partner_cca_rsbb$anx_age2, na.rm = TRUE):max(data_partner_cca_rsbb$anx_age2, na.rm = TRUE), 
                each = 2) + c(-0.4, 0.4), freq = TRUE,
     xlab = "CCEI-A anxiety score", main = "")


## Anxiety (binary)

# Full sample
table(data_partner$anx_age2_bin)
round(prop.table(table(data_partner$anx_age2_bin)) * 100, 2)

# Complete-cases
table(data_partner_cca_rsbb$anx_age2_bin)
round(prop.table(table(data_partner_cca_rsbb$anx_age2_bin)) * 100, 2)


## Correlation between depression and anxiety
cor.test(data_partner_cca_rsbb$dep_age2, data_partner_cca_rsbb$anx_age2)



### Religious outcomes

## Religious belief (categorical)

# Full sample
sum(is.na(data_partner$belief_age5))
round(sum(is.na(data_partner$belief_age5) / nrow(data_partner) * 100), 2)

table(data_partner$belief_age5)
round(prop.table(table(data_partner$belief_age5)) * 100, 2)

# Complete-cases
table(data_partner_cca_rsbb$belief_age5)
round(prop.table(table(data_partner_cca_rsbb$belief_age5)) * 100, 2)


## Religious belief (binary)

# Full sample
table(data_partner$belief_age5_bin)
round(prop.table(table(data_partner$belief_age5_bin)) * 100, 2)

# Complete-cases
table(data_partner_cca_rsbb$belief_age5_bin)
round(prop.table(table(data_partner_cca_rsbb$belief_age5_bin)) * 100, 2)


## Religious identity (categorical)

# Full sample
sum(is.na(data_partner$identity_age5))
round(sum(is.na(data_partner$identity_age5) / nrow(data_partner) * 100), 2)

table(data_partner$identity_age5)
round(prop.table(table(data_partner$identity_age5)) * 100, 2)

# Complete-cases
table(data_partner_cca_rsbb$identity_age5)
round(prop.table(table(data_partner_cca_rsbb$identity_age5)) * 100, 2)


## Religious identity (binary)

# Full sample
table(data_partner$identity_age5_bin)
round(prop.table(table(data_partner$identity_age5_bin)) * 100, 2)

# Complete-cases
table(data_partner_cca_rsbb$identity_age5_bin)
round(prop.table(table(data_partner_cca_rsbb$identity_age5_bin)) * 100, 2)


## Religious attendance (categorical)

# Full sample
sum(is.na(data_partner$attend_age5))
round(sum(is.na(data_partner$attend_age5) / nrow(data_partner) * 100), 2)

table(data_partner$attend_age5)
round(prop.table(table(data_partner$attend_age5)) * 100, 2)

# Complete-cases
table(data_partner_cca_rsbb$attend_age5)
round(prop.table(table(data_partner_cca_rsbb$attend_age5)) * 100, 2)


## Religious attendance (binary)

# Full sample
table(data_partner$attend_age5_bin)
round(prop.table(table(data_partner$attend_age5_bin)) * 100, 2)

# Complete-cases
table(data_partner_cca_rsbb$attend_age5_bin)
round(prop.table(table(data_partner_cca_rsbb$attend_age5_bin)) * 100, 2)



### Comparing outcomes by exposures

## 1a - Continuous depression by categorical religious belief
data_partner_cca_rsbb %>%
  group_by(belief_age5) %>%
  summarise(n = n(), mean = mean(dep_age2), sd = sd(dep_age2), min = min(dep_age2), perc25 = quantile(dep_age2, 0.25),
            median = median(dep_age2), perc75 = quantile(dep_age2, 0.75), max = max(dep_age2))

## 1b - Continuous anxiety by categorical religious belief
data_partner_cca_rsbb %>%
  group_by(belief_age5) %>%
  summarise(n = n(), mean = mean(anx_age2), sd = sd(anx_age2), min = min(anx_age2), perc25 = quantile(anx_age2, 0.25),
            median = median(anx_age2), perc75 = quantile(anx_age2, 0.75), max = max(anx_age2))

## 2a - Continuous depression by categorical religious identity
data_partner_cca_rsbb %>%
  group_by(identity_age5) %>%
  summarise(n = n(), mean = mean(dep_age2), sd = sd(dep_age2), min = min(dep_age2), perc25 = quantile(dep_age2, 0.25),
            median = median(dep_age2), perc75 = quantile(dep_age2, 0.75), max = max(dep_age2))

## 2b - Continuous anxiety by categorical religious identity
data_partner_cca_rsbb %>%
  group_by(identity_age5) %>%
  summarise(n = n(), mean = mean(anx_age2), sd = sd(anx_age2), min = min(anx_age2), perc25 = quantile(anx_age2, 0.25),
            median = median(anx_age2), perc75 = quantile(anx_age2, 0.75), max = max(anx_age2))

## 3a - Continuous depression by categorical religious attendance
data_partner_cca_rsbb %>%
  group_by(attend_age5) %>%
  summarise(n = n(), mean = mean(dep_age2), sd = sd(dep_age2), min = min(dep_age2), perc25 = quantile(dep_age2, 0.25),
            median = median(dep_age2), perc75 = quantile(dep_age2, 0.75), max = max(dep_age2))

## 3b - Continuous anxiety by categorical religious attendance
data_partner_cca_rsbb %>%
  group_by(attend_age5) %>%
  summarise(n = n(), mean = mean(anx_age2), sd = sd(anx_age2), min = min(anx_age2), perc25 = quantile(anx_age2, 0.25),
            median = median(anx_age2), perc75 = quantile(anx_age2, 0.75), max = max(anx_age2))

## 4a - Binary depression by categorical religious belief
table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$belief_age5)
round(prop.table(table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$belief_age5), margin = 1) * 100, 2)

## 4b - Binary anxiety by categorical religious belief
table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$belief_age5)
round(prop.table(table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$belief_age5), margin = 1) * 100, 2)

## 5a - Binary depression by categorical religious identity
table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$identity_age5)
round(prop.table(table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$identity_age5), margin = 1) * 100, 2)

## 5b - Binary anxiety by categorical religious identity
table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$identity_age5)
round(prop.table(table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$identity_age5), margin = 1) * 100, 2)

## 6a - Binary depression by categorical religious attendance
table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$attend_age5)
round(prop.table(table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$attend_age5), margin = 1) * 100, 2)

## 6b - Binary anxiety by categorical religious attendance
table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$attend_age5)
round(prop.table(table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$attend_age5), margin = 1) * 100, 2)

## 7a - Continuous depression by binary religious belief
data_partner_cca_rsbb %>%
  group_by(belief_age5_bin) %>%
  summarise(n = n(), mean = mean(dep_age2), sd = sd(dep_age2), min = min(dep_age2), perc25 = quantile(dep_age2, 0.25),
            median = median(dep_age2), perc75 = quantile(dep_age2, 0.75), max = max(dep_age2))

## 7b - Continuous anxiety by binary religious belief
data_partner_cca_rsbb %>%
  group_by(belief_age5_bin) %>%
  summarise(n = n(), mean = mean(anx_age2), sd = sd(anx_age2), min = min(anx_age2), perc25 = quantile(anx_age2, 0.25),
            median = median(anx_age2), perc75 = quantile(anx_age2, 0.75), max = max(anx_age2))

## 8a - Continuous depression by binary religious identity
data_partner_cca_rsbb %>%
  group_by(identity_age5_bin) %>%
  summarise(n = n(), mean = mean(dep_age2), sd = sd(dep_age2), min = min(dep_age2), perc25 = quantile(dep_age2, 0.25),
            median = median(dep_age2), perc75 = quantile(dep_age2, 0.75), max = max(dep_age2))

## 8b - Continuous anxiety by binary religious identity
data_partner_cca_rsbb %>%
  group_by(identity_age5_bin) %>%
  summarise(n = n(), mean = mean(anx_age2), sd = sd(anx_age2), min = min(anx_age2), perc25 = quantile(anx_age2, 0.25),
            median = median(anx_age2), perc75 = quantile(anx_age2, 0.75), max = max(anx_age2))

## 9a - Continuous depression by binary religious attendance
data_partner_cca_rsbb %>%
  group_by(attend_age5_bin) %>%
  summarise(n = n(), mean = mean(dep_age2), sd = sd(dep_age2), min = min(dep_age2), perc25 = quantile(dep_age2, 0.25),
            median = median(dep_age2), perc75 = quantile(dep_age2, 0.75), max = max(dep_age2))

## 9b - Continuous anxiety by binary religious attendance
data_partner_cca_rsbb %>%
  group_by(attend_age5_bin) %>%
  summarise(n = n(), mean = mean(anx_age2), sd = sd(anx_age2), min = min(anx_age2), perc25 = quantile(anx_age2, 0.25),
            median = median(anx_age2), perc75 = quantile(anx_age2, 0.75), max = max(anx_age2))

## 10a - Binary depression by binary religious belief
table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$belief_age5_bin)
round(prop.table(table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$belief_age5_bin), margin = 1) * 100, 2)

## 10b - Binary anxiety by binary religious belief
table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$belief_age5_bin)
round(prop.table(table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$belief_age5_bin), margin = 1) * 100, 2)

## 11a - Binary depression by binary religious identity
table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$identity_age5_bin)
round(prop.table(table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$identity_age5_bin), margin = 1) * 100, 2)

## 11b - Binary anxiety by binary religious identity
table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$identity_age5_bin)
round(prop.table(table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$identity_age5_bin), margin = 1) * 100, 2)

## 12a - Binary depression by binary religious attendance
table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$attend_age5_bin)
round(prop.table(table(data_partner_cca_rsbb$dep_age2_bin, data_partner_cca_rsbb$attend_age5_bin), margin = 1) * 100, 2)

## 12b - Binary anxiety by binary religious attendance
table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$attend_age5_bin)
round(prop.table(table(data_partner_cca_rsbb$anx_age2_bin, data_partner_cca_rsbb$attend_age5_bin), margin = 1) * 100, 2)



## Make a table to save results to
partner_results_rsbb <- as.data.frame(cbind(model = c("1a", "1a", "1b", "1b", 
                                                  "2a", "2a", "2b", "2b",
                                                  "3a", "3a", "3a", "3b", "3b", "3b",
                                                  "4a", "4a", "4b", "4b", 
                                                  "5a", "5a", "5b", "5b",
                                                  "6a", "6a", "6a", "6b", "6b", "6b",
                                                  "7a", "7b", "8a", "8b", "9a", "9b",
                                                  "10a", "10b", "11a", "11b", "12a", "12b"),
                                   exposure = c("Dep (cont)", "Dep (cont)", "Anx (cont)", "Anx (cont)",
                                                "Dep (cont)", "Dep (cont)", "Anx (cont)", "Anx (cont)",
                                                "Dep (cont)", "Dep (cont)", "Dep (cont)", 
                                                "Anx (cont)", "Anx (cont)", "Anx (cont)",
                                                "Dep (bin)", "Dep (bin)", "Anx (bin)", "Anx (bin)",
                                                "Dep (bin)", "Dep (bin)", "Anx (bin)", "Anx (bin)",
                                                "Dep (bin)", "Dep (bin)", "Dep (bin)", 
                                                "Anx (bin)", "Anx (bin)", "Anx (bin)",
                                                "Dep (cont)", "Anx (cont)", "Dep (cont)", "Anx (cont)",
                                                "Dep (cont)", "Anx (cont)", 
                                                "Dep (bin)", "Anx (bin)", "Dep (bin)", "Anx (bin)", 
                                                "Dep (bin)", "Anx (bin)"),
                                   outcome = c("Belief (cat)", "Belief (cat)", "Belief (cat)", "Belief (cat)", 
                                               "Identity (cat)", "Identity (cat)", "Identity (cat)", 
                                               "Identity (cat)", "Attend (cat)", "Attend (cat)", "Attend (cat)", 
                                               "Attend (cat)", "Attend (cat)", "Attend (cat)", 
                                               "Belief (cat)", "Belief (cat)", "Belief (cat)", "Belief (cat)", 
                                               "Identity (cat)", "Identity (cat)", "Identity (cat)", 
                                               "Identity (cat)", "Attend (cat)", "Attend (cat)", "Attend (cat)", 
                                               "Attend (cat)", "Attend (cat)", "Attend (cat)",
                                               "Belief (bin)", "Belief (bin)", "Identity (bin)", "Identity (bin)", 
                                               "Attend (bin)", "Attend (bin)", 
                                               "Belief (bin)", "Belief (bin)", "Identity (bin)", "Identity (bin)", 
                                               "Attend (bin)", "Attend (bin)"),
                                   level = c("Not sure", "Yes", "Not sure", "Yes", 
                                             "Christian", "Other", "Christian", "Other", 
                                             "1/yr", "1/mth", "1/wk", "1/yr", "1/mth", "1/wk",
                                             "Not sure", "Yes", "Not sure", "Yes", 
                                             "Christian", "Other", "Christian", "Other", 
                                             "1/yr", "1/mth", "1/wk", "1/yr", "1/mth", "1/wk",
                                             "Yes", "Yes", "Religious", "Religious", "Regular", "Regular",
                                             "Yes", "Yes", "Religious", "Religious", "Regular", "Regular"),
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
partner_results_rsbb


## Also make a table for the predicted differences in the probability of being depressed/anxious for multinomial and logistic models
partner_results_rsbb_probs <- as.data.frame(cbind(model = c("1a", "1a", "1a", "1b", "1b", "1b", 
                                                        "2a", "2a", "2a", "2b", "2b", "2b",
                                                        "3a", "3a", "3a", "3a", "3b", "3b", "3b", "3b",
                                                        "4a", "4a", "4a", "4b", "4b", "4b", 
                                                        "5a", "5a", "5a", "5b", "5b", "5b",
                                                        "6a", "6a", "6a", "6a", "6b", "6b", "6b", "6b",
                                                        "7a", "7b", "8a", "8b", "9a", "9b",
                                                        "10a", "10b", "11a", "11b", "12a", "12b"),
                                              exposure = c("Dep (cont)", "Dep (cont)", "Dep (cont)",
                                                           "Anx (cont)", "Anx (cont)", "Anx (cont)",
                                                           "Dep (cont)", "Dep (cont)", "Dep (cont)",
                                                           "Anx (cont)", "Anx (cont)", "Anx (cont)",
                                                           "Dep (cont)", "Dep (cont)", "Dep (cont)", "Dep (cont)", 
                                                           "Anx (cont)", "Anx (cont)", "Anx (cont)", "Anx (cont)",
                                                           "Dep (bin)", "Dep (bin)", "Dep (bin)",
                                                           "Anx (bin)", "Anx (bin)", "Anx (bin)",
                                                           "Dep (bin)", "Dep (bin)", "Dep (bin)",
                                                           "Anx (bin)", "Anx (bin)", "Anx (bin)",
                                                           "Dep (bin)", "Dep (bin)", "Dep (bin)", "Dep (bin)", 
                                                           "Anx (bin)", "Anx (bin)", "Anx (bin)", "Anx (bin)",
                                                           "Dep (cont)", "Anx (cont)", "Dep (cont)", "Anx (cont)",
                                                           "Dep (cont)", "Anx (cont)", 
                                                           "Dep (bin)", "Anx (bin)", "Dep (bin)", "Anx (bin)", 
                                                           "Dep (bin)", "Anx (bin)"),
                                              outcome = c("Belief (cat)", "Belief (cat)", "Belief (cat)",
                                                          "Belief (cat)", "Belief (cat)", "Belief (cat)",
                                                          "Identity (cat)", "Identity (cat)", "Identity (cat)", 
                                                          "Identity (cat)", "Identity (cat)", "Identity (cat)",
                                                          "Attend (cat)", "Attend (cat)", "Attend (cat)", 
                                                          "Attend (cat)", "Attend (cat)", "Attend (cat)", 
                                                          "Attend (cat)", "Attend (cat)", 
                                                          "Belief (cat)", "Belief (cat)", "Belief (cat)",
                                                          "Belief (cat)", "Belief (cat)", "Belief (cat)",
                                                          "Identity (cat)", "Identity (cat)", "Identity (cat)", 
                                                          "Identity (cat)", "Identity (cat)", "Identity (cat)",
                                                          "Attend (cat)", "Attend (cat)", "Attend (cat)", 
                                                          "Attend (cat)", "Attend (cat)", "Attend (cat)", 
                                                          "Attend (cat)", "Attend (cat)", 
                                                          "Belief (bin)", "Belief (bin)", 
                                                          "Identity (bin)", "Identity (bin)", 
                                                          "Attend (bin)", "Attend (bin)", 
                                                          "Belief (bin)", "Belief (bin)", 
                                                          "Identity (bin)", "Identity (bin)", 
                                                          "Attend (bin)", "Attend (bin)"),
                                              level = c("No", "Not sure", "Yes", "No", "Not sure", "Yes", 
                                                        "None", "Christian", "Other", "None", "Christian", "Other", 
                                                        "Never", "1/yr", "1/mth", "1/wk", 
                                                        "Never", "1/yr", "1/mth", "1/wk",
                                                        "No", "Not sure", "Yes", "No", "Not sure", "Yes", 
                                                        "None", "Christian", "Other", "None", "Christian", "Other", 
                                                        "Never", "1/yr", "1/mth", "1/wk", 
                                                        "Never", "1/yr", "1/mth", "1/wk",
                                                        "Yes", "Yes", "Religious", "Religious", "Regular", "Regular",
                                                        "Yes", "Yes", "Religious", "Religious", "Regular", "Regular"),
                                              est_unadj = rep(NA, 52), 
                                              lci_unadj = rep(NA, 52),
                                              uci_unadj = rep(NA, 52),
                                              est_unadj_z = rep(NA, 52), 
                                              lci_unadj_z = rep(NA, 52),
                                              uci_unadj_z = rep(NA, 52),
                                              est_adj = rep(NA, 52), 
                                              lci_adj = rep(NA, 52),
                                              uci_adj = rep(NA, 52),
                                              est_adj_z = rep(NA, 52), 
                                              lci_adj_z = rep(NA, 52),
                                              uci_adj_z = rep(NA, 52)
))
partner_results_rsbb_probs



#################################################################################
### Analysis 1: Categorical religious belief as outcome, with continuous mental health exposures

## 1a: Depression

# Unadjusted - raw and standardised scales
mod_1a_unadj <- multinom(belief_age5 ~ dep_age2, data = data_partner_cca_rsbb)
summary(mod_1a_unadj)

mod_1a_unadj_z <- multinom(belief_age5 ~ dep_age2_z, data = data_partner_cca_rsbb)
summary(mod_1a_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_1a_unadj))["Not sure", "dep_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1a_unadj)["dep_age2", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1a_unadj)["dep_age2", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_1a_unadj_z))["Not sure", "dep_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1a_unadj_z)["dep_age2_z", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1a_unadj_z)["dep_age2_z", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round((1 - pnorm(abs(summary(mod_1a_unadj)$coefficients["Not sure", "dep_age2"]/
                         summary(mod_1a_unadj)$standard.errors["Not sure", "dep_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_1a_unadj))["Yes", "dep_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1a_unadj)["dep_age2", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1a_unadj)["dep_age2", "97.5 %", "Yes"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_1a_unadj_z))["Yes", "dep_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1a_unadj_z)["dep_age2_z", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1a_unadj_z)["dep_age2_z", "97.5 %", "Yes"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round((1 - pnorm(abs(summary(mod_1a_unadj)$coefficients["Yes", "dep_age2"]/
                         summary(mod_1a_unadj)$standard.errors["Yes", "dep_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_1a_unadj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_1a_unadj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "1a" & 
                                     partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "1a" & 
                                     partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "1a" & 
                                     partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.high[3] * 100, 2)


# Adjusted - raw and standardised scales
mod_1a_adj <- multinom(belief_age5 ~ dep_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_1a_adj)

mod_1a_adj_z <- multinom(belief_age5 ~ dep_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                   age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                   carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                   socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_1a_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_1a_adj))["Not sure", "dep_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1a_adj)["dep_age2", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1a_adj)["dep_age2", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_1a_adj_z))["Not sure", "dep_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1a_adj_z)["dep_age2_z", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1a_adj_z)["dep_age2_z", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Not sure"] <- 
  round((1 - pnorm(abs(summary(mod_1a_adj)$coefficients["Not sure", "dep_age2"]/
                         summary(mod_1a_adj)$standard.errors["Not sure", "dep_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_1a_adj))["Yes", "dep_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1a_adj)["dep_age2", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1a_adj)["dep_age2", "97.5 %", "Yes"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_1a_adj_z))["Yes", "dep_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1a_adj_z)["dep_age2_z", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1a_adj_z)["dep_age2_z", "97.5 %", "Yes"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "1a" & partner_results_rsbb$level == "Yes"] <- 
  round((1 - pnorm(abs(summary(mod_1a_adj)$coefficients["Yes", "dep_age2"]/
                         summary(mod_1a_adj)$standard.errors["Yes", "dep_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_1a_adj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_1a_adj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "1a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.high[3] * 100, 2)


## 1b: Anxiety

# Unadjusted - raw and standardised scales
mod_1b_unadj <- multinom(belief_age5 ~ anx_age2, data = data_partner_cca_rsbb)
summary(mod_1b_unadj)

mod_1b_unadj_z <- multinom(belief_age5 ~ anx_age2_z, data = data_partner_cca_rsbb)
summary(mod_1b_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_1b_unadj))["Not sure", "anx_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1b_unadj)["anx_age2", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1b_unadj)["anx_age2", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_1b_unadj_z))["Not sure", "anx_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1b_unadj_z)["anx_age2_z", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1b_unadj_z)["anx_age2_z", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round((1 - pnorm(abs(summary(mod_1b_unadj)$coefficients["Not sure", "anx_age2"]/
                         summary(mod_1b_unadj)$standard.errors["Not sure", "anx_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_1b_unadj))["Yes", "anx_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1b_unadj)["anx_age2", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1b_unadj)["anx_age2", "97.5 %", "Yes"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_1b_unadj_z))["Yes", "anx_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1b_unadj_z)["anx_age2_z", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1b_unadj_z)["anx_age2_z", "97.5 %", "Yes"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round((1 - pnorm(abs(summary(mod_1b_unadj)$coefficients["Yes", "anx_age2"]/
                         summary(mod_1b_unadj)$standard.errors["Yes", "anx_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_1b_unadj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_1b_unadj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "1b" & 
                                     partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "1b" & 
                                     partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "1b" & 
                                     partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.high[3] * 100, 2)


# Adjusted - raw and standardised scales
mod_1b_adj <- multinom(belief_age5 ~ anx_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                         age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                         carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                         socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_1b_adj)

mod_1b_adj_z <- multinom(belief_age5 ~ anx_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                           age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                           carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + 
                           alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_1b_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_1b_adj))["Not sure", "anx_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1b_adj)["anx_age2", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1b_adj)["anx_age2", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_1b_adj_z))["Not sure", "anx_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1b_adj_z)["anx_age2_z", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_1b_adj_z)["anx_age2_z", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Not sure"] <- 
  round((1 - pnorm(abs(summary(mod_1b_adj)$coefficients["Not sure", "anx_age2"]/
                         summary(mod_1b_adj)$standard.errors["Not sure", "anx_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_1b_adj))["Yes", "anx_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1b_adj)["anx_age2", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1b_adj)["anx_age2", "97.5 %", "Yes"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_1b_adj_z))["Yes", "anx_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1b_adj_z)["anx_age2_z", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_1b_adj_z)["anx_age2_z", "97.5 %", "Yes"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "1b" & partner_results_rsbb$level == "Yes"] <- 
  round((1 - pnorm(abs(summary(mod_1b_adj)$coefficients["Yes", "anx_age2"]/
                         summary(mod_1b_adj)$standard.errors["Yes", "anx_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_1b_adj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_1b_adj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "1b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.high[3] * 100, 2)



#################################################################################
### Analysis 2: Categorical religious affiliation as outcome, with continuous mental health exposures

## 2a: Depression

# Unadjusted - raw and standardised scales
mod_2a_unadj <- multinom(identity_age5 ~ dep_age2, data = data_partner_cca_rsbb)
summary(mod_2a_unadj)

mod_2a_unadj_z <- multinom(identity_age5 ~ dep_age2_z, data = data_partner_cca_rsbb)
summary(mod_2a_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_2a_unadj))["Christian", "dep_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2a_unadj)["dep_age2", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2a_unadj)["dep_age2", "97.5 %", "Christian"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_2a_unadj_z))["Christian", "dep_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2a_unadj_z)["dep_age2_z", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2a_unadj_z)["dep_age2_z", "97.5 %", "Christian"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round((1 - pnorm(abs(summary(mod_2a_unadj)$coefficients["Christian", "dep_age2"]/
                         summary(mod_2a_unadj)$standard.errors["Christian", "dep_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_2a_unadj))["Other", "dep_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2a_unadj)["dep_age2", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2a_unadj)["dep_age2", "97.5 %", "Other"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_2a_unadj_z))["Other", "dep_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2a_unadj_z)["dep_age2_z", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2a_unadj_z)["dep_age2_z", "97.5 %", "Other"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round((1 - pnorm(abs(summary(mod_2a_unadj)$coefficients["Other", "dep_age2"]/
                         summary(mod_2a_unadj)$standard.errors["Other", "dep_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_2a_unadj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_2a_unadj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "2a" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "2a" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "2a" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "2a" & 
                                     partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "2a" & 
                                     partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "2a" & 
                                     partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$conf.high[3] * 100, 2)


# Adjusted - raw and standardised scales
mod_2a_adj <- multinom(identity_age5 ~ dep_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                         age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                         carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                         socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_2a_adj)

mod_2a_adj_z <- multinom(identity_age5 ~ dep_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + 
                           anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                           finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                           activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                         data = data_partner_cca_rsbb)
summary(mod_2a_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_2a_adj))["Christian", "dep_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2a_adj)["dep_age2", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2a_adj)["dep_age2", "97.5 %", "Christian"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_2a_adj_z))["Christian", "dep_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2a_adj_z)["dep_age2_z", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2a_adj_z)["dep_age2_z", "97.5 %", "Christian"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Christian"] <- 
  round((1 - pnorm(abs(summary(mod_2a_adj)$coefficients["Christian", "dep_age2"]/
                         summary(mod_2a_adj)$standard.errors["Christian", "dep_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_2a_adj))["Other", "dep_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2a_adj)["dep_age2", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2a_adj)["dep_age2", "97.5 %", "Other"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_2a_adj_z))["Other", "dep_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2a_adj_z)["dep_age2_z", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2a_adj_z)["dep_age2_z", "97.5 %", "Other"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "2a" & partner_results_rsbb$level == "Other"] <- 
  round((1 - pnorm(abs(summary(mod_2a_adj)$coefficients["Other", "dep_age2"]/
                         summary(mod_2a_adj)$standard.errors["Other", "dep_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_2a_adj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_2a_adj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "2a" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "2a" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "2a" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "2a" & 
                                     partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "2a" & 
                                     partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "2a" & 
                                     partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "2a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$conf.high[3] * 100, 2)


## 2b: Anxiety

# Unadjusted - raw and standardised scales
mod_2b_unadj <- multinom(identity_age5 ~ anx_age2, data = data_partner_cca_rsbb)
summary(mod_2b_unadj)

mod_2b_unadj_z <- multinom(identity_age5 ~ anx_age2_z, data = data_partner_cca_rsbb)
summary(mod_2b_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_2b_unadj))["Christian", "anx_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2b_unadj)["anx_age2", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2b_unadj)["anx_age2", "97.5 %", "Christian"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_2b_unadj_z))["Christian", "anx_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2b_unadj_z)["anx_age2_z", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2b_unadj_z)["anx_age2_z", "97.5 %", "Christian"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round((1 - pnorm(abs(summary(mod_2b_unadj)$coefficients["Christian", "anx_age2"]/
                         summary(mod_2b_unadj)$standard.errors["Christian", "anx_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_2b_unadj))["Other", "anx_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2b_unadj)["anx_age2", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2b_unadj)["anx_age2", "97.5 %", "Other"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_2b_unadj_z))["Other", "anx_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2b_unadj_z)["anx_age2_z", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2b_unadj_z)["anx_age2_z", "97.5 %", "Other"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round((1 - pnorm(abs(summary(mod_2b_unadj)$coefficients["Other", "anx_age2"]/
                         summary(mod_2b_unadj)$standard.errors["Other", "anx_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_2b_unadj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_2b_unadj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "2b" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "2b" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "2b" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "2b" & 
                                     partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "2b" & 
                                     partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "2b" & 
                                     partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$conf.high[3] * 100, 2)


# Adjusted - raw and standardised scales
mod_2b_adj <- multinom(identity_age5 ~ anx_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                         age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                         carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                         socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_2b_adj)

mod_2b_adj_z <- multinom(identity_age5 ~ anx_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + 
                           anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                           finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                           activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                         data = data_partner_cca_rsbb)
summary(mod_2b_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_2b_adj))["Christian", "anx_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2b_adj)["anx_age2", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2b_adj)["anx_age2", "97.5 %", "Christian"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_2b_adj_z))["Christian", "anx_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2b_adj_z)["anx_age2_z", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_2b_adj_z)["anx_age2_z", "97.5 %", "Christian"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Christian"] <- 
  round((1 - pnorm(abs(summary(mod_2b_adj)$coefficients["Christian", "anx_age2"]/
                         summary(mod_2b_adj)$standard.errors["Christian", "anx_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_2b_adj))["Other", "anx_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2b_adj)["anx_age2", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2b_adj)["anx_age2", "97.5 %", "Other"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_2b_adj_z))["Other", "anx_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2b_adj_z)["anx_age2_z", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_2b_adj_z)["anx_age2_z", "97.5 %", "Other"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "2b" & partner_results_rsbb$level == "Other"] <- 
  round((1 - pnorm(abs(summary(mod_2b_adj)$coefficients["Other", "anx_age2"]/
                         summary(mod_2b_adj)$standard.errors["Other", "anx_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_2b_adj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_2b_adj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "2b" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "2b" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "2b" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "2b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob_z$conf.high[3] * 100, 2)



#################################################################################
### Analysis 3: Categorical religious attendance as outcome, with continuous mental health exposures

## 3a: Depression

# Unadjusted - raw and standardised scales
mod_3a_unadj <- multinom(attend_age5 ~ dep_age2, data = data_partner_cca_rsbb)
summary(mod_3a_unadj)

mod_3a_unadj_z <- multinom(attend_age5 ~ dep_age2_z, data = data_partner_cca_rsbb)
summary(mod_3a_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_3a_unadj))["MIN 1 a YR", "dep_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3a_unadj)["dep_age2", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3a_unadj)["dep_age2", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_3a_unadj_z))["MIN 1 a YR", "dep_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3a_unadj_z)["dep_age2_z", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3a_unadj_z)["dep_age2_z", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round((1 - pnorm(abs(summary(mod_3a_unadj)$coefficients["MIN 1 a YR", "dep_age2"]/
                         summary(mod_3a_unadj)$standard.errors["MIN 1 a YR", "dep_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_3a_unadj))["MIN 1 a MTH", "dep_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3a_unadj)["dep_age2", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3a_unadj)["dep_age2", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_3a_unadj_z))["MIN 1 a MTH", "dep_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3a_unadj_z)["dep_age2_z", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3a_unadj_z)["dep_age2_z", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round((1 - pnorm(abs(summary(mod_3a_unadj)$coefficients["MIN 1 a MTH", "dep_age2"]/
                         summary(mod_3a_unadj)$standard.errors["MIN 1 a MTH", "dep_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_3a_unadj))["MIN 1 a WK", "dep_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3a_unadj)["dep_age2", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3a_unadj)["dep_age2", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_3a_unadj_z))["MIN 1 a WK", "dep_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3a_unadj_z)["dep_age2_z", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3a_unadj_z)["dep_age2_z", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round((1 - pnorm(abs(summary(mod_3a_unadj)$coefficients["MIN 1 a WK", "dep_age2"]/
                         summary(mod_3a_unadj)$standard.errors["MIN 1 a WK", "dep_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_3a_unadj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_3a_unadj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$conf.high[3] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.high[4] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$conf.high[4] * 100, 2)


# Adjusted - raw and standardised scales
mod_3a_adj <- multinom(attend_age5 ~ dep_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                         age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                         carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                         socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_3a_adj)

mod_3a_adj_z <- multinom(attend_age5 ~ dep_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + 
                           anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                           finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                           activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                         data = data_partner_cca_rsbb)
summary(mod_3a_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_3a_adj))["MIN 1 a YR", "dep_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3a_adj)["dep_age2", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3a_adj)["dep_age2", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_3a_adj_z))["MIN 1 a YR", "dep_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3a_adj_z)["dep_age2_z", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3a_adj_z)["dep_age2_z", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/yr"] <- 
  round((1 - pnorm(abs(summary(mod_3a_adj)$coefficients["MIN 1 a YR", "dep_age2"]/
                         summary(mod_3a_adj)$standard.errors["MIN 1 a YR", "dep_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_3a_adj))["MIN 1 a MTH", "dep_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3a_adj)["dep_age2", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3a_adj)["dep_age2", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_3a_adj_z))["MIN 1 a MTH", "dep_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3a_adj_z)["dep_age2_z", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3a_adj_z)["dep_age2_z", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/mth"] <- 
  round((1 - pnorm(abs(summary(mod_3a_adj)$coefficients["MIN 1 a MTH", "dep_age2"]/
                         summary(mod_3a_adj)$standard.errors["MIN 1 a MTH", "dep_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_3a_adj))["MIN 1 a WK", "dep_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3a_adj)["dep_age2", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3a_adj)["dep_age2", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_3a_adj_z))["MIN 1 a WK", "dep_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3a_adj_z)["dep_age2_z", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3a_adj_z)["dep_age2_z", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "3a" & partner_results_rsbb$level == "1/wk"] <- 
  round((1 - pnorm(abs(summary(mod_3a_adj)$coefficients["MIN 1 a WK", "dep_age2"]/
                         summary(mod_3a_adj)$standard.errors["MIN 1 a WK", "dep_age2"]), 0, 1)) * 2, 4)


## As is an association here - with higher anxiety scores associated with lower rates of attending at least once a year, relative to not at all - will calculate the E-value/level of unmeasured confounding necessary to make this result null, using approximate conversions from odds ratios (or relative risk ratios, in this case) to risk ratios - As rates of attendance at least once a year relatively common, will use the 'rare = FALSE' option (see ?evalues.OR).

evalues.OR(est = 0.858, lo = 0.735, hi = 1.003, true = 1, rare = FALSE)

# E-value to null is 1.37, meaning that an unmeasured confounder which increases the risk of both the exposure and outcome by ~35% is necessary to completely remove the observed association - Although the upper 95% CI already crosses the null...


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_3a_adj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_3a_adj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$conf.high[3] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "3a" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.high[4] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "3a" & 
                                     partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$conf.high[4] * 100, 2)


## 3b: Anxiety

# Unadjusted - raw and standardised scales
mod_3b_unadj <- multinom(attend_age5 ~ anx_age2, data = data_partner_cca_rsbb)
summary(mod_3b_unadj)

mod_3b_unadj_z <- multinom(attend_age5 ~ anx_age2_z, data = data_partner_cca_rsbb)
summary(mod_3b_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_3b_unadj))["MIN 1 a YR", "anx_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3b_unadj)["anx_age2", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3b_unadj)["anx_age2", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_3b_unadj_z))["MIN 1 a YR", "anx_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3b_unadj_z)["anx_age2_z", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3b_unadj_z)["anx_age2_z", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round((1 - pnorm(abs(summary(mod_3b_unadj)$coefficients["MIN 1 a YR", "anx_age2"]/
                         summary(mod_3b_unadj)$standard.errors["MIN 1 a YR", "anx_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_3b_unadj))["MIN 1 a MTH", "anx_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3b_unadj)["anx_age2", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3b_unadj)["anx_age2", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_3b_unadj_z))["MIN 1 a MTH", "anx_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3b_unadj_z)["anx_age2_z", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3b_unadj_z)["anx_age2_z", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round((1 - pnorm(abs(summary(mod_3b_unadj)$coefficients["MIN 1 a MTH", "anx_age2"]/
                         summary(mod_3b_unadj)$standard.errors["MIN 1 a MTH", "anx_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_3b_unadj))["MIN 1 a WK", "anx_age2"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3b_unadj)["anx_age2", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3b_unadj)["anx_age2", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_3b_unadj_z))["MIN 1 a WK", "anx_age2_z"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3b_unadj_z)["anx_age2_z", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3b_unadj_z)["anx_age2_z", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round((1 - pnorm(abs(summary(mod_3b_unadj)$coefficients["MIN 1 a WK", "anx_age2"]/
                         summary(mod_3b_unadj)$standard.errors["MIN 1 a WK", "anx_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_3b_unadj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_3b_unadj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$conf.high[3] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.high[4] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "3b" & 
                                     partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$conf.high[4] * 100, 2)


# Adjusted - raw and standardised scales
mod_3b_adj <- multinom(attend_age5 ~ anx_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                         age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                         carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                         socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_3b_adj)

mod_3b_adj_z <- multinom(attend_age5 ~ anx_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + 
                           anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                           finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                           activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                         data = data_partner_cca_rsbb)
summary(mod_3b_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_3b_adj))["MIN 1 a YR", "anx_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3b_adj)["anx_age2", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3b_adj)["anx_age2", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_3b_adj_z))["MIN 1 a YR", "anx_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3b_adj_z)["anx_age2_z", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_3b_adj_z)["anx_age2_z", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/yr"] <- 
  round((1 - pnorm(abs(summary(mod_3b_adj)$coefficients["MIN 1 a YR", "anx_age2"]/
                         summary(mod_3b_adj)$standard.errors["MIN 1 a YR", "anx_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_3b_adj))["MIN 1 a MTH", "anx_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3b_adj)["anx_age2", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3b_adj)["anx_age2", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_3b_adj_z))["MIN 1 a MTH", "anx_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3b_adj_z)["anx_age2_z", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_3b_adj_z)["anx_age2_z", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/mth"] <- 
  round((1 - pnorm(abs(summary(mod_3b_adj)$coefficients["MIN 1 a MTH", "anx_age2"]/
                         summary(mod_3b_adj)$standard.errors["MIN 1 a MTH", "anx_age2"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_3b_adj))["MIN 1 a WK", "anx_age2"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3b_adj)["anx_age2", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3b_adj)["anx_age2", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_3b_adj_z))["MIN 1 a WK", "anx_age2_z"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3b_adj_z)["anx_age2_z", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_3b_adj_z)["anx_age2_z", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "3b" & partner_results_rsbb$level == "1/wk"] <- 
  round((1 - pnorm(abs(summary(mod_3b_adj)$coefficients["MIN 1 a WK", "anx_age2"]/
                         summary(mod_3b_adj)$standard.errors["MIN 1 a WK", "anx_age2"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_3b_adj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_3b_adj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob_z$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.high[2] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob_z$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.high[3] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob_z$conf.high[3] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "Never"] <-
  round(prob$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "3b" & 
                                 partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.high[4] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "3b" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob_z$conf.high[4] * 100, 2)




#################################################################################
### Analysis 4: Categorical religious belief as outcome, with binary mental health exposures

## 4a: Depression

# Unadjusted
mod_4a_unadj <- multinom(belief_age5 ~ dep_age2_bin, data = data_partner_cca_rsbb)
summary(mod_4a_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_4a_unadj))["Not sure", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_4a_unadj)["dep_age2_binDep", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_4a_unadj)["dep_age2_binDep", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Not sure"] <- 
  round((1 - pnorm(abs(summary(mod_4a_unadj)$coefficients["Not sure", "dep_age2_binDep"]/
                         summary(mod_4a_unadj)$standard.errors["Not sure", "dep_age2_binDep"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_4a_unadj))["Yes", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_4a_unadj)["dep_age2_binDep", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_4a_unadj)["dep_age2_binDep", "97.5 %", "Yes"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Yes"] <- 
  round((1 - pnorm(abs(summary(mod_4a_unadj)$coefficients["Yes", "dep_age2_binDep"]/
                         summary(mod_4a_unadj)$standard.errors["Yes", "dep_age2_binDep"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_4a_unadj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[3] * 100, 2)


# Adjusted
mod_4a_adj <- multinom(belief_age5 ~ dep_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                         age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                         carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                         socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_4a_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_4a_adj))["Not sure", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_4a_adj)["dep_age2_binDep", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_4a_adj)["dep_age2_binDep", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Not sure"] <- 
  round((1 - pnorm(abs(summary(mod_4a_adj)$coefficients["Not sure", "dep_age2_binDep"]/
                         summary(mod_4a_adj)$standard.errors["Not sure", "dep_age2_binDep"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_4a_adj))["Yes", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_4a_adj)["dep_age2_binDep", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_4a_adj)["dep_age2_binDep", "97.5 %", "Yes"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "4a" & partner_results_rsbb$level == "Yes"] <- 
  round((1 - pnorm(abs(summary(mod_4a_adj)$coefficients["Yes", "dep_age2_binDep"]/
                         summary(mod_4a_adj)$standard.errors["Yes", "dep_age2_binDep"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_4a_adj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "4a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[3] * 100, 2)


## 4b: Anxiety

# Unadjusted
mod_4b_unadj <- multinom(belief_age5 ~ anx_age2_bin, data = data_partner_cca_rsbb)
summary(mod_4b_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_4b_unadj))["Not sure", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_4b_unadj)["anx_age2_binAnx", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_4b_unadj)["anx_age2_binAnx", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Not sure"] <- 
  round((1 - pnorm(abs(summary(mod_4b_unadj)$coefficients["Not sure", "anx_age2_binAnx"]/
                         summary(mod_4b_unadj)$standard.errors["Not sure", "anx_age2_binAnx"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_4b_unadj))["Yes", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_4b_unadj)["anx_age2_binAnx", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_4b_unadj)["anx_age2_binAnx", "97.5 %", "Yes"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Yes"] <- 
  round((1 - pnorm(abs(summary(mod_4b_unadj)$coefficients["Yes", "anx_age2_binAnx"]/
                         summary(mod_4b_unadj)$standard.errors["Yes", "anx_age2_binAnx"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_4b_unadj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[3] * 100, 2)


# Adjusted
mod_4b_adj <- multinom(belief_age5 ~ anx_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                         age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                         carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                         socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_4b_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(coef(summary(mod_4b_adj))["Not sure", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_4b_adj)["anx_age2_binAnx", "2.5 %", "Not sure"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Not sure"] <- 
  round(exp(confint(mod_4b_adj)["anx_age2_binAnx", "97.5 %", "Not sure"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Not sure"] <- 
  round((1 - pnorm(abs(summary(mod_4b_adj)$coefficients["Not sure", "anx_age2_binAnx"]/
                         summary(mod_4b_adj)$standard.errors["Not sure", "anx_age2_binAnx"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_4b_adj))["Yes", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_4b_adj)["anx_age2_binAnx", "2.5 %", "Yes"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_4b_adj)["anx_age2_binAnx", "97.5 %", "Yes"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "4b" & partner_results_rsbb$level == "Yes"] <- 
  round((1 - pnorm(abs(summary(mod_4b_adj)$coefficients["Yes", "anx_age2_binAnx"]/
                         summary(mod_4b_adj)$standard.errors["Yes", "anx_age2_binAnx"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_4b_adj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "No"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Not sure"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "4b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[3] * 100, 2)



#################################################################################
### Analysis 5: Categorical religious affiliation as outcome, with binary mental health exposures

## 5a: Depression

# Unadjusted
mod_5a_unadj <- multinom(identity_age5 ~ dep_age2_bin, data = data_partner_cca_rsbb)
summary(mod_5a_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_5a_unadj))["Christian", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_5a_unadj)["dep_age2_binDep", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_5a_unadj)["dep_age2_binDep", "97.5 %", "Christian"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Christian"] <- 
  round((1 - pnorm(abs(summary(mod_5a_unadj)$coefficients["Christian", "dep_age2_binDep"]/
                         summary(mod_5a_unadj)$standard.errors["Christian", "dep_age2_binDep"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_5a_unadj))["Other", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_5a_unadj)["dep_age2_binDep", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_5a_unadj)["dep_age2_binDep", "97.5 %", "Other"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Other"] <- 
  round((1 - pnorm(abs(summary(mod_5a_unadj)$coefficients["Other", "dep_age2_binDep"]/
                         summary(mod_5a_unadj)$standard.errors["Other", "dep_age2_binDep"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_5a_unadj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "5a" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "5a" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "5a" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.high[3] * 100, 2)


# Adjusted
mod_5a_adj <- multinom(identity_age5 ~ dep_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + 
                         anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                         finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                         activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                       data = data_partner_cca_rsbb)
summary(mod_5a_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_5a_adj))["Christian", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_5a_adj)["dep_age2_binDep", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_5a_adj)["dep_age2_binDep", "97.5 %", "Christian"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Christian"] <- 
  round((1 - pnorm(abs(summary(mod_5a_adj)$coefficients["Christian", "dep_age2_binDep"]/
                         summary(mod_5a_adj)$standard.errors["Christian", "dep_age2_binDep"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_5a_adj))["Other", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_5a_adj)["dep_age2_binDep", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_5a_adj)["dep_age2_binDep", "97.5 %", "Other"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "5a" & partner_results_rsbb$level == "Other"] <- 
  round((1 - pnorm(abs(summary(mod_5a_adj)$coefficients["Other", "dep_age2_binDep"]/
                         summary(mod_5a_adj)$standard.errors["Other", "dep_age2_binDep"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_5a_adj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "5a" & 
                                 partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "5a" & 
                                 partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "5a" & 
                                 partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "5a" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.high[3] * 100, 2)


## 5b: Anxiety

# Unadjusted
mod_5b_unadj <- multinom(identity_age5 ~ anx_age2_bin, data = data_partner_cca_rsbb)
summary(mod_5b_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_5b_unadj))["Christian", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_5b_unadj)["anx_age2_binAnx", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_5b_unadj)["anx_age2_binAnx", "97.5 %", "Christian"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Christian"] <- 
  round((1 - pnorm(abs(summary(mod_5b_unadj)$coefficients["Christian", "anx_age2_binAnx"]/
                         summary(mod_5b_unadj)$standard.errors["Christian", "anx_age2_binAnx"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_5b_unadj))["Other", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_5b_unadj)["anx_age2_binAnx", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_5b_unadj)["anx_age2_binAnx", "97.5 %", "Other"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Other"] <- 
  round((1 - pnorm(abs(summary(mod_5b_unadj)$coefficients["Other", "anx_age2_binAnx"]/
                         summary(mod_5b_unadj)$standard.errors["Other", "anx_age2_binAnx"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_5b_unadj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "5b" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "5b" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "5b" & 
                                   partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.high[3] * 100, 2)


# Adjusted
mod_5b_adj <- multinom(identity_age5 ~ anx_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + 
                         anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                         finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                         activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                       data = data_partner_cca_rsbb)
summary(mod_5b_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(coef(summary(mod_5b_adj))["Christian", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_5b_adj)["anx_age2_binAnx", "2.5 %", "Christian"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Christian"] <- 
  round(exp(confint(mod_5b_adj)["anx_age2_binAnx", "97.5 %", "Christian"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Christian"] <- 
  round((1 - pnorm(abs(summary(mod_5b_adj)$coefficients["Christian", "anx_age2_binAnx"]/
                         summary(mod_5b_adj)$standard.errors["Christian", "anx_age2_binAnx"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(coef(summary(mod_5b_adj))["Other", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_5b_adj)["anx_age2_binAnx", "2.5 %", "Other"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Other"] <- 
  round(exp(confint(mod_5b_adj)["anx_age2_binAnx", "97.5 %", "Other"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "5b" & partner_results_rsbb$level == "Other"] <- 
  round((1 - pnorm(abs(summary(mod_5b_adj)$coefficients["Other", "anx_age2_binAnx"]/
                         summary(mod_5b_adj)$standard.errors["Other", "anx_age2_binAnx"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_5b_adj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "Christian"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "None"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "5b" & partner_results_rsbb_probs $level == "Other"] <-
  round(prob$conf.high[3] * 100, 2)



#################################################################################
### Analysis 6: Categorical religious attendance as outcome, with binary mental health exposures

## 6a: Depression

# Unadjusted
mod_6a_unadj <- multinom(attend_age5 ~ dep_age2_bin, data = data_partner_cca_rsbb)
summary(mod_6a_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_6a_unadj))["MIN 1 a YR", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_6a_unadj)["dep_age2_binDep", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_6a_unadj)["dep_age2_binDep", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/yr"] <- 
  round((1 - pnorm(abs(summary(mod_6a_unadj)$coefficients["MIN 1 a YR", "dep_age2_binDep"]/
                         summary(mod_6a_unadj)$standard.errors["MIN 1 a YR", "dep_age2_binDep"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_6a_unadj))["MIN 1 a MTH", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_6a_unadj)["dep_age2_binDep", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_6a_unadj)["dep_age2_binDep", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/mth"] <- 
  round((1 - pnorm(abs(summary(mod_6a_unadj)$coefficients["MIN 1 a MTH", "dep_age2_binDep"]/
                         summary(mod_6a_unadj)$standard.errors["MIN 1 a MTH", "dep_age2_binDep"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_6a_unadj))["MIN 1 a WK", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_6a_unadj)["dep_age2_binDep", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_6a_unadj)["dep_age2_binDep", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/wk"] <- 
  round((1 - pnorm(abs(summary(mod_6a_unadj)$coefficients["MIN 1 a WK", "dep_age2_binDep"]/
                         summary(mod_6a_unadj)$standard.errors["MIN 1 a WK", "dep_age2_binDep"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_6a_unadj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.high[3] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "6a" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.high[4] * 100, 2)


# Adjusted
mod_6a_adj <- multinom(attend_age5 ~ dep_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                         age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                         carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                         socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_6a_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_6a_adj))["MIN 1 a YR", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_6a_adj)["dep_age2_binDep", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_6a_adj)["dep_age2_binDep", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/yr"] <- 
  round((1 - pnorm(abs(summary(mod_6a_adj)$coefficients["MIN 1 a YR", "dep_age2_binDep"]/
                         summary(mod_6a_adj)$standard.errors["MIN 1 a YR", "dep_age2_binDep"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_6a_adj))["MIN 1 a MTH", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_6a_adj)["dep_age2_binDep", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_6a_adj)["dep_age2_binDep", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/mth"] <- 
  round((1 - pnorm(abs(summary(mod_6a_adj)$coefficients["MIN 1 a MTH", "dep_age2_binDep"]/
                         summary(mod_6a_adj)$standard.errors["MIN 1 a MTH", "dep_age2_binDep"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_6a_adj))["MIN 1 a WK", "dep_age2_binDep"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_6a_adj)["dep_age2_binDep", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_6a_adj)["dep_age2_binDep", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "6a" & partner_results_rsbb$level == "1/wk"] <- 
  round((1 - pnorm(abs(summary(mod_6a_adj)$coefficients["MIN 1 a WK", "dep_age2_binDep"]/
                         summary(mod_6a_adj)$standard.errors["MIN 1 a WK", "dep_age2_binDep"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_6a_adj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.high[3] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "Never"] <-
  round(prob$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "6a" & 
                                 partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.high[4] * 100, 2)


## 6b: Anxiety

# Unadjusted
mod_6b_unadj <- multinom(attend_age5 ~ anx_age2_bin, data = data_partner_cca_rsbb)
summary(mod_6b_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_6b_unadj))["MIN 1 a YR", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_6b_unadj)["anx_age2_binAnx", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_6b_unadj)["anx_age2_binAnx", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/yr"] <- 
  round((1 - pnorm(abs(summary(mod_6b_unadj)$coefficients["MIN 1 a YR", "anx_age2_binAnx"]/
                         summary(mod_6b_unadj)$standard.errors["MIN 1 a YR", "anx_age2_binAnx"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_6b_unadj))["MIN 1 a MTH", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_6b_unadj)["anx_age2_binAnx", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_6b_unadj)["anx_age2_binAnx", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/mth"] <- 
  round((1 - pnorm(abs(summary(mod_6b_unadj)$coefficients["MIN 1 a MTH", "anx_age2_binAnx"]/
                         summary(mod_6b_unadj)$standard.errors["MIN 1 a MTH", "anx_age2_binAnx"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_6b_unadj))["MIN 1 a WK", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_6b_unadj)["anx_age2_binAnx", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_6b_unadj)["anx_age2_binAnx", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/wk"] <- 
  round((1 - pnorm(abs(summary(mod_6b_unadj)$coefficients["MIN 1 a WK", "anx_age2_binAnx"]/
                         summary(mod_6b_unadj)$standard.errors["MIN 1 a WK", "anx_age2_binAnx"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_6b_unadj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.high[3] * 100, 2)

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "6b" & 
                                   partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.high[4] * 100, 2)


# Adjusted
mod_6b_adj <- multinom(attend_age5 ~ anx_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                         age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                         carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                         socNetwork + socSupport + maternalMH + paternalMH, data = data_partner_cca_rsbb)
summary(mod_6b_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(coef(summary(mod_6b_adj))["MIN 1 a YR", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_6b_adj)["anx_age2_binAnx", "2.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/yr"] <- 
  round(exp(confint(mod_6b_adj)["anx_age2_binAnx", "97.5 %", "MIN 1 a YR"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/yr"] <- 
  round((1 - pnorm(abs(summary(mod_6b_adj)$coefficients["MIN 1 a YR", "anx_age2_binAnx"]/
                         summary(mod_6b_adj)$standard.errors["MIN 1 a YR", "anx_age2_binAnx"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(coef(summary(mod_6b_adj))["MIN 1 a MTH", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_6b_adj)["anx_age2_binAnx", "2.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/mth"] <- 
  round(exp(confint(mod_6b_adj)["anx_age2_binAnx", "97.5 %", "MIN 1 a MTH"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/mth"] <- 
  round((1 - pnorm(abs(summary(mod_6b_adj)$coefficients["MIN 1 a MTH", "anx_age2_binAnx"]/
                         summary(mod_6b_adj)$standard.errors["MIN 1 a MTH", "anx_age2_binAnx"]), 0, 1)) * 2, 4)

partner_results_rsbb$est_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(coef(summary(mod_6b_adj))["MIN 1 a WK", "anx_age2_binAnx"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_6b_adj)["anx_age2_binAnx", "2.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/wk"] <- 
  round(exp(confint(mod_6b_adj)["anx_age2_binAnx", "97.5 %", "MIN 1 a WK"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "6b" & partner_results_rsbb$level == "1/wk"] <- 
  round((1 - pnorm(abs(summary(mod_6b_adj)$coefficients["MIN 1 a WK", "anx_age2_binAnx"]/
                         summary(mod_6b_adj)$standard.errors["MIN 1 a WK", "anx_age2_binAnx"]), 0, 1)) * 2, 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_6b_adj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "1/mth"] <-
  round(prob$conf.high[1] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$estimate[2] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.low[2] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "1/wk"] <-
  round(prob$conf.high[2] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$estimate[3] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.low[3] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "1/yr"] <-
  round(prob$conf.high[3] * 100, 2)

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "Never"] <-
  round(prob$estimate[4] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.low[4] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "6b" & 
                                 partner_results_rsbb_probs $level == "Never"] <-
  round(prob$conf.high[4] * 100, 2)




#################################################################################
### Analysis 7: Binary religious belief as outcome, with continuous mental health exposures

## 7a: Depression

# Unadjusted - raw and standardised scales
mod_7a_unadj <- glm(belief_age5_bin ~ dep_age2, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_7a_unadj)

mod_7a_unadj_z <- glm(belief_age5_bin ~ dep_age2_z, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_7a_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_7a_unadj))["dep_age2", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7a_unadj)["dep_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7a_unadj)["dep_age2", "97.5 %"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_7a_unadj_z))["dep_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7a_unadj_z)["dep_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7a_unadj_z)["dep_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(coef(summary(mod_7a_unadj))["dep_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_7a_unadj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_7a_unadj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.high[1] * 100, 2)


# Adjusted - raw and standardised scales
mod_7a_adj <- glm(belief_age5_bin ~ dep_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_7a_adj)

mod_7a_adj_z <- glm(belief_age5_bin ~ dep_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                      age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                      carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                      socNetwork + socSupport + maternalMH + paternalMH, 
                    data = data_partner_cca_rsbb, family = "binomial")
summary(mod_7a_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_7a_adj))["dep_age2", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7a_adj)["dep_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7a_adj)["dep_age2", "97.5 %"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_7a_adj_z))["dep_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7a_adj_z)["dep_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7a_adj_z)["dep_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "7a" & partner_results_rsbb$level == "Yes"] <- 
  round(coef(summary(mod_7a_adj))["dep_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_7a_adj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_7a_adj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "7a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.high[1] * 100, 2)


## 7b: Anxiety

# Unadjusted - raw and standardised scales
mod_7b_unadj <- glm(belief_age5_bin ~ anx_age2, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_7b_unadj)

mod_7b_unadj_z <- glm(belief_age5_bin ~ anx_age2_z, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_7b_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_7b_unadj))["anx_age2", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7b_unadj)["anx_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7b_unadj)["anx_age2", "97.5 %"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_7b_unadj_z))["anx_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7b_unadj_z)["anx_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7b_unadj_z)["anx_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(coef(summary(mod_7b_unadj))["anx_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_7b_unadj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_7b_unadj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.high[1] * 100, 2)


# Adjusted - raw and standardised scales
mod_7b_adj <- glm(belief_age5_bin ~ anx_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_7b_adj)

mod_7b_adj_z <- glm(belief_age5_bin ~ anx_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                      age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                      carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + 
                      alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                    data = data_partner_cca_rsbb, family = "binomial")
summary(mod_7b_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_7b_adj))["anx_age2", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7b_adj)["anx_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7b_adj)["anx_age2", "97.5 %"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_7b_adj_z))["anx_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7b_adj_z)["anx_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_7b_adj_z)["anx_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "7b" & partner_results_rsbb$level == "Yes"] <- 
  round(coef(summary(mod_7b_adj))["anx_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_7b_adj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_7b_adj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "7b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob_z$conf.high[1] * 100, 2)



#################################################################################
### Analysis 8: Binary religious affiliation as outcome, with continuous mental health exposures

## 8a: Depression

# Unadjusted - raw and standardised scales
mod_8a_unadj <- glm(identity_age5_bin ~ dep_age2, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_8a_unadj)

mod_8a_unadj_z <- glm(identity_age5_bin ~ dep_age2_z, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_8a_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_8a_unadj))["dep_age2", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8a_unadj)["dep_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8a_unadj)["dep_age2", "97.5 %"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_8a_unadj_z))["dep_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8a_unadj_z)["dep_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8a_unadj_z)["dep_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(coef(summary(mod_8a_unadj))["dep_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_8a_unadj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_8a_unadj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "8a" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "8a" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "8a" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "8a" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "8a" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "8a" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$conf.high[1] * 100, 2)


# Adjusted - raw and standardised scales
mod_8a_adj <- glm(identity_age5_bin ~ dep_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_8a_adj)

mod_8a_adj_z <- glm(identity_age5_bin ~ dep_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + 
                      anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                      finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                      activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                    data = data_partner_cca_rsbb, family = "binomial")
summary(mod_8a_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_8a_adj))["dep_age2", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8a_adj)["dep_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8a_adj)["dep_age2", "97.5 %"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_8a_adj_z))["dep_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8a_adj_z)["dep_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8a_adj_z)["dep_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "8a" & partner_results_rsbb$level == "Religious"] <- 
  round(coef(summary(mod_8a_adj))["dep_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_8a_adj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_8a_adj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "8a" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "8a" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "8a" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "8a" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "8a" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "8a" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$conf.high[1] * 100, 2)


## 8b: Anxiety

# Unadjusted - raw and standardised scales
mod_8b_unadj <- glm(identity_age5_bin ~ anx_age2, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_8b_unadj)

mod_8b_unadj_z <- glm(identity_age5_bin ~ anx_age2_z, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_8b_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_8b_unadj))["anx_age2", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8b_unadj)["anx_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8b_unadj)["anx_age2", "97.5 %"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_8b_unadj_z))["anx_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8b_unadj_z)["anx_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8b_unadj_z)["anx_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(coef(summary(mod_8b_unadj))["anx_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_8b_unadj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_8b_unadj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "8b" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "8b" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "8b" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "8b" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "8b" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "8b" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$conf.high[1] * 100, 2)


# Adjusted - raw and standardised scales
mod_8b_adj <- glm(identity_age5_bin ~ anx_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_8b_adj)

mod_8b_adj_z <- glm(identity_age5_bin ~ anx_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + 
                      anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                      finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                      activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                    data = data_partner_cca_rsbb, family = "binomial")
summary(mod_8b_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_8b_adj))["anx_age2", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8b_adj)["anx_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8b_adj)["anx_age2", "97.5 %"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_8b_adj_z))["anx_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8b_adj_z)["anx_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_8b_adj_z)["anx_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "8b" & partner_results_rsbb$level == "Religious"] <- 
  round(coef(summary(mod_8b_adj))["anx_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_8b_adj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_8b_adj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "8b" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "8b" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "8b" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "8b" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "8b" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "8b" & 
                                     partner_results_rsbb_probs $level == "Religious"] <-
  round(prob_z$conf.high[1] * 100, 2)



#################################################################################
### Analysis 9: Binary religious attendance as outcome, with continuous mental health exposures

## 9a: Depression

# Unadjusted - raw and standardised scales
mod_9a_unadj <- glm(attend_age5_bin ~ dep_age2, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_9a_unadj)

mod_9a_unadj_z <- glm(attend_age5_bin ~ dep_age2_z, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_9a_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_9a_unadj))["dep_age2", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9a_unadj)["dep_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9a_unadj)["dep_age2", "97.5 %"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_9a_unadj_z))["dep_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9a_unadj_z)["dep_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9a_unadj_z)["dep_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(coef(summary(mod_9a_unadj))["dep_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_9a_unadj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_9a_unadj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "9a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "9a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "9a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "9a" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "9a" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "9a" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$conf.high[1] * 100, 2)


# Adjusted - raw and standardised scales
mod_9a_adj <- glm(attend_age5_bin ~ dep_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_9a_adj)

mod_9a_adj_z <- glm(attend_age5_bin ~ dep_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + 
                      anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                      finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                      activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                    data = data_partner_cca_rsbb, family = "binomial")
summary(mod_9a_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_9a_adj))["dep_age2", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9a_adj)["dep_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9a_adj)["dep_age2", "97.5 %"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_9a_adj_z))["dep_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9a_adj_z)["dep_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9a_adj_z)["dep_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "9a" & partner_results_rsbb$level == "Regular"] <- 
  round(coef(summary(mod_9a_adj))["dep_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_9a_adj, variable = "dep_age2"))
(prob_z <- avg_comparisons(mod_9a_adj_z, variable = "dep_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "9a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "9a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "9a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "9a" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "9a" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "9a" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$conf.high[1] * 100, 2)


## 9b: Anxiety

# Unadjusted - raw and standardised scales
mod_9b_unadj <- glm(attend_age5_bin ~ anx_age2, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_9b_unadj)

mod_9b_unadj_z <- glm(attend_age5_bin ~ anx_age2_z, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_9b_unadj_z)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_9b_unadj))["anx_age2", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9b_unadj)["anx_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9b_unadj)["anx_age2", "97.5 %"]), 3)
partner_results_rsbb$est_unadj_z[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_9b_unadj_z))["anx_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_unadj_z[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9b_unadj_z)["anx_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj_z[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9b_unadj_z)["anx_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(coef(summary(mod_9b_unadj))["anx_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_9b_unadj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_9b_unadj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "9b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "9b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "9b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_unadj_z[partner_results_rsbb_probs$model == "9b" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj_z[partner_results_rsbb_probs$model == "9b" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj_z[partner_results_rsbb_probs$model == "9b" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$conf.high[1] * 100, 2)


# Adjusted - raw and standardised scales
mod_9b_adj <- glm(attend_age5_bin ~ anx_age2 + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_9b_adj)

mod_9b_adj_z <- glm(attend_age5_bin ~ anx_age2_z + belief_preg + identity_preg + attend_preg + dep_preg + 
                      anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + 
                      finDiffs + home + carAccess + employed + ACEs + locus + IPSM + health + BMI + 
                      activity + smoking + alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                    data = data_partner_cca_rsbb, family = "binomial")
summary(mod_9b_adj_z)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_9b_adj))["anx_age2", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9b_adj)["anx_age2", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9b_adj)["anx_age2", "97.5 %"]), 3)
partner_results_rsbb$est_adj_z[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_9b_adj_z))["anx_age2_z", "Estimate"]), 3)
partner_results_rsbb$lci_adj_z[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9b_adj_z)["anx_age2_z", "2.5 %"]), 3)
partner_results_rsbb$uci_adj_z[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_9b_adj_z)["anx_age2_z", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "9b" & partner_results_rsbb$level == "Regular"] <- 
  round(coef(summary(mod_9b_adj))["anx_age2", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_9b_adj, variable = "anx_age2"))
(prob_z <- avg_comparisons(mod_9b_adj_z, variable = "anx_age2_z"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "9b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "9b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "9b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.high[1] * 100, 2)
partner_results_rsbb_probs$est_adj_z[partner_results_rsbb_probs$model == "9b" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj_z[partner_results_rsbb_probs$model == "9b" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj_z[partner_results_rsbb_probs$model == "9b" & 
                                     partner_results_rsbb_probs $level == "Regular"] <-
  round(prob_z$conf.high[1] * 100, 2)




#################################################################################
### Analysis 10: Binary religious belief as outcome, with binary mental health exposures

## 10a: Depression

# Unadjusted
mod_10a_unadj <- glm(belief_age5_bin ~ dep_age2_bin, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_10a_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "10a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_10a_unadj))["dep_age2_binDep", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "10a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_10a_unadj)["dep_age2_binDep", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "10a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_10a_unadj)["dep_age2_binDep", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "10a" & partner_results_rsbb$level == "Yes"] <- 
  round(coef(summary(mod_10a_unadj))["dep_age2_binDep", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_10a_unadj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "10a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "10a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "10a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[1] * 100, 2)


# Adjusted
mod_10a_adj <- glm(belief_age5_bin ~ dep_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_10a_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "10a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_10a_adj))["dep_age2_binDep", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "10a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_10a_adj)["dep_age2_binDep", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "10a" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_10a_adj)["dep_age2_binDep", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "10a" & partner_results_rsbb$level == "Yes"] <- 
  round(coef(summary(mod_10a_adj))["dep_age2_binDep", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_10a_adj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "10a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "10a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "10a" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[1] * 100, 2)


## 10b: Anxiety

# Unadjusted
mod_10b_unadj <- glm(belief_age5_bin ~ anx_age2_bin, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_10b_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "10b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_10b_unadj))["anx_age2_binAnx", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "10b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_10b_unadj)["anx_age2_binAnx", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "10b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_10b_unadj)["anx_age2_binAnx", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "10b" & partner_results_rsbb$level == "Yes"] <- 
  round(coef(summary(mod_10b_unadj))["anx_age2_binAnx", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_10b_unadj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "10b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "10b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "10b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[1] * 100, 2)


# Adjusted
mod_10b_adj <- glm(belief_age5_bin ~ anx_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_10b_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "10b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(coef(summary(mod_10b_adj))["anx_age2_binAnx", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "10b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_10b_adj)["anx_age2_binAnx", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "10b" & partner_results_rsbb$level == "Yes"] <- 
  round(exp(confint(mod_10b_adj)["anx_age2_binAnx", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "10b" & partner_results_rsbb$level == "Yes"] <- 
  round(coef(summary(mod_10b_adj))["anx_age2_binAnx", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_10b_adj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "10b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "10b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "10b" & partner_results_rsbb_probs $level == "Yes"] <-
  round(prob$conf.high[1] * 100, 2)



#################################################################################
### Analysis 11: Binary religious affiliation as outcome, with binary mental health exposures

## 11a: Depression

# Unadjusted
mod_11a_unadj <- glm(identity_age5_bin ~ dep_age2_bin, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_11a_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "11a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_11a_unadj))["dep_age2_binDep", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "11a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_11a_unadj)["dep_age2_binDep", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "11a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_11a_unadj)["dep_age2_binDep", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "11a" & partner_results_rsbb$level == "Religious"] <- 
  round(coef(summary(mod_11a_unadj))["dep_age2_binDep", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_11a_unadj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "11a" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "11a" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "11a" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.high[1] * 100, 2)


# Adjusted
mod_11a_adj <- glm(identity_age5_bin ~ dep_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + 
                     anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + 
                     home + carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + 
                     alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                   data = data_partner_cca_rsbb, family = "binomial")
summary(mod_11a_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "11a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_11a_adj))["dep_age2_binDep", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "11a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_11a_adj)["dep_age2_binDep", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "11a" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_11a_adj)["dep_age2_binDep", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "11a" & partner_results_rsbb$level == "Religious"] <- 
  round(coef(summary(mod_11a_adj))["dep_age2_binDep", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_11a_adj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "11a" & 
                                 partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "11a" & 
                                 partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "11a" & 
                                 partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.high[1] * 100, 2)


## 11b: Anxiety

# Unadjusted
mod_11b_unadj <- glm(identity_age5_bin ~ anx_age2_bin, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_11b_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "11b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_11b_unadj))["anx_age2_binAnx", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "11b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_11b_unadj)["anx_age2_binAnx", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "11b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_11b_unadj)["anx_age2_binAnx", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "11b" & partner_results_rsbb$level == "Religious"] <- 
  round(coef(summary(mod_11b_unadj))["anx_age2_binAnx", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_11b_unadj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "11b" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "11b" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "11b" & 
                                   partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.high[1] * 100, 2)


# Adjusted
mod_11b_adj <- glm(identity_age5_bin ~ anx_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + 
                     anx_preg + age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + 
                     home + carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + 
                     alcPrePreg + socNetwork + socSupport + maternalMH + paternalMH, 
                   data = data_partner_cca_rsbb, family = "binomial")
summary(mod_11b_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "11b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(coef(summary(mod_11b_adj))["anx_age2_binAnx", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "11b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_11b_adj)["anx_age2_binAnx", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "11b" & partner_results_rsbb$level == "Religious"] <- 
  round(exp(confint(mod_11b_adj)["anx_age2_binAnx", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "11b" & partner_results_rsbb$level == "Religious"] <- 
  round(coef(summary(mod_11b_adj))["anx_age2_binAnx", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_11b_adj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "11b" & 
                                 partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "11b" & 
                                 partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "11b" & 
                                 partner_results_rsbb_probs $level == "Religious"] <-
  round(prob$conf.high[1] * 100, 2)




#################################################################################
### Analysis 12: Binary religious attendance as outcome, with binary mental health exposures

## 12a: Depression

# Unadjusted
mod_12a_unadj <- glm(attend_age5_bin ~ dep_age2_bin, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_12a_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "12a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_12a_unadj))["dep_age2_binDep", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "12a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_12a_unadj)["dep_age2_binDep", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "12a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_12a_unadj)["dep_age2_binDep", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "12a" & partner_results_rsbb$level == "Regular"] <- 
  round(coef(summary(mod_12a_unadj))["dep_age2_binDep", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_12a_unadj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "12a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "12a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "12a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.high[1] * 100, 2)


# Adjusted
mod_12a_adj <- glm(attend_age5_bin ~ dep_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_12a_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "12a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_12a_adj))["dep_age2_binDep", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "12a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_12a_adj)["dep_age2_binDep", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "12a" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_12a_adj)["dep_age2_binDep", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "12a" & partner_results_rsbb$level == "Regular"] <- 
  round(coef(summary(mod_12a_adj))["dep_age2_binDep", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_12a_adj, variable = "dep_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "12a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "12a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "12a" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.high[1] * 100, 2)



## 12b: Anxiety

# Unadjusted
mod_12b_unadj <- glm(attend_age5_bin ~ anx_age2_bin, data = data_partner_cca_rsbb, family = "binomial")
summary(mod_12b_unadj)

# Store results in table
partner_results_rsbb$est_unadj[partner_results_rsbb$model == "12b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_12b_unadj))["anx_age2_binAnx", "Estimate"]), 3)
partner_results_rsbb$lci_unadj[partner_results_rsbb$model == "12b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_12b_unadj)["anx_age2_binAnx", "2.5 %"]), 3)
partner_results_rsbb$uci_unadj[partner_results_rsbb$model == "12b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_12b_unadj)["anx_age2_binAnx", "97.5 %"]), 3)
partner_results_rsbb$p_unadj[partner_results_rsbb$model == "12b" & partner_results_rsbb$level == "Regular"] <- 
  round(coef(summary(mod_12b_unadj))["anx_age2_binAnx", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_12b_unadj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_unadj[partner_results_rsbb_probs$model == "12b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_unadj[partner_results_rsbb_probs$model == "12b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_unadj[partner_results_rsbb_probs$model == "12b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.high[1] * 100, 2)


# Adjusted
mod_12b_adj <- glm(attend_age5_bin ~ anx_age2_bin + belief_preg + identity_preg + attend_preg + dep_preg + anx_preg + 
                    age + ethnicity + marital + parity + rural + edu + occSocClass + imd + finDiffs + home +
                    carAccess + employed + ACEs + locus + IPSM + health + BMI + activity + smoking + alcPrePreg +
                    socNetwork + socSupport + maternalMH + paternalMH, 
                  data = data_partner_cca_rsbb, family = "binomial")
summary(mod_12b_adj)

# Store results in table
partner_results_rsbb$est_adj[partner_results_rsbb$model == "12b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(coef(summary(mod_12b_adj))["anx_age2_binAnx", "Estimate"]), 3)
partner_results_rsbb$lci_adj[partner_results_rsbb$model == "12b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_12b_adj)["anx_age2_binAnx", "2.5 %"]), 3)
partner_results_rsbb$uci_adj[partner_results_rsbb$model == "12b" & partner_results_rsbb$level == "Regular"] <- 
  round(exp(confint(mod_12b_adj)["anx_age2_binAnx", "97.5 %"]), 3)
partner_results_rsbb$p_adj[partner_results_rsbb$model == "12b" & partner_results_rsbb$level == "Regular"] <- 
  round(coef(summary(mod_12b_adj))["anx_age2_binAnx", "Pr(>|z|)"], 4)


## Probability of difference in RSBB outcome
(prob <- avg_comparisons(mod_12b_adj, variable = "anx_age2_bin"))

partner_results_rsbb_probs$est_adj[partner_results_rsbb_probs$model == "12b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$estimate[1] * 100, 2)
partner_results_rsbb_probs$lci_adj[partner_results_rsbb_probs$model == "12b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.low[1] * 100, 2)
partner_results_rsbb_probs$uci_adj[partner_results_rsbb_probs$model == "12b" & partner_results_rsbb_probs $level == "Regular"] <-
  round(prob$conf.high[1] * 100, 2)



################################################################################################
#### Make plots to summarise results

partner_results_rsbb

# Convert results to numeric
partner_results_rsbb <- partner_results_rsbb %>%
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

partner_results_rsbb
glimpse(partner_results_rsbb)

# Save these results
write_csv(partner_results_rsbb, file = "./PartnerResults/MHCauseRSBB/partner_results.csv")

#partner_results_rsbb <- read_csv(file = "./PartnerResults/MHCauseRSBB/partner_results.csv")
#head(partner_results_rsbb)


### Turn these results into plots

## Models 1-3 - Categorical RSBB outcomes and continuous MH exposures

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- partner_results_rsbb %>%
  filter(model == "1a" | model == "1b" | model == "2a" | model == "2b" | model == "3a" | model == "3b") %>%
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
  mutate(outcome = recode(outcome, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                           "Attend (cat)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot1 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
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
    scale_y_continuous(trans = "log", breaks = c(0.9, 1, 1.1, 1.2, 1.3)) +
    labs(x = "Religious outcome", y = "Relative risk ratio for 1-unit increase in exposure") +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                                "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
    )

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod123.pdf", height = 10, width = 10)
plot1
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- partner_results_rsbb %>%
  filter(model == "1a" | model == "1b" | model == "2a" | model == "2b" | model == "3a" | model == "3b") %>%
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
  mutate(outcome = recode(outcome, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                          "Attend (cat)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot1_z <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
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
    scale_y_continuous(trans = "log", breaks = c(0.75, 1, 1.5, 2)) +
    labs(x = "Religious outcome", y = "Relative risk ratio for 1-unit increase in standardised exposure") +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                                "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod123_std.pdf", height = 10, width = 10)
plot1_z
dev.off()


## Models 4-6 - Categorical RSBB outcomes and binary MH exposures

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- partner_results_rsbb %>%
  filter(model == "4a" | model == "4b" | model == "5a" | model == "5b" | model == "6a" | model == "6b") %>%
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
  mutate(outcome = recode(outcome, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                          "Attend (cat)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (bin)", "Anx (bin)"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                      ifelse(level == "Yes", 2, 
                             ifelse(level == "Christian", 4,
                                    ifelse(level == "Other", 5,
                                           ifelse(level == "1/yr", 7,
                                                  ifelse(level == "1/mth", 8, 9)))))))

res1

(plot4 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
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
    scale_y_continuous(trans = "log", breaks = c(0.25, 0.5, 1, 2, 4, 8)) +
    labs(x = "Religious outcome", y = "Relative risk ratio for change in exposure (ref = No)") +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (bin)" = "Depression", 
                                                                "Anx (bin)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod456.pdf", height = 10, width = 10)
plot4
dev.off()



## Models 7-9 - Binary RSBB outcomes and continuous MH exposures

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- partner_results_rsbb %>%
  filter(model == "7a" | model == "7b" | model == "8a" | model == "8b" | model == "9a" | model == "9b") %>%
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
  mutate(outcome = recode(outcome, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                          "Attend (bin)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (cont)", "Anx (cont)"))) %>%
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
    scale_y_continuous(trans = "log", breaks = c(0.95, 1, 1.05, 1.1)) +
    labs(x = "Religious outcome", y = "Odds ratio for 1-unit increase in exposure") +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                                "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod789.pdf", height = 10, width = 10)
plot7
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- partner_results_rsbb %>%
  filter(model == "7a" | model == "7b" | model == "8a" | model == "8b" | model == "9a" | model == "9b") %>%
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
  mutate(outcome = recode(outcome, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                          "Attend (bin)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot7_z <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    scale_y_continuous(trans = "log", breaks = c(0.9, 1, 1.1, 1.2, 1.3)) +
    labs(x = "Religious outcome", y = "Odds ratio for 1-unit increase in exposure") +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                                "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod789_std.pdf", height = 10, width = 10)
plot7_z
dev.off()



## Models 10-12 - Binary RSBB outcomes and binary MH exposures

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- partner_results_rsbb %>%
  filter(model == "10a" | model == "10b" | model == "11a" | model == "11b" | model == "12a" | model == "12b") %>%
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
  mutate(outcome = recode(outcome, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                          "Attend (bin)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (bin)", "Anx (bin)"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot10 <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    scale_y_continuous(trans = "log", breaks = c(0.75, 1, 1.25, 1.5, 2)) +
    labs(x = "Religious outcome", y = "Odds ratio for change in exposure (ref = No)") +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (bin)" = "Depression", 
                                                                "Anx (bin)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod101112.pdf", height = 10, width = 10)
plot10
dev.off()




########################################################################################
#### Also make plots for predicted differences in depression and anxiety from multinomial and logistic models

partner_results_rsbb_probs

# Convert results to numeric
partner_results_rsbb_probs <- partner_results_rsbb_probs %>%
  mutate(est_unadj = as.numeric(est_unadj)) %>%
  mutate(lci_unadj = as.numeric(lci_unadj)) %>%
  mutate(uci_unadj = as.numeric(uci_unadj)) %>%
  mutate(est_unadj_z = as.numeric(est_unadj_z)) %>%
  mutate(lci_unadj_z = as.numeric(lci_unadj_z)) %>%
  mutate(uci_unadj_z = as.numeric(uci_unadj_z)) %>%
  mutate(est_adj = as.numeric(est_adj)) %>%
  mutate(lci_adj = as.numeric(lci_adj)) %>%
  mutate(uci_adj = as.numeric(uci_adj)) %>%
  mutate(est_adj_z = as.numeric(est_adj_z)) %>%
  mutate(lci_adj_z = as.numeric(lci_adj_z)) %>%
  mutate(uci_adj_z = as.numeric(uci_adj_z))

partner_results_rsbb_probs
glimpse(partner_results_rsbb_probs)

# Save these results
write_csv(partner_results_rsbb_probs, file = "./PartnerResults/MHCauseRSBB/partner_results_probs.csv")

#partner_results_rsbb_probs <- read_csv(file = "./PartnerResults/MHCauseRSBB/partner_results_probs.csv")
#head(partner_results_rsbb_probs)


### Turn these results into plots

## Models 1-3 - Categorical RSBB outcomes and continuous MH exposures

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- partner_results_rsbb_probs %>%
  filter(model == "1a" | model == "1b" | model == "2a" | model == "2b" | model == "3a" | model == "3b") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, est_adj_z, lci_adj_z, uci_adj_z))

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
  mutate(outcome = recode(outcome, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                          "Attend (cat)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "No", 1,
                      ifelse(level == "Not sure", 2,
                             ifelse(level == "Yes", 3,
                                    ifelse(level == "None", 5,
                                           ifelse(level == "Christian", 6,
                                                  ifelse(level == "Other", 7,
                                                         ifelse(level == "Never", 9,
                                                                ifelse(level == "1/yr", 10,
                                                                       ifelse(level == "1/mth", 11, 12))))))))))

res1

(plot1_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 3, 5, 6, 7, 9, 10, 11, 12), 
                    labels = c("Belief - No", "Belief - Not sure", "Belief - Yes",
                               "Identity - None", "Identity - Christian", "Identity - Other",
                               "Attendance - Never", "Attendance - 1/Yr", 
                               "Attendance - 1/Mth", "Attendance - 1/Wk")) +
    labs(x = "Religious outcome", y = "Change in predicted probability for 1-unit increase in exposure") +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                                "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod123_probs.pdf", height = 10, width = 10)
plot1_probs
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- partner_results_rsbb_probs %>%
  filter(model == "1a" | model == "1b" | model == "2a" | model == "2b" | model == "3a" | model == "3b") %>%
  select(-c(est_unadj, lci_unadj, uci_unadj, est_adj, lci_adj, uci_adj))

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
  mutate(outcome = recode(outcome, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                          "Attend (cat)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "No", 1,
                      ifelse(level == "Not sure", 2,
                             ifelse(level == "Yes", 3,
                                    ifelse(level == "None", 5,
                                           ifelse(level == "Christian", 6,
                                                  ifelse(level == "Other", 7,
                                                         ifelse(level == "Never", 9,
                                                                ifelse(level == "1/yr", 10,
                                                                       ifelse(level == "1/mth", 11, 12))))))))))

res1

(plot1_z_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 3, 5, 6, 7, 9, 10, 11, 12), 
                    labels = c("Belief - No", "Belief - Not sure", "Belief - Yes",
                               "Identity - None", "Identity - Christian", "Identity - Other",
                               "Attendance - Never", "Attendance - 1/Yr", 
                               "Attendance - 1/Mth", "Attendance - 1/Wk")) +
    labs(x = "Religious outcome", y = "Change in predicted probability for 1-unit increase in standardised exposure") +
    scale_y_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                                "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod123_probs_std.pdf", height = 10, width = 10)
plot1_z_probs
dev.off()


## Models 4-6 - Categorical RSBB outcomes and binary MH exposures

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- partner_results_rsbb_probs %>%
  filter(model == "4a" | model == "4b" | model == "5a" | model == "5b" | model == "6a" | model == "6b") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, est_adj_z, lci_adj_z, uci_adj_z))

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
  mutate(outcome = recode(outcome, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                          "Attend (cat)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (bin)", "Anx (bin)"))) %>%
  mutate(num = ifelse(level == "No", 1,
                      ifelse(level == "Not sure", 2,
                             ifelse(level == "Yes", 3,
                                    ifelse(level == "None", 5,
                                           ifelse(level == "Christian", 6,
                                                  ifelse(level == "Other", 7,
                                                         ifelse(level == "Never", 9,
                                                                ifelse(level == "1/yr", 10,
                                                                       ifelse(level == "1/mth", 11, 12))))))))))

res1

(plot4_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2, 3, 5, 6, 7, 9, 10, 11, 12), 
                    labels = c("Belief - No", "Belief - Not sure", "Belief - Yes",
                               "Identity - None", "Identity - Christian", "Identity - Other",
                               "Attendance - Never", "Attendance - 1/Yr", 
                               "Attendance - 1/Mth", "Attendance - 1/Wk")) +
    labs(x = "Religious outcome", y = "Change in predicted probability for change in exposure") +
    scale_y_continuous(breaks = c(-9, -6, -3, 0, 3, 6, 9)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (bin)" = "Depression", 
                                                                "Anx (bin)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod456_probs.pdf", height = 10, width = 10)
plot4_probs
dev.off()


## Models 7-9 - Binary RSBB outcomes and continuous MH exposures

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- partner_results_rsbb_probs %>%
  filter(model == "7a" | model == "7b" | model == "8a" | model == "8b" | model == "9a" | model == "9b") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, est_adj_z, lci_adj_z, uci_adj_z))

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
  mutate(outcome = recode(outcome, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                          "Attend (cat)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (cont)", "Anx (cont)"))) %>%
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
    scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5)) +
    labs(x = "Religious outcome", y = "Change in predicted probability for 1-unit increase in exposure") +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                                "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod789_probs.pdf", height = 10, width = 10)
plot7_probs
dev.off()


## Same plot as above, but now with standardised scores
res_temp <- partner_results_rsbb_probs %>%
  filter(model == "7a" | model == "7b" | model == "8a" | model == "8b" | model == "9a" | model == "9b") %>%
  select(-c(est_unadj, lci_unadj, uci_unadj, est_adj, lci_adj, uci_adj))

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
  mutate(outcome = recode(outcome, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                          "Attend (cat)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (cont)", "Anx (cont)"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot7_z_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious outcome", y = "Change in predicted probability for 1-unit increase in standardised exposure") +
    scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (cont)" = "Depression", 
                                                                "Anx (cont)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod789_probs_std.pdf", height = 10, width = 10)
plot7_z_probs
dev.off()


## Models 10-12 - Binary RSBB outcomes and binary MH exposures

# Reduce data, select just unstandardised estimates, and convert to long format
res_temp <- partner_results_rsbb_probs %>%
  filter(model == "10a" | model == "10b" | model == "11a" | model == "11b" | model == "12a" | model == "12b") %>%
  select(-c(est_unadj_z, lci_unadj_z, uci_unadj_z, est_adj_z, lci_adj_z, uci_adj_z))

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
  mutate(outcome = recode(outcome, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                          "Attend (bin)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(exposure = factor(exposure, levels = c("Dep (bin)", "Anx (bin)"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot10_probs <- ggplot(res1, aes(x = num, y = est, ymin = lci, ymax = uci, col = adj, fill = adj)) + 
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious outcome", y = "Change in predicted probability for change in exposure") +
    scale_y_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(exposure ~ ., ncol = 1, labeller = as_labeller(c("Dep (bin)" = "Depression", 
                                                                "Anx (bin)" = "Anxiety"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/partner_results_mod101112_probs.pdf", height = 10, width = 10)
plot10_probs
dev.off()



##############################################################################################
### Formal comparison of whether mother and partner results differ

## Read in mother's results
mum_results_rsbb <- read_csv(file = "./MotherResults/MHCauseRSBB/mum_results.csv")
head(mum_results_rsbb)

# Focus first on adjusted results for multinomial outcomes - Convert to log scale, convert CIs to SE and rename variables - For continuous exposures, use standardised scores
mum_results_rsbb_rrr <- mum_results_rsbb %>%
  filter(outcome == "Belief (cat)" | outcome == "Identity (cat)" | outcome == "Attend (cat)") %>%
  mutate(est_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", est_adj_z, est_adj)) %>%
  mutate(lci_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", lci_adj_z, lci_adj)) %>%
  mutate(uci_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", uci_adj_z, uci_adj)) %>%
  select(model:level, est_adj:uci_adj) %>%
  mutate(est_log = log(est_adj), lci_log = log(lci_adj), uci_log = log(uci_adj)) %>%
  mutate(se_mum = (uci_log - lci_log) / (2 * 1.96)) %>%
  rename(rrr_mum = est_adj, log_rrr_mum = est_log) %>%
  select(-c(lci_adj, uci_adj, lci_log, uci_log))
head(mum_results_rsbb_rrr)

# Next on adjusted logistic results - Convert to log scale, convert CIs to SE and rename variables - For continuous exposures, use standardised scores
mum_results_rsbb_or <- mum_results_rsbb %>%
  filter(outcome == "Belief (bin)" | outcome == "Identity (bin)" | outcome == "Attend (bin)") %>%
  mutate(est_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", est_adj_z, est_adj)) %>%
  mutate(lci_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", lci_adj_z, lci_adj)) %>%
  mutate(uci_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", uci_adj_z, uci_adj)) %>%
  select(model:level, est_adj:uci_adj) %>%
  mutate(est_log = log(est_adj), lci_log = log(lci_adj), uci_log = log(uci_adj)) %>%
  mutate(se_mum = (uci_log - lci_log) / (2 * 1.96)) %>%
  rename(or_mum = est_adj, log_or_mum = est_log) %>%
  select(-c(lci_adj, uci_adj, lci_log, uci_log))
head(mum_results_rsbb_or)


## Read in partner's results (f needed)
#partner_results_rsbb <- read_csv(file = "./PartnerResults/MHCauseRSBB/partner_results.csv")
head(partner_results_rsbb)

# Focus first on adjusted results for multinomial outcomes - Convertn to log scale, convert CIs to SE and rename variables - For continuous exposures, use standardised scores
partner_results_rsbb_rrr <- partner_results_rsbb %>%
  filter(outcome == "Belief (cat)" | outcome == "Identity (cat)" | outcome == "Attend (cat)") %>%
  mutate(est_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", est_adj_z, est_adj)) %>%
  mutate(lci_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", lci_adj_z, lci_adj)) %>%
  mutate(uci_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", uci_adj_z, uci_adj)) %>%
  select(model:level, est_adj:uci_adj) %>%
  mutate(est_log = log(est_adj), lci_log = log(lci_adj), uci_log = log(uci_adj)) %>%
  mutate(se_partner = (uci_log - lci_log) / (2 * 1.96)) %>%
  rename(rrr_partner = est_adj, log_rrr_partner = est_log) %>%
  select(-c(lci_adj, uci_adj, lci_log, uci_log))
head(partner_results_rsbb_rrr)

# Next on adjusted logistic results - Convert to log scale, convert CIs to SE and rename variables - For continuous exposures, use standardised scores
partner_results_rsbb_or <- partner_results_rsbb %>%
  filter(outcome == "Belief (bin)" | outcome == "Identity (bin)" | outcome == "Attend (bin)") %>%
  mutate(est_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", est_adj_z, est_adj)) %>%
  mutate(lci_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", lci_adj_z, lci_adj)) %>%
  mutate(uci_adj = ifelse(exposure == "Dep (cont)" | exposure == "Anx (cont)", uci_adj_z, uci_adj)) %>%
  select(model:level, est_adj:uci_adj) %>%
  mutate(est_log = log(est_adj), lci_log = log(lci_adj), uci_log = log(uci_adj)) %>%
  mutate(se_partner = (uci_log - lci_log) / (2 * 1.96)) %>%
  rename(or_partner = est_adj, log_or_partner = est_log) %>%
  select(-c(lci_adj, uci_adj, lci_log, uci_log))
head(partner_results_rsbb_or)


### Combine mother and partner results together and calculate differences

## Multinomial results first

# Merge together
results_rsbb_rrr <- left_join(mum_results_rsbb_rrr, partner_results_rsbb_rrr)
head(results_rsbb_rrr)

# RRR difference between mothers and partners and combined SE, plus CIs and p-value (based on z-distribution), then convert back to RRR scale
results_rsbb_rrr <- results_rsbb_rrr %>%
  mutate(diff_log = log_rrr_mum - log_rrr_partner) %>%
  mutate(diff_se = sqrt((se_mum ^ 2) + (se_partner ^ 2))) %>%
  mutate(diff_p = 2 * pnorm(q = abs(diff_log / diff_se), lower.tail = FALSE)) %>%
  mutate(diff_rrr = exp(diff_log)) %>%
  mutate(diff_lci = exp(diff_log - (1.96 * diff_se))) %>%
  mutate(diff_uci = exp(diff_log + (1.96 * diff_se)))

head(results_rsbb_rrr)
summary(results_rsbb_rrr)

# Save these results
write_csv(results_rsbb_rrr, file = "./PartnerResults/MHCauseRSBB/MumVsPartner_Multinomial_Results.csv")


## Now for logistic results

# Merge together
results_rsbb_or <- left_join(mum_results_rsbb_or, partner_results_rsbb_or)
head(results_rsbb_or)

# Odds ratio difference between mothers and partners and combined SE, plus CIs and p-value (based on z-distribution), then convert back to odds ratio scale
results_rsbb_or <- results_rsbb_or %>%
  mutate(diff_log = log_or_mum - log_or_partner) %>%
  mutate(diff_se = sqrt((se_mum ^ 2) + (se_partner ^ 2))) %>%
  mutate(diff_p = 2 * pnorm(q = abs(diff_log / diff_se), lower.tail = FALSE)) %>%
  mutate(diff_or = exp(diff_log)) %>%
  mutate(diff_lci = exp(diff_log - (1.96 * diff_se))) %>%
  mutate(diff_uci = exp(diff_log + (1.96 * diff_se)))

head(results_rsbb_or)
summary(results_rsbb_or)

# Save these results
write_csv(results_rsbb_or, file = "./PartnerResults/MHCauseRSBB/MumVsPartner_Logistic_Results.csv")


### Make plots of these results

## First for multinomial models
results_rsbb_rrr

res1 <- results_rsbb_rrr %>%
  mutate(exp_type = recode(exposure, "Dep (cont)" = "Cont", "Dep (bin)" = "Binary",
                           "Anx (cont)" = "Cont", "Anx (bin)" = "Binary")) %>%
  mutate(exp_type = factor(exp_type, levels = c("Cont", "Binary"))) %>%
  mutate(exposure = recode(exposure, "Dep (cont)" = "Depression", "Dep (bin)" = "Depression",
                           "Anx (cont)" = "Anxiety", "Anx (bin)" = "Anxiety")) %>%
  mutate(exposure = factor(exposure, levels = c("Anxiety", "Depression"))) %>%
  mutate(outcome = recode(outcome, "Belief (cat)" = "Belief", "Identity (cat)" = "Identity",
                          "Attend (cat)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Not sure", 1,
                             ifelse(level == "Yes", 2,
                                           ifelse(level == "Christian", 4,
                                                  ifelse(level == "Other", 5,
                                                                ifelse(level == "1/yr", 7,
                                                                       ifelse(level == "1/mth", 8, 9)))))))

res1

(plot_rrr <- ggplot(res1, aes(x = num, y = diff_rrr, ymin = diff_lci, ymax = diff_uci, 
                              col = exposure, fill = exposure)) + 
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
    labs(x = "Religious outcome", y = "Ratio of relative risk ratios") +
    scale_y_continuous(trans = "log", breaks = c(0.1, 0.25, 0.5, 1, 2, 3)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(exp_type ~ ., ncol = 1, labeller = as_labeller(c("Cont" = "Mental health score (standardised)", 
                                                                "Binary" = "Probable diagnosis"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/motherVsPartner_Multinomial_plots.pdf", height = 10, width = 10)
plot_rrr
dev.off()


## Now for logistic models
results_rsbb_or

res1 <- results_rsbb_or %>%
  mutate(exp_type = recode(exposure, "Dep (cont)" = "Cont", "Dep (bin)" = "Binary",
                           "Anx (cont)" = "Cont", "Anx (bin)" = "Binary")) %>%
  mutate(exp_type = factor(exp_type, levels = c("Cont", "Binary"))) %>%
  mutate(exposure = recode(exposure, "Dep (cont)" = "Depression", "Dep (bin)" = "Depression",
                           "Anx (cont)" = "Anxiety", "Anx (bin)" = "Anxiety")) %>%
  mutate(exposure = factor(exposure, levels = c("Anxiety", "Depression"))) %>%
  mutate(outcome = recode(outcome, "Belief (bin)" = "Belief", "Identity (bin)" = "Identity",
                          "Attend (bin)" = "Attend")) %>%
  mutate(outcome = factor(outcome, levels = c("Belief", "Identity", "Attend"))) %>%
  mutate(num = ifelse(level == "Yes", 1,
                      ifelse(level == "Religious", 2.5, 4)))

res1

(plot_or <- ggplot(res1, aes(x = num, y = diff_or, ymin = diff_lci, ymax = diff_uci, 
                              col = exposure, fill = exposure)) + 
    geom_hline(yintercept = 1, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, title = "")) +
    scale_x_reverse(breaks = c(1, 2.5, 4), 
                    labels = c("Belief - Yes \n(ref = No)", "Identity - Religious \n(ref = None)", 
                               "Attendance - Regular \n(ref = Occasional/Never)")) +
    labs(x = "Religious outcome", y = "Ratio of odds ratios") +
    scale_y_continuous(trans = "log", breaks = c(0.25, 0.5, 1, 2)) +
    coord_flip() +
    theme_classic() +
    facet_wrap(exp_type ~ ., ncol = 1, labeller = as_labeller(c("Cont" = "Mental health score (standardised)", 
                                                                "Binary" = "Probable diagnosis"))) +
    theme(axis.text = element_text(size = 12), legend.text = element_text(size = 14),
          axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14),
          strip.background = element_blank(), strip.text = element_text(size = 18))
)

# Save this plot as PDF
pdf("./PartnerResults/MHCauseRSBB/motherVsPartner_Logistic_plots.pdf", height = 10, width = 10)
plot_or
dev.off()


