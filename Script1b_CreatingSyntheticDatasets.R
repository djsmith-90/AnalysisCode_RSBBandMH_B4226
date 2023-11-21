### Script for paper 'Exploring bidirectional causality between religion and mental health: A longitudinal study using data from the parental generation of a UK birth cohort (ALSPAC)' (ALSPAC B-number B4226)
### Script 1b: Creating synthetic ALSPAC datasets
### Created 13/7/2023 by Dan Major-Smith
### R version 4.0.4

## The analysis plan for this paper has been pre-registered and is available on the OSF: https://osf.io/qtdze/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4226 - RSBB and MH")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("synthpop")
library(synthpop)

library(nnet)


###########################################################################################
###### Read in the processed data and create synthetic datasets

#### First for mother's data with RSBB as exposures and mental health as outcomes

## Read in the data
load("data_mum_processed_B4226.RData")

# Make a complete-case dataset with no missing values for the analyses with mental health at age 6 as the outcome - Also drop the binary RSBB and MH variables, as will re-create these post-synthesis - And move exposures and outcomes to end of dataset so are synthesised last (which may help preserve relations between variables)
data_mum_cca_mh <- data_mum %>%
  select(c(belief_preg:attend_age5_bin, dep_preg, anx_preg, dep_age6:anx_age6_bin, age:paternalMH)) %>%
  select(-c(belief_age5_bin, identity_age5_bin, attend_age5_bin, dep_age6_bin, anx_age6_bin)) %>%
  relocate(age:paternalMH, belief_preg:attend_preg, dep_preg, anx_preg, belief_age5:attend_age5, dep_age6, anx_age6) %>%
  filter(complete.cases(.))

head(data_mum_cca_mh)

# Get information about variables in the dataset
codebook.syn(data_mum_cca_mh)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
data_mum_cca_mh_syn <- syn(data_mum_cca_mh, seed = 186805)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, are no unique replicates
replicated.uniques(data_mum_cca_mh_syn, data_mum_cca_mh)
data_mum_cca_mh_syn <- sdc(data_mum_cca_mh_syn, data_mum_cca_mh, rm.replicated.uniques = TRUE)

## Take a few unique true observations, and make sure not fully-replicated in synthetic dataset (based on the 'replicated.uniques' command from the 'synthpop' package)

# Make a dataset just of unique individuals using the observed data (as if two or more participants share exactly the same data, then it's impossible to link back to a unique individual)
sum(!(duplicated(data_mum_cca_mh) | duplicated(data_mum_cca_mh, fromLast = TRUE)))
dat_unique <- data_mum_cca_mh[!(duplicated(data_mum_cca_mh) | duplicated(data_mum_cca_mh, fromLast = TRUE)), ]

# Make a dataset just of unique individuals from the synthetic dataset
sum(!(duplicated(data_mum_cca_mh_syn$syn) | duplicated(data_mum_cca_mh_syn$syn, fromLast = TRUE)))
syn_unique <- data_mum_cca_mh_syn$syn[!(duplicated(data_mum_cca_mh_syn$syn) | 
                                          duplicated(data_mum_cca_mh_syn$syn, fromLast = TRUE)), ]

# Select a random row from the observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 1), ])

# Combine observed row with the synthetic data, and see if any duplicates
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Repeat for a few more rows of observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 10), ])
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Explore this synthetic dataset
data_mum_cca_mh_syn
summary(data_mum_cca_mh_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is good). Save this as a PDF
compare(data_mum_cca_mh_syn, data_mum_cca_mh, stat = "counts")

pdf("./SynthpopChecks/ComparingDescStats_mothers_MH.pdf", height = 6, width = 10)
compare(data_mum_cca_mh_syn, data_mum_cca_mh, stat = "counts")
dev.off()


## Simple analysis of depression score as outcome and religious belief as exposure to check that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.real <- lm(dep_age6 ~ belief_age5, data = data_mum_cca_mh)
summary(model.real)

model.syn <- lm.synds(dep_age6 ~ belief_age5, data = data_mum_cca_mh_syn)
summary(model.syn)

# Correspondence not exact, but 95% CIs overlap and pattern of results is similar (and store as PDF)
compare(model.syn, data_mum_cca_mh)

pdf("./SynthpopChecks/ComparingUnadjustedModel_mothers_MH.pdf", height = 8, width = 12)
compare(model.syn, data_mum_cca_mh)
dev.off()


## Test this with a more complex model, with additional covariates
model.real2 <- lm(dep_age6 ~ belief_age5 + age + ethnicity + marital + edu + belief_preg + dep_preg, 
                  data = data_mum_cca_mh)
summary(model.real2)

model.syn2 <- lm.synds(dep_age6 ~ belief_age5 + age + ethnicity + marital + edu + belief_preg + dep_preg, 
                       data = data_mum_cca_mh_syn)
summary(model.syn2)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, data_mum_cca_mh)

pdf("./SynthpopChecks/ComparingAdjustedModel_mothers_MH.pdf", height = 8, width = 12)
compare(model.syn2, data_mum_cca_mh)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
data_mum_cca_mh_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(data_mum_cca_mh_syn$syn)), data_mum_cca_mh_syn$syn)
summary(data_mum_cca_mh_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
data_mum_cca_mh_syn_df <- data_mum_cca_mh_syn$syn
head(data_mum_cca_mh_syn_df)
glimpse(data_mum_cca_mh_syn_df)
summary(data_mum_cca_mh_syn_df)


## Add back in the binary RSBB exposures and MH outcomes

# Religious belief at age 5
data_mum_cca_mh_syn_df <- data_mum_cca_mh_syn_df %>%
  mutate(belief_age5_bin = recode(belief_age5, "Not sure" = "No")) %>%
  mutate(belief_age5_bin = factor(belief_age5_bin, levels = c("No", "Yes")))

table(data_mum_cca_mh_syn_df$belief_age5)
table(data_mum_cca_mh_syn_df$belief_age5_bin)

# Religious identity at age 5
data_mum_cca_mh_syn_df <- data_mum_cca_mh_syn_df %>%
  mutate(identity_age5_bin = recode(identity_age5, "Christian" = "Religious", "Other" = "Religious")) %>%
  mutate(identity_age5_bin = factor(identity_age5_bin, levels = c("None", "Religious")))

table(data_mum_cca_mh_syn_df$identity_age5)
table(data_mum_cca_mh_syn_df$identity_age5_bin)

# Religious attendance at age 5
data_mum_cca_mh_syn_df <- data_mum_cca_mh_syn_df %>%
  mutate(attend_age5_bin = recode(attend_age5, "MIN 1 a MTH" = "Regular", "MIN 1 a WK" = "Regular", 
                                  "MIN 1 a YR" = "Occasional/None", "Not at all" = "Occasional/None")) %>%
  mutate(attend_age5_bin = factor(attend_age5_bin, levels = c("Occasional/None", "Regular")))

table(data_mum_cca_mh_syn_df$attend_age5)
table(data_mum_cca_mh_syn_df$attend_age5_bin)

# Probable depression diagnosis at age 6
data_mum_cca_mh_syn_df <- data_mum_cca_mh_syn_df %>%
  mutate(dep_age6_bin = ifelse(dep_age6 >= 13, "Dep", "Not dep")) %>%
  mutate(dep_age6_bin = factor(dep_age6_bin, levels = c("Not dep", "Dep")))

table(data_mum_cca_mh_syn_df$dep_age6)
table(data_mum_cca_mh_syn_df$dep_age6_bin)

# Probable anxiety diagnosis at age 6
data_mum_cca_mh_syn_df <- data_mum_cca_mh_syn_df %>%
  mutate(anx_age6_bin = ifelse(anx_age6 >= 9, "Anx", "Not anx")) %>%
  mutate(anx_age6_bin = factor(anx_age6_bin, levels = c("Not anx", "Anx")))

table(data_mum_cca_mh_syn_df$anx_age6)
table(data_mum_cca_mh_syn_df$anx_age6_bin)


## Final check of the data
head(data_mum_cca_mh_syn_df)
glimpse(data_mum_cca_mh_syn_df)
summary(data_mum_cca_mh_syn_df)


### Store the synthetic dataset for others to use
save(data_mum_cca_mh_syn_df, 
     file = "./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_mum_mhOutcome_B4226.RData")
write_csv(data_mum_cca_mh_syn_df, 
          file = "./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_mum_mhOutcome_B4226.csv")



##################################################
#### Next, for partners's data with RSBB as exposures and mental health as outcomes

## Read in the data
load("data_partner_processed_B4226.RData")

# Make a complete-case dataset with no missing values for the analyses with mental health at age 6 as the outcome - Also drop the binary RSBB and MH variables, as will re-create these post-synthesis - And move exposures and outcomes to end of dataset so are synthesised last (which may help preserve relations between variables)
data_partner_cca_mh <- data_partner %>%
  select(c(belief_preg:attend_age5_bin, dep_preg, anx_preg, dep_age6:anx_age6_bin, age:paternalMH)) %>%
  select(-c(belief_age5_bin, identity_age5_bin, attend_age5_bin, dep_age6_bin, anx_age6_bin)) %>%
  relocate(age:paternalMH, belief_preg:attend_preg, dep_preg, anx_preg, belief_age5:attend_age5, dep_age6, anx_age6) %>%
  filter(complete.cases(.))

head(data_partner_cca_mh)

# Get information about variables in the dataset
codebook.syn(data_partner_cca_mh)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
data_partner_cca_mh_syn <- syn(data_partner_cca_mh, seed = 61308)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, are no unique replicates
replicated.uniques(data_partner_cca_mh_syn, data_partner_cca_mh)
data_partner_cca_mh_syn <- sdc(data_partner_cca_mh_syn, data_partner_cca_mh, rm.replicated.uniques = TRUE)

## Take a few unique true observations, and make sure not fully-replicated in synthetic dataset (based on the 'replicated.uniques' command from the 'synthpop' package)

# Make a dataset just of unique individuals using the observed data (as if two or more participants share exactly the same data, then it's impossible to link back to a unique individual)
sum(!(duplicated(data_partner_cca_mh) | duplicated(data_partner_cca_mh, fromLast = TRUE)))
dat_unique <- data_partner_cca_mh[!(duplicated(data_partner_cca_mh) | 
                                      duplicated(data_partner_cca_mh, fromLast = TRUE)), ]

# Make a dataset just of unique individuals from the synthetic dataset
sum(!(duplicated(data_partner_cca_mh_syn$syn) | duplicated(data_partner_cca_mh_syn$syn, fromLast = TRUE)))
syn_unique <- data_partner_cca_mh_syn$syn[!(duplicated(data_partner_cca_mh_syn$syn) | 
                                          duplicated(data_partner_cca_mh_syn$syn, fromLast = TRUE)), ]

# Select a random row from the observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 1), ])

# Combine observed row with the synthetic data, and see if any duplicates
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Repeat for a few more rows of observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 10), ])
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Explore this synthetic dataset
data_partner_cca_mh_syn
summary(data_partner_cca_mh_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is good). Save this as a PDF
compare(data_partner_cca_mh_syn, data_partner_cca_mh, stat = "counts")

pdf("./SynthpopChecks/ComparingDescStats_partners_MH.pdf", height = 6, width = 10)
compare(data_partner_cca_mh_syn, data_partner_cca_mh, stat = "counts")
dev.off()


## Simple analysis of depression score as outcome and religious belief as exposure to check that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.real <- lm(dep_age6 ~ belief_age5, data = data_partner_cca_mh)
summary(model.real)

model.syn <- lm.synds(dep_age6 ~ belief_age5, data = data_partner_cca_mh_syn)
summary(model.syn)

# Correspondence not exact, but 95% CIs overlap and pattern of results is similar (and store as PDF)
compare(model.syn, data_partner_cca_mh)

pdf("./SynthpopChecks/ComparingUnadjustedModel_partners_MH.pdf", height = 8, width = 12)
compare(model.syn, data_partner_cca_mh)
dev.off()


## Test this with a more complex model, with additional covariates
model.real2 <- lm(dep_age6 ~ belief_age5 + age + ethnicity + marital + edu + belief_preg + dep_preg, 
                  data = data_partner_cca_mh)
summary(model.real2)

model.syn2 <- lm.synds(dep_age6 ~ belief_age5 + age + ethnicity + marital + edu + belief_preg + dep_preg, 
                       data = data_partner_cca_mh_syn)
summary(model.syn2)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, data_partner_cca_mh)

pdf("./SynthpopChecks/ComparingAdjustedModel_partners_MH.pdf", height = 8, width = 12)
compare(model.syn2, data_partner_cca_mh)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
data_partner_cca_mh_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(data_partner_cca_mh_syn$syn)),
                                 data_partner_cca_mh_syn$syn)
summary(data_partner_cca_mh_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
data_partner_cca_mh_syn_df <- data_partner_cca_mh_syn$syn
head(data_partner_cca_mh_syn_df)
glimpse(data_partner_cca_mh_syn_df)
summary(data_partner_cca_mh_syn_df)


## Add back in the binary RSBB exposures and MH outcomes

# Religious belief at age 5
data_partner_cca_mh_syn_df <- data_partner_cca_mh_syn_df %>%
  mutate(belief_age5_bin = recode(belief_age5, "Not sure" = "No")) %>%
  mutate(belief_age5_bin = factor(belief_age5_bin, levels = c("No", "Yes")))

table(data_partner_cca_mh_syn_df$belief_age5)
table(data_partner_cca_mh_syn_df$belief_age5_bin)

# Religious identity at age 5
data_partner_cca_mh_syn_df <- data_partner_cca_mh_syn_df %>%
  mutate(identity_age5_bin = recode(identity_age5, "Christian" = "Religious", "Other" = "Religious")) %>%
  mutate(identity_age5_bin = factor(identity_age5_bin, levels = c("None", "Religious")))

table(data_partner_cca_mh_syn_df$identity_age5)
table(data_partner_cca_mh_syn_df$identity_age5_bin)

# Religious attendance at age 5
data_partner_cca_mh_syn_df <- data_partner_cca_mh_syn_df %>%
  mutate(attend_age5_bin = recode(attend_age5, "MIN 1 a MTH" = "Regular", "MIN 1 a WK" = "Regular", 
                                  "MIN 1 a YR" = "Occasional/None", "Not at all" = "Occasional/None")) %>%
  mutate(attend_age5_bin = factor(attend_age5_bin, levels = c("Occasional/None", "Regular")))

table(data_partner_cca_mh_syn_df$attend_age5)
table(data_partner_cca_mh_syn_df$attend_age5_bin)

# Probable depression diagnosis at age 6
data_partner_cca_mh_syn_df <- data_partner_cca_mh_syn_df %>%
  mutate(dep_age6_bin = ifelse(dep_age6 >= 10, "Dep", "Not dep")) %>%
  mutate(dep_age6_bin = factor(dep_age6_bin, levels = c("Not dep", "Dep")))

table(data_partner_cca_mh_syn_df$dep_age6)
table(data_partner_cca_mh_syn_df$dep_age6_bin)

# Probable anxiety diagnosis at age 6
data_partner_cca_mh_syn_df <- data_partner_cca_mh_syn_df %>%
  mutate(anx_age6_bin = ifelse(anx_age6 >= 6, "Anx", "Not anx")) %>%
  mutate(anx_age6_bin = factor(anx_age6_bin, levels = c("Not anx", "Anx")))

table(data_partner_cca_mh_syn_df$anx_age6)
table(data_partner_cca_mh_syn_df$anx_age6_bin)


## Final check of the data
head(data_partner_cca_mh_syn_df)
glimpse(data_partner_cca_mh_syn_df)
summary(data_partner_cca_mh_syn_df)


### Store the synthetic dataset for others to use
save(data_partner_cca_mh_syn_df, 
     file = "./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_partner_mhOutcome_B4226.RData")
write_csv(data_partner_cca_mh_syn_df, 
          file = "./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_partner_mhOutcome_B4226.csv")



###################################
#### Now for mother's data with mental health as exposures and RSBB as outcomes

## Read in the data
load("data_mum_processed_B4226.RData")

# Make a complete-case dataset with no missing values for the analyses with RSBB at age 5 as the outcome - Also drop the binary RSBB and MH variables, as will re-create these post-synthesis - And move exposures and outcomes to end of dataset so are synthesised last (which may help preserve relations between variables)
data_mum_cca_rsbb <- data_mum %>%
  select(c(belief_preg:attend_age5_bin, dep_preg:anx_age2_bin, age:paternalMH)) %>%
  select(-c(belief_age5_bin, identity_age5_bin, attend_age5_bin, dep_age2_bin, anx_age2_bin)) %>%
  relocate(age:paternalMH, belief_preg:attend_preg, dep_preg, anx_preg, dep_age2, anx_age2, belief_age5:attend_age5) %>%
  filter(complete.cases(.))

head(data_mum_cca_rsbb)

# Get information about variables in the dataset
codebook.syn(data_mum_cca_rsbb)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
data_mum_cca_rsbb_syn <- syn(data_mum_cca_rsbb, seed = 249268)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, are no unique replicates
replicated.uniques(data_mum_cca_rsbb_syn, data_mum_cca_rsbb)
data_mum_cca_rsbb_syn <- sdc(data_mum_cca_rsbb_syn, data_mum_cca_rsbb, rm.replicated.uniques = TRUE)

## Take a few unique true observations, and make sure not fully-replicated in synthetic dataset (based on the 'replicated.uniques' command from the 'synthpop' package)

# Make a dataset just of unique individuals using the observed data (as if two or more participants share exactly the same data, then it's impossible to link back to a unique individual)
sum(!(duplicated(data_mum_cca_rsbb) | duplicated(data_mum_cca_rsbb, fromLast = TRUE)))
dat_unique <- data_mum_cca_rsbb[!(duplicated(data_mum_cca_rsbb) | 
                                      duplicated(data_mum_cca_rsbb, fromLast = TRUE)), ]

# Make a dataset just of unique individuals from the synthetic dataset
sum(!(duplicated(data_mum_cca_rsbb_syn$syn) | duplicated(data_mum_cca_rsbb_syn$syn, fromLast = TRUE)))
syn_unique <- data_mum_cca_rsbb_syn$syn[!(duplicated(data_mum_cca_rsbb_syn$syn) | 
                                              duplicated(data_mum_cca_rsbb_syn$syn, fromLast = TRUE)), ]

# Select a random row from the observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 1), ])

# Combine observed row with the synthetic data, and see if any duplicates
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Repeat for a few more rows of observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 10), ])
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Explore this synthetic dataset
data_mum_cca_rsbb_syn
summary(data_mum_cca_rsbb_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is good). Save this as a PDF
compare(data_mum_cca_rsbb_syn, data_mum_cca_rsbb, stat = "counts")

pdf("./SynthpopChecks/ComparingDescStats_mothers_RSBB.pdf", height = 6, width = 10)
compare(data_mum_cca_rsbb_syn, data_mum_cca_rsbb, stat = "counts")
dev.off()


## Simple analysis of religious belief as outcome and depression score as exposure to check that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.real <- multinom(belief_age5 ~ dep_age2, data = data_mum_cca_rsbb)
summary(model.real)

model.syn <- multinom.synds(belief_age5 ~ dep_age2, data = data_mum_cca_rsbb_syn)
summary(model.syn)

# Correspondence not exact, but 95% CIs overlap and pattern of results is similar (and store as PDF)
compare(model.syn, data_mum_cca_rsbb)

pdf("./SynthpopChecks/ComparingUnadjustedModel_mothers_RSBB.pdf", height = 8, width = 12)
compare(model.syn, data_mum_cca_rsbb)
dev.off()


## Test this with a more complex model, with additional covariates
model.real2 <- multinom(belief_age5 ~ dep_age2 + age + ethnicity + marital + edu + belief_preg + dep_preg, 
                  data = data_mum_cca_rsbb)
summary(model.real2)

model.syn2 <- multinom.synds(belief_age5 ~ dep_age2 + age + ethnicity + marital + edu + belief_preg + dep_preg, 
                       data = data_mum_cca_rsbb_syn)
summary(model.syn2)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, data_mum_cca_rsbb)

pdf("./SynthpopChecks/ComparingAdjustedModel_mothers_RSBB.pdf", height = 8, width = 12)
compare(model.syn2, data_mum_cca_rsbb)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
data_mum_cca_rsbb_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(data_mum_cca_rsbb_syn$syn)),
                                   data_mum_cca_rsbb_syn$syn)
summary(data_mum_cca_rsbb_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
data_mum_cca_rsbb_syn_df <- data_mum_cca_rsbb_syn$syn
head(data_mum_cca_rsbb_syn_df)
glimpse(data_mum_cca_rsbb_syn_df)
summary(data_mum_cca_rsbb_syn_df)


## Add back in the binary RSBB outcomes and MH exposures

# Religious belief at age 5
data_mum_cca_rsbb_syn_df <- data_mum_cca_rsbb_syn_df %>%
  mutate(belief_age5_bin = recode(belief_age5, "Not sure" = "No")) %>%
  mutate(belief_age5_bin = factor(belief_age5_bin, levels = c("No", "Yes")))

table(data_mum_cca_rsbb_syn_df$belief_age5)
table(data_mum_cca_rsbb_syn_df$belief_age5_bin)

# Religious identity at age 5
data_mum_cca_rsbb_syn_df <- data_mum_cca_rsbb_syn_df %>%
  mutate(identity_age5_bin = recode(identity_age5, "Christian" = "Religious", "Other" = "Religious")) %>%
  mutate(identity_age5_bin = factor(identity_age5_bin, levels = c("None", "Religious")))

table(data_mum_cca_rsbb_syn_df$identity_age5)
table(data_mum_cca_rsbb_syn_df$identity_age5_bin)

# Religious attendance at age 5
data_mum_cca_rsbb_syn_df <- data_mum_cca_rsbb_syn_df %>%
  mutate(attend_age5_bin = recode(attend_age5, "MIN 1 a MTH" = "Regular", "MIN 1 a WK" = "Regular", 
                                  "MIN 1 a YR" = "Occasional/None", "Not at all" = "Occasional/None")) %>%
  mutate(attend_age5_bin = factor(attend_age5_bin, levels = c("Occasional/None", "Regular")))

table(data_mum_cca_rsbb_syn_df$attend_age5)
table(data_mum_cca_rsbb_syn_df$attend_age5_bin)

# Probable depression diagnosis at age 2
data_mum_cca_rsbb_syn_df <- data_mum_cca_rsbb_syn_df %>%
  mutate(dep_age2_bin = ifelse(dep_age2 >= 13, "Dep", "Not dep")) %>%
  mutate(dep_age2_bin = factor(dep_age2_bin, levels = c("Not dep", "Dep")))

table(data_mum_cca_rsbb_syn_df$dep_age2)
table(data_mum_cca_rsbb_syn_df$dep_age2_bin)

# Probable anxiety diagnosis at age 2
data_mum_cca_rsbb_syn_df <- data_mum_cca_rsbb_syn_df %>%
  mutate(anx_age2_bin = ifelse(anx_age2 >= 9, "Anx", "Not anx")) %>%
  mutate(anx_age2_bin = factor(anx_age2_bin, levels = c("Not anx", "Anx")))

table(data_mum_cca_rsbb_syn_df$anx_age2)
table(data_mum_cca_rsbb_syn_df$anx_age2_bin)


## Final check of the data
head(data_mum_cca_rsbb_syn_df)
glimpse(data_mum_cca_rsbb_syn_df)
summary(data_mum_cca_rsbb_syn_df)


### Store the synthetic dataset for others to use
save(data_mum_cca_rsbb_syn_df, 
     file = "./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_mum_rsbbOutcome_B4226.RData")
write_csv(data_mum_cca_rsbb_syn_df, 
          file = "./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_mum_rsbbOutcome_B4226.csv")



##################################################
#### Next, for partner's data with mental health as exposures and RSBB as outcomes

## Read in the data
load("data_partner_processed_B4226.RData")

# Make a complete-case dataset with no missing values for the analyses with RSBB at age 6 as the outcome - Also drop the binary RSBB and MH variables, as will re-create these post-synthesis - And move exposures and outcomes to end of dataset so are synthesised last (which may help preserve relations between variables)
data_partner_cca_rsbb <- data_partner %>%
  select(c(belief_preg:attend_age5_bin, dep_preg:anx_age2_bin, age:paternalMH)) %>%
  select(-c(belief_age5_bin, identity_age5_bin, attend_age5_bin, dep_age2_bin, anx_age2_bin)) %>%
  relocate(age:paternalMH, belief_preg:attend_preg, dep_preg, anx_preg, dep_age2, anx_age2, belief_age5:attend_age5) %>%
  filter(complete.cases(.))

head(data_partner_cca_rsbb)

# Get information about variables in the dataset
codebook.syn(data_partner_cca_rsbb)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
data_partner_cca_rsbb_syn <- syn(data_partner_cca_rsbb, seed = 127862)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, are no unique replicates
replicated.uniques(data_partner_cca_rsbb_syn, data_partner_cca_rsbb)
data_partner_cca_rsbb_syn <- sdc(data_partner_cca_rsbb_syn, data_partner_cca_rsbb, rm.replicated.uniques = TRUE)

## Take a few unique true observations, and make sure not fully-replicated in synthetic dataset (based on the 'replicated.uniques' command from the 'synthpop' package)

# Make a dataset just of unique individuals using the observed data (as if two or more participants share exactly the same data, then it's impossible to link back to a unique individual)
sum(!(duplicated(data_partner_cca_rsbb) | duplicated(data_partner_cca_rsbb, fromLast = TRUE)))
dat_unique <- data_partner_cca_rsbb[!(duplicated(data_partner_cca_rsbb) | 
                                    duplicated(data_partner_cca_rsbb, fromLast = TRUE)), ]

# Make a dataset just of unique individuals from the synthetic dataset
sum(!(duplicated(data_partner_cca_rsbb_syn$syn) | duplicated(data_partner_cca_rsbb_syn$syn, fromLast = TRUE)))
syn_unique <- data_partner_cca_rsbb_syn$syn[!(duplicated(data_partner_cca_rsbb_syn$syn) | 
                                            duplicated(data_partner_cca_rsbb_syn$syn, fromLast = TRUE)), ]

# Select a random row from the observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 1), ])

# Combine observed row with the synthetic data, and see if any duplicates
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Repeat for a few more rows of observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 10), ])
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Explore this synthetic dataset
data_partner_cca_rsbb_syn
summary(data_partner_cca_rsbb_syn)

# Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is good). Save this as a PDF
compare(data_partner_cca_rsbb_syn, data_partner_cca_rsbb, stat = "counts")

pdf("./SynthpopChecks/ComparingDescStats_partners_RSBB.pdf", height = 6, width = 10)
compare(data_partner_cca_rsbb_syn, data_partner_cca_rsbb, stat = "counts")
dev.off()


## Simple analysis of depression score as outcome and religious belief as exposure to check that get similar results in both datasets (i.e., that the structures of the dataset are preserved)
model.real <- multinom(belief_age5 ~ dep_age2, data = data_partner_cca_rsbb)
summary(model.real)

model.syn <- multinom.synds(belief_age5 ~ dep_age2, data = data_partner_cca_rsbb_syn)
summary(model.syn)

# Correspondence not exact, but 95% CIs overlap and pattern of results is similar (and store as PDF)
compare(model.syn, data_partner_cca_rsbb)

pdf("./SynthpopChecks/ComparingUnadjustedModel_partners_RSBB.pdf", height = 8, width = 12)
compare(model.syn, data_partner_cca_rsbb)
dev.off()


## Test this with a more complex model, with additional covariates
model.real2 <- multinom(belief_age5 ~ dep_age2 + age + ethnicity + marital + edu + belief_preg + dep_preg, 
                  data = data_partner_cca_rsbb)
summary(model.real2)

model.syn2 <- multinom.synds(belief_age5 ~ dep_age2 + age + ethnicity + marital + edu + belief_preg + dep_preg, 
                       data = data_partner_cca_rsbb_syn)
summary(model.syn2)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, data_partner_cca_rsbb)

pdf("./SynthpopChecks/ComparingAdjustedModel_partners_RSBB.pdf", height = 8, width = 12)
compare(model.syn2, data_partner_cca_rsbb)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
data_partner_cca_rsbb_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(data_partner_cca_rsbb_syn$syn)),
                                       data_partner_cca_rsbb_syn$syn)
summary(data_partner_cca_rsbb_syn)

# Extract the synthetic dataset (rather than it being stored within a list)
data_partner_cca_rsbb_syn_df <- data_partner_cca_rsbb_syn$syn
head(data_partner_cca_rsbb_syn_df)
glimpse(data_partner_cca_rsbb_syn_df)
summary(data_partner_cca_rsbb_syn_df)


## Add back in the binary RSBB outcomes and MH exposures

# Religious belief at age 5
data_partner_cca_rsbb_syn_df <- data_partner_cca_rsbb_syn_df %>%
  mutate(belief_age5_bin = recode(belief_age5, "Not sure" = "No")) %>%
  mutate(belief_age5_bin = factor(belief_age5_bin, levels = c("No", "Yes")))

table(data_partner_cca_rsbb_syn_df$belief_age5)
table(data_partner_cca_rsbb_syn_df$belief_age5_bin)

# Religious identity at age 5
data_partner_cca_rsbb_syn_df <- data_partner_cca_rsbb_syn_df %>%
  mutate(identity_age5_bin = recode(identity_age5, "Christian" = "Religious", "Other" = "Religious")) %>%
  mutate(identity_age5_bin = factor(identity_age5_bin, levels = c("None", "Religious")))

table(data_partner_cca_rsbb_syn_df$identity_age5)
table(data_partner_cca_rsbb_syn_df$identity_age5_bin)

# Religious attendance at age 5
data_partner_cca_rsbb_syn_df <- data_partner_cca_rsbb_syn_df %>%
  mutate(attend_age5_bin = recode(attend_age5, "MIN 1 a MTH" = "Regular", "MIN 1 a WK" = "Regular", 
                                  "MIN 1 a YR" = "Occasional/None", "Not at all" = "Occasional/None")) %>%
  mutate(attend_age5_bin = factor(attend_age5_bin, levels = c("Occasional/None", "Regular")))

table(data_partner_cca_rsbb_syn_df$attend_age5)
table(data_partner_cca_rsbb_syn_df$attend_age5_bin)

# Probable depression diagnosis at age 2
data_partner_cca_rsbb_syn_df <- data_partner_cca_rsbb_syn_df %>%
  mutate(dep_age2_bin = ifelse(dep_age2 >= 10, "Dep", "Not dep")) %>%
  mutate(dep_age2_bin = factor(dep_age2_bin, levels = c("Not dep", "Dep")))

table(data_partner_cca_rsbb_syn_df$dep_age2)
table(data_partner_cca_rsbb_syn_df$dep_age2_bin)

# Probable anxiety diagnosis at age 2
data_partner_cca_rsbb_syn_df <- data_partner_cca_rsbb_syn_df %>%
  mutate(anx_age2_bin = ifelse(anx_age2 >= 6, "Anx", "Not anx")) %>%
  mutate(anx_age2_bin = factor(anx_age2_bin, levels = c("Not anx", "Anx")))

table(data_partner_cca_rsbb_syn_df$anx_age2)
table(data_partner_cca_rsbb_syn_df$anx_age2_bin)


## Final check of the data
head(data_partner_cca_rsbb_syn_df)
glimpse(data_partner_cca_rsbb_syn_df)
summary(data_partner_cca_rsbb_syn_df)


### Store the synthetic dataset for others to use
save(data_partner_cca_rsbb_syn_df, 
     file = "./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_partner_rsbbOutcome_B4226.RData")
write_csv(data_partner_cca_rsbb_syn_df, 
          file = "./AnalysisCode_RSBB_MH_B4226/SyntheticData/syntheticData_partner_rsbbOutcome_B4226.csv")


