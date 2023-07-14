### Script for paper 'Exploring bidirectional causality between religion and mental health: A longitudinal study using data from the parental generation of a UK birth cohort (ALSPAC)' (ALSPAC B-number B4226)
### Script 1a: Data preparation/cleaning
### Created 22/6/2023 by Dan Major-Smith
### R version 4.0.4

## The analysis plan for this paper has been pre-registered and is available on the OSF: https://osf.io/qtdze/


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4226 - RSBB and MH")

#install.packages("tidyverse")
library(tidyverse)



###########################################################################################
#### Read in the raw data, and start processing/cleaning the data

data_raw <- read_csv("B4226_RSBB_MH_data.csv")

## Quick check of data
head(data_raw)
glimpse(data_raw)

# Remove the 'problems' and 'spec' attributes, ot make output easier to read
attr(data_raw, "problems") <- NULL
attr(data_raw, "spec") <- NULL
str(data_raw)



#### Start by processing the mother's data
data_mum <- data_raw


### Removing some observations 

## Want to drop data from one mother if linked to two pregnancies (else data possibly repeated)
table(data_mum$mz005l, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz005l != "Yes, drop these mult mums")

## Drop data if mother withdrew consent for data to be used
table(data_mum$d810, useNA = "ifany")

data_mum <- data_mum %>%
  filter(d810 != ".a" | is.na(d810))

## Drop if pregnancy not result in a live birth
table(data_mum$mz012, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz012 == "All survived")

## Also drop data if not enrolled in ALSPAC during pregnancy
table(data_mum$mz028b, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz028b != "Not in core sample")

## And drop parents of trips/quads
table(data_mum$mz028b, useNA = "ifany")

data_mum <- data_mum %>%
  filter(mz028b != "Triplet / quadruplet")


### Keep just the variables of interest and re-order
data_mum <- data_mum %>%
  relocate(aln, d810, d813, d816, k6240, k6243, k6247, 
           b371, b372, b352a, b352b, g291, g292, g269, g270, l2010:l2019, l2000:l2007,
           mz028b, c800, a006, a525, b032, b650, b659, c645a, c755, jan1993ur01ind_M, jan1993imd2010q5_M, b594, a053, 
           c710, c711, c712, c713, c433, d842, b921, b040, dw042, b633, b720, d780, d800, d536a, d586a) %>%
  select(aln:d586a)

colnames(data_mum)



#### Now process the data and prep variables for analysis

### Start with religion data in pregnancy (religious belief, affiliation and attendance)

## Religious belief (Yes vs Not sure vs No)
table(data_mum$d810, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(d810 = na_if(d810, "-1")) %>%
  mutate(d810 = factor(d810, levels = c("No", "Not sure", "Yes"))) %>%
  rename(belief_preg = d810)

table(data_mum$belief_preg, useNA = "ifany")
round(prop.table(table(data_mum$belief_preg)) * 100, 1)
sum(table(data_mum$belief_preg))


## Religious affiliation/identity (Christian vs None vs Other)
table(data_mum$d813, useNA = "ifany")

# If missing, code as NA, group religions together, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(d813 = na_if(d813, "-1")) %>%
  mutate(d813 = recode(d813, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                               "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(d813 = recode(d813, "C of E" = "Christian", "Christian SCI" = "Christian", 
                               "Jehovah Witness" = "Christian", "Mormon" = "Christian", "Other Christian" = "Christian",
                               "Roman Catholic" = "Christian")) %>%
  mutate(d813 = factor(d813, levels = c("None", "Christian", "Other"))) %>%
  rename(identity_preg = d813)

table(data_mum$identity_preg, useNA = "ifany")
round(prop.table(table(data_mum$identity_preg)) * 100, 1)
sum(table(data_mum$identity_preg))


## Religious attendance (Min once a week vs Min once a month vs Min once a year vs Not at all)
table(data_mum$d816, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(d816 = na_if(d816, "-1")) %>%
  mutate(d816 = factor(d816, levels = c("Not at all",  "MIN 1 a YR", "MIN 1 a MTH", "MIN 1 a WK"))) %>%
  rename(attend_preg = d816)

table(data_mum$attend_preg, useNA = "ifany")
round(prop.table(table(data_mum$attend_preg)) * 100, 1)
sum(table(data_mum$attend_preg))



### Next to religion data 5 years after delivery

## Religious belief (Yes vs Not sure vs No)
table(data_mum$k6240, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels - Also make a binary variable
data_mum <- data_mum %>%
  mutate(k6240 = na_if(k6240, "No response")) %>%
  mutate(k6240 = na_if(k6240, "Not completed")) %>%
  mutate(k6240 = recode(k6240, "No, not at all" = "No")) %>%
  mutate(belief_age5_bin = recode(k6240, "Not sure" = "No")) %>%
  mutate(k6240 = factor(k6240, levels = c("No", "Not sure", "Yes"))) %>%
  mutate(belief_age5_bin = factor(belief_age5_bin, levels = c("No", "Yes"))) %>%
  rename(belief_age5 = k6240)

table(data_mum$belief_age5, useNA = "ifany")
round(prop.table(table(data_mum$belief_age5)) * 100, 1)
sum(table(data_mum$belief_age5))

table(data_mum$belief_age5_bin, useNA = "ifany")
round(prop.table(table(data_mum$belief_age5_bin)) * 100, 1)
sum(table(data_mum$belief_age5_bin))


## Religious affiliation/identity (Christian vs None vs Other)
table(data_mum$k6243, useNA = "ifany")

# If missing, code as NA, group religions together, then convert to factor and order levels - Also make a binary variable
data_mum <- data_mum %>%
  mutate(k6243 = na_if(k6243, "No response")) %>%
  mutate(k6243 = na_if(k6243, "Not completed")) %>%
  mutate(k6243 = recode(k6243, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                                "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(k6243 = recode(k6243, "Church of England" = "Christian", "Christian Science" = "Christian", 
                                "Jehovahs Witness" = "Christian", "Mormon" = "Christian", 
                                "Other Christian" = "Christian", "Roman Catholic" = "Christian")) %>%
  mutate(identity_age5_bin = recode(k6243, "Christian" = "Religious", "Other" = "Religious")) %>%
  mutate(k6243 = factor(k6243, levels = c("None", "Christian", "Other"))) %>%
  mutate(identity_age5_bin = factor(identity_age5_bin, levels = c("None", "Religious"))) %>%
  rename(identity_age5 = k6243)

table(data_mum$identity_age5, useNA = "ifany")
round(prop.table(table(data_mum$identity_age5)) * 100, 1)
sum(table(data_mum$identity_age5))

table(data_mum$identity_age5_bin, useNA = "ifany")
round(prop.table(table(data_mum$identity_age5_bin)) * 100, 1)
sum(table(data_mum$identity_age5_bin))


## Religious attendance (Min once a week vs Min once a month vs Min once a year vs Not at all)
table(data_mum$k6247, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_mum <- data_mum %>%
  mutate(k6247 = na_if(k6247, "No response")) %>%
  mutate(k6247 = na_if(k6247, "Not completed")) %>%
  mutate(k6247 = na_if(k6247, "Text response")) %>%
  mutate(k6247 = recode(k6247, "Occasional worship" = "Yes, least once a year")) %>%
  mutate(attend_age5_bin = recode(k6247, "Yes, least once a month" = "Regular", "Yes, least once a week" = "Regular", 
                            "Yes, least once a year" = "Occasional/None", "No, not at all" = "Occasional/None")) %>%
  mutate(k6247 = recode(k6247, "Yes, least once a week" = "MIN 1 a WK", "Yes, least once a month" = "MIN 1 a MTH", 
                        "Yes, least once a year" = "MIN 1 a YR", "No, not at all" = "Not at all")) %>%
  mutate(k6247 = factor(k6247, levels = c("Not at all",  "MIN 1 a YR", "MIN 1 a MTH", "MIN 1 a WK"))) %>%
  mutate(attend_age5_bin = factor(attend_age5_bin, levels = c("Occasional/None", "Regular"))) %>%
  rename(attend_age5 = k6247)

table(data_mum$attend_age5, useNA = "ifany")
round(prop.table(table(data_mum$attend_age5)) * 100, 1)
sum(table(data_mum$attend_age5))

table(data_mum$attend_age5_bin, useNA = "ifany")
round(prop.table(table(data_mum$attend_age5_bin)) * 100, 1)
sum(table(data_mum$attend_age5_bin))



### Now to the mental health data

## Edinburgh post-natal depression score (EPDS) in pregnancy - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_mum$b371, useNA = "ifany")
table(data_mum$b372, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b371 = na_if(b371, "Missing")) %>%
  mutate(b371 = na_if(b371, "YHL")) %>%
  mutate(b371 = recode(b371, "not depressed" = "0", "very depressed" = "30")) %>%
  mutate(b371 = as.numeric(b371)) %>%
  mutate(b371 = ifelse(b372 == "6" | b372 == "7" | b372 == "8" | b372 == "9", NA, b371)) %>%
  rename(dep_preg = b371) %>%
  select(-b372)

table(data_mum$dep_preg, useNA = "ifany")
summary(data_mum$dep_preg)
hist(data_mum$dep_preg, breaks=rep(min(data_mum$dep_preg, na.rm = TRUE):max(data_mum$dep_preg, na.rm = TRUE), 
                                   each = 2) + c(-0.4, 0.4), freq = TRUE)


## Crown-Crisp Experiential Index - Anxiety subscale score (CCEI-A) in pregnancy - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_mum$b352a, useNA = "ifany")
table(data_mum$b352b, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b352a = na_if(b352a, "Missing")) %>%
  mutate(b352a = na_if(b352a, "HaB short / YHL")) %>%
  mutate(b352a = recode(b352a, "not anxious" = "0", "very anxious" = "16")) %>%
  mutate(b352a = as.numeric(b352a)) %>%
  mutate(b352a = ifelse(b352b == "5" | b352b == "6" | b352b == "7", NA, b352a)) %>%
  rename(anx_preg = b352a) %>%
  select(-b352b)

table(data_mum$anx_preg, useNA = "ifany")
summary(data_mum$anx_preg)
hist(data_mum$anx_preg, breaks=rep(min(data_mum$anx_preg, na.rm = TRUE):max(data_mum$anx_preg, na.rm = TRUE), 
                                   each = 2) + c(-0.4, 0.4), freq = TRUE)


## EPDS two years post-delivery - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_mum$g291, useNA = "ifany")
table(data_mum$g292, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(g291 = na_if(g291, "-1")) %>%
  mutate(g291 = recode(g291, "not depressed" = "0", "very depressed" = "30")) %>%
  mutate(g291 = as.numeric(g291)) %>%
  mutate(g291 = ifelse(g292 == "6" | g292 == "7" | g292 == "8" | g292 == "9", NA, g291)) %>%
  rename(dep_age2 = g291) %>%
  select(-g292)

table(data_mum$dep_age2, useNA = "ifany")
summary(data_mum$dep_age2)
hist(data_mum$dep_age2, breaks=rep(min(data_mum$dep_age2, na.rm = TRUE):max(data_mum$dep_age2, na.rm = TRUE), 
                                   each = 2) + c(-0.4, 0.4), freq = TRUE)

# Also create binary variable of EPDS is 13 or more
data_mum <- data_mum %>%
  mutate(dep_age2_bin = ifelse(is.na(dep_age2), NA,
                               ifelse(dep_age2 >= 13, "Dep", "Not dep"))) %>%
  mutate(dep_age2_bin = factor(dep_age2_bin, levels = c("Not dep", "Dep")))

table(data_mum$dep_age2_bin, useNA = "ifany")
round(prop.table(table(data_mum$dep_age2_bin)) * 100, 1)
sum(table(data_mum$dep_age2_bin))


## CCEI-A two years post-delivery - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_mum$g269, useNA = "ifany")
table(data_mum$g270, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(g269 = na_if(g269, "-1")) %>%
  mutate(g269 = recode(g269, "not anxious" = "0", "very anxious" = "16")) %>%
  mutate(g269 = as.numeric(g269)) %>%
  mutate(g269 = ifelse(g270 == "5" | g270 == "6" | g270 == "7", NA, g269)) %>%
  rename(anx_age2 = g269) %>%
  select(-g270)

table(data_mum$anx_age2, useNA = "ifany")
summary(data_mum$anx_age2)
hist(data_mum$anx_age2, breaks=rep(min(data_mum$anx_age2, na.rm = TRUE):max(data_mum$anx_age2, na.rm = TRUE), 
                                   each = 2) + c(-0.4, 0.4), freq = TRUE)

# Also create binary variable of CCEI-A is 9 or more
data_mum <- data_mum %>%
  mutate(anx_age2_bin = ifelse(is.na(anx_age2), NA,
                               ifelse(anx_age2 >= 9, "Anx", "Not anx"))) %>%
  mutate(anx_age2_bin = factor(anx_age2_bin, levels = c("Not anx", "Anx")))

table(data_mum$anx_age2_bin, useNA = "ifany")
round(prop.table(table(data_mum$anx_age2_bin)) * 100, 1)
sum(table(data_mum$anx_age2_bin))



## EPDS six years post-delivery - Need to derive scores manually, as not yet created in ALSPAC data

# Q1 - Laugh and see funny side of things
table(data_mum$l2010, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2010 = na_if(l2010, "No response")) %>%
  mutate(l2010 = na_if(l2010, "Not completed")) %>%
  mutate(l2010 = recode(l2010, "As much as always could" = "0", "Not quite so much now" = "1",
                        "Definitely not so much now" = "2", "Not at all" = "3")) %>%
  mutate(l2010 = as.numeric(l2010))

table(data_mum$l2010, useNA = "ifany")

# Q2 - Looked forward to things
table(data_mum$l2011, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2011 = na_if(l2011, "No response")) %>%
  mutate(l2011 = na_if(l2011, "Not completed")) %>%
  mutate(l2011 = recode(l2011, "As much as ever did" = "0", "Rather < used to" = "1",
                        "Definitely < used to" = "2", "Hardly at all" = "3")) %>%
  mutate(l2011 = as.numeric(l2011))

table(data_mum$l2011, useNA = "ifany")

# Q3 - Blamed self when things went wrong
table(data_mum$l2012, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2012 = na_if(l2012, "No response")) %>%
  mutate(l2012 = na_if(l2012, "Not completed")) %>%
  mutate(l2012 = recode(l2012, "Never" = "0", "Not very often" = "1", 
                        "Yes, some of time" = "2", "Yes, most of time" = "3")) %>%
  mutate(l2012 = as.numeric(l2012))

table(data_mum$l2012, useNA = "ifany")

# Q4 - Worried/anxious for no reason
table(data_mum$l2013, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2013 = na_if(l2013, "No response")) %>%
  mutate(l2013 = na_if(l2013, "Not completed")) %>%
  mutate(l2013 = recode(l2013, "No, not at all" = "0", "Hardly ever" = "1",
                        "Yes, sometimes" = "2", "Yes, often" = "3")) %>%
  mutate(l2013 = as.numeric(l2013))

table(data_mum$l2013, useNA = "ifany")

# Q5 - Scared/panicky for no reason
table(data_mum$l2014, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2014 = na_if(l2014, "No response")) %>%
  mutate(l2014 = na_if(l2014, "Not completed")) %>%
  mutate(l2014 = recode(l2014, "No, not at all" = "0", "No, not much" = "1", 
                        "Yes, sometimes" = "2", "Yes, quite a lot" = "3")) %>%
  mutate(l2014 = as.numeric(l2014))

table(data_mum$l2014, useNA = "ifany")

# Q6 - Things getting on top of them
table(data_mum$l2015, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2015 = na_if(l2015, "No response")) %>%
  mutate(l2015 = na_if(l2015, "Not completed")) %>%
  mutate(l2015 = recode(l2015, "No, coping as well as ever" = "0", "No, most of time coped quite well" = "1", 
                        "Yes, sometimes not coping as well as usual" = "2", 
                        "Yes, most of time unable to cope" = "3")) %>%
  mutate(l2015 = as.numeric(l2015))

table(data_mum$l2015, useNA = "ifany")

# Q7 - So unhappy had trouble sleeping
table(data_mum$l2016, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2016 = na_if(l2016, "No response")) %>%
  mutate(l2016 = na_if(l2016, "Not completed")) %>%
  mutate(l2016 = recode(l2016, "No, not at all" = "0", "Not very often" = "1", 
                        "Yes, sometimes" = "2", "Yes, most of time" = "3")) %>%
  mutate(l2016 = as.numeric(l2016))

table(data_mum$l2016, useNA = "ifany")

# Q8 - Felt sad or miserable
table(data_mum$l2017, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2017 = na_if(l2017, "No response")) %>%
  mutate(l2017 = na_if(l2017, "Not completed")) %>%
  mutate(l2017 = recode(l2017, "No, not at all" = "0", "Not very often" = "1", 
                        "Yes, sometimes" = "2", "Yes, most of time" = "3")) %>%
  mutate(l2017 = as.numeric(l2017))

table(data_mum$l2017, useNA = "ifany")

# Q9 - So unhappy been crying
table(data_mum$l2018, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2018 = na_if(l2018, "No response")) %>%
  mutate(l2018 = na_if(l2018, "Not completed")) %>%
  mutate(l2018 = recode(l2018, "Never" = "0", "Only occasionally" = "1", 
                        "Yes, quite often" = "2", "Yes, most of time" = "3")) %>%
  mutate(l2018 = as.numeric(l2018))

table(data_mum$l2018, useNA = "ifany")

# Q10 - Thought of harming self
table(data_mum$l2019, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2019 = na_if(l2019, "No response")) %>%
  mutate(l2019 = na_if(l2019, "Not completed")) %>%
  mutate(l2019 = na_if(l2019, "Text response")) %>%
  mutate(l2019 = recode(l2019, "Never" = "0", "Hardly ever" = "1", 
                        "Sometimes" = "2", "Yes, quite often" = "3")) %>%
  mutate(l2019 = as.numeric(l2019))

table(data_mum$l2019, useNA = "ifany")


# Count number of missing values
data_mum <- data_mum %>%
  mutate(dep_age6_miss = rowSums(is.na(select(., c(l2010:l2019)))))

table(data_mum$dep_age6_miss)

# Next, get the modal value for each person - Have to make a function for this, which also excludes NAs
getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data_mum <- data_mum %>% 
  rowwise() %>%
  mutate(dep_age6_mode = getmode(c_across(l2010:l2019))) %>%
  ungroup()

table(data_mum$dep_age6_mode)

table(data_mum$dep_age6_miss, data_mum$dep_age6_mode, useNA = "ifany")

# If missing EPDS data, fill in with modal values
data_mum <- data_mum %>%
  mutate(l2010 = ifelse(is.na(l2010), dep_age6_mode, l2010)) %>%
  mutate(l2011 = ifelse(is.na(l2011), dep_age6_mode, l2011)) %>%
  mutate(l2012 = ifelse(is.na(l2012), dep_age6_mode, l2012)) %>%
  mutate(l2013 = ifelse(is.na(l2013), dep_age6_mode, l2013)) %>%
  mutate(l2014 = ifelse(is.na(l2014), dep_age6_mode, l2014)) %>%
  mutate(l2015 = ifelse(is.na(l2015), dep_age6_mode, l2015)) %>%
  mutate(l2016 = ifelse(is.na(l2016), dep_age6_mode, l2016)) %>%
  mutate(l2017 = ifelse(is.na(l2017), dep_age6_mode, l2017)) %>%
  mutate(l2018 = ifelse(is.na(l2018), dep_age6_mode, l2018)) %>%
  mutate(l2019 = ifelse(is.na(l2019), dep_age6_mode, l2019))

table(data_mum$l2010, useNA = "ifany")
table(data_mum$l2011, useNA = "ifany")
table(data_mum$l2012, useNA = "ifany")
table(data_mum$l2013, useNA = "ifany")
table(data_mum$l2014, useNA = "ifany")
table(data_mum$l2015, useNA = "ifany")
table(data_mum$l2016, useNA = "ifany")
table(data_mum$l2017, useNA = "ifany")
table(data_mum$l2018, useNA = "ifany")
table(data_mum$l2019, useNA = "ifany")


## Now generate a total EPDS score, and code as missing is more than half of values missing - Also create binary variable of EPDS is 13 or more
data_mum <- data_mum %>%
  mutate(dep_age6 = rowSums(select(., c(l2010:l2019)))) %>%
  mutate(dep_age6 = ifelse(dep_age6_miss > 5, NA, dep_age6)) %>%
  select(-c(l2010:l2019, dep_age6_miss, dep_age6_mode)) %>%
  mutate(dep_age6_bin = ifelse(is.na(dep_age6), NA,
                               ifelse(dep_age6 >= 13, "Dep", "Not dep"))) %>%
  mutate(dep_age6_bin = factor(dep_age6_bin, levels = c("Not dep", "Dep")))
  
table(data_mum$dep_age6, useNA = "ifany")
summary(data_mum$dep_age6)
hist(data_mum$dep_age6, breaks=rep(min(data_mum$dep_age6, na.rm = TRUE):max(data_mum$dep_age6, na.rm = TRUE), 
                                   each = 2) + c(-0.4, 0.4), freq = TRUE)

table(data_mum$dep_age6_bin, useNA = "ifany")
round(prop.table(table(data_mum$dep_age6_bin)) * 100, 1)
sum(table(data_mum$dep_age6_bin))



## CCEI-A six years post-delivery - Need to derive scores manually, as not yet created in ALSPAC data

# Q1 - Upset for no reason
table(data_mum$l2000, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2000 = na_if(l2000, "No response")) %>%
  mutate(l2000 = na_if(l2000, "Not completed")) %>%
  mutate(l2000 = recode(l2000, "Very often" = "1", "Often" = "2", "Not very often" = "3", "Never" = "4")) %>%
  mutate(l2000 = as.numeric(l2000))

table(data_mum$l2000, useNA = "ifany")

# Q2 - Felt might faint
table(data_mum$l2001, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2001 = na_if(l2001, "No response")) %>%
  mutate(l2001 = na_if(l2001, "Not completed")) %>%
  mutate(l2001 = na_if(l2001, "Text response")) %>%
  mutate(l2001 = recode(l2001, "Very often" = "1", "Often" = "2", "Not very often" = "3", "Never" = "4")) %>%
  mutate(l2001 = as.numeric(l2001))

table(data_mum$l2001, useNA = "ifany")

# Q3 - Feels uneasy and restless
table(data_mum$l2002, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2002 = na_if(l2002, "No response")) %>%
  mutate(l2002 = na_if(l2002, "Not completed")) %>%
  mutate(l2002 = recode(l2002, "Very often" = "1", "Often" = "2", "Not very often" = "3", "Never" = "4")) %>%
  mutate(l2002 = as.numeric(l2002))

table(data_mum$l2002, useNA = "ifany")

# Q4 - Feels panicky
table(data_mum$l2003, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2003 = na_if(l2003, "No response")) %>%
  mutate(l2003 = na_if(l2003, "Not completed")) %>%
  mutate(l2003 = recode(l2003, "Very often" = "1", "Often" = "2", "Not very often" = "3", "Never" = "4")) %>%
  mutate(l2003 = as.numeric(l2003))

table(data_mum$l2003, useNA = "ifany")

# Q5 - Worries a lot
table(data_mum$l2004, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2004 = na_if(l2004, "No response")) %>%
  mutate(l2004 = na_if(l2004, "Not completed")) %>%
  mutate(l2004 = recode(l2004, "Very often" = "1", "Often" = "2", "Not very often" = "3", "Never" = "4")) %>%
  mutate(l2004 = as.numeric(l2004))

table(data_mum$l2004, useNA = "ifany")

# Q6 - Strung-up inside
table(data_mum$l2005, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2005 = na_if(l2005, "No response")) %>%
  mutate(l2005 = na_if(l2005, "Not completed")) %>%
  mutate(l2005 = recode(l2005, "Very often" = "1", "Often" = "2", "Not very often" = "3", "Never" = "4")) %>%
  mutate(l2005 = as.numeric(l2005))

table(data_mum$l2005, useNA = "ifany")

# Q7 - Feel going to pieces
table(data_mum$l2006, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2006 = na_if(l2006, "No response")) %>%
  mutate(l2006 = na_if(l2006, "Not completed")) %>%
  mutate(l2006 = recode(l2006, "Very often" = "1", "Often" = "2", "Not very often" = "3", "Never" = "4")) %>%
  mutate(l2006 = as.numeric(l2006))

table(data_mum$l2006, useNA = "ifany")

# Q8 - Upsetting dreams
table(data_mum$l2007, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(l2007 = na_if(l2007, "No response")) %>%
  mutate(l2007 = na_if(l2007, "Not completed")) %>%
  mutate(l2007 = recode(l2007, "Very often" = "1", "Often" = "2", "Not very often" = "3", "Never" = "4")) %>%
  mutate(l2007 = as.numeric(l2007))

table(data_mum$l2007, useNA = "ifany")


# Count number of missing values
data_mum <- data_mum %>%
  mutate(anx_age6_miss = rowSums(is.na(select(., c(l2000:l2007)))))

table(data_mum$anx_age6_miss)

# Next, get the modal value for each person
data_mum <- data_mum %>% 
  rowwise() %>%
  mutate(anx_age6_mode = getmode(c_across(l2000:l2007))) %>%
  ungroup()

table(data_mum$anx_age6_mode)

table(data_mum$anx_age6_miss, data_mum$anx_age6_mode, useNA = "ifany")

# If missing CCEI-A data, fill in with modal values
data_mum <- data_mum %>%
  mutate(l2000 = ifelse(is.na(l2000), anx_age6_mode, l2000)) %>%
  mutate(l2001 = ifelse(is.na(l2001), anx_age6_mode, l2001)) %>%
  mutate(l2002 = ifelse(is.na(l2002), anx_age6_mode, l2002)) %>%
  mutate(l2003 = ifelse(is.na(l2003), anx_age6_mode, l2003)) %>%
  mutate(l2004 = ifelse(is.na(l2004), anx_age6_mode, l2004)) %>%
  mutate(l2005 = ifelse(is.na(l2005), anx_age6_mode, l2005)) %>%
  mutate(l2006 = ifelse(is.na(l2006), anx_age6_mode, l2006)) %>%
  mutate(l2007 = ifelse(is.na(l2007), anx_age6_mode, l2007))

table(data_mum$l2000, useNA = "ifany")
table(data_mum$l2001, useNA = "ifany")
table(data_mum$l2002, useNA = "ifany")
table(data_mum$l2003, useNA = "ifany")
table(data_mum$l2004, useNA = "ifany")
table(data_mum$l2005, useNA = "ifany")
table(data_mum$l2006, useNA = "ifany")
table(data_mum$l2007, useNA = "ifany")


# Need to recode some of the variables first
data_mum <- data_mum %>%
  mutate(l2000 = ifelse(is.na(l2000), NA,
                        ifelse(l2000 == 1 | l2000 == 2, 2, 0))) %>%
  mutate(l2001 = ifelse(is.na(l2001), NA, 
                        ifelse(l2001 == 1 | l2001 == 2, 2, 
                               ifelse(l2001 == 3, 1, 0)))) %>%
  mutate(l2002 = ifelse(is.na(l2002), NA,
                        ifelse(l2002 == 1 | l2002 == 2, 2, 
                               ifelse(l2002 == 3, 1, 0)))) %>%
  mutate(l2003 = ifelse(is.na(l2003), NA,
                        ifelse(l2003 == 4, 0, 2))) %>%
  mutate(l2004 = ifelse(is.na(l2004), NA,
                        ifelse(l2004 == 1 | l2004 == 2, 2, 
                               ifelse(l2004 == 3, 1, 0)))) %>%
  mutate(l2005 = ifelse(is.na(l2005), NA,
                        ifelse(l2005 == 1 | l2005 == 2, 2, 0))) %>%
  mutate(l2006 = ifelse(is.na(l2006), NA,
                        ifelse(l2006 == 1 | l2006 == 2, 2, 0))) %>%
  mutate(l2007 = ifelse(is.na(l2007), NA,
                        ifelse(l2007 == 1 | l2007 == 2, 2, 
                               ifelse(l2007 == 3, 1, 0))))

table(data_mum$l2000, useNA = "ifany")
table(data_mum$l2001, useNA = "ifany")
table(data_mum$l2002, useNA = "ifany")
table(data_mum$l2003, useNA = "ifany")
table(data_mum$l2004, useNA = "ifany")
table(data_mum$l2005, useNA = "ifany")
table(data_mum$l2006, useNA = "ifany")
table(data_mum$l2007, useNA = "ifany")


## Now generate a total CCEI-A score, and code as missing is more than half of values missing - Also create binary variable if CCEI-A is 9 or more
data_mum <- data_mum %>%
  mutate(anx_age6 = rowSums(select(., c(l2000:l2007)))) %>%
  mutate(anx_age6 = ifelse(anx_age6_miss > 4, NA, anx_age6)) %>%
  select(-c(l2000:l2007, anx_age6_miss, anx_age6_mode)) %>%
  mutate(anx_age6_bin = ifelse(is.na(anx_age6), NA,
                               ifelse(anx_age6 >= 9, "Anx", "Not anx"))) %>%
  mutate(anx_age6_bin = factor(anx_age6_bin, levels = c("Not anx", "Anx")))

table(data_mum$anx_age6, useNA = "ifany")
summary(data_mum$anx_age6)
hist(data_mum$anx_age6, breaks=rep(min(data_mum$anx_age6, na.rm = TRUE):max(data_mum$anx_age6, na.rm = TRUE), 
                                   each = 2) + c(-0.4, 0.4), freq = TRUE)

table(data_mum$anx_age6_bin, useNA = "ifany")
round(prop.table(table(data_mum$anx_age6_bin)) * 100, 1)
sum(table(data_mum$anx_age6_bin))



### Now tidy the confounder variables

## Age (at birth; years)
table(data_mum$mz028b, useNA = "ifany")

# If missing, code as NA, then convert to numeric
data_mum <- data_mum %>%
  mutate(mz028b = recode(mz028b, "< 16" = "15", ">43" = "44")) %>%
  mutate(mz028b = as.numeric(mz028b)) %>%
  rename(age = mz028b)

table(data_mum$age, useNA = "ifany")
summary(data_mum$age)


# Ethnicity (White vs other than White)
table(data_mum$c800, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(c800 = na_if(c800, "Missing")) %>%
  mutate(c800 = recode(c800, "Bangladeshi" = "Other than White", "Black African" = "Other than White", 
                       "Black Caribbean" = "Other than White", "Chinese" = "Other than White", 
                       "Indian" = "Other than White", "Other" = "Other than White", 
                       "Other black" = "Other than White", "Pakistani" = "Other than White")) %>%
  mutate(c800 = factor(c800, levels = c("White", "Other than White"))) %>%
  rename(ethnicity = c800)

table(data_mum$ethnicity, useNA = "ifany")


# Marital status (Married vs Never married vs widowed/divorced/separated)
table(data_mum$a525, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(a525 = na_if(a525, "Missing")) %>%
  mutate(a525 = recode(a525, "1st marriage" = "Married", "Divorced" = "Sep/Div/Widow", "Marriage 2 or 3" = "Married",
                       "Separated" = "Sep/Div/Widow", "Widowed" = "Sep/Div/Widow")) %>%
  mutate(a525 = factor(a525, levels = c("Married", "Never married", "Sep/Div/Widow"))) %>%
  rename(marital = a525)

table(data_mum$marital, useNA = "ifany")


# Maternal parity (0 vs 1 vs 2 or more)
table(data_mum$b032, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b032 = na_if(b032, "Missing")) %>%
  mutate(b032 = na_if(b032, "HaB short")) %>%
  mutate(b032 = na_if(b032, "Inconsistent data")) %>%
  mutate(b032 = as.numeric(b032)) %>%
  mutate(b032 = ifelse(b032 > 2 & !is.na(b032), 2, b032)) %>%
  mutate(b032 = as.factor(b032)) %>%
  mutate(b032 = recode(b032, "0" = "0", "1" = "1", "2" = "2 or more")) %>%
  mutate(b032 = factor(b032, levels = c("0", "1", "2 or more"))) %>%
  rename(parity = b032)

table(data_mum$parity, useNA = "ifany")


# Urban/rural location (Urban vs Rural)
table(data_mum$jan1993ur01ind_M, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(jan1993ur01ind_M = na_if(jan1993ur01ind_M, "Missing")) %>%
  mutate(jan1993ur01ind_M = recode(jan1993ur01ind_M, "Hamlet and Isolated Dwelling" = "Rural", 
                                   "Town and Fringe" = "Rural", "Village" = "Rural", 
                                   "Urban (pop. >= 10k)" = "Urban")) %>%
  mutate(jan1993ur01ind_M = factor(jan1993ur01ind_M, levels = c("Urban", "Rural"))) %>%
  rename(rural = jan1993ur01ind_M)

table(data_mum$rural, useNA = "ifany")


# Highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(data_mum$c645a, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(c645a = na_if(c645a, "Missing")) %>%
  mutate(c645a = recode(c645a, "CSE" = "CSE/None")) %>%
  mutate(c645a = factor(c645a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree"))) %>%
  rename(edu = c645a)

table(data_mum$edu, useNA = "ifany")


# Occupational social class (combine lowest social classes - Binary (low [III manual/IV/V] vs high [I/II/III non-manual]) 
table(data_mum$c755, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(c755 = na_if(c755, "Missing")) %>%
  mutate(c755 = na_if(c755, "Armed forces")) %>%
  mutate(c755 = recode(c755, "I" = "High", "II" = "High", "III (non-manual)" = "High", "III (manual)" = "Low", 
                       "IV" = "Low", "V" = "Low")) %>%
  mutate(c755 = factor(c755, levels = c("Low", "High"))) %>%
  rename(occSocClass = c755)

table(data_mum$occSocClass, useNA = "ifany")


# Area-level index of multiple deprivation (IMD; quintiles)
table(data_mum$jan1993imd2010q5_M, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(jan1993imd2010q5_M = na_if(jan1993imd2010q5_M, "Missing")) %>%
  mutate(jan1993imd2010q5_M = recode(jan1993imd2010q5_M, "Least deprived" = "Quin. 1/Least deprived", 
                                     "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4", 
                                     "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(jan1993imd2010q5_M = factor(jan1993imd2010q5_M, levels = c("Quin. 1/Least deprived", 
                                                                    "Quintile 2", "Quintile 3", 
                                                                    "Quintile 4", "Quin. 5/Most deprived"))) %>%
  rename(imd = jan1993imd2010q5_M)

table(data_mum$imd, useNA = "ifany")


# Recent financial difficulties (Code as binary yes vs no)
table(data_mum$b594, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b594 = na_if(b594, "Missing")) %>%
  mutate(b594 = na_if(b594, "HaB short / YHL")) %>%
  mutate(b594 = recode(b594, "affected a lot" = "Yes", "didnt happen" = "No", "fairly affected" = "Yes",
                       "mildly affected" = "Yes", "N effect at all" = "Yes")) %>%
  mutate(b594 = factor(b594, levels = c("No", "Yes"))) %>%
  rename(finDiffs = b594)

table(data_mum$finDiffs, useNA = "ifany")


# Home ownership status (owned/mortgaged vs rented vs Council/housing association vs other)
table(data_mum$a006, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(a006 = na_if(a006, "Missing")) %>%
  mutate(a006 = na_if(a006, "YE short")) %>%
  mutate(a006 = recode(a006, "Council rented" = "Council/HA", "HA rented" = "Council/HA",
                       "Mortgaged" = "Owned/Mortgaged", "Owned" = "Owned/Mortgaged",
                       "RENT PRIV FURN" = "Rented", "RENT PRIV UNFURN" = "Rented")) %>%
  mutate(a006 = factor(a006, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other"))) %>%
  rename(home = a006)

table(data_mum$home, useNA = "ifany")


# Household access to a car
table(data_mum$a053, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(a053 = na_if(a053, "Missing")) %>%
  mutate(a053 = na_if(a053, "YE short")) %>%
  mutate(a053 = factor(a053, levels = c("No", "Yes"))) %>%
  rename(carAccess = a053)

table(data_mum$carAccess, useNA = "ifany")


# Currently employed (code to binary variable, based on answers to 4 questions about different types of employment - Then drop this individual variables)
table(data_mum$c710, useNA = "ifany")
table(data_mum$c711, useNA = "ifany")
table(data_mum$c712, useNA = "ifany")
table(data_mum$c713, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(employed = ifelse(is.na(c710), NA,
                               ifelse(c710 == "Yes" | c711 == "Yes" | c712 == "Yes" | c713 == "Yes", "Yes", "No"))) %>%
  mutate(employed = factor(employed, levels = c("No", "Yes"))) %>%
  select(-c(c710:c713))

table(data_mum$employed, useNA = "ifany")


## Adverse childhood experiences (total number experienced)
table(data_mum$c433, useNA = "ifany")

# If missing, code as NA, then convert to numeric
data_mum <- data_mum %>%
  mutate(c433 = na_if(c433, "Missing")) %>%
  mutate(c433 = as.numeric(c433)) %>%
  rename(ACEs = c433)

table(data_mum$ACEs, useNA = "ifany")
summary(data_mum$ACEs)


# Locus of control (score from 0 to 12, with higher scores indicating greater external locus of control)
table(data_mum$d842, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(d842 = na_if(d842, "Missing")) %>%
  mutate(d842 = as.numeric(d842)) %>%
  rename(locus = d842)

table(data_mum$locus, useNA = "ifany")
summary(data_mum$locus)


# Interpersonal sensitivity personality scores (with higher scores indicating greater interpersonal sensitivity)
table(data_mum$b921, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b921 = na_if(b921, "Missing")) %>%
  mutate(b921 = as.numeric(b921)) %>%
  rename(IPSM = b921)

table(data_mum$IPSM, useNA = "ifany")
summary(data_mum$IPSM)


# Self-reported health status (recode to always well vs usually well vs often/sometimes/always unwell)
table(data_mum$b040, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b040 = na_if(b040, "Missing")) %>%
  mutate(b040 = na_if(b040, "YHL")) %>%
  mutate(b040 = recode(b040, "always unwell" = "often unwell", "SMTS unwell" = "often unwell")) %>%
  mutate(b040 = factor(b040, levels = c("always well", "usually well", "often unwell"))) %>%
  rename(health = b040)

table(data_mum$health, useNA = "ifany")


# Body mass index (BMI)
data_mum <- data_mum %>%
  mutate(dw042 = as.numeric(dw042)) %>%
  rename(BMI = dw042)

summary(data_mum$BMI)
hist(data_mum$BMI)


# Self-reported physical activity levels, relative to others (Much more active vs Somewhat more active vs About the same vs Somewhat less active Vs Much less active)
table(data_mum$b633, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b633 = na_if(b633, "Missing")) %>%
  mutate(b633 = na_if(b633, "YHL")) %>%
  mutate(b633 = factor(b633, levels = c("much less", "somewhat less", "about same", "somewhat more", "much more"))) %>%
  rename(activity = b633)

table(data_mum$activity, useNA = "ifany")


# Smoking status (Currently smokes vs Formerly smoked vs Never smoked) - b650 = ever smoked; b659 = Stopped smoking
table(data_mum$b650, useNA = "ifany")
table(data_mum$b659, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b650 = na_if(b650, "Missing")) %>%
  mutate(b659 = na_if(b659, "Missing")) %>%
  mutate(b659 = na_if(b659, "HaB short"))

# Quick consistency check - Some mis-matches/missing data, so will exclude in combined variable
table(data_mum$b650, data_mum$b659, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(smoking = ifelse(is.na(b650) | (is.na(b659) & b650 == "Y"), NA,
                          ifelse(b650 == "N" & is.na(b659), "Never",
                                 ifelse(b650 == "Y" & b659 == "Y", "Former", "Current")))) %>%
  mutate(smoking = factor(smoking, levels = c("Never", "Former", "Current"))) %>%
  select(-c(b650, b659))

table(data_mum$smoking, useNA = "ifany")


# Alcohol intake pre-pregnancy 
table(data_mum$b720, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(b720 = na_if(b720, "Missing")) %>%
  mutate(b720 = recode(b720, "never" = "Never", "<1 glass PWK" = "<1 p/wk", "1+ glasses PWK" = "1+ p/wk",
                       "1-2 glasses PDAY" = "1+ p/day", "3-9 glasses PDAY" = "3+ p/day",
                       "10+ glasses PDAY" = "3+ p/day")) %>%
  mutate(b720 = factor(b720, levels = c("Never", "<1 p/wk", "1+ p/wk", "1+ p/day", "3+ p/day"))) %>%
  rename(alcPrePreg = b720)

table(data_mum$alcPrePreg, useNA = "ifany")


# Social network score (with higher scores indicating a larger and more supportive network)
table(data_mum$d780, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(d780 = na_if(d780, "-1")) %>%
  mutate(d780 = as.numeric(d780)) %>%
  rename(socNetwork = d780)

table(data_mum$socNetwork, useNA = "ifany")
summary(data_mum$socNetwork)


# Social support score (with higher scores indicating greater social support)
table(data_mum$d800, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(d800 = na_if(d800, "Missing")) %>%
  mutate(d800 = as.numeric(d800)) %>%
  rename(socSupport = d800)

table(data_mum$socSupport, useNA = "ifany")
summary(data_mum$socSupport)


# Maternal history of depression/anxiety
table(data_mum$d536a, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(d536a = factor(d536a, levels = c("No", "Yes"))) %>%
  rename(maternalMH = d536a)

table(data_mum$maternalMH, useNA = "ifany")


# Paternal history of depression/anxiety
table(data_mum$d586a, useNA = "ifany")

data_mum <- data_mum %>%
  mutate(d586a = factor(d586a, levels = c("No", "Yes"))) %>%
  rename(paternalMH = d586a)

table(data_mum$paternalMH, useNA = "ifany")



## Re-order variables for the final dataset
data_mum <- data_mum %>%
  relocate(aln, belief_preg:attend_preg, belief_age5, belief_age5_bin, identity_age5, identity_age5_bin,
           attend_age5, attend_age5_bin, 
           dep_preg:dep_age2, dep_age2_bin, anx_age2, anx_age2_bin, dep_age6:anx_age6_bin, 
           age, ethnicity, marital, parity, rural, edu, occSocClass, imd, finDiffs, home, carAccess,
           employed, ACEs, locus, IPSM, health, BMI, activity, smoking, alcPrePreg, socNetwork:paternalMH)

colnames(data_mum)



### Save mother's data (in R and CSV formats; the R format will keep all the factor formatting, while the CSV file will lose this)

save(data_mum, file = "data_mum_processed_B4226.RData")
write_csv(data_mum, file = "data_mum_processed_B4226.csv")



############################################################################################################
#### And now on to processing the partner's data
data_partner <- data_raw


### Removing some observations 

## Want to drop data from one mother if linked to two pregnancies (else data possibly repeated)
table(data_partner$mz005l, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz005l != "Yes, drop these mult mums")

## Drop data if mother or partner withdrew consent for data to be used
table(data_partner$d810, useNA = "ifany")
table(data_partner$pb150, useNA = "ifany")

data_partner <- data_partner %>%
  filter(d810 != ".a" | is.na(d810)) %>%
  filter(pb150 != ".c" | is.na(pb150))

## Drop if pregnancy not result in a live birth
table(data_partner$mz012, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz012 == "All survived")

## Also drop data if not enrolled in ALSPAC during pregnancy
table(data_partner$mz028b, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz028b != "Not in core sample")

## And drop parents of trips/quads
table(data_partner$mz028b, useNA = "ifany")

data_partner <- data_partner %>%
  filter(mz028b != "Triplet / quadruplet")

## See if any additional repeated partners linked to two pregnancies (are no extra partners)
table(data_partner$pz_mult, useNA = "ifany")

## Also remove any partners who changed identity between the time-periods explored here (before the PJ questionnaire at age 6; n = 379)
table(data_partner$partner_changed_when, useNA = "ifany")

data_partner <- data_partner %>%
  filter((partner_changed_when != "PA" & partner_changed_when != "PB" & partner_changed_when != "PC" &
           partner_changed_when != "PD" & partner_changed_when != "PE" & partner_changed_when != "PF" &
           partner_changed_when != "PG") | is.na(partner_changed_when))

table(data_partner$partner_changed_when, useNA = "ifany")



### Keep just the variables of interest and re-order
data_partner <- data_partner %>%
  relocate(aln, pb150:pb155, ph6240:ph6247, 
           pb261, pb262, pb234, pb235, pe291, pe292, pe240, pe242, pe245, pe248, pe251, pe254, pe256, pe259,
           pe264, pe265, pj2104, pj2105, pj2102, pj2101,
           partner_age, pb440, pa065, b032, jan1993ur01ind_M, pb325a, pb_sc_p, jan1993imd2010q5_M, 
           pb184, a006, a053, pb380, pb381, pb382, pb383, pb482, pa782, pb551, a524, paw002, paw010, 
           pb012, pb071, pb074, pb099, pb120, pb140, pa536a, pa586a) %>%
  select(aln:pa586a)

colnames(data_partner)



#### Now process the data and prep variables for analysis

### Start with religion data in pregnancy (religious belief, affiliation and attendance)

## Religious belief (Yes vs Not sure vs No)
table(data_partner$pb150, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(pb150 = na_if(pb150, "-1")) %>%
  mutate(pb150 = recode(pb150, "N" = "No", "Y" = "Yes")) %>%
  mutate(pb150 = factor(pb150, levels = c("No", "Not sure", "Yes"))) %>%
  rename(belief_preg = pb150)

table(data_partner$belief_preg, useNA = "ifany")
round(prop.table(table(data_partner$belief_preg)) * 100, 1)
sum(table(data_partner$belief_preg))


## Religious affiliation/identity (Christian vs None vs Other)
table(data_partner$pb153, useNA = "ifany")

# If missing, code as NA, group religions together, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(pb153 = na_if(pb153, "-1")) %>%
  mutate(pb153 = recode(pb153, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", "Muslim" = "Other",
                       "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(pb153 = recode(pb153, "C of E" = "Christian", "Christian SCI" = "Christian", 
                        "Jehovah witness" = "Christian", "Mormon" = "Christian", 
                        "Other Christian" = "Christian", "Roman Catholic" = "Christian")) %>%
  mutate(pb153 = factor(pb153, levels = c("None", "Christian", "Other"))) %>%
  rename(identity_preg = pb153)

table(data_partner$identity_preg, useNA = "ifany")
round(prop.table(table(data_partner$identity_preg)) * 100, 1)
sum(table(data_partner$identity_preg))


## Religious attendance (Min once a week vs Min once a month vs Min once a year vs Not at all)
table(data_partner$pb155, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(pb155 = na_if(pb155, "-1")) %>%
  mutate(pb155 = factor(pb155, levels = c("Not at all", "MIN 1 PYR", "MIN 1 PMTH", "MIN 1 PWK"))) %>%
  rename(attend_preg = pb155)

table(data_partner$attend_preg, useNA = "ifany")
round(prop.table(table(data_partner$attend_preg)) * 100, 1)
sum(table(data_partner$attend_preg))



### Next to religion data 5 years after delivery

## Religious belief (Yes vs Not sure vs No)
table(data_partner$ph6240, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels - Also make a binary variable
data_partner <- data_partner %>%
  mutate(ph6240 = na_if(ph6240, "No response")) %>%
  mutate(ph6240 = na_if(ph6240, "Not completed")) %>%
  mutate(ph6240 = recode(ph6240, "No, not at all" = "No", "Am not sure" = "Not sure")) %>%
  mutate(belief_age5_bin = recode(ph6240, "Not sure" = "No")) %>%
  mutate(ph6240 = factor(ph6240, levels = c("No", "Not sure", "Yes"))) %>%
  mutate(belief_age5_bin = factor(belief_age5_bin, levels = c("No", "Yes"))) %>%
  rename(belief_age5 = ph6240)

table(data_partner$belief_age5, useNA = "ifany")
round(prop.table(table(data_partner$belief_age5)) * 100, 1)
sum(table(data_partner$belief_age5))

table(data_partner$belief_age5_bin, useNA = "ifany")
round(prop.table(table(data_partner$belief_age5_bin)) * 100, 1)
sum(table(data_partner$belief_age5_bin))


## Religious affiliation/identity (Christian vs None vs Other)
table(data_partner$ph6243, useNA = "ifany")

# If missing, code as NA, group religions together, then convert to factor and order levels - Also make a binary variable
data_partner <- data_partner %>%
  mutate(ph6243 = na_if(ph6243, "No response")) %>%
  mutate(ph6243 = na_if(ph6243, "Not completed")) %>%
  mutate(ph6243 = recode(ph6243, "Buddhist" = "Other", "Hindu" = "Other", "Jewish" = "Other", 
                        "Muslim" = "Other", "Rastafarian" = "Other", "Sikh" = "Other")) %>%
  mutate(ph6243 = recode(ph6243, "Church of England" = "Christian", "Christian Science" = "Christian",
                                 "Jehovah's Witness" = "Christian", "Mormon" = "Christian", 
                                 "Other Christian" = "Christian", "Roman Catholic" = "Christian")) %>%
  mutate(identity_age5_bin = recode(ph6243, "Christian" = "Religious", "Other" = "Religious")) %>%
  mutate(ph6243 = factor(ph6243, levels = c("None", "Christian", "Other"))) %>%
  mutate(identity_age5_bin = factor(identity_age5_bin, levels = c("None", "Religious"))) %>%
  rename(identity_age5 = ph6243)

table(data_partner$identity_age5, useNA = "ifany")
round(prop.table(table(data_partner$identity_age5)) * 100, 1)
sum(table(data_partner$identity_age5))

table(data_partner$identity_age5_bin, useNA = "ifany")
round(prop.table(table(data_partner$identity_age5_bin)) * 100, 1)
sum(table(data_partner$identity_age5_bin))


## Religious attendance (Min once a week vs Min once a month vs Min once a year vs Not at all)
table(data_partner$ph6247, useNA = "ifany")

# If missing, code as NA, then convert to factor and order levels
data_partner <- data_partner %>%
  mutate(ph6247 = na_if(ph6247, "No response")) %>%
  mutate(ph6247 = na_if(ph6247, "Not completed")) %>%
  mutate(ph6247 = recode(ph6247, "Occasional worship" = "Yes, least once a year")) %>%
  mutate(attend_age5_bin = recode(ph6247, "Yes, at least once a month" = "Regular", 
                                  "Yes at least once a week" = "Regular", 
                                  "Yes, at least once a year" = "Occasional/None", 
                                  "No, not at all" = "Occasional/None")) %>%
  mutate(ph6247 = recode(ph6247, "Yes at least once a week" = "MIN 1 a WK", 
                         "Yes, at least once a month" = "MIN 1 a MTH", 
                        "Yes, at least once a year" = "MIN 1 a YR", "No, not at all" = "Not at all")) %>%
  mutate(ph6247 = factor(ph6247, levels = c("Not at all", "MIN 1 a YR", "MIN 1 a MTH", "MIN 1 a WK"))) %>%
  mutate(attend_age5_bin = factor(attend_age5_bin, levels = c("Occasional/None", "Regular"))) %>%
  rename(attend_age5 = ph6247)

table(data_partner$attend_age5, useNA = "ifany")
round(prop.table(table(data_partner$attend_age5)) * 100, 1)
sum(table(data_partner$attend_age5))

table(data_partner$attend_age5_bin, useNA = "ifany")
round(prop.table(table(data_partner$attend_age5_bin)) * 100, 1)
sum(table(data_partner$attend_age5_bin))



### Now to the mental health data

## Edinburgh post-natal depression score (EPDS) in pregnancy - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_partner$pb261, useNA = "ifany")
table(data_partner$pb262, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb261 = na_if(pb261, -1)) %>%
  mutate(pb261 = ifelse(pb262 > 5, NA, pb261)) %>%
  rename(dep_preg = pb261) %>%
  select(-pb262)

table(data_partner$dep_preg, useNA = "ifany")
summary(data_partner$dep_preg)
hist(data_partner$dep_preg, 
     breaks=rep(min(data_partner$dep_preg, na.rm = TRUE):max(data_partner$dep_preg, na.rm = TRUE), 
                                   each = 2) + c(-0.4, 0.4), freq = TRUE)


## Crown-Crisp Experiential Index - Anxiety subscale score (CCEI-A) in pregnancy - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_partner$pb234, useNA = "ifany")
table(data_partner$pb235, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb234 = na_if(pb234, "-1")) %>%
  mutate(pb234 = recode(pb234, "Not anxious" = "0", "Very anxious" = "16")) %>%
  mutate(pb234 = as.numeric(pb234)) %>%
  mutate(pb234 = ifelse(pb235 > 4, NA, pb234)) %>%
  rename(anx_preg = pb234) %>%
  select(-pb235)

table(data_partner$anx_preg, useNA = "ifany")
summary(data_partner$anx_preg)
hist(data_partner$anx_preg, 
     breaks=rep(min(data_partner$anx_preg, na.rm = TRUE):max(data_partner$anx_preg, na.rm = TRUE), 
                                   each = 2) + c(-0.4, 0.4), freq = TRUE)


## EPDS two years post-delivery - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_partner$pe291, useNA = "ifany")
table(data_partner$pe292, useNA = "ifany")

# For some reason this variable has been coded the wrong way (i.e., 0 = not depressed), so will switch round

data_partner <- data_partner %>%
  mutate(pe291 = na_if(pe291, -1)) %>%
  mutate(pe291 = ifelse(is.na(pe291), NA, 30 - pe291)) %>%
  mutate(pe291 = ifelse(pe292 > 5, NA, pe291)) %>%
  rename(dep_age2 = pe291) %>%
  select(-pe292)

table(data_partner$dep_age2, useNA = "ifany")
summary(data_partner$dep_age2)
hist(data_partner$dep_age2, 
     breaks=rep(min(data_partner$dep_age2, na.rm = TRUE):max(data_partner$dep_age2, na.rm = TRUE), 
                                   each = 2) + c(-0.4, 0.4), freq = TRUE)

# Also create binary variable of EPDS is 10 or more (NOTE: The original pre-registered analysis plan said that a cut-off of 13 would be used, but this did not take into account gender differences, with lower EPDS scores associated with probable depression in men
data_partner <- data_partner %>%
  mutate(dep_age2_bin = ifelse(is.na(dep_age2), NA,
                               ifelse(dep_age2 >= 10, "Dep", "Not dep"))) %>%
  mutate(dep_age2_bin = factor(dep_age2_bin, levels = c("Not dep", "Dep")))

table(data_partner$dep_age2_bin, useNA = "ifany")
round(prop.table(table(data_partner$dep_age2_bin)) * 100, 1)
sum(table(data_partner$dep_age2_bin))


## CCEI-A two years post-delivery - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_partner$pe264, useNA = "ifany")
table(data_partner$pe265, useNA = "ifany")

# Something has gone wrong with the pro-rated coding here, and missing values haven't been pro-rated (so pe264 is the same as the complete-case variable pe263) - Will calculate CCEI-A manually.

# Q1 - Upset for no reason
table(data_partner$pe240, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pe240 = na_if(pe240, "-1")) %>%
  mutate(pe240 = recode(pe240, "Very Often" = "1", "Often" = "2", "Not Very Often" = "3", "Never" = "4")) %>%
  mutate(pe240 = as.numeric(pe240))

table(data_partner$pe240, useNA = "ifany")

# Q2 - Felt might faint
table(data_partner$pe242, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pe242 = na_if(pe242, "-1")) %>%
  mutate(pe242 = recode(pe242, "Very Often" = "1", "Often" = "2", "Not Very Often" = "3", "Never" = "4")) %>%
  mutate(pe242 = as.numeric(pe242))

table(data_partner$pe242, useNA = "ifany")

# Q3 - Feels uneasy and restless
table(data_partner$pe245, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pe245 = na_if(pe245, "-1")) %>%
  mutate(pe245 = recode(pe245, "Very Often" = "1", "Often" = "2", "Not Very Often" = "3", "Never" = "4")) %>%
  mutate(pe245 = as.numeric(pe245))

table(data_partner$pe245, useNA = "ifany")

# Q4 - Feels panicky
table(data_partner$pe248, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pe248 = na_if(pe248, "-1")) %>%
  mutate(pe248 = recode(pe248, "Very Often" = "1", "Often" = "2", "Not Very Often" = "3", "Never" = "4")) %>%
  mutate(pe248 = as.numeric(pe248))

table(data_partner$pe248, useNA = "ifany")

# Q5 - Worries a lot
table(data_partner$pe251, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pe251 = na_if(pe251, "-1")) %>%
  mutate(pe251 = recode(pe251, "Very Often" = "1", "Often" = "2", "Not Very Often" = "3", "Never" = "4")) %>%
  mutate(pe251 = as.numeric(pe251))

table(data_partner$pe251, useNA = "ifany")

# Q6 - Strung-up inside
table(data_partner$pe254, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pe254 = na_if(pe254, "-1")) %>%
  mutate(pe254 = recode(pe254, "Very Often" = "1", "Often" = "2", "Not Very Often" = "3", "Never" = "4")) %>%
  mutate(pe254 = as.numeric(pe254))

table(data_partner$pe254, useNA = "ifany")

# Q7 - Feel going to pieces
table(data_partner$pe256, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pe256 = na_if(pe256, "-1")) %>%
  mutate(pe256 = recode(pe256, "Very Often" = "1", "Often" = "2", "Not Very Often" = "3", "Never" = "4")) %>%
  mutate(pe256 = as.numeric(pe256))

table(data_partner$pe256, useNA = "ifany")

# Q8 - Upsetting dreams
table(data_partner$pe259, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pe259 = na_if(pe259, "-1")) %>%
  mutate(pe259 = recode(pe259, "Very Often" = "1", "Often" = "2", "Not Very Often" = "3", "Never" = "4")) %>%
  mutate(pe259 = as.numeric(pe259))

table(data_partner$pe259, useNA = "ifany")


# Count number of missing values
data_partner <- data_partner %>%
  mutate(anx_age2_miss = rowSums(is.na(select(., c(pe240:pe259)))))

table(data_partner$anx_age2_miss)

# Next, get the modal value for each person - Have to make a function for this, which also excludes NAs
getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data_partner <- data_partner %>% 
  rowwise() %>%
  mutate(anx_age2_mode = getmode(c_across(pe240:pe259))) %>%
  ungroup()

table(data_partner$anx_age2_mode)

table(data_partner$anx_age2_mode, data_partner$anx_age2_miss, useNA = "ifany")

# If missing CCEI-A data, fill in with modal values
data_partner <- data_partner %>%
  mutate(pe240 = ifelse(is.na(pe240), anx_age2_mode, pe240)) %>%
  mutate(pe242 = ifelse(is.na(pe242), anx_age2_mode, pe242)) %>%
  mutate(pe245 = ifelse(is.na(pe245), anx_age2_mode, pe245)) %>%
  mutate(pe248 = ifelse(is.na(pe248), anx_age2_mode, pe248)) %>%
  mutate(pe251 = ifelse(is.na(pe251), anx_age2_mode, pe251)) %>%
  mutate(pe254 = ifelse(is.na(pe254), anx_age2_mode, pe254)) %>%
  mutate(pe256 = ifelse(is.na(pe256), anx_age2_mode, pe256)) %>%
  mutate(pe259 = ifelse(is.na(pe259), anx_age2_mode, pe259))

table(data_partner$pe240, useNA = "ifany")
table(data_partner$pe242, useNA = "ifany")
table(data_partner$pe245, useNA = "ifany")
table(data_partner$pe248, useNA = "ifany")
table(data_partner$pe251, useNA = "ifany")
table(data_partner$pe254, useNA = "ifany")
table(data_partner$pe256, useNA = "ifany")
table(data_partner$pe259, useNA = "ifany")


# Need to recode some of the variables first
data_partner <- data_partner %>%
  mutate(pe240 = ifelse(is.na(pe240), NA,
                        ifelse(pe240 == 1 | pe240 == 2, 2, 0))) %>%
  mutate(pe242 = ifelse(is.na(pe242), NA, 
                        ifelse(pe242 == 1 | pe242 == 2, 2, 
                               ifelse(pe242 == 3, 1, 0)))) %>%
  mutate(pe245 = ifelse(is.na(pe245), NA,
                        ifelse(pe245 == 1 | pe245 == 2, 2, 
                               ifelse(pe245 == 3, 1, 0)))) %>%
  mutate(pe248 = ifelse(is.na(pe248), NA,
                        ifelse(pe248 == 4, 0, 2))) %>%
  mutate(pe251 = ifelse(is.na(pe251), NA,
                        ifelse(pe251 == 1 | pe251 == 2, 2, 
                               ifelse(pe251 == 3, 1, 0)))) %>%
  mutate(pe254 = ifelse(is.na(pe254), NA,
                        ifelse(pe254 == 1 | pe254 == 2, 2, 0))) %>%
  mutate(pe256 = ifelse(is.na(pe256), NA,
                        ifelse(pe256 == 1 | pe256 == 2, 2, 0))) %>%
  mutate(pe259 = ifelse(is.na(pe259), NA,
                        ifelse(pe259 == 1 | pe259 == 2, 2, 
                               ifelse(pe259 == 3, 1, 0))))

table(data_partner$pe240, useNA = "ifany")
table(data_partner$pe242, useNA = "ifany")
table(data_partner$pe245, useNA = "ifany")
table(data_partner$pe248, useNA = "ifany")
table(data_partner$pe251, useNA = "ifany")
table(data_partner$pe254, useNA = "ifany")
table(data_partner$pe256, useNA = "ifany")
table(data_partner$pe259, useNA = "ifany")


## Now generate a total CCEI-A score, and code as missing is more than half of values missing - Also create binary variable if CCEI-A is 6 or more (NOTE: The original pre-registered analysis plan said that a cut-off of 9 would be used, but this did not take into account gender differences, with CCEI-A scores among males (as with EPDS scores) lower among men. As with mother's CCEI-A cut-offs, will use 85th percentile as cut-off, meaning a value of 6 or higher here)
data_partner <- data_partner %>%
  mutate(anx_age2 = rowSums(select(., c(pe240:pe259)))) %>%
  mutate(anx_age2 = ifelse(anx_age2_miss > 4, NA, anx_age2)) %>%
  select(-c(pe240:pe259, pe264, pe265, anx_age2_miss, anx_age2_mode))

table(data_partner$anx_age2, useNA = "ifany")
summary(data_partner$anx_age2)
quantile(data_partner$anx_age2, 0.85, na.rm = TRUE)
hist(data_partner$anx_age2, 
     breaks=rep(min(data_partner$anx_age2, na.rm = TRUE):max(data_partner$anx_age2, na.rm = TRUE), 
                each = 2) + c(-0.4, 0.4), freq = TRUE)

data_partner <- data_partner %>%
  mutate(anx_age2_bin = ifelse(is.na(anx_age2), NA,
                               ifelse(anx_age2 >= 6, "Anx", "Not anx"))) %>%
  mutate(anx_age2_bin = factor(anx_age2_bin, levels = c("Not anx", "Anx")))

table(data_partner$anx_age2_bin, useNA = "ifany")
round(prop.table(table(data_partner$anx_age2_bin)) * 100, 1)
sum(table(data_partner$anx_age2_bin))



## EPDS six years post-delivery - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_partner$pj2105, useNA = "ifany")
table(data_partner$pj2104, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pj2105 = na_if(pj2105, "Missing")) %>%
  mutate(pj2105 = na_if(pj2105, "Not completed")) %>%
  mutate(pj2105 = as.numeric(pj2105)) %>%
  mutate(pj2105 = ifelse(pj2104 == "6" | pj2104 == "9", NA, pj2105)) %>%
  rename(dep_age6 = pj2105) %>%
  select(-pj2104)

table(data_partner$dep_age6, useNA = "ifany")
summary(data_partner$dep_age6)
hist(data_partner$dep_age6, 
     breaks=rep(min(data_partner$dep_age6, na.rm = TRUE):max(data_partner$dep_age6, na.rm = TRUE), 
                each = 2) + c(-0.4, 0.4), freq = TRUE)

# Also create binary variable of EPDS is 10 or more
data_partner <- data_partner %>%
  mutate(dep_age6_bin = ifelse(is.na(dep_age6), NA,
                               ifelse(dep_age6 >= 10, "Dep", "Not dep"))) %>%
  mutate(dep_age6_bin = factor(dep_age6_bin, levels = c("Not dep", "Dep")))

table(data_partner$dep_age6_bin, useNA = "ifany")
round(prop.table(table(data_partner$dep_age6_bin)) * 100, 1)
sum(table(data_partner$dep_age6_bin))


## CCEI-A six years post-delivery - Using the pro-rated score, with modal value used for missing values - Will drop if more than half of items missing, though. Also need to do some recoding to remove missing values and recode to numeric
table(data_partner$pj2102, useNA = "ifany")
table(data_partner$pj2101, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pj2102 = na_if(pj2102, "Missing")) %>%
  mutate(pj2102 = na_if(pj2102, "Not completed")) %>%
  mutate(pj2102 = as.numeric(pj2102)) %>%
  mutate(pj2102 = ifelse(pj2101 == "5", NA, pj2102)) %>%
  rename(anx_age6 = pj2102) %>%
  select(-pj2101)

table(data_partner$anx_age6, useNA = "ifany")
summary(data_partner$anx_age6)
hist(data_partner$anx_age6, 
     breaks=rep(min(data_partner$anx_age6, na.rm = TRUE):max(data_partner$anx_age6, na.rm = TRUE), 
                each = 2) + c(-0.4, 0.4), freq = TRUE)

# Also create binary variable of CCEI-A is 6 or more
data_partner <- data_partner %>%
  mutate(anx_age6_bin = ifelse(is.na(anx_age6), NA,
                               ifelse(anx_age6 >= 6, "Anx", "Not anx"))) %>%
  mutate(anx_age6_bin = factor(anx_age6_bin, levels = c("Not anx", "Anx")))

table(data_partner$anx_age6_bin, useNA = "ifany")
round(prop.table(table(data_partner$anx_age6_bin)) * 100, 1)
sum(table(data_partner$anx_age6_bin))



### Now tidy the confounder variables

## Age (at birth; years)
table(data_partner$partner_age, useNA = "ifany")

# If missing, code as NA, then convert to numeric - Also recode some high and low values which could be disclosive
data_partner <- data_partner %>%
  mutate(partner_age = na_if(partner_age, "Insufficient DOB information")) %>%
  mutate(partner_age = as.numeric(partner_age)) %>%
  mutate(partner_age = ifelse(is.na(partner_age), NA, 
                              ifelse(partner_age < 15, 15, 
                                     ifelse(partner_age > 60, 60, partner_age)))) %>%
  rename(age = partner_age)

table(data_partner$age, useNA = "ifany")
summary(data_partner$age)


# Ethnicity (White vs other than White)
table(data_partner$pb440, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb440 = na_if(pb440, "Missing")) %>%
  mutate(pb440 = recode(pb440, "Bangladeshi" = "Other than White", "black African" = "Other than White", 
                       "black Caribbean" = "Other than White", "Chinese" = "Other than White", 
                       "Indian" = "Other than White", "other ethnic" = "Other than White", 
                       "black other" = "Other than White", "Pakistani" = "Other than White",
                       "white" = "White")) %>%
  mutate(pb440 = factor(pb440, levels = c("White", "Other than White"))) %>%
  rename(ethnicity = pb440)

table(data_partner$ethnicity, useNA = "ifany")


# Marital status (Married vs Never married vs widowed/divorced/separated)
table(data_partner$pa065, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pa065 = na_if(pa065, "Missing")) %>%
  mutate(pa065 = recode(pa065, "1st marriage" = "Married", "Divorced" = "Sep/Div/Widow", "Marriage 2 or 3" = "Married",
                       "Separated" = "Sep/Div/Widow", "Widowed" = "Sep/Div/Widow")) %>%
  mutate(pa065 = factor(pa065, levels = c("Married", "Never married", "Sep/Div/Widow"))) %>%
  rename(marital = pa065)

table(data_partner$marital, useNA = "ifany")


# Maternal parity (0 vs 1 vs 2 or more)
table(data_partner$b032, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(b032 = na_if(b032, "Missing")) %>%
  mutate(b032 = na_if(b032, "HaB short")) %>%
  mutate(b032 = na_if(b032, "Inconsistent data")) %>%
  mutate(b032 = as.numeric(b032)) %>%
  mutate(b032 = ifelse(b032 > 2 & !is.na(b032), 2, b032)) %>%
  mutate(b032 = as.factor(b032)) %>%
  mutate(b032 = recode(b032, "0" = "0", "1" = "1", "2" = "2 or more")) %>%
  mutate(b032 = factor(b032, levels = c("0", "1", "2 or more"))) %>%
  rename(parity = b032)

table(data_partner$parity, useNA = "ifany")


# Urban/rural location (Urban vs Rural)
table(data_partner$jan1993ur01ind_M, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(jan1993ur01ind_M = na_if(jan1993ur01ind_M, "Missing")) %>%
  mutate(jan1993ur01ind_M = recode(jan1993ur01ind_M, "Hamlet and Isolated Dwelling" = "Rural", 
                                   "Town and Fringe" = "Rural", "Village" = "Rural", 
                                   "Urban (pop. >= 10k)" = "Urban")) %>%
  mutate(jan1993ur01ind_M = factor(jan1993ur01ind_M, levels = c("Urban", "Rural"))) %>%
  rename(rural = jan1993ur01ind_M)

table(data_partner$rural, useNA = "ifany")


# Highest education qualification (CSE/None vs Vocational vs O-level vs A-level vs Degree)
table(data_partner$pb325a, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb325a = na_if(pb325a, "Missing")) %>%
  mutate(pb325a = recode(pb325a, "CSE" = "CSE/None")) %>%
  mutate(pb325a = factor(pb325a, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree"))) %>%
  rename(edu = pb325a)

table(data_partner$edu, useNA = "ifany")


# Occupational social class (combine lowest social classes - Binary (low [III manual/IV/V] vs high [I/II/III non-manual]) 
table(data_partner$pb_sc_p, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb_sc_p = recode(pb_sc_p, "I - Professional" = "High", "II - Mangerial and technical" = "High", 
                          "IIINM - Skilled non-manual" = "High", "IIIM - Skilled manual" = "Low", 
                          "IV - Partly skilled" = "Low", "V - Unskileld" = "Low")) %>%
  mutate(pb_sc_p = factor(pb_sc_p, levels = c("Low", "High"))) %>%
  rename(occSocClass = pb_sc_p)

table(data_partner$occSocClass, useNA = "ifany")


# Area-level index of multiple deprivation (IMD; quintiles)
table(data_partner$jan1993imd2010q5_M, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(jan1993imd2010q5_M = na_if(jan1993imd2010q5_M, "Missing")) %>%
  mutate(jan1993imd2010q5_M = recode(jan1993imd2010q5_M, "Least deprived" = "Quin. 1/Least deprived", 
                                     "2" = "Quintile 2", "3" = "Quintile 3", "4" = "Quintile 4", 
                                     "Most deprived" = "Quin. 5/Most deprived")) %>%
  mutate(jan1993imd2010q5_M = factor(jan1993imd2010q5_M, levels = c("Quin. 1/Least deprived", 
                                                                    "Quintile 2", "Quintile 3", 
                                                                    "Quintile 4", "Quin. 5/Most deprived"))) %>%
  rename(imd = jan1993imd2010q5_M)

table(data_partner$imd, useNA = "ifany")


# Recent financial difficulties (Code as binary yes vs no)
table(data_partner$pb184, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb184 = na_if(pb184, "-1")) %>%
  mutate(pb184 = recode(pb184, "affected a lot" = "Yes", "didnt happen" = "No", "fairly affected" = "Yes",
                        "mildly affected" = "Yes", "N affect at all" = "Yes")) %>%
  mutate(pb184 = factor(pb184, levels = c("No", "Yes"))) %>%
  rename(finDiffs = pb184)

table(data_partner$finDiffs, useNA = "ifany")


# Home ownership status (owned/mortgaged vs rented vs Council/housing association vs other)
table(data_partner$a006, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(a006 = na_if(a006, "Missing")) %>%
  mutate(a006 = na_if(a006, "YE short")) %>%
  mutate(a006 = recode(a006, "Council rented" = "Council/HA", "HA rented" = "Council/HA",
                       "Mortgaged" = "Owned/Mortgaged", "Owned" = "Owned/Mortgaged",
                       "RENT PRIV FURN" = "Rented", "RENT PRIV UNFURN" = "Rented")) %>%
  mutate(a006 = factor(a006, levels = c("Owned/Mortgaged", "Rented", "Council/HA", "Other"))) %>%
  rename(home = a006)

table(data_partner$home, useNA = "ifany")


# Household access to a car
table(data_partner$a053, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(a053 = na_if(a053, "Missing")) %>%
  mutate(a053 = na_if(a053, "YE short")) %>%
  mutate(a053 = factor(a053, levels = c("No", "Yes"))) %>%
  rename(carAccess = a053)

table(data_partner$carAccess, useNA = "ifany")


# Currently employed (code to binary variable, based on answers to 4 questions about different types of employment - Then drop this individual variables)
table(data_partner$pb380, useNA = "ifany")
table(data_partner$pb381, useNA = "ifany")
table(data_partner$pb382, useNA = "ifany")
table(data_partner$pb383, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(employed = ifelse(is.na(pb380), NA,
                           ifelse(pb380 == "Y" | pb381 == "Y" | pb382 == "Y" | pb383 == "Y", "Yes", "No"))) %>%
  mutate(employed = factor(employed, levels = c("No", "Yes"))) %>%
  select(-c(pb380:pb383))

table(data_partner$employed, useNA = "ifany")


## Adverse childhood experiences (total number experienced)
table(data_partner$pb482, useNA = "ifany")

# If missing, code as NA, then convert to numeric
data_partner <- data_partner %>%
  rename(ACEs = pb482)

table(data_partner$ACEs, useNA = "ifany")
summary(data_partner$ACEs)


# Locus of control (score from 0 to 12, with higher scores indicating greater external locus of control)
table(data_partner$pa782, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pa782 = na_if(pa782, -1)) %>%
  mutate(pa782 = as.numeric(pa782)) %>%
  rename(locus = pa782)

table(data_partner$locus, useNA = "ifany")
summary(data_partner$locus)


# Interpersonal sensitivity personality scores (with higher scores indicating greater interpersonal sensitivity)
table(data_partner$pb551, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb551 = na_if(pb551, -1)) %>%
  rename(IPSM = pb551)

table(data_partner$IPSM, useNA = "ifany")
summary(data_partner$IPSM)


# Self-reported health status (recode to always well vs usually well vs often/sometimes/always unwell)
table(data_partner$a524, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(a524 = na_if(a524, "Missing")) %>%
  mutate(a524 = na_if(a524, "No partner")) %>%
  mutate(a524 = recode(a524, "Always unwell" = "Often unwell", "Sometimes unwell" = "Often unwell")) %>%
  mutate(a524 = factor(a524, levels = c("Always well", "Usually well", "Often unwell"))) %>%
  rename(health = a524)

table(data_partner$health, useNA = "ifany")


# Body mass index (BMI) - Need to create from individual height and weight variables

# Weight (kg)
table(data_partner$paw002, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(paw002 = na_if(paw002, "missing")) %>%
  mutate(paw002 = na_if(paw002, "not known")) %>%
  mutate(paw002 = na_if(paw002, "unreasonable or illegable")) %>%
  mutate(paw002 = as.numeric(paw002))

summary(data_partner$paw002)

# Height (cm)
table(data_partner$paw010, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(paw010 = na_if(paw010, "missing")) %>%
  mutate(paw010 = na_if(paw010, "not known")) %>%
  mutate(paw010 = na_if(paw010, "unreasonable or illegable")) %>%
  mutate(paw010 = as.numeric(paw010))

summary(data_partner$paw010)

# Create BMI
data_partner <- data_partner %>%
  mutate(BMI = ifelse(is.na(paw002) | is.na(paw010), NA, paw002 / ((paw010 / 100) ^ 2))) %>%
  select(-c(paw002, paw010))

summary(data_partner$BMI)
hist(data_partner$BMI)


# Self-reported physical activity levels, relative to others (Much more active vs Somewhat more active vs About the same vs Somewhat less active Vs Much less active)
table(data_partner$pb012, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb012 = na_if(pb012, "-1")) %>%
  mutate(pb012 = factor(pb012, levels = c("Much less", "Somewhat less", "About same", 
                                          "Somewhat more", "Much more"))) %>%
  rename(activity = pb012)

table(data_partner$activity, useNA = "ifany")


# Smoking status (Currently smokes vs Formerly smoked vs Never smoked) - b650 = ever smoked; b659 = Stopped smoking
table(data_partner$pb071, useNA = "ifany")
table(data_partner$pb074, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb071 = na_if(pb071, "-1")) %>%
  mutate(pb071 = recode(pb071, "Y" = "Yes", "N" = "No")) %>%
  mutate(pb071 = factor(pb071, levels = c("No", "Yes"))) %>%
  mutate(pb074 = na_if(pb074, "-1")) %>%
  mutate(pb074 = recode(pb074, "Y" = "Yes", "N" = "No")) %>%
  mutate(pb074 = factor(pb074, levels = c("No", "Yes")))

table(data_partner$pb071, useNA = "ifany")
table(data_partner$pb074, useNA = "ifany")


# Quick consistency check - Some mis-matches/missing data, so will exclude in combined variable
table(data_partner$pb071, data_partner$pb074, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(smoking = ifelse(is.na(pb071) | (is.na(pb074) & pb071 == "Yes"), NA,
                          ifelse(pb071 == "No" & is.na(pb074), "Never",
                                 ifelse(pb071 == "Yes" & pb074 == "Yes", "Former", 
                                        ifelse(pb071 == "Yes" & pb074 == "No", "Current", NA))))) %>%
  mutate(smoking = factor(smoking, levels = c("Never", "Former", "Current"))) %>%
  select(-c(pb071, pb074))
  
table(data_partner$smoking, useNA = "ifany")


# Alcohol intake pre-pregnancy 
table(data_partner$pb099, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb099 = na_if(pb099, "Missing")) %>%
  mutate(pb099 = recode(pb099, "<1 glass PWK" = "<1 p/wk", "1+ glasses PWK" = "1+ p/wk",
                       "1-2 glasses PDAY" = "1+ p/day", "3-9 glasses PDAY" = "3+ p/day",
                       "10+ glasses PDAY" = "3+ p/day")) %>%
  mutate(pb099 = factor(pb099, levels = c("Never", "<1 p/wk", "1+ p/wk", "1+ p/day", "3+ p/day"))) %>%
  rename(alcPrePreg = pb099)

table(data_partner$alcPrePreg, useNA = "ifany")


# Social network score (with higher scores indicating a larger and more supportive network)
table(data_partner$pb120, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb120 = na_if(pb120, -1)) %>%
  rename(socNetwork = pb120)

table(data_partner$socNetwork, useNA = "ifany")
summary(data_partner$socNetwork)


# Social support score (with higher scores indicating greater social support)
table(data_partner$pb140, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pb140 = na_if(pb140, -1)) %>%
  rename(socSupport = pb140)

table(data_partner$socSupport, useNA = "ifany")
summary(data_partner$socSupport)


# Maternal history of depression/anxiety
table(data_partner$pa536a, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pa536a = factor(pa536a, levels = c("No", "Yes"))) %>%
  rename(maternalMH = pa536a)

table(data_partner$maternalMH, useNA = "ifany")


# Paternal history of depression/anxiety
table(data_partner$pa586a, useNA = "ifany")

data_partner <- data_partner %>%
  mutate(pa586a = factor(pa586a, levels = c("No", "Yes"))) %>%
  rename(paternalMH = pa586a)

table(data_partner$paternalMH, useNA = "ifany")



## Re-order variables for the final dataset
data_partner <- data_partner %>%
  relocate(aln, belief_preg:attend_preg, belief_age5, belief_age5_bin, identity_age5, identity_age5_bin,
           attend_age5, attend_age5_bin, 
           dep_preg:dep_age2, dep_age2_bin, anx_age2, anx_age2_bin, dep_age6, dep_age6_bin, anx_age6, anx_age6_bin, 
           age, ethnicity, marital, parity, rural, edu, occSocClass, imd, finDiffs, home, carAccess,
           employed, ACEs, locus, IPSM, health, BMI, activity, smoking, alcPrePreg, socNetwork:paternalMH)

colnames(data_partner)



### Save partner's data (in R and CSV formats; the R format will keep all the factor formatting, while the CSV file will lose this)

save(data_partner, file = "data_partner_processed_B4226.RData")
write_csv(data_partner, file = "data_partner_processed_B4226.csv")



