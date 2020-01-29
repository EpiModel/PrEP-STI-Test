
# PrEP STI Screening Analysis Script

library("ARTnetData")
sessioninfo::session_info()
# ARTnetData  * 1.0     2019-04-25 [1] Github (EpiModel/ARTnetData@f07ba02)

library("tidyverse") 
library("lubridate") # used to manipulate dates

## Load Wide and Long Form Datasets
d <- ARTnet.wide
l <- ARTnet.long

# Create new variable - any rectal anal intercourse
d <- l %>%
  filter(RAI == 1) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(nRAIpart = n) %>%
  right_join(d, by = "AMIS_ID") %>%
  as.data.frame()
d$nRAIpart <- ifelse(is.na(d$nRAIpart), 0, d$nRAIpart)
table(d$nRAIpart)
d$anyRAI <- ifelse(d$nRAIpart > 0, 1, 0)
table(d$anyRAI)

# Create new variable - any insertive anal intercourse
d <- l %>%
  filter(IAI == 1) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(nIAIpart = n) %>%
  right_join(d, by = "AMIS_ID") %>%
  as.data.frame()
d$nIAIpart <- ifelse(is.na(d$nIAIpart), 0, d$nIAIpart)
table(d$nIAIpart)
d$anyIAI <- ifelse(d$nIAIpart > 0, 1, 0)
table(d$anyIAI)

# Create new variable - any receptive oral intercourse
d <- l %>%
  filter(ROI == 1) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(nROIpart = n) %>%
  right_join(d, by = "AMIS_ID") %>%
  as.data.frame()
d$nROIpart <- ifelse(is.na(d$nROIpart), 0, d$nROIpart)
table(d$nROIpart)
d$anyROI <- ifelse(d$nROIpart > 0, 1, 0)
table(d$anyROI)

# Create new variable - any insertive oral intercourse
d <- l %>%
  filter(IOI == 1) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(nIOIpart = n) %>%
  right_join(d, by = "AMIS_ID") %>%
  as.data.frame()
d$nIOIpart <- ifelse(is.na(d$nIOIpart), 0, d$nIOIpart)
table(d$nIOIpart)
d$anyIOI <- ifelse(d$nIOIpart > 0, 1, 0)
table(d$anyIOI)

## Limit Analysis to HIV-negative and ever HIV tested
d <- filter(d, hiv2 == 0 & EVERTEST == 1)
nrow(d)

# Reviewing patterns of missingness for PrEP Use Variable
table(d$survey.year, d$PREP_REVISED, useNA = "always")
table(d$SUB_DATE, d$PREP_REVISED, useNA = "always")

## Limit analysis to MSM not missing PrEP use
d <- filter(d, !is.na(PREP_REVISED))
nrow(d)


# Table 1 -----------------------------------------------------------------

# demographics, geography (DIVCODE), risk behavior by PrEP never, 
#    PrEP ever not current, PrEP current

## Q13: Have you ever taken PrEP (i.e. Truvada)?
table(d$PREP_REVISED, useNA = "always")


## Q14. Are you currently taking PrEP (i.e. Truvada)?
table(d$artnetPREP_CURRENT, useNA = "always")
table(d$artnetPREP_CURRENT, d$PREP_REVISED, useNA = "always")

# Create new variable (prep_cat) for PrEP use:
  # 0: Never 
  # 1: Non-current PrEP user
  # 2: Current PrEP user

d$prep_cat <- ifelse(d$PREP_REVISED == 0, 0,
              ifelse(d$artnetPREP_CURRENT == 0, 1,
              ifelse(d$artnetPREP_CURRENT == 1, 2, NA)))
                    
table(d$prep_cat, useNA = "always")

# Limit analysis to MSM not missing Current/Non-Current PrEP use
d <- filter(d, !is.na(prep_cat))

## Race
addmargins(table(d$race.cat, d$prep_cat, useNA = "always"))

## Age
summary(d$age)
sd(d$age)

d %>%
  group_by(prep_cat) %>%
  summarize(mean = mean(age),
            sd = sd(age),
            median = median(age))

### Create categories for age
  # 0: 15-24
  # 1: 25-34
  # 2: 35-44
  # 3: 45-54
  # 4: 55-65
  # 5: 66+

d$age_cat <- ifelse(d$age >= 15 & d$age <= 24, 0,
             ifelse(d$age >= 25 & d$age <= 34, 1,
             ifelse(d$age >= 35 & d$age <= 44, 2,
             ifelse(d$age >= 45 & d$age <= 54, 3,
             ifelse(d$age >= 55 & d$age <= 65, 4,
             ifelse(d$age >= 66, 5, NA)))))) 

addmargins(table(d$age_cat, d$prep_cat, useNA = 'always'))


## Census division
addmargins(table(d$DIVCODE, d$prep_cat, useNA = 'always'))

## Deep South
summary(d$State)
table(is.na(d$State))

### Create "deep south" indicator variable
  # 0: Not in deep south
  # 1: In deep south (NC, SC, GA, AL, MS, LA, FL)

d$deep_south <- ifelse(d$State == "NC" |
                       d$State == "SC" |
                       d$State == "GA" |
                       d$State == "AL" |
                       d$State == "MS" |
                       d$State == "LA" |
                       d$State == "FL", 1, 0)

table(d$State, d$deep_south)
addmargins(table(d$deep_south, d$prep_cat, useNA = "always"))

## Urbanicity
addmargins(table(d$NCHS_2013, d$prep_cat, useNA = 'always'))

## Highest level of education
### Re-categorize education

# 0: High school or below
# 1: Some college
# 2: College and above
# NA: Missing ()

d$HLEDUCAT_2 <- ifelse(d$HLEDUCAT <= 3, 0,
                ifelse(d$HLEDUCAT == 4, 1,
                ifelse(d$HLEDUCAT == 5, 2, NA)))

addmargins(table(d$HLEDUCAT, useNA = "always"))
addmargins(table(d$HLEDUCAT_2, d$prep_cat, useNA = 'always'))

## Annual income

### Collapse 'Prefer not to answer', 'Don't know', and 'Missing' to 'Missing'
addmargins(table(d$HHINCOME, useNA = "always"))

d$HHINCOME <- ifelse(d$HHINCOME == 77 |
                     d$HHINCOME == 99, NA,
                     d$HHINCOME)

addmargins(table(d$HHINCOME, d$prep_cat, useNA = 'always'))

## Health insurance
# Re-categorize health insurance
# 0: No health insurance
# 1: Health insurance, not private 
# 2: Health insurance, any private insurance 
  ## (Including those with non-private insurance)

d$insurance <- ifelse(d$TYP_INSA == 1 | d$TYP_INSA2 == 1, 2, 
               ifelse(d$TYP_INSG == 1 | d$TYP_INSH == 1 |
                      d$TYP_INSD == 1 | d$TYP_INSE == 1 |
                      d$TYP_INSF == 1, 1,
               ifelse(d$TYP_INSI == 1, 0, NA)))

addmargins(table(d$insurance, d$prep_cat, useNA = 'always'))

# Any STI test in past 12 months
## Re-categorize 7 and 9 as missing
addmargins(table(d$EVERSTI_TEST, useNA = "always"))
d$EVERSTI_TEST <- ifelse(d$EVERSTI_TEST == 7 | d$EVERSTI_TEST == 9,
                         NA, d$EVERSTI_TEST)
addmargins(table(d$EVERSTI_TEST, d$prep_cat, useNA = "always"))

addmargins(table(d$ANYSTI_TEST, useNA = "always"))
d$ANYSTI_TEST <- ifelse(d$ANYSTI_TEST == 7 | d$ANYSTI_TEST == 9,
                         NA, d$ANYSTI_TEST)
addmargins(table(d$ANYSTI_TEST, d$prep_cat, useNA = "always"))

# Create indicator variable for having an STI Test in past 12 months
# that recodes those who have never been tested from NA to 0
d$STITEST_P12M <- ifelse(d$EVERSTI_TEST == 0, 0, d$ANYSTI_TEST)

addmargins(table(d$STITEST_P12M, d$prep_cat, useNA = "always"))

# Recent STI testing 
## Number of times tested for an STI within the past 2 years
## STITEST_2YR_PREP asks question for NOT at PrEP visits

### Clean data
table(d$STITEST_2YR, d$prep_cat, useNA = "always")
d$STITEST_2YR <- ifelse(d$STITEST_2YR == 2015, NA, d$STITEST_2YR) 

table(d$STITEST_2YR_PREP, d$prep_cat, useNA = "always")
d$STITEST_2YR_PREP <- ifelse(d$STITEST_2YR_PREP == 2000, NA, d$STITEST_2YR_PREP)

### Combine data for NEVER and EVER PrEP users
d$STITEST_2YR_COMBINED <- ifelse(is.na(d$STITEST_2YR),
                                 d$STITEST_2YR_PREP, 
                                 d$STITEST_2YR)

summary(d$STITEST_2YR_COMBINED)
sd(d$STITEST_2YR_COMBINED, na.rm = T)

d %>% 
  group_by(prep_cat) %>%
  summarise(mean = mean(STITEST_2YR_COMBINED, na.rm = T),
            sd = sd(STITEST_2YR_COMBINED, na.rm = T),
            median = median(STITEST_2YR_COMBINED, na.rm = T))

addmargins(table(d$STITEST_2YR_COMBINED, d$survey.year, useNA = "always"))

## Had an HIV test in the past 12 months
table(d$TSTP12M, useNA = "always")
d$TSTP12M <- ifelse(d$TSTP12M == 9, NA, d$TSTP12M)

# Combine recent test month and year as one date variable 
## (marks the first of the month as default day)
d$RCNTSTDATE <- make_date(year = d$RCNTSTYEAR, month = d$RCNTSTMONTH)

# Create variable to calculate # months between RCNTSTDATE and SUB_DATE
# Create indicator variable for combined HIV testing in past 12 months
d <- d %>%
  mutate(TSTP12M_date = (d$RCNTSTDATE %--% d$SUB_DATE)/months(1),
         TSTP12M_COMB = ifelse(TSTP12M_date <= 12, 1,
                        ifelse(TSTP12M_date > 12, 0,
                               ifelse(TSTP12M == 1, 1,
                               ifelse(TSTP12M == 0, 0, NA)))))

addmargins(table(d$TSTP12M_COMB, d$prep_cat, useNA = "always"))

# Number of times tested for HIV in the past 2 years 
summary(d$TEST2YRS)
sd(d$TEST2YRS, na.rm = T)

d %>%
  group_by(prep_cat) %>%
  summarise(mean = mean(TEST2YRS, na.rm = T),
            sd = sd(TEST2YRS, na.rm = T),
            median = median(TEST2YRS, na.rm = T))


# Table 2 -----------------------------------------------------------------

# causal question: is HIV screening rate and STI screening rate higher
# in current PrEP users compared to non-current PrEP users and never PrEP users?

## Descriptive Table 2

# Frequency of PrEP-related check-ups among ever PrEP users
## Combine data for NEVER and EVER PrEP users / frequency of PrEP checkups
d$PREPCHKFREQ_COMB <- ifelse(is.na(d$PREPCHKFREQ),
                               d$PREPCHKFREQ_CURR, 
                               d$PREPCHKFREQ)

table(d$PREPCHKFREQ_COMB, d$prep_cat, useNA = "always")

# Frequency of STI testing after using PrEP compared to before  among current users
table(d$STIFREQ_PREP, d$prep_cat, useNA = "always")

# Frequency of throat swab/STI testing at PrEP visits
table(d$PREP_STITHROATFREQ, d$prep_cat, useNA = "always")

# Frequency of rectal swab/STI testing at PrEP visits
table(d$PREP_STIRECTFREQ, d$prep_cat, useNA = "always")

# Frequency of urine sample/STI testing at PrEP visits
table(d$PREP_STIURETHFREQ, d$prep_cat, useNA = "always")

# Frequency of blood sample/STI testing at PrEP visits
table(d$PREP_BLOODFREQ, d$prep_cat, useNA = "always")


# Table 3 -----------------------------------------------------------------

# What are the predictors of recommended HIV and blood STI screening in ever PrEP 
# users -- is there variation by demographics, risk behavior, or geogrpahy?

# Create subset of ever PrEP users 
# Excludes 3 PrEP users with missing data on current use
d2 <- filter(d, d$PREP_REVISED == 1)

# Recoding for always/not for HIV testing at PrEP visits
d2$prep.hiv.always <- ifelse(d2$PREP_HIVTESTFREQ %in% 2:4, 0, d2$PREP_HIVTESTFREQ)
table(d2$prep.hiv.always, useNA = "always")

# Descriptive tables by exposure
addmargins(table(d2$prep.hiv.always))
addmargins(table(d2$DIVCODE, d2$prep.hiv.always))
addmargins(table(d2$deep_south, d2$prep.hiv.always))
addmargins(table(d2$race.cat, d2$prep.hiv.always))
addmargins(table(d2$age_cat, d2$prep.hiv.always))
addmargins(table(d2$insurance, d2$prep.hiv.always, useNA = "always"))

# Logistic regression model (unadjusted)
# E: Census division  O: HIV testing
mod1.hiv <- glm(prep.hiv.always ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod1.hiv)
exp(coef(mod1.hiv))
exp(confint(mod1.hiv))

a <- as.data.frame(coef(mod1.hiv))
a$alpha <- mod1.hiv$coefficients[1]
a$plogis <- plogis(a[,1] + a[,2])
a

# Logistic regression model (unadjusted)
# E: Deep South  O: HIV testing
mod2.hiv <- glm(prep.hiv.always ~ deep_south, data = d2, family = binomial())
summary(mod2.hiv)
exp(coef(mod2.hiv))
exp(confint(mod2.hiv))

plogis(sum(coef(mod2.hiv)))

# Logistic regression model (unadjusted)
# E: Race/ethnicity  O: HIV testing
mod3.hiv <- glm(prep.hiv.always ~ race.cat, data = d2, family = binomial())
summary(mod3.hiv)
exp(coef(mod3.hiv))
exp(confint(mod3.hiv))

a3 <- as.data.frame(coef(mod3.hiv))
a3$alpha <- mod3.hiv$coefficients[1]
a3$plogis <- plogis(a3[,1] + a3[,2])
a3

# Logistic regression model (unadjusted)
# E: Insurance  O: HIV testing
mod4.hiv <- glm(prep.hiv.always ~ as.factor(insurance), data = d2, family = binomial())
summary(mod4.hiv)
exp(coef(mod4.hiv))
exp(confint(mod4.hiv))

a4 <- as.data.frame(coef(mod4.hiv))
a4$alpha <- mod4.hiv$coefficients[1]
a4$plogis <- plogis(a4[,1] + a4[,2])
a4

# Logistic regression model (unadjusted)
# E: Age category  O: HIV testing
mod5.hiv <- glm(prep.hiv.always ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod5.hiv)
exp(coef(mod5.hiv))
exp(confint(mod5.hiv))

a5 <- as.data.frame(coef(mod5.hiv))
a5$alpha <- mod5.hiv$coefficients[1]
a5$plogis <- plogis(a5[,1] + a5[,2])
a5

## Q17d: How Often Tested for STIs Blood
##       1:4: always, sometimes, rarely, never
table(d2$PREP_BLOODFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never blood STI testings
d2$prep.blood.sometimes <- ifelse(d2$PREP_BLOODFREQ %in% 1:2, 1, 
                                  ifelse(d2$PREP_BLOODFREQ %in% 3:4, 0, NA))
table(d2$prep.blood.sometimes, d2$PREP_BLOODFREQ)
addmargins(table(d2$prep.blood.sometimes))

# Descriptive tables by exposure
addmargins(table(d2$prep.blood.sometimes))
addmargins(table(d2$DIVCODE, d2$prep.blood.sometimes))
addmargins(table(d2$deep_south, d2$prep.blood.sometimes))
addmargins(table(d2$race.cat, d2$prep.blood.sometimes))
addmargins(table(d2$age_cat, d2$prep.blood.sometimes))
addmargins(table(d2$insurance, d2$prep.blood.sometimes, useNA = "always"))

# Logistic regression model (unadjusted)
# E: Census division  O: Blood STI testing
mod1.blood <- glm(prep.blood.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod1.blood)
exp(coef(mod1.blood))
exp(confint(mod1.blood))

y <- as.data.frame(coef(mod1.blood))
y$alpha <- mod1.blood$coefficients[1]
y$plogis <- plogis(y[,1] + y[,2])
y

# Logistic regression model (unadjusted)
# E: Deep south  O: Blood STI testing
mod2.blood <- glm(prep.blood.sometimes ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod2.blood)
exp(coef(mod2.blood))
exp(confint(mod2.blood))

plogis(sum(coef(mod2.blood)))

# Logistic regression model (unadjusted)
# E: Race/Ethnicity  O: Blood STI testing
mod3.blood <- glm(prep.blood.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod3.blood)
exp(coef(mod3.blood))
exp(confint(mod3.blood))

y3 <- as.data.frame(coef(mod3.blood))
y3$alpha <- mod3.blood$coefficients[1]
y3$plogis <- plogis(y3[,1] + y3[,2])
y3

# Logistic regression model (unadjusted)
# E: Age category  O: Blood STI testing
mod4.blood <- glm(prep.blood.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod4.blood)
exp(coef(mod4.blood))
exp(confint(mod4.blood))

y4 <- as.data.frame(coef(mod4.blood))
y4$alpha <- mod4.blood$coefficients[1]
y4$plogis <- plogis(y4[,1] + y4[,2])
y4

# Logistic regression model (unadjusted)
# E: Insurance  O: Blood STI testing
mod5.blood <- glm(prep.blood.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod5.blood)
exp(coef(mod5.blood))
exp(confint(mod5.blood))

y5 <- as.data.frame(coef(mod5.blood))
y5$alpha <- mod5.blood$coefficients[1]
y5$plogis <- plogis(y5[,1] + y5[,2])
y5


# Table 4 -----------------------------------------------------------------

# What are the predictors of urethral, rectal, and pharyngeal STI screening in ever PrEP 
# users -- is there variation by demographics, risk behavior, or geogrpahy?

## Q17b: How Often Tested for STIs in Throat
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STITHROATFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never pharyngeal STI testings
d2$prep.throat.sometimes <- ifelse(d2$PREP_STITHROATFREQ %in% 1:2, 1, 
                                   ifelse(d2$PREP_STITHROATFREQ %in% 3:4, 0, NA))
table(d2$prep.throat.sometimes, d2$PREP_STITHROATFREQ)
addmargins(table(d2$prep.throat.sometimes))

## Descriptive tables by exposure
addmargins(table(d2$prep.throat.sometimes))
addmargins(table(d2$DIVCODE, d2$prep.throat.sometimes))
addmargins(table(d2$deep_south, d2$prep.throat.sometimes))
addmargins(table(d2$race.cat, d2$prep.throat.sometimes))
addmargins(table(d2$age_cat, d2$prep.throat.sometimes))
addmargins(table(d2$insurance, d2$prep.throat.sometimes, useNA = "always"))
addmargins(table(d2$anyROI, d2$prep.throat.sometimes))

# Logistic regression model (unadjusted)
# E: Census division  O: Pharyngeal STI testing
mod1.throat <- glm(prep.throat.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod1.throat)
exp(coef(mod1.throat))
exp(confint(mod1.throat))

b <- as.data.frame(coef(mod1.throat))
b$alpha <- mod1.throat$coefficients[1]
b$plogis <- plogis(b[,1] + b[,2])
b

# Logistic regression model (unadjusted)
# E: Deep south O: Pharyngeal STI testing
mod2.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod2.throat)
exp(coef(mod2.throat))
exp(confint(mod2.throat))

plogis(sum(coef(mod2.throat)))

# Logistic regression model (unadjusted)
# E: Race/ethnicity O: Pharyngeal STI testing
mod3.throat <- glm(prep.throat.sometimes ~ race.cat, data = d2, family = binomial())
summary(mod3.throat)
exp(coef(mod3.throat))
exp(confint(mod3.throat))

b3 <- as.data.frame(coef(mod3.throat))
b3$alpha <- mod3.throat$coefficients[1]
b3$plogis <- plogis(b3[,1] + b3[,2])
b3

# Logistic regression model (unadjusted)
# E: Age category O: Pharyngeal STI testing
mod4.throat <- glm(prep.throat.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod4.throat)
exp(coef(mod4.throat))
exp(confint(mod4.throat))

b4 <- as.data.frame(coef(mod4.throat))
b4$alpha <- mod4.throat$coefficients[1]
b4$plogis <- plogis(b4[,1] + b4[,2])
b4

# Logistic regression model (unadjusted)
# E: Insurance O: Pharyngeal STI testing
mod5.throat <- glm(prep.throat.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod5.throat)
exp(coef(mod5.throat))
exp(confint(mod5.throat))

b5 <- as.data.frame(coef(mod5.throat))
b5$alpha <- mod5.throat$coefficients[1]
b5$plogis <- plogis(b5[,1] + b5[,2])
b5

# Logistic regression model (unadjusted)
# E: Exposure at pharyngeal site O: Pharyngeal STI testing
mod6.throat <- glm(prep.throat.sometimes ~ as.factor(anyROI),
                    data = d2, family = binomial())
summary(mod6.throat)
exp(coef(mod6.throat))
exp(confint(mod6.throat))

plogis(sum(coef(mod6.throat)))

## Q17c: How Often Tested for STIs Rectal
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STIRECTFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never rectal STI testings
d2$prep.rect.sometimes <- ifelse(d2$PREP_STIRECTFREQ %in% 1:2, 1, 
                                 ifelse(d2$PREP_STIRECTFREQ %in% 3:4, 0, NA))
table(d2$prep.rect.sometimes, d2$PREP_STIRECTFREQ)
addmargins(table(d2$prep.rect.sometimes))

## Descriptive tables by exposure
addmargins(table(d2$prep.rect.sometimes))
addmargins(table(d2$DIVCODE, d2$prep.rect.sometimes))
addmargins(table(d2$deep_south, d2$prep.rect.sometimes))
addmargins(table(d2$race.cat, d2$prep.rect.sometimes))
addmargins(table(d2$age_cat, d2$prep.rect.sometimes))
addmargins(table(d2$insurance, d2$prep.rect.sometimes, useNA = "always"))
addmargins(table(d2$anyRAI, d2$prep.rect.sometimes))

# Logistic regression model (unadjusted)
# E: Census division  O: Rectal STI testing
mod1.rectal <- glm(prep.rect.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod1.rectal)
exp(coef(mod1.rectal))
exp(confint(mod1.rectal))

c <- as.data.frame(coef(mod1.rectal))
c$alpha <- mod1.rectal$coefficients[1]
c$plogis <- plogis(c[,1] + c[,2])
c

# Logistic regression model (unadjusted)
# E: Deep south O: Rectal STI testing
mod2.rectal <- glm(prep.rect.sometimes ~ deep_south, data = d2, family = binomial())
summary(mod2.rectal)
exp(coef(mod2.rectal))
exp(confint(mod2.rectal))

plogis(sum(coef(mod2.rectal)))


# Logistic regression model (unadjusted)
# E: Race/Ethnicity O: Rectal STI testing
mod3.rectal <- glm(prep.rect.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod3.rectal)
exp(coef(mod3.rectal))
exp(confint(mod3.rectal))

c3 <- as.data.frame(coef(mod3.rectal))
c3$alpha <- mod3.rectal$coefficients[1]
c3$plogis <- plogis(c3[,1] + c3[,2])
c3

# Logistic regression model (unadjusted)
# E:Age category  O: Rectal STI testing
mod4.rectal <- glm(prep.rect.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod4.rectal)
exp(coef(mod4.rectal))
exp(confint(mod4.rectal))

c4 <- as.data.frame(coef(mod4.rectal))
c4$alpha <- mod4.rectal$coefficients[1]
c4$plogis <- plogis(c4[,1] + c4[,2])
c4

# Logistic regression model (unadjusted)
# E: Insurance O: Rectal STI testing
mod5.rectal <- glm(prep.rect.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod5.rectal)
exp(coef(mod5.rectal))
exp(confint(mod5.rectal))

c5 <- as.data.frame(coef(mod5.rectal))
c5$alpha <- mod5.rectal$coefficients[1]
c5$plogis <- plogis(c5[,1] + c5[,2])
c5

# Logistic regression model (unadjusted)
# E: Exposure at site  O: Rectal STI testing
mod6.rectal <- glm(prep.rect.sometimes ~ anyRAI, data = d2, family = binomial())
summary(mod6.rectal)
exp(coef(mod6.rectal))
exp(confint(mod6.rectal))

plogis(sum(coef(mod6.rectal)))

## Q17c: How Often Tested for STIs Urethral
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STIURETHFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never urethral STI testings
d2$prep.ureth.sometimes <- ifelse(d2$PREP_STIURETHFREQ %in% 1:2, 1, 
                                 ifelse(d2$PREP_STIURETHFREQ %in% 3:4, 0, NA))
table(d2$prep.ureth.sometimes, d2$PREP_STIURETHFREQ)
addmargins(table(d2$prep.ureth.sometimes))

# Descriptive tables by exposure
addmargins(table(d2$prep.ureth.sometimes))
addmargins(table(d2$DIVCODE, d2$prep.ureth.sometimes))
addmargins(table(d2$deep_south, d2$prep.ureth.sometimes))
addmargins(table(d2$race.cat, d2$prep.ureth.sometimes))
addmargins(table(d2$age_cat, d2$prep.ureth.sometimes))
addmargins(table(d2$insurance, d2$prep.ureth.sometimes, useNA = "always"))

# Create indicator for exposure at site
d2$exp.ureth <- ifelse(d2$anyIAI == 1, 1,
                       ifelse(d2$anyIOI ==1, 1, 0))
addmargins(table(d2$exp.ureth, d2$prep.ureth.sometimes))

# Logistic regression model (unadjusted)
# E: Census division O: Urethral STI testing
mod1.ureth <- glm(prep.ureth.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod1.ureth)
exp(coef(mod1.ureth))
exp(confint(mod1.ureth))

x <- as.data.frame(coef(mod1.ureth))
x$alpha <- mod1.ureth$coefficients[1]
x$plogis <- plogis(x[,1] + x[,2])
x

# Logistic regression model (unadjusted)
# E: Deep south O: Urethral STI testing
mod2.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod2.ureth)
exp(coef(mod2.ureth))
exp(confint(mod2.ureth))

plogis(sum(coef(mod2.ureth)))

# Logistic regression model (unadjusted)
# E: Race/ethnicity O: Urethral STI testing
mod3.ureth <- glm(prep.ureth.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod3.ureth)
exp(coef(mod3.ureth))
exp(confint(mod3.ureth))

x3 <- as.data.frame(coef(mod3.ureth))
x3$alpha <- mod3.ureth$coefficients[1]
x3$plogis <- plogis(x3[,1] + x3[,2])
x3

# Logistic regression model (unadjusted)
# E: Age category O: Urethral STI testing
mod4.ureth <- glm(prep.ureth.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod4.ureth)
exp(coef(mod4.ureth))
exp(confint(mod4.ureth))

x4 <- as.data.frame(coef(mod4.ureth))
x4$alpha <- mod4.ureth$coefficients[1]
x4$plogis <- plogis(x4[,1] + x4[,2])
x4

# Logistic regression model (unadjusted)
# E: Insurance O: Urethral STI testing
mod5.ureth <- glm(prep.ureth.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod5.ureth)
exp(coef(mod5.ureth))
exp(confint(mod5.ureth))

x5 <- as.data.frame(coef(mod5.ureth))
x5$alpha <- mod5.ureth$coefficients[1]
x5$plogis <- plogis(x5[,1] + x5[,2])
x5

# Logistic regression model (unadjusted)
# E: Exposure at site O: Urethral STI testing
mod6.ureth <- glm(prep.ureth.sometimes ~ as.factor(exp.ureth), data = d2, family = binomial())
summary(mod6.ureth)
exp(coef(mod6.ureth))
exp(confint(mod6.ureth))

plogis(sum(coef(mod6.ureth)))

# Table 5 -----------------------------------------------------------------

# Main predictor: Deep south
# Outcomes: HIV testing and blood, urethral, rectal, & pharyngeal STI testing
# Multivariable associations
## Adjusted for age, race/ethnicity, and exposure at site (where applicable)

# Logistic regression model (adjusted / control for race/eth, age)
# E: Deep South  O: HIV testing
mod6.hiv <- glm(prep.hiv.always ~ deep_south + race.cat + age,
                data = d2, family = binomial())
summary(mod6.hiv)
exp(coef(mod6.hiv))
exp(confint(mod6.hiv))

# Logistic regression model (adjusted / control for race/eth, age)
# E: Deep south  O: Blood STI testing
mod6.blood <- glm(prep.blood.sometimes ~ as.factor(deep_south) + age + race.cat,
                  data = d2, family = binomial())
summary(mod6.blood)
exp(coef(mod6.blood))
exp(confint(mod6.blood))

# Logistic regression model (adjusted / control for race/eth, age, exposure at site)
# E: Deep south  O: Pharyngeal STI testing
mod7.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south) + age + race.cat + anyROI,
                   data = d2, family = binomial())
summary(mod7.throat)
exp(coef(mod7.throat))
exp(confint(mod7.throat))

# Logistic regression model (adjusted / control for race/eth, age, and exposure at site)
# E: Deep south  O: Rectal STI testing
mod7.rectal <- glm(prep.rect.sometimes ~ deep_south + age + race.cat + anyRAI,
                   data = d2, family = binomial())
summary(mod7.rectal)
exp(coef(mod7.rectal))
exp(confint(mod7.rectal))

# Logistic regression model (adjusted / control for race/eth, age, and exposure at site)
# E: Deep south  O: Urethral STI testing
mod7.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south) + age + race.cat + exp.ureth,
                  data = d2, family = binomial())
summary(mod7.ureth)
exp(coef(mod7.ureth))
exp(confint(mod7.ureth))

