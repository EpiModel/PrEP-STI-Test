
# PrEP STI Screening Analysis Script

library("ARTnetData")
sessioninfo::session_info()
# ARTnetData  * 1.0     2019-04-25 [1] Github (EpiModel/ARTnetData@f07ba02)

library("tidyverse")
library("multcomp") # used to obtain OR and 95% CI for models considering effect measure modification

## Load Wide and Long Form Datasets
d <- ARTnet.wide
l <- ARTnet.long

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
table(d$race.cat, useNA = "always")
table(d$race.cat, useNA = "always")/3259*100

race <- table(d$race.cat, d$prep_cat, useNA = "always")
race
race_perc <- t(t(race)/colSums(race)*100)
race_perc

## Age
mean(d$age)
sd(d$age)

d %>%
  group_by(prep_cat) %>%
  summarize(mean = mean(age),
            sd = sd(age))

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

table(d$age_cat, useNA = "always")
table(d$age_cat)/3259*100

age <- table(d$age_cat, d$prep_cat, useNA = 'always')
age
colSums(age)

age_perc <- t(t(age)/colSums(age)*100)
age_perc

## Census division

table(d$DIVCODE, useNA = "always")
table(d$DIVCODE)/3259*100

census <- table(d$DIVCODE, d$prep_cat, useNA = 'always')
census
census_perc <- t(t(census)/colSums(census)*100)
census_perc

## Census region
# Turn REGCODE from character class to numeric

d$REGCODE <- as.numeric(d$REGCODE)
class(d$REGCODE)

table(d$DIVCODE, d$REGCODE, useNA = "always")

## Deep South
class(d$State)
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
table(d$deep_south, d$prep_cat)

### Create second "deep south" indicator variable
# 0: Not in deep south
# 1: In deep south (NC, SC, GA, AL, MS, LA, FL, TX, TN)

d$deep_south2 <- ifelse(d$State == "NC" |
                        d$State == "SC" |
                        d$State == "GA" |
                        d$State == "AL" |
                        d$State == "MS" |
                        d$State == "LA" |
                        d$State == "FL" |
                        d$State == "TX" |
                        d$State == "TN", 1, 0)

table(d$State, d$deep_south2)
table(d$deep_south2, d$prep_cat)

## Urbanicity

table(d$NCHS_2013, useNA = "always")
table(d$NCHS_2013)/3259*100

urban <- table(d$NCHS_2013, d$prep_cat, useNA = 'always')
urban
urban_perc <- t(t(urban)/colSums(urban)*100)
urban_perc

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
table(d$HLEDUCAT_2, useNA = "always")
table(d$HLEDUCAT_2, useNA = "always")/3259*100

education <- table(d$HLEDUCAT_2, d$prep_cat, useNA = 'always')
education
education_perc <- t(t(education)/colSums(education)*100)
education_perc

## Annual income

### Collapse 'Prefer not to answer', 'Don't know', and 'Missing' to 'Missing'

d$HHINCOME <- ifelse(d$HHINCOME == 77 |
                     d$HHINCOME == 99, NA,
                     d$HHINCOME)

table(d$HHINCOME, useNA = "always")
table(d$HHINCOME, useNA = "always")/3259*100

income <- table(d$HHINCOME, d$prep_cat, useNA = 'always')
income
income_perc <- t(t(income)/colSums(income)*100)
income_perc

## Health insurance

# Re-categorize health insurance
# 0: No health insurance
# 1: Health insurance, not private 
# 2: Health insurance, private (Including those with non-private insurance)

d$insurance <- ifelse(d$TYP_INSA == 1 | d$TYP_INSA2 == 1, 2, 
               ifelse(d$TYP_INSG == 1 | d$TYP_INSH == 1 |
                      d$TYP_INSD == 1 | d$TYP_INSE == 1 |
                      d$TYP_INSF == 1, 1,
               ifelse(d$TYP_INSI == 1, 0, NA)))

addmargins(table(d$insurance, useNA = 'always'))
table(d$insurance, useNA = 'always')/3259*100

insurance <- table(d$insurance, d$prep_cat, useNA = 'always')
insurance
insurance_perc <- t(t(insurance)/colSums(insurance)*100)
insurance_perc


# Table 2 -----------------------------------------------------------------

# causal question: is HIV screening rate and STI screening rate higher
# in current PrEP users compared to non-current PrEP users and never PrEP users?

## Descriptive Table 2

# Ever tested for STI (AMIS)
addmargins(table(d$EVERSTI_TEST, useNA = "always"))
table(d$EVERSTI_TEST, useNA = "always")/3259*100

evertest <- table(d$EVERSTI_TEST, d$prep_cat, useNA = "always")
evertest
evertest_perc <- t(t(evertest)/colSums(evertest)*100)
evertest_perc

# Any STI test in past 12 months (AMIS)
addmargins(table(d$ANYSTI_TEST, useNA = "always"))
table(d$ANYSTI_TEST, useNA = "always")/3259*100

anytest <- table(d$ANYSTI_TEST, d$prep_cat, useNA = "always")
anytest
anytest_perc <- t(t(anytest)/colSums(anytest)*100)
anytest_perc

# Samples used for testing in the past 12 months (AMIS)
d_samples <- d %>%
  select(AMIS_ID, STITEST_BLOOD:STITEST_DK) %>%
  gather(Sample, YN, STITEST_BLOOD:STITEST_DK)

table(d_samples$Sample, d_samples$YN, useNA = "always")
table(d_samples$Sample, d_samples$YN, useNA = "always")/3259*100

## Blood sample by PrEP category
blood <- table(d$STITEST_BLOOD, d$prep_cat, useNA = "always")
blood
blood_perc <- t(t(blood)/colSums(blood)*100)
blood_perc

## Urine sample by PrEP category
urine <- table(d$STITEST_URINE, d$prep_cat, useNA = "always")
urine
urine_perc <- t(t(urine)/colSums(urine)*100)
urine_perc

## Rectal sample by PrEP category
rectal <- table(d$STITEST_RECTUM, d$prep_cat, useNA = "always")
rectal
rectal_perc <- t(t(rectal)/colSums(rectal)*100)
rectal_perc

## Throat sample by PrEP category
throat <- table(d$STITEST_THROAT, d$prep_cat, useNA = "always")
throat
throat_perc <- t(t(throat)/colSums(throat)*100)
throat_perc

## Don't know sample by PrEP category
DK <- table(d$STITEST_DK, d$prep_cat, useNA = "always")
DK
DK_perc <- t(t(DK)/colSums(DK)*100)
DK_perc

# STI Diagnosis (AMIS)

d_diag <- d %>%
  select(AMIS_ID, BSTIA:BSTIF) %>%
  gather(Diagnosis, YN, BSTIA:BSTIF)

table(d_diag$Diagnosis, d_diag$YN, useNA = "always")
table(d_diag$Diagnosis, d_diag$YN, useNA = "always")/3259*100

## Gonorrhea diagnosis by PrEP category
gonorrhea <- table(d$BSTIA, d$prep_cat, useNA = "always")
gonorrhea
gonorrhea_perc <- t(t(gonorrhea)/colSums(gonorrhea)*100)
gonorrhea_perc

## Chlamydia diagnosis by PrEP category
chlamydia <- table(d$BSTIB, d$prep_cat, useNA = "always")
chlamydia
chlamydia_perc <- t(t(chlamydia)/colSums(chlamydia)*100)
chlamydia_perc

## Syphilis diagnosis by PrEP category
syphilis <- table(d$BSTIC, d$prep_cat, useNA = "always")
syphilis
syphilis_perc <- t(t(syphilis)/colSums(syphilis)*100)
syphilis_perc

## No diagnosis by PrEP category
noSTI <- table(d$BSTID, d$prep_cat, useNA = "always")
noSTI
noSTI_perc <- t(t(noSTI)/colSums(noSTI)*100)
noSTI_perc

## Prefer not to answer re: diagnosis by PrEP category
PNTA_STI <- table(d$BSTIE, d$prep_cat, useNA = "always")
PNTA_STI
PNTA_STI_perc <- t(t(PNTA_STI)/colSums(PNTA_STI)*100)
PNTA_STI_perc

## Don't know diagnosis by PrEP category
DK_STI <- table(d$BSTIF, d$prep_cat, useNA = "always")
DK_STI
DK_STI_perc <- t(t(DK_STI)/colSums(DK_STI)*100)
DK_STI_perc

# Recent STI testing 
## Number of times tested for an STI within the past 2 years
## NOT at PrEP visits

### Clean data
table(d$STITEST_2YR, useNA = "always")
d$STITEST_2YR <- ifelse(d$STITEST_2YR == 2015, NA, d$STITEST_2YR) 
table(d$STITEST_2YR_PREP, useNA = "always")
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

## Recent STI testing due to symptoms 
### Check logic discrepancies between STI tests and test based on symptoms
### None found 
table(d$STITEST_2YR, d$STITEST_2YR_SYMPT, useNA = "always") 
table(d$STITEST_2YR_PREP, d$STITEST_2YR_SYMPT_PREP, useNA = "always")

### Combine data for NEVER and EVER PrEP users / STI testing due to symptoms
d$STITEST_SYMPT_COMB <- ifelse(is.na(d$STITEST_2YR_SYMPT),
                               d$STITEST_2YR_SYMPT_PREP, 
                               d$STITEST_2YR_SYMPT)

summary(d$STITEST_SYMPT_COMB)
sd(d$STITEST_SYMPT_COMB, na.rm = T)

d %>% 
  group_by(prep_cat) %>%
  summarise(mean = mean(STITEST_SYMPT_COMB, na.rm = T),
            sd = sd(STITEST_SYMPT_COMB, na.rm = T),
            median = median(STITEST_SYMPT_COMB, na.rm = T))

addmargins(table(d$STITEST_SYMPT_COMB, d$survey.year, useNA = "always"))

## Recent STI testing due to partner notification 
### Check logic discrepancies between STI tests and test based on partner notification
### None found 
table(d$STITEST_2YR, d$STITEST_2YR_NOTIF, useNA = "always") 
table(d$STITEST_2YR_PREP, d$STITEST_2YR_NOTIF_PREP, useNA = "always")

### Combine data for NEVER and EVER PrEP users / STI testing due to partner notification
d$STITEST_NOTIF_COMB <- ifelse(is.na(d$STITEST_2YR_NOTIF),
                               d$STITEST_2YR_NOTIF_PREP, 
                               d$STITEST_2YR_NOTIF)

summary(d$STITEST_NOTIF_COMB)
sd(d$STITEST_NOTIF_COMB, na.rm = T)

d %>% 
  group_by(prep_cat) %>%
  summarise(mean = mean(STITEST_NOTIF_COMB, na.rm = T),
            sd = sd(STITEST_NOTIF_COMB, na.rm = T),
            median = median(STITEST_NOTIF_COMB, na.rm = T))

addmargins(table(d$STITEST_NOTIF_COMB, d$survey.year, useNA = "always"))

# Regular testing for STIs (NEVER PrEP users)
table(d$STIREG, d$prep_cat, useNA = "always")
table(d$STIREG, d$prep_cat, useNA = "always")/2450*100

# STI testing frequency if regularly tested for STIs (NEVER PrEP users)
table(d$STITESTFREQ, d$prep_cat, useNA = "always")
d$STITESTFREQ <- ifelse(d$prep_cat == 2, NA, d$STITESTFREQ)
table(d$STITESTFREQ, d$prep_cat, useNA = "always")
table(d$STITESTFREQ, d$prep_cat, useNA = "always")/2450*100

# Frequency of PrEP-related check-ups among ever PrEP users
## Combine data for NEVER and EVER PrEP users / frequency of PrEP checkups
d$PREPCHKFREQ_COMB <- ifelse(is.na(d$PREPCHKFREQ),
                               d$PREPCHKFREQ_CURR, 
                               d$PREPCHKFREQ)

table(d$PREPCHKFREQ_COMB, d$prep_cat, useNA = "always")
table(d$PREPCHKFREQ, d$prep_cat, useNA = "always")/178*100
table(d$PREPCHKFREQ_CURR, d$prep_cat, useNA = "always")/631*100

# Frequency of STI testing after using PrEP compared to before  among current users
table(d$STIFREQ_PREP, d$prep_cat, useNA = "always")
table(d$STIFREQ_PREP, d$prep_cat, useNA = "always")/631*100

# Frequency of throat swab/STI testing at PrEP visits
prep_throat <- table(d$PREP_STITHROATFREQ, d$prep_cat, useNA = "always")
prep_throat
prep_throat_perc <- t(t(prep_throat)/colSums(prep_throat)*100)
prep_throat_perc

# Frequency of rectal swab/STI testing at PrEP visits
prep_rectal <- table(d$PREP_STIRECTFREQ, d$prep_cat, useNA = "always")
prep_rectal
prep_rectal_perc <- t(t(prep_rectal)/colSums(prep_rectal)*100)
prep_rectal_perc

# Frequency of urine sample/STI testing at PrEP visits
prep_urine <- table(d$PREP_STIURETHFREQ, d$prep_cat, useNA = "always")
prep_urine
prep_urine_perc <- t(t(prep_urine)/colSums(prep_urine)*100)
prep_urine_perc

# Frequency of blood sample/STI testing at PrEP visits
prep_blood <- table(d$PREP_BLOODFREQ, d$prep_cat, useNA = "always")
prep_blood
prep_blood_perc <- t(t(prep_blood)/colSums(prep_blood)*100)
prep_blood_perc

# Table 3 -----------------------------------------------------------------
## Descriptive table of HIV testing by PrEP use

# Ever tested for HIV (100% have been tested)
table(d$EVERTEST, d$prep_cat, useNA = "always")

# Were offered an HIV test during a health care visit in past 12 mo
table(d$RECCHIV, useNA = "always")
table(d$RECCHIV, useNA = "always")/3259*100

recHIVtest <- table(d$RECCHIV, d$prep_cat, useNA = "always")
recHIVtest
recHIVtest_perc <- t(t(recHIVtest)/colSums(recHIVtest)*100)
recHIVtest_perc

# Number of times tested for HIV in the past 2 years (AMIS)
summary(d$TEST2YRS)
sd(d$TEST2YRS, na.rm = T)

d %>%
  group_by(prep_cat) %>%
  summarise(mean = mean(TEST2YRS, na.rm = T),
            sd = sd(TEST2YRS, na.rm = T),
            median = median(TEST2YRS, na.rm = T))

# Had an HIV test in past 12 months
table(d$TSTP12M, useNA = "always")
d$TSTP12M <- ifelse(d$TSTP12M == 9, NA, d$TSTP12M)
table(d$TSTP12M, useNA = "always")
table(d$TSTP12M, useNA = "always")/3259*100

HIVtest_p12m <- table(d$TSTP12M, d$prep_cat, useNA = "always")
HIVtest_p12m
HIVtest_p12m_perc <- t(t(HIVtest_p12m)/colSums(HIVtest_p12m)*100)
HIVtest_p12m_perc

# Location of most recent HIV test
table(d$LOCHIV_T, useNA = "always")
d$LOCHIV_T <- ifelse(d$LOCHIV_T == 77 | 
                     d$LOCHIV_T == 99, NA, d$LOCHIV_T)
table(d$LOCHIV_T, useNA = "always")
table(d$LOCHIV_T, useNA = "always")/3259*100

HIVtest_loc <- table(d$LOCHIV_T, d$prep_cat, useNA = "always")
HIVtest_loc
HIVtest_loc_perc <- t(t(HIVtest_loc)/colSums(HIVtest_loc)*100)
HIVtest_loc_perc

# Frequency of HIV testing during PrEP visits
prepHIVtest_freq <- table(d$PREP_HIVTESTFREQ, d$prep_cat, useNA = "always")
prepHIVtest_freq
prepHIVtest_perc <- t(t(prepHIVtest_freq)/colSums(prepHIVtest_freq)*100)
prepHIVtest_perc

# Frequency of HIV tests after PrEP use compared to before
prepHIVtest_after <- table(d$HIVFREQ_PREP, d$prep_cat, useNA = "always")
prepHIVtest_after
prepHIVtest_afterp <- t(t(prepHIVtest_after)/colSums(prepHIVtest_after)*100)
prepHIVtest_afterp

# Table 4 -----------------------------------------------------------------

# What are the predictors of recommended HIV and all-site STI screening in ever PrEP 
# users -- is there variation by demographics, risk behavior, or geogrpahy?

# Create subset of ever PrEP users 
# Excludes 3 PrEP users with missing data on current use

d2 <- filter(d, d$PREP_REVISED == 1)

# Recoding for always/not for HIV testing at PrEP visits
d2$prep.hiv.always <- ifelse(d2$PREP_HIVTESTFREQ %in% 2:4, 0, d2$PREP_HIVTESTFREQ)
table(d2$prep.hiv.always, useNA = "always")

# Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.hiv.always))
addmargins(table(d2$REGCODE, d2$prep.hiv.always))
addmargins(table(d2$deep_south, d2$prep.hiv.always))
addmargins(table(d2$NCHS_2013, d2$prep.hiv.always))
addmargins(table(d2$race.cat, d2$prep.hiv.always))
addmargins(table(d2$age_cat, d2$prep.hiv.always))
addmargins(table(d2$insurance, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$deep_south2, d2$prep.hiv.always))
addmargins(table(d2$PREPCHKFREQ_COMB, d2$prep.hiv.always))

# Logistic regression model (unadjusted)
# E: Census division  O: HIV testing
mod.hiv <- glm(prep.hiv.always ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.hiv)
exp(coef(mod.hiv))
exp(confint(mod.hiv))

a <- as.data.frame(coef(mod.hiv))
a$alpha <- mod.hiv$coefficients[1]
a$plogis <- plogis(a[,1] + a[,2])
a

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: HIV testing
mod1.hiv <- glm(prep.hiv.always ~ as.factor(DIVCODE) + race.cat + age,
                data = d2, family = binomial())
summary(mod1.hiv)
exp(coef(mod1.hiv))
exp(confint(mod1.hiv))

# Logistic regression model (unadjusted)
# E: Census region O: HIV testing
mod2.hiv <- glm(prep.hiv.always ~ as.factor(REGCODE), data = d2, family = binomial())
summary(mod2.hiv)
exp(coef(mod2.hiv))
exp(confint(mod2.hiv))

a2 <- as.data.frame(coef(mod2.hiv))
a2$alpha <- mod2.hiv$coefficients[1]
a2$plogis <- plogis(a2[,1] + a2[,2])
a2

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region & covariates  O: HIV testing
mod3.hiv <- glm(prep.hiv.always ~ as.factor(REGCODE) + race.cat + age,
                data = d2, family = binomial())
summary(mod3.hiv)
exp(coef(mod3.hiv))
exp(confint(mod3.hiv))

# Logistic regression model (unadjusted)
# E: Deep South  O: HIV testing
mod4.hiv <- glm(prep.hiv.always ~ deep_south, data = d2, family = binomial())
summary(mod4.hiv)
exp(coef(mod4.hiv))
exp(confint(mod4.hiv))

plogis(sum(coef(mod4.hiv)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep South  O: HIV testing
mod5.hiv <- glm(prep.hiv.always ~ deep_south + race.cat + age,
                data = d2, family = binomial())
summary(mod5.hiv)
exp(coef(mod5.hiv))
exp(confint(mod5.hiv))

# Logistic regression model (unadjusted)
# E: Urbanicity  O: HIV testing
mod6.hiv <- glm(prep.hiv.always ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod6.hiv)
exp(coef(mod6.hiv))
exp(confint(mod6.hiv))

a6 <- as.data.frame(coef(mod6.hiv))
a6$alpha <- mod6.hiv$coefficients[1]
a6$plogis <- plogis(a6[,1] + a6[,2])
a6

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Urbanicity  O: HIV testing
mod7.hiv <- glm(prep.hiv.always ~ as.factor(NCHS_2013) + race.cat + age,
                data = d2, family = binomial())
summary(mod7.hiv)
exp(coef(mod7.hiv))
exp(confint(mod7.hiv))

# Logistic regression model (unadjusted)
# E: Race  O: HIV testing
mod8.hiv <- glm(prep.hiv.always ~ race.cat, data = d2, family = binomial())
summary(mod8.hiv)
exp(coef(mod8.hiv))
exp(confint(mod8.hiv))

a8 <- as.data.frame(coef(mod8.hiv))
a8$alpha <- mod8.hiv$coefficients[1]
a8$plogis <- plogis(a8[,1] + a8[,2])
a8

# Logistic regression model (unadjusted)
# E: Insurance  O: HIV testing
mod9.hiv <- glm(prep.hiv.always ~ as.factor(insurance), data = d2, family = binomial())
summary(mod9.hiv)
exp(coef(mod9.hiv))
exp(confint(mod9.hiv))

a9 <- as.data.frame(coef(mod9.hiv))
a9$alpha <- mod9.hiv$coefficients[1]
a9$plogis <- plogis(a9[,1] + a9[,2])
a9

# Logistic regression model (unadjusted)
# E: HH Income  O: HIV testing
mod10.hiv <- glm(prep.hiv.always ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod10.hiv)
exp(coef(mod10.hiv))
exp(confint(mod10.hiv))

a10 <- as.data.frame(coef(mod10.hiv))
a10$alpha <- mod10.hiv$coefficients[1]
a10$plogis <- plogis(a10[,1] + a10[,2])
a10

# Logistic regression model (unadjusted)
# E: HL Education  O: HIV testing
mod11.hiv <- glm(prep.hiv.always ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod11.hiv)
exp(coef(mod11.hiv))
exp(confint(mod11.hiv))

a11 <- as.data.frame(coef(mod11.hiv))
a11$alpha <- mod11.hiv$coefficients[1]
a11$plogis <- plogis(a11[,1] + a11[,2])
a11

# Logistic regression model (unadjusted)
# E: Age category  O: HIV testing
mod12.hiv <- glm(prep.hiv.always ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod12.hiv)
exp(coef(mod12.hiv))
exp(confint(mod12.hiv))

a12 <- as.data.frame(coef(mod12.hiv))
a12$alpha <- mod12.hiv$coefficients[1]
a12$plogis <- plogis(a12[,1] + a12[,2])
a12

# Logistic regression model (unadjusted)
# E: Deep South 2  O: HIV testing
mod13.hiv <- glm(prep.hiv.always ~ deep_south2, data = d2, family = binomial())
summary(mod13.hiv)
exp(coef(mod13.hiv))
exp(confint(mod13.hiv))

plogis(sum(coef(mod13.hiv)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep South  O: HIV testing
mod14.hiv <- glm(prep.hiv.always ~ deep_south2 + race.cat + age,
                data = d2, family = binomial())
summary(mod14.hiv)
exp(coef(mod14.hiv))
exp(confint(mod14.hiv))

## Q17b: How Often Tested for STIs in Throat
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STITHROATFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never pharyngeal STI testings
d2$prep.throat.sometimes <- ifelse(d2$PREP_STITHROATFREQ %in% 1:2, 1, 
                                   ifelse(d2$PREP_STITHROATFREQ %in% 3:4, 0, NA))
table(d2$prep.throat.sometimes, d2$PREP_STITHROATFREQ)
addmargins(table(d2$prep.throat.sometimes))

## Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.throat.sometimes))
addmargins(table(d2$REGCODE, d2$prep.throat.sometimes))
addmargins(table(d2$deep_south, d2$prep.throat.sometimes))
addmargins(table(d2$NCHS_2013, d2$prep.throat.sometimes))
addmargins(table(d2$race.cat, d2$prep.throat.sometimes))
addmargins(table(d2$age_cat, d2$prep.throat.sometimes))
addmargins(table(d2$insurance, d2$prep.throat.sometimes, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.throat.sometimes, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.throat.sometimes, useNA = "always"))
addmargins(table(d2$deep_south2, d2$prep.throat.sometimes))
addmargins(table(d2$PREPCHKFREQ_COMB, d2$prep.throat.sometimes))
addmargins(table(d2$anyROI, d2$prep.throat.sometimes))

# Logistic regression model (unadjusted)
# E: Census division  O: Pharyngeal STI testing
mod.throat <- glm(prep.throat.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.throat)
exp(coef(mod.throat))
exp(confint(mod.throat))

b <- as.data.frame(coef(mod.throat))
b$alpha <- mod.throat$coefficients[1]
b$plogis <- plogis(b[,1] + b[,2])
b

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: Pharyngeal STI testing
mod1.throat <- glm(prep.throat.always ~ as.factor(DIVCODE) + age + race.cat,
                 data = d2, family = binomial())
summary(mod1.throat)
exp(coef(mod1.throat))
exp(confint(mod1.throat))

# Logistic regression model (unadjusted)
# E: Census region  O: Pharyngeal STI testing
mod2.throat <- glm(prep.throat.sometimes ~ as.factor(REGCODE), data = d2, family = binomial())
summary(mod2.throat)
exp(coef(mod2.throat))
exp(confint(mod2.throat))

b2 <- as.data.frame(coef(mod2.throat))
b2$alpha <- mod2.throat$coefficients[1]
b2$plogis <- plogis(b2[,1] + b2[,2])
b2

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region & covariates  O: Pharyngeal STI testing
mod3.throat <- glm(prep.throat.sometimes ~ as.factor(REGCODE) + age + race.cat,
                   data = d2, family = binomial())
summary(mod3.throat)
exp(coef(mod3.throat))
exp(confint(mod3.throat))

# Logistic regression model (unadjusted)
# E: Deep south O: Pharyngeal STI testing
mod4.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod4.throat)
exp(coef(mod4.throat))
exp(confint(mod4.throat))

plogis(sum(coef(mod4.throat)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Pharyngeal STI testing
mod5.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south) + age + race.cat,
                   data = d2, family = binomial())
summary(mod5.throat)
exp(coef(mod5.throat))
exp(confint(mod5.throat))

# Logistic regression model (adjusted #2 / control for race/eth, age, exposure at site as confounder)
# E: Deep south  O: Pharyngeal STI testing
mod6.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south) + age + race.cat + anyROI,
                   data = d2, family = binomial())
summary(mod6.throat)
exp(coef(mod6.throat))
exp(confint(mod6.throat))

# Logistic regression model (adjusted #3 / control for race/eth, age, exposure at site as effect modifier)
# E: Deep south  O: Pharyngeal STI testing
mod7.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south) + age + race.cat + anyROI + as.factor(deep_south)*anyROI,
                   data = d2, family = binomial())
summary(mod7.throat)

ROI.em <- rbind("OR - deep_south - anyROI = 0" = c(0, 1, 0, 0, 0, 0, 0, 0),
                "OR - deep_south - anyROI = 1" = c(0, 1, 0, 0, 0, 0, 0, 1))
mod7.contrasts <- glht(mod7.throat, linfct = ROI.em)
summary(mod7.contrasts)
cbind(exp(coef(mod7.contrasts)), exp(confint.default(mod7.contrasts)))

# Logistic regression model (unadjusted)
# E: Deep south #2 O: Pharyngeal STI testing
mod8.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south2), data = d2, family = binomial())
summary(mod8.throat)
exp(coef(mod8.throat))
exp(confint(mod8.throat))

plogis(sum(coef(mod8.throat)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south #2  O: Pharyngeal STI testing
mod9.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south2) + age + race.cat,
                    data = d2, family = binomial())
summary(mod9.throat)
exp(coef(mod9.throat))
exp(confint(mod9.throat))

# Logistic regression model (adjusted #2 / control for race/eth, age, exposure at site as confounder)
# E: Deep south #2  O: Pharyngeal STI testing
mod10.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south2) + age + race.cat + anyROI,
                   data = d2, family = binomial())
summary(mod10.throat)
exp(coef(mod10.throat))
exp(confint(mod10.throat))

# Logistic regression model (adjusted #3 / control for race/eth, age, exposure at site as effect modifier)
# E: Deep south #2  O: Pharyngeal STI testing
mod11.throat <- glm(prep.throat.sometimes ~ as.factor(deep_south2) + age + race.cat + anyROI + as.factor(deep_south2)*anyROI,
                   data = d2, family = binomial())
summary(mod11.throat)

mod11.contrasts <- glht(mod11.throat, linfct = ROI.em)
summary(mod11.contrasts)
cbind(exp(coef(mod11.contrasts)), exp(confint.default(mod11.contrasts)))

# Logistic regression model (unadjusted)
# E: Urbanicity O: Pharyngeal STI testing
mod12.throat <- glm(prep.throat.sometimes ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod12.throat)
exp(coef(mod12.throat))
exp(confint(mod12.throat))

b12 <- as.data.frame(coef(mod12.throat))
b12$alpha <- mod12.throat$coefficients[1]
b12$plogis <- plogis(b12[,1] + b12[,2])
b12

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Pharyngeal STI testing
mod13.throat <- glm(prep.throat.sometimes ~ as.factor(NCHS_2013) + age + race.cat,
                   data = d2, family = binomial())
summary(mod13.throat)
exp(coef(mod13.throat))
exp(confint(mod13.throat))

# Logistic regression model (unadjusted)
# E: Race O: Pharyngeal STI testing
mod14.throat <- glm(prep.throat.sometimes ~ race.cat, data = d2, family = binomial())
summary(mod14.throat)
exp(coef(mod14.throat))
exp(confint(mod14.throat))

b14 <- as.data.frame(coef(mod14.throat))
b14$alpha <- mod14.throat$coefficients[1]
b14$plogis <- plogis(b14[,1] + b14[,2])
b14

# Logistic regression model (unadjusted)
# E: Age category O: Pharyngeal STI testing
mod15.throat <- glm(prep.throat.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod15.throat)
exp(coef(mod15.throat))
exp(confint(mod15.throat))

b15 <- as.data.frame(coef(mod15.throat))
b15$alpha <- mod15.throat$coefficients[1]
b15$plogis <- plogis(b15[,1] + b15[,2])
b15

# Logistic regression model (unadjusted)
# E: Insurance O: Pharyngeal STI testing
mod16.throat <- glm(prep.throat.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod16.throat)
exp(coef(mod16.throat))
exp(confint(mod16.throat))

b16 <- as.data.frame(coef(mod16.throat))
b16$alpha <- mod16.throat$coefficients[1]
b16$plogis <- plogis(b16[,1] + b16[,2])
b16

# Logistic regression model (unadjusted)
# E: HH income O: Pharyngeal STI testing
mod17.throat <- glm(prep.throat.sometimes ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod17.throat)
exp(coef(mod17.throat))
exp(confint(mod17.throat))

b17 <- as.data.frame(coef(mod17.throat))
b17$alpha <- mod17.throat$coefficients[1]
b17$plogis <- plogis(b17[,1] + b17[,2])
b17

# Logistic regression model (unadjusted)
# E: Education O: Pharyngeal STI testing
mod18.throat <- glm(prep.throat.sometimes ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod18.throat)
exp(coef(mod18.throat))
exp(confint(mod18.throat))

b18 <- as.data.frame(coef(mod18.throat))
b18$alpha <- mod18.throat$coefficients[1]
b18$plogis <- plogis(b18[,1] + b18[,2])
b18

# Logistic regression model (unadjusted)
# E: Exposure at pharyngeal site O: Pharyngeal STI testing
mod19.throat <- glm(prep.throat.sometimes ~ as.factor(anyROI),
                    data = d2, family = binomial())
summary(mod19.throat)
exp(coef(mod19.throat))
exp(confint(mod19.throat))

plogis(sum(coef(mod19.throat)))

## Q17c: How Often Tested for STIs Rectal
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STIRECTFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never rectal STI testings
d2$prep.rect.sometimes <- ifelse(d2$PREP_STIRECTFREQ %in% 1:2, 1, 
                                 ifelse(d2$PREP_STIRECTFREQ %in% 3:4, 0, NA))
table(d2$prep.rect.sometimes, d2$PREP_STIRECTFREQ)
addmargins(table(d2$prep.rect.sometimes))

## Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.rect.sometimes))
addmargins(table(d2$REGCODE, d2$prep.rect.sometimes))
addmargins(table(d2$deep_south, d2$prep.rect.sometimes))
addmargins(table(d2$NCHS_2013, d2$prep.rect.sometimes))
addmargins(table(d2$race.cat, d2$prep.rect.sometimes))
addmargins(table(d2$age_cat, d2$prep.rect.sometimes))
addmargins(table(d2$insurance, d2$prep.rect.sometimes, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.rect.sometimes, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.rect.sometimes, useNA = "always"))
addmargins(table(d2$deep_south2, d2$prep.rect.sometimes))
addmargins(table(d2$PREPCHKFREQ_COMB, d2$prep.rect.sometimes))
addmargins(table(d2$anyRAI, d2$prep.rect.sometimes))

# Logistic regression model (unadjusted)
# E: Census division  O: Rectal STI testing
mod.rectal <- glm(prep.rect.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.rectal)
exp(coef(mod.rectal))
exp(confint(mod.rectal))

c <- as.data.frame(coef(mod.rectal))
c$alpha <- mod.rectal$coefficients[1]
c$plogis <- plogis(c[,1] + c[,2])
c

# Logistic regression model (adjusted #1 / control for race/eth, age, income, insurance, education))
# E: Census division  O: Rectal STI testing
mod1.rectal <- glm(prep.rect.sometimes ~ as.factor(DIVCODE) + age + race.cat,
                   data = d2, family = binomial())
summary(mod1.rectal)
exp(coef(mod1.rectal))
exp(confint(mod1.rectal))

# Logistic regression model (unadjusted)
# E: Census region O: Rectal STI testing
mod2.rectal <- glm(prep.rect.sometimes ~ as.factor(REGCODE), data = d2, family = binomial())
summary(mod2.rectal)
exp(coef(mod2.rectal))
exp(confint(mod2.rectal))

c2 <- as.data.frame(coef(mod2.rectal))
c2$alpha <- mod2.rectal$coefficients[1]
c2$plogis <- plogis(c2[,1] + c2[,2])
c2

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region  O: Rectal STI testing
mod3.rectal <- glm(prep.rect.sometimes ~ as.factor(REGCODE) + age + race.cat,
                   data = d2, family = binomial())
summary(mod3.rectal)
exp(coef(mod3.rectal))
exp(confint(mod3.rectal))

# Logistic regression model (unadjusted)
# E: Deep south O: Rectal STI testing
mod4.rectal <- glm(prep.rect.sometimes ~ deep_south, data = d2, family = binomial())
summary(mod4.rectal)
exp(coef(mod4.rectal))
exp(confint(mod4.rectal))

plogis(sum(coef(mod4.rectal)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Rectal STI testing
mod5.rectal <- glm(prep.rect.sometimes ~ deep_south + age + race.cat,
                   data = d2, family = binomial())
summary(mod5.rectal)
exp(coef(mod5.rectal))
exp(confint(mod5.rectal))

# Logistic regression model (adjusted #2 / control for race/eth, age, and exposure at site as confounder)
# E: Deep south  O: Rectal STI testing
mod6.rectal <- glm(prep.rect.sometimes ~ deep_south + age + race.cat + anyRAI,
                   data = d2, family = binomial())
summary(mod6.rectal)
exp(coef(mod6.rectal))
exp(confint(mod6.rectal))

# Logistic regression model (adjusted #3 / control for race/eth, age, and exposure at site as effect modifier)
# E: Deep south  O: Rectal STI testing
mod7.rectal <- glm(prep.rect.sometimes ~ as.factor(deep_south) + age + race.cat + anyRAI + as.factor(deep_south)*anyRAI,
                   data = d2, family = binomial())
summary(mod7.rectal)

RAI.em <- rbind("OR - deep_south - anyRAI = 0" = c(0, 1, 0, 0, 0, 0, 0, 0),
                "OR - deep_south - anyRAI = 1" = c(0, 1, 0, 0, 0, 0, 0, 1))
mod7.rect.contrasts <- glht(mod7.rectal, linfct = RAI.em)
summary(mod7.rect.contrasts)
cbind(exp(coef(mod7.rect.contrasts)), exp(confint.default(mod7.rect.contrasts)))

# Logistic regression model (unadjusted)
# E: Deep south #2 O: Rectal STI testing
mod8.rectal <- glm(prep.rect.sometimes ~ deep_south2, data = d2, family = binomial())
summary(mod8.rectal)
exp(coef(mod8.rectal))
exp(confint(mod8.rectal))

plogis(sum(coef(mod8.rectal)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Rectal STI testing
mod9.rectal <- glm(prep.rect.sometimes ~ deep_south2 + age + race.cat,
                   data = d2, family = binomial())
summary(mod9.rectal)
exp(coef(mod9.rectal))
exp(confint(mod9.rectal))

# Logistic regression model (adjusted #2 / control for race/eth, age, and exposure at site as confounder)
# E: Deep south #2  O: Rectal STI testing
mod10.rectal <- glm(prep.rect.sometimes ~ deep_south2 + age + race.cat + anyRAI,
                   data = d2, family = binomial())
summary(mod10.rectal)
exp(coef(mod10.rectal))
exp(confint(mod10.rectal))

# Logistic regression model (adjusted #3 / control for race/eth, age, and exposure at site as effect modifier)
# E: Deep south #2  O: Rectal STI testing
mod11.rectal <- glm(prep.rect.sometimes ~ as.factor(deep_south2) + age + race.cat + anyRAI + as.factor(deep_south2)*anyRAI,
                   data = d2, family = binomial())
summary(mod11.rectal)

mod11.rect.contrasts <- glht(mod11.rectal, linfct = RAI.em)
summary(mod11.rect.contrasts)
cbind(exp(coef(mod11.rect.contrasts)), exp(confint.default(mod11.rect.contrasts)))

# Logistic regression model (unadjusted)
# E: Urbanicity O: Rectal STI testing
mod12.rectal <- glm(prep.rect.sometimes ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod12.rectal)
exp(coef(mod12.rectal))
exp(confint(mod12.rectal))

c12 <- as.data.frame(coef(mod12.rectal))
c12$alpha <- mod12.rectal$coefficients[1]
c12$plogis <- plogis(c12[,1] + c12[,2])
c12

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Urbanicity  O: Rectal STI testing
mod13.rectal <- glm(prep.rect.sometimes ~ as.factor(NCHS_2013) + age + race.cat,
                   data = d2, family = binomial())
summary(mod13.rectal)
exp(coef(mod13.rectal))
exp(confint(mod13.rectal))

# Logistic regression model (unadjusted)
# E: Race/Ethnicity O: Rectal STI testing
mod14.rectal <- glm(prep.rect.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod14.rectal)
exp(coef(mod14.rectal))
exp(confint(mod14.rectal))

c14 <- as.data.frame(coef(mod14.rectal))
c14$alpha <- mod14.rectal$coefficients[1]
c14$plogis <- plogis(c14[,1] + c14[,2])
c14

# Logistic regression model (unadjusted)
# E:Age category  O: Rectal STI testing
mod15.rectal <- glm(prep.rect.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod15.rectal)
exp(coef(mod15.rectal))
exp(confint(mod15.rectal))

c15 <- as.data.frame(coef(mod15.rectal))
c15$alpha <- mod15.rectal$coefficients[1]
c15$plogis <- plogis(c15[,1] + c15[,2])
c15

# Logistic regression model (unadjusted)
# E: Insurance O: Rectal STI testing
mod16.rectal <- glm(prep.rect.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod16.rectal)
exp(coef(mod16.rectal))
exp(confint(mod16.rectal))

c16 <- as.data.frame(coef(mod16.rectal))
c16$alpha <- mod16.rectal$coefficients[1]
c16$plogis <- plogis(c16[,1] + c16[,2])
c16

# Logistic regression model (unadjusted)
# E: Houshold income  O: Rectal STI testing
mod17.rectal <- glm(prep.rect.sometimes ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod17.rectal)
exp(coef(mod17.rectal))
exp(confint(mod17.rectal))

c17 <- as.data.frame(coef(mod17.rectal))
c17$alpha <- mod17.rectal$coefficients[1]
c17$plogis <- plogis(c17[,1] + c17[,2])
c17

# Logistic regression model (unadjusted)
# E:Education  O: Rectal STI testing
mod18.rectal <- glm(prep.rect.sometimes ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod18.rectal)
exp(coef(mod18.rectal))
exp(confint(mod18.rectal))

c18 <- as.data.frame(coef(mod18.rectal))
c18$alpha <- mod18.rectal$coefficients[1]
c18$plogis <- plogis(c18[,1] + c18[,2])
c18

# Logistic regression model (unadjusted)
# E: Exposure at site  O: Rectal STI testing
mod19.rectal <- glm(prep.rect.sometimes ~ anyRAI, data = d2, family = binomial())
summary(mod19.rectal)
exp(coef(mod19.rectal))
exp(confint(mod19.rectal))

plogis(sum(coef(mod19.rectal)))

## Q17c: How Often Tested for STIs Urethral
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STIURETHFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never urethral STI testings
d2$prep.ureth.sometimes <- ifelse(d2$PREP_STIURETHFREQ %in% 1:2, 1, 
                                 ifelse(d2$PREP_STIURETHFREQ %in% 3:4, 0, NA))
table(d2$prep.ureth.sometimes, d2$PREP_STIURETHFREQ)
addmargins(table(d2$prep.ureth.sometimes))

# Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.ureth.sometimes))
addmargins(table(d2$REGCODE, d2$prep.ureth.sometimes))
addmargins(table(d2$deep_south, d2$prep.ureth.sometimes))
addmargins(table(d2$deep_south2, d2$prep.ureth.sometimes))
addmargins(table(d2$NCHS_2013, d2$prep.ureth.sometimes))
addmargins(table(d2$race.cat, d2$prep.ureth.sometimes))
addmargins(table(d2$age_cat, d2$prep.ureth.sometimes))
addmargins(table(d2$insurance, d2$prep.ureth.sometimes, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.ureth.sometimes, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.ureth.sometimes, useNA = "always"))
addmargins(table(d2$PREPCHKFREQ_COMB, d2$prep.ureth.sometimes))

# Create indicator for exposure at site
d2$exp.ureth <- ifelse(d2$anyIAI == 1, 1,
                       ifelse(d2$anyIOI ==1, 1, 0))
addmargins(table(d2$exp.ureth, d2$prep.ureth.sometimes))

# Logistic regression model (unadjusted)
# E: Census division O: Urethral STI testing
mod.ureth <- glm(prep.ureth.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.ureth)
exp(coef(mod.ureth))
exp(confint(mod.ureth))

x <- as.data.frame(coef(mod.ureth))
x$alpha <- mod.ureth$coefficients[1]
x$plogis <- plogis(x[,1] + x[,2])
x

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: Urethral STI testing
mod1.ureth <- glm(prep.ureth.sometimes ~ as.factor(DIVCODE) + age + race.cat,
                   data = d2, family = binomial())
summary(mod1.ureth)
exp(coef(mod1.ureth))
exp(confint(mod1.ureth))

# Logistic regression model (unadjusted)
# E: Census region O: Urethral STI testing
mod2.ureth <- glm(prep.ureth.sometimes ~ as.factor(REGCODE), data = d2, family = binomial())
summary(mod2.ureth)
exp(coef(mod2.ureth))
exp(confint(mod2.ureth))

x2 <- as.data.frame(coef(mod2.ureth))
x2$alpha <- mod2.ureth$coefficients[1]
x2$plogis <- plogis(x2[,1] + x2[,2])
x2

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region  O: Urethral STI testing
mod3.ureth <- glm(prep.ureth.sometimes ~ as.factor(REGCODE) + age + race.cat,
                  data = d2, family = binomial())
summary(mod3.ureth)
exp(coef(mod3.ureth))
exp(confint(mod3.ureth))

# Logistic regression model (unadjusted)
# E: Deep south O: Urethral STI testing
mod4.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod4.ureth)
exp(coef(mod4.ureth))
exp(confint(mod4.ureth))

plogis(sum(coef(mod4.ureth)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Urethral STI testing
mod5.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south) + age + race.cat,
                  data = d2, family = binomial())
summary(mod5.ureth)
exp(coef(mod5.ureth))
exp(confint(mod5.ureth))

# Logistic regression model (adjusted #2 / control for race/eth, age, and exposure at site as confounder)
# E: Deep south  O: Urethral STI testing
mod6.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south) + age + race.cat + exp.ureth,
                  data = d2, family = binomial())
summary(mod6.ureth)
exp(coef(mod6.ureth))
exp(confint(mod6.ureth))

# Logistic regression model (adjusted #3 / control for race/eth, age, and exposure at site as effect modifier)
# E: Deep south  O: Urethral STI testing
mod7.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south) + age + race.cat + exp.ureth + as.factor(deep_south)*exp.ureth,
                   data = d2, family = binomial())
summary(mod7.ureth)

exp.ureth.em <- rbind("OR - deep_south - exp.ureth = 0" = c(0, 1, 0, 0, 0, 0, 0, 0),
                      "OR - deep_south - exp.ureth = 1" = c(0, 1, 0, 0, 0, 0, 0, 1))
mod7.ureth.contrasts <- glht(mod7.ureth, linfct = exp.ureth.em)
summary(mod7.ureth.contrasts)
cbind(exp(coef(mod7.ureth.contrasts)), exp(confint.default(mod7.ureth.contrasts)))

# Logistic regression model (unadjusted)
# E: Deep south #2 O: Urethral STI testing
mod8.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south2), data = d2, family = binomial())
summary(mod8.ureth)
exp(coef(mod8.ureth))
exp(confint(mod8.ureth))

plogis(sum(coef(mod8.ureth)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south #2  O: Urethral STI testing
mod9.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south2) + age + race.cat,
                   data = d2, family = binomial())
summary(mod9.ureth)
exp(coef(mod9.ureth))
exp(confint(mod9.ureth))

# Logistic regression model (adjusted #2 / control for race/eth, age, and exposure at site as confounder)
# E: Deep south #2  O: Urethral STI testing
mod10.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south2) + age + race.cat + exp.ureth,
                  data = d2, family = binomial())
summary(mod10.ureth)
exp(coef(mod10.ureth))
exp(confint(mod10.ureth))

# Logistic regression model (adjusted #2 / control for race/eth, age, and exposure at site as effect modifier)
# E: Deep south #2  O: Urethral STI testing
mod11.ureth <- glm(prep.ureth.sometimes ~ as.factor(deep_south2) + age + race.cat + exp.ureth + as.factor(deep_south2)*exp.ureth,
                   data = d2, family = binomial())
summary(mod11.ureth)

mod11.ureth.contrasts <- glht(mod11.ureth, linfct = exp.ureth.em)
summary(mod11.ureth.contrasts)
cbind(exp(coef(mod11.ureth.contrasts)), exp(confint.default(mod11.ureth.contrasts)))

# Logistic regression model (unadjusted)
# E: Urbanicity O: Urethral STI testing
mod12.ureth <- glm(prep.ureth.sometimes ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod12.ureth)
exp(coef(mod12.ureth))
exp(confint(mod12.ureth))

x12 <- as.data.frame(coef(mod12.ureth))
x12$alpha <- mod12.ureth$coefficients[1]
x12$plogis <- plogis(x12[,1] + x12[,2])
x12

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Urbanicity  O: Urethral STI testing
mod13.ureth <- glm(prep.ureth.sometimes ~ as.factor(NCHS_2013) + age + race.cat,
                  data = d2, family = binomial())
summary(mod13.ureth)
exp(coef(mod13.ureth))
exp(confint(mod13.ureth))

# Logistic regression model (unadjusted)
# E: Race/ethnicity O: Urethral STI testing
mod14.ureth <- glm(prep.ureth.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod14.ureth)
exp(coef(mod14.ureth))
exp(confint(mod14.ureth))

x14 <- as.data.frame(coef(mod14.ureth))
x14$alpha <- mod14.ureth$coefficients[1]
x14$plogis <- plogis(x14[,1] + x14[,2])
x14

# Logistic regression model (unadjusted)
# E: Age category O: Urethral STI testing
mod15.ureth <- glm(prep.ureth.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod15.ureth)
exp(coef(mod15.ureth))
exp(confint(mod15.ureth))

x15 <- as.data.frame(coef(mod15.ureth))
x15$alpha <- mod15.ureth$coefficients[1]
x15$plogis <- plogis(x15[,1] + x15[,2])
x15

# Logistic regression model (unadjusted)
# E: Insurance O: Urethral STI testing
mod16.ureth <- glm(prep.ureth.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod16.ureth)
exp(coef(mod16.ureth))
exp(confint(mod16.ureth))

x16 <- as.data.frame(coef(mod16.ureth))
x16$alpha <- mod16.ureth$coefficients[1]
x16$plogis <- plogis(x16[,1] + x16[,2])
x16

# Logistic regression model (unadjusted)
# E: Annual HH Income O: Urethral STI testing
mod17.ureth <- glm(prep.ureth.sometimes ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod17.ureth)
exp(coef(mod17.ureth))
exp(confint(mod17.ureth))

x17 <- as.data.frame(coef(mod17.ureth))
x17$alpha <- mod17.ureth$coefficients[1]
x17$plogis <- plogis(x17[,1] + x17[,2])
x17

# Logistic regression model (unadjusted)
# E: Highest level of education O: Urethral STI testing
mod18.ureth <- glm(prep.ureth.sometimes ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod18.ureth)
exp(coef(mod18.ureth))
exp(confint(mod18.ureth))

x18 <- as.data.frame(coef(mod18.ureth))
x18$alpha <- mod18.ureth$coefficients[1]
x18$plogis <- plogis(x18[,1] + x18[,2])
x18

# Logistic regression model (unadjusted)
# E: Exposure at site O: Urethral STI testing
mod19.ureth <- glm(prep.ureth.sometimes ~ as.factor(exp.ureth), data = d2, family = binomial())
summary(mod19.ureth)
exp(coef(mod19.ureth))
exp(confint(mod19.ureth))

plogis(sum(coef(mod19.ureth)))

# Logistic regression model (unadjusted)
# E: PrEP visit frequency O: Urethral STI testing
mod16.ureth <- glm(prep.ureth.always ~ as.factor(PREPCHKFREQ_COMB), data = d2, family = binomial())
summary(mod16.ureth)
exp(coef(mod16.ureth))
exp(confint(mod16.ureth))

## Q17d: How Often Tested for STIs Blood
##       1:4: always, sometimes, rarely, never
table(d2$PREP_BLOODFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never blood STI testings
d2$prep.blood.sometimes <- ifelse(d2$PREP_BLOODFREQ %in% 1:2, 1, 
                                  ifelse(d2$PREP_BLOODFREQ %in% 3:4, 0, NA))
table(d2$prep.blood.sometimes, d2$PREP_BLOODFREQ)
addmargins(table(d2$prep.blood.sometimes))

# Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.blood.sometimes))
addmargins(table(d2$REGCODE, d2$prep.blood.sometimes))
addmargins(table(d2$deep_south, d2$prep.blood.sometimes))
addmargins(table(d2$deep_south2, d2$prep.blood.sometimes))
addmargins(table(d2$NCHS_2013, d2$prep.blood.sometimes))
addmargins(table(d2$race.cat, d2$prep.blood.sometimes))
addmargins(table(d2$age_cat, d2$prep.blood.sometimes))
addmargins(table(d2$insurance, d2$prep.blood.sometimes, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.blood.sometimes, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.blood.sometimes, useNA = "always"))
addmargins(table(d2$PREPCHKFREQ_COMB, d2$prep.blood.sometimes))

# Logistic regression model (unadjusted)
# E: Census division  O: Blood STI testing
mod.blood <- glm(prep.blood.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.blood)
exp(coef(mod.blood))
exp(confint(mod.blood))

y <- as.data.frame(coef(mod.blood))
y$alpha <- mod.blood$coefficients[1]
y$plogis <- plogis(y[,1] + y[,2])
y

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: Blood STI testing
mod1.blood <- glm(prep.blood.sometimes ~ as.factor(DIVCODE) + age + race.cat,
                  data = d2, family = binomial())
summary(mod1.blood)
exp(coef(mod1.blood))
exp(confint(mod1.blood))

# Logistic regression model (unadjusted)
# E: Census region  O: Blood STI testing
mod2.blood <- glm(prep.blood.sometimes ~ as.factor(REGCODE), data = d2, family = binomial())
summary(mod2.blood)
exp(coef(mod2.blood))
exp(confint(mod2.blood))

y2 <- as.data.frame(coef(mod2.blood))
y2$alpha <- mod2.blood$coefficients[1]
y2$plogis <- plogis(y2[,1] + y2[,2])
y2

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region  O: Blood STI testing
mod3.blood <- glm(prep.blood.sometimes ~ as.factor(REGCODE) + age + race.cat,
                  data = d2, family = binomial())
summary(mod3.blood)
exp(coef(mod3.blood))
exp(confint(mod3.blood))

# Logistic regression model (unadjusted)
# E: Deep south  O: Blood STI testing
mod4.blood <- glm(prep.blood.sometimes ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod4.blood)
exp(coef(mod4.blood))
exp(confint(mod4.blood))

plogis(sum(coef(mod4.blood)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Blood STI testing
mod5.blood <- glm(prep.blood.sometimes ~ as.factor(deep_south) + age + race.cat,
                  data = d2, family = binomial())
summary(mod5.blood)
exp(coef(mod5.blood))
exp(confint(mod5.blood))

# Logistic regression model (unadjusted)
# E: Deep south #2  O: Blood STI testing
mod6.blood <- glm(prep.blood.sometimes ~ as.factor(deep_south2),
                   data = d2, family = binomial())
summary(mod6.blood)
exp(coef(mod6.blood))
exp(confint(mod6.blood))

plogis(sum(coef(mod6.blood)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south #2  O: Blood STI testing
mod7.blood <- glm(prep.blood.sometimes ~ as.factor(deep_south2) + age + race.cat,
                   data = d2, family = binomial())
summary(mod7.blood)
exp(coef(mod7.blood))
exp(confint(mod7.blood))

# Logistic regression model (unadjusted)
# E: Urbanicity  O: Blood STI testing
mod8.blood <- glm(prep.blood.sometimes ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod8.blood)
exp(coef(mod8.blood))
exp(confint(mod8.blood))

y8 <- as.data.frame(coef(mod8.blood))
y8$alpha <- mod8.blood$coefficients[1]
y8$plogis <- plogis(y8[,1] + y8[,2])
y8

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Urbanicity   O: Blood STI testing
mod9.blood <- glm(prep.blood.sometimes ~ as.factor(NCHS_2013) + age + race.cat,
                  data = d2, family = binomial())
summary(mod9.blood)
exp(coef(mod9.blood))
exp(confint(mod9.blood))

# Logistic regression model (unadjusted)
# E: Race/Ethnicity  O: Blood STI testing
mod10.blood <- glm(prep.blood.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod10.blood)
exp(coef(mod10.blood))
exp(confint(mod10.blood))

y10 <- as.data.frame(coef(mod10.blood))
y10$alpha <- mod10.blood$coefficients[1]
y10$plogis <- plogis(y10[,1] + y10[,2])
y10

# Logistic regression model (unadjusted)
# E: Age category  O: Blood STI testing
mod11.blood <- glm(prep.blood.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod11.blood)
exp(coef(mod11.blood))
exp(confint(mod11.blood))

y11 <- as.data.frame(coef(mod11.blood))
y11$alpha <- mod11.blood$coefficients[1]
y11$plogis <- plogis(y11[,1] + y11[,2])
y11

# Logistic regression model (unadjusted)
# E: Insurance  O: Blood STI testing
mod12.blood <- glm(prep.blood.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod12.blood)
exp(coef(mod12.blood))
exp(confint(mod12.blood))

y12 <- as.data.frame(coef(mod12.blood))
y12$alpha <- mod12.blood$coefficients[1]
y12$plogis <- plogis(y12[,1] + y12[,2])
y12

# Logistic regression model (unadjusted)
# E: Annual household income  O: Blood STI testing
mod13.blood <- glm(prep.blood.sometimes ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod13.blood)
exp(coef(mod13.blood))
exp(confint(mod13.blood))

y13 <- as.data.frame(coef(mod13.blood))
y13$alpha <- mod13.blood$coefficients[1]
y13$plogis <- plogis(y13[,1] + y13[,2])
y13

# Logistic regression model (unadjusted)
# E: Education  O: Blood STI testing
mod14.blood <- glm(prep.blood.sometimes ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod14.blood)
exp(coef(mod14.blood))
exp(confint(mod14.blood))

y14 <- as.data.frame(coef(mod14.blood))
y14$alpha <- mod14.blood$coefficients[1]
y14$plogis <- plogis(y14[,1] + y14[,2])
y14

## Q17d: How Often Tested for STIs: Blood, Urethra, Throat, Urine
##       1:4: always, sometimes, rarely, never

# Coding to always/sometimes vs. rarely/never all-site STI testings
d2$prep.allsite.sometimes <- ifelse(d2$PREP_BLOODFREQ %in% 1:2 &
                                      d2$PREP_STIRECTFREQ %in% 1:2 &
                                      d2$PREP_STIURETHFREQ %in% 1:2 &
                                      d2$PREP_STITHROATFREQ %in% 1:2, 1, 
                                    ifelse(d2$PREP_BLOODFREQ %in% 3:4 |
                                             d2$PREP_STIRECTFREQ %in% 3:4 |
                                             d2$PREP_STIURETHFREQ %in% 3:4 |
                                             d2$PREP_STITHROATFREQ %in% 3:4, 0, NA))

addmargins(table(d2$prep.allsite.sometimes))

## Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.allsite.sometimes))
addmargins(table(d2$REGCODE, d2$prep.allsite.sometimes))
addmargins(table(d2$deep_south, d2$prep.allsite.sometimes))
addmargins(table(d2$deep_south2, d2$prep.allsite.sometimes))
addmargins(table(d2$NCHS_2013, d2$prep.allsite.sometimes))
addmargins(table(d2$race.cat, d2$prep.allsite.sometimes))
addmargins(table(d2$age_cat, d2$prep.allsite.sometimes))
addmargins(table(d2$insurance, d2$prep.allsite.sometimes, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.allsite.sometimes, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.allsite.sometimes, useNA = "always"))
addmargins(table(d2$PREPCHKFREQ_COMB, d2$prep.allsite.sometimes))

# Create variable for exposure at all sites to all-site testing

d2$exp.allsite <- ifelse(d2$exp.ureth == 1 &
                           d2$anyRAI == 1 &
                           d2$anyROI == 1, 1, 0)
addmargins(table(d2$exp.allsite, useNA = "always"))

addmargins(table(d2$exp.allsite, d2$prep.allsite.sometimes, useNA = 'always'))

# Logistic regression model (unadjusted)
# E: Census division  O: All-site STI testing
mod.allsite <- glm(prep.allsite.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.allsite)
exp(coef(mod.allsite))
exp(confint(mod.allsite))

z <- as.data.frame(coef(mod.allsite))
z$alpha <- mod.allsite$coefficients[1]
z$plogis <- plogis(z[,1] + z[,2])
z

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: All-site STI testing
mod1.allsite <- glm(prep.allsite.sometimes ~ as.factor(DIVCODE) + age + race.cat,
                  data = d2, family = binomial())
summary(mod1.allsite)
exp(coef(mod1.allsite))
exp(confint(mod1.allsite))

# Logistic regression model (unadjusted)
# E: Census region O: All-site STI testing
mod2.allsite <- glm(prep.allsite.sometimes ~ as.factor(REGCODE), data = d2, family = binomial())
summary(mod2.allsite)
exp(coef(mod2.allsite))
exp(confint(mod2.allsite))

z2 <- as.data.frame(coef(mod2.allsite))
z2$alpha <- mod2.allsite$coefficients[1]
z2$plogis <- plogis(z2[,1] + z2[,2])
z2

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region  O: All-site STI testing
mod3.allsite <- glm(prep.allsite.sometimes ~ as.factor(REGCODE) + age + race.cat,
                    data = d2, family = binomial())
summary(mod3.allsite)
exp(coef(mod3.allsite))
exp(confint(mod3.allsite))

# Logistic regression model (unadjusted)
# E: Deep south O: All-site STI testing
mod4.allsite <- glm(prep.allsite.sometimes ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod4.allsite)
exp(coef(mod4.allsite))
exp(confint(mod4.allsite))

plogis(sum(coef(mod4.allsite)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: All-site STI testing
mod5.allsite <- glm(prep.allsite.sometimes ~ as.factor(deep_south) + age + race.cat,
                    data = d2, family = binomial())
summary(mod5.allsite)
exp(coef(mod5.allsite))
exp(confint(mod5.allsite))

# Logistic regression model (adjusted #2 / control for race/eth, age, and exposure at site as confounder)
# E: Deep south  O: All-site STI testing
mod6.allsite <- glm(prep.allsite.sometimes ~ as.factor(deep_south) + age + race.cat + exp.allsite,
                    data = d2, family = binomial())
summary(mod6.allsite)
exp(coef(mod6.allsite))
exp(confint(mod6.allsite))

# Logistic regression model (adjusted #3 / control for race/eth, age, and exposure at site as effect modifier)
# E: Deep south  O: All-site STI testing
mod7.allsite <- glm(prep.allsite.sometimes ~ as.factor(deep_south) + age + race.cat + exp.allsite + as.factor(deep_south)*exp.allsite,
                    data = d2, family = binomial())
summary(mod7.allsite)

exp.allsite.em <- rbind("OR - deep_south - exp.allsite = 0" = c(0, 1, 0, 0, 0, 0, 0, 0),
                        "OR - deep_south - exp.allsite = 1" = c(0, 1, 0, 0, 0, 0, 0, 1))
mod7.allsite.contrasts <- glht(mod7.allsite, linfct = exp.allsite.em)
summary(mod7.allsite.contrasts)
cbind(exp(coef(mod7.allsite.contrasts)), exp(confint.default(mod7.allsite.contrasts)))

# Logistic regression model (unadjusted)
# E: Deep south #2 O: All-site STI testing
mod8.allsite <- glm(prep.allsite.sometimes ~ as.factor(deep_south2), data = d2, family = binomial())
summary(mod8.allsite)
exp(coef(mod8.allsite))
exp(confint(mod8.allsite))

plogis(sum(coef(mod8.allsite)))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south #2  O: All-site STI testing
mod9.allsite <- glm(prep.allsite.sometimes ~ as.factor(deep_south2) + age + race.cat,
                     data = d2, family = binomial())
summary(mod9.allsite)
exp(coef(mod9.allsite))
exp(confint(mod9.allsite))

# Logistic regression model (adjusted #2 / control for race/eth, age, and exposure at site as confounder)
# E: Deep south #2  O: All-site STI testing
mod10.allsite <- glm(prep.allsite.sometimes ~ as.factor(deep_south2) + age + race.cat + exp.allsite,
                    data = d2, family = binomial())
summary(mod10.allsite)
exp(coef(mod10.allsite))
exp(confint(mod10.allsite))

# Logistic regression model (adjusted #3 / control for race/eth, age, and exposure at site as confounder)
# E: Deep south #2  O: All-site STI testing
mod11.allsite <- glm(prep.allsite.sometimes ~ as.factor(deep_south2) + age + race.cat + exp.allsite + as.factor(deep_south2)*exp.allsite,
                     data = d2, family = binomial())
summary(mod11.allsite)

mod11.allsite.contrasts <- glht(mod11.allsite, linfct = exp.allsite.em)
summary(mod11.allsite.contrasts)
cbind(exp(coef(mod11.allsite.contrasts)), exp(confint.default(mod11.allsite.contrasts)))

# Logistic regression model (unadjusted)
# E: Urbanicity O: All-site STI testing
mod12.allsite <- glm(prep.allsite.sometimes ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod12.allsite)
exp(coef(mod12.allsite))
exp(confint(mod12.allsite))

z12 <- as.data.frame(coef(mod12.allsite))
z12$alpha <- mod12.allsite$coefficients[1]
z12$plogis <- plogis(z12[,1] + z12[,2])
z12

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Urbanicity  O: All-site STI testing
mod13.allsite <- glm(prep.allsite.sometimes ~ as.factor(NCHS_2013) + age + race.cat,
                    data = d2, family = binomial())
summary(mod13.allsite)
exp(coef(mod13.allsite))
exp(confint(mod13.allsite))

# Logistic regression model (unadjusted)
# E: Race/ethnicity O: All-site STI testing
mod14.allsite <- glm(prep.allsite.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod14.allsite)
exp(coef(mod14.allsite))
exp(confint(mod14.allsite))

z14 <- as.data.frame(coef(mod14.allsite))
z14$alpha <- mod14.allsite$coefficients[1]
z14$plogis <- plogis(z14[,1] + z14[,2])
z14

# Logistic regression model (unadjusted)
# E: Age category O: All-site STI testing
mod15.allsite <- glm(prep.allsite.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod15.allsite)
exp(coef(mod15.allsite))
exp(confint(mod15.allsite))

z15 <- as.data.frame(coef(mod15.allsite))
z15$alpha <- mod15.allsite$coefficients[1]
z15$plogis <- plogis(z15[,1] + z15[,2])
z15

# Logistic regression model (unadjusted)
# E: Insurance O: All-site STI testing
mod16.allsite <- glm(prep.allsite.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod16.allsite)
exp(coef(mod16.allsite))
exp(confint(mod16.allsite))

z16 <- as.data.frame(coef(mod16.allsite))
z16$alpha <- mod16.allsite$coefficients[1]
z16$plogis <- plogis(z16[,1] + z16[,2])
z16

# Logistic regression model (unadjusted)
# E: Annual household income  O: All-site STI testing
mod17.allsite <- glm(prep.allsite.sometimes ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod17.allsite)
exp(coef(mod17.allsite))
exp(confint(mod17.allsite))

z17 <- as.data.frame(coef(mod17.allsite))
z17$alpha <- mod17.allsite$coefficients[1]
z17$plogis <- plogis(z17[,1] + z17[,2])
z17

# Logistic regression model (unadjusted)
# E: Education  O: All-site STI testing
mod18.allsite <- glm(prep.allsite.sometimes ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod18.allsite)
exp(coef(mod18.allsite))
exp(confint(mod18.allsite))

z18 <- as.data.frame(coef(mod18.allsite))
z18$alpha <- mod18.allsite$coefficients[1]
z18$plogis <- plogis(z18[,1] + z18[,2])
z18

# Logistic regression model (unadjusted)
# E: Exposure at all sites  O: All-site STI testing
mod19.allsite <- glm(prep.allsite.sometimes ~ exp.allsite, data = d2, family = binomial())
summary(mod19.allsite)
exp(coef(mod19.allsite))
exp(confint(mod19.allsite))

plogis(sum(coef(mod19.allsite)))

# Stratified tables by exposure at site -------------------------------
# For effect of deep south on STI testing

# No exposure at throat
e0.throat <- filter(d2, d2$anyROI == 0)
addmargins(table(e0.throat$prep.throat.sometimes, e0.throat$deep_south))

# Exposure at throat
e1.throat <- filter(d2, d2$anyROI == 1)
addmargins(table(e1.throat$prep.throat.sometimes, e1.throat$deep_south))

# No exposure at rectum
e0.rect <- filter(d2, d2$anyRAI == 0)
addmargins(table(e0.rect$prep.rect.sometimes, e0.rect$deep_south))

# Exposure at rectum
e1.rect <- filter(d2, d2$anyRAI == 1)
addmargins(table(e1.rect$prep.rect.sometimes, e1.rect$deep_south))

# No exposure at urethra
e0.ureth <- filter(d2, d2$exp.ureth == 0)
addmargins(table(e0.ureth$prep.ureth.sometimes, e0.ureth$deep_south))

# Exposure at urethra
e1.ureth <- filter(d2, d2$exp.ureth == 1)
addmargins(table(e1.ureth$prep.ureth.sometimes, e1.ureth$deep_south))

# No exposure at all site
e0.allsite <- filter(d2, d2$exp.allsite == 0)
addmargins(table(e0.allsite$prep.ureth.sometimes, e0.allsite$deep_south))

# Exposure at all site
e1.allsite <- filter(d2, d2$exp.allsite == 1)
addmargins(table(e1.allsite$prep.allsite.sometimes, e1.allsite$deep_south))

# Predicted probability plots -------------------------------
# Logistic regression model for predicting always STI testing

pred.mod.hiv <- glm(prep.hiv.always ~ as.factor(REGCODE) + race.cat + age + 
                      as.factor(insurance) + as.factor(HHINCOME) + as.factor(HLEDUCAT_2),
                    data = d2, family = binomial())

m <- expand.grid(REGCODE = 1:4,
                 race.cat = c("black", "white", "hispanic", "other"),
                 age = 15:65,
                 insurance = 0:2,
                 HHINCOME = 0:3,
                 HLEDUCAT_2 = 0:2)

x <- expand.grid(REGCODE = 1:4,
                 age = median(d2$age, na.rm = T), 
                 race.cat = "black", 
                 HHINCOME = median(d2$HHINCOME, na.rm = T), 
                 HLEDUCAT_2 = median(d2$HLEDUCAT_2, na.rm = T),
                 insurance = median(d2$insurance, na.rm = T))

pred <- predict(pred.mod.hiv, newdata = m, type = "response", se.fit = F)
pred.hiv <- cbind(m, pred = pred)
plot(pred.hiv$REGCODE, pred.hiv$pred)
plot(pred.hiv$age, pred.hiv$pred)

pred2 <- predict(pred.mod.hiv, newdata = x, type = "response", se.fit = F)
pred.hiv2 <- cbind(x, pred2 = pred2)
plot(pred.hiv2$REGCODE, pred.hiv2$pred)

# Figure 1 ----------------------------------------------------------------

# Boxplot type figure related to Table 2



# Figure 2 ----------------------------------------------------------------

# Map related to Table 3
