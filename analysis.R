
# PrEP STI Screening Analysis Script

library("ARTnetData")
sessioninfo::session_info()
# ARTnetData  * 1.0     2019-04-25 [1] Github (EpiModel/ARTnetData@f07ba02)

library("tidyverse")

## Load Wide and Long Form Datasets
d <- ARTnet.wide
l <- ARTnet.long

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
### Create region variable
  # 1: Northeast
  # 2: Midewest
  # 3: South
  # 4: Pacific

d$region <- ifelse(d$DIVCODE == 1 | d$DIVCODE == 2, 1,
            ifelse(d$DIVCODE == 3 | d$DIVCODE == 4, 2,
            ifelse(d$DIVCODE == 5 | d$DIVCODE == 6 |
                                    d$DIVCODE == 7, 3,
            ifelse(d$DIVCODE == 8 | d$DIVCODE == 9, 4,
            NA))))

table(d$DIVCODE, d$region, useNA = "always")

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

# Table 4 -----------------------------------------------------------------
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
HIVtest_p12m_perc <- t(t(HIVtest_p12m)/colSums(recHIVtest)*100)
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

## Q15. While you were using PrEP (i.e. Truvada), how often did you return to the 
##      clinic or doctor’s office who prescribed you PrEP for check-ups related to PrEP?
##      [For Non Current PrEP Users]
##      1: 1 Month; 2: 3 Months; 3: 6 Months; 4: 9 Months; 6: Only Once; 
##      7: Some other interval; 8: Did not return regularly
table(d2$PREPCHKFREQ, useNA = "always")

## Q16. Same as Q15, for Current PrEP Users
table(d2$PREPCHKFREQ_CURR, useNA = "always")

## Q17a: How Often Tested for HIV
##       1:4: always, sometimes, rarely, never
table(d2$PREP_HIVTESTFREQ, useNA = "always")

# Recoding for always/not for HIV testing at PrEP visits
d2$prep.hiv.always <- ifelse(d2$PREP_HIVTESTFREQ %in% 2:4, 0, d2$PREP_HIVTESTFREQ)
table(d2$prep.hiv.always, useNA = "always")

# Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$region, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$deep_south, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$NCHS_2013, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$race.cat, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$age_cat, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$insurance, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.hiv.always, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.hiv.always, useNA = "always"))

# Logistic regression model (unadjusted)
# E: Census division  O: HIV testing
mod.hiv <- glm(prep.hiv.always ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.hiv)
exp(coef(mod.hiv))
exp(confint(mod.hiv))
plogis(coef(mod.hiv))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: HIV testing
mod1.hiv <- glm(prep.hiv.always ~ as.factor(DIVCODE) + race.cat + age,
                data = d2, family = binomial())
summary(mod1.hiv)
exp(coef(mod1.hiv))
exp(confint(mod1.hiv))

# Logistic regression model (unadjusted)
# E: Census region O: HIV testing
mod2.hiv <- glm(prep.hiv.always ~ as.factor(region), data = d2, family = binomial())
summary(mod2.hiv)
exp(coef(mod2.hiv))
exp(confint(mod2.hiv))
plogis(coef(mod2.hiv))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region & covariates  O: HIV testing
mod3.hiv <- glm(prep.hiv.always ~ as.factor(region) + race.cat + age,
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
plogis(coef(mod4.hiv))

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
plogis(coef(mod6.hiv))

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
plogis(coef(mod8.hiv))

# Logistic regression model (unadjusted)
# E: Insurance  O: HIV testing
mod9.hiv <- glm(prep.hiv.always ~ as.factor(insurance), data = d2, family = binomial())
summary(mod9.hiv)
exp(coef(mod9.hiv))
exp(confint(mod9.hiv))
plogis(coef(mod9.hiv))

# Logistic regression model (unadjusted)
# E: HH Income  O: HIV testing
mod10.hiv <- glm(prep.hiv.always ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod10.hiv)
exp(coef(mod10.hiv))
exp(confint(mod10.hiv))
plogis(coef(mod10.hiv))

# Logistic regression model (unadjusted)
# E: HL Education  O: HIV testing
mod11.hiv <- glm(prep.hiv.always ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod11.hiv)
exp(coef(mod11.hiv))
exp(confint(mod11.hiv))
plogis(coef(mod11.hiv))

# Logistic regression model (unadjusted)
# E: Age category  O: HIV testing
mod12.hiv <- glm(prep.hiv.always ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod12.hiv)
exp(coef(mod12.hiv))
exp(confint(mod12.hiv))
plogis(coef(mod12.hiv))


## Q17b: How Often Tested for STIs in Throat
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STITHROATFREQ, useNA = "always")

# Recoding for always/not always pharyngeal STI testing 
d2$prep.throat.always <- ifelse(d2$PREP_STITHROATFREQ %in% 2:4, 0, d2$PREP_STITHROATFREQ)
addmargins(table(d2$prep.throat.always))

# Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.throat.always))
addmargins(table(d2$region, d2$prep.throat.always))
addmargins(table(d2$deep_south, d2$prep.throat.always))
addmargins(table(d2$NCHS_2013, d2$prep.throat.always))
addmargins(table(d2$race.cat, d2$prep.throat.always))
addmargins(table(d2$age_cat, d2$prep.throat.always))
addmargins(table(d2$insurance, d2$prep.throat.always, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.throat.always, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.throat.always, useNA = "always"))

# Logistic regression model (unadjusted)
# E: Census division  O: Pharyngeal STI testing
mod.throat <- glm(prep.throat.always ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.throat)
exp(coef(mod.throat))
exp(confint(mod.throat))
plogis(coef(mod.throat))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: Pharyngeal STI testing
mod1.throat <- glm(prep.throat.always ~ as.factor(DIVCODE) + age + race.cat,
                 data = d2, family = binomial())
summary(mod1.throat)
exp(coef(mod1.throat))
exp(confint(mod1.throat))

# Logistic regression model (unadjusted)
# E: Census region  O: Pharyngeal STI testing
mod2.throat <- glm(prep.throat.always ~ as.factor(region), data = d2, family = binomial())
summary(mod2.throat)
exp(coef(mod2.throat))
exp(confint(mod2.throat))
plogis(coef(mod2.throat))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region & covariates  O: Pharyngeal STI testing
mod3.throat <- glm(prep.throat.always ~ as.factor(region) + age + race.cat,
                   data = d2, family = binomial())
summary(mod3.throat)
exp(coef(mod3.throat))
exp(confint(mod3.throat))

# Logistic regression model (unadjusted)
# E: Deep south O: Pharyngeal STI testing
mod4.throat <- glm(prep.throat.always ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod4.throat)
exp(coef(mod4.throat))
exp(confint(mod4.throat))
plogis(coef(mod4.throat))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Pharyngeal STI testing
mod5.throat <- glm(prep.throat.always ~ as.factor(deep_south) + age + race.cat,
                   data = d2, family = binomial())
summary(mod5.throat)
exp(coef(mod5.throat))
exp(confint(mod5.throat))

# Logistic regression model (unadjusted)
# E: Urbanicity O: Pharyngeal STI testing
mod6.throat <- glm(prep.throat.always ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod6.throat)
exp(coef(mod6.throat))
exp(confint(mod6.throat))
plogis(coef(mod6.throat))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Pharyngeal STI testing
mod7.throat <- glm(prep.throat.always ~ as.factor(NCHS_2013) + age + race.cat,
                   data = d2, family = binomial())
summary(mod7.throat)
exp(coef(mod7.throat))
exp(confint(mod7.throat))
plogis(coef(mod7.throat))

# Logistic regression model (unadjusted)
# E: Race O: Pharyngeal STI testing
mod8.throat <- glm(prep.throat.always ~ race.cat, data = d2, family = binomial())
summary(mod8.throat)
exp(coef(mod8.throat))
exp(confint(mod8.throat))
plogis(coef(mod8.throat))

# Logistic regression model (unadjusted)
# E: Insurance O: Pharyngeal STI testing
mod9.throat <- glm(prep.throat.always ~ as.factor(insurance), data = d2, family = binomial())
summary(mod9.throat)
exp(coef(mod9.throat))
exp(confint(mod9.throat))
plogis(coef(mod9.throat))

# Logistic regression model (unadjusted)
# E: HH income O: Pharyngeal STI testing
mod10.throat <- glm(prep.throat.always ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod10.throat)
exp(coef(mod10.throat))
exp(confint(mod10.throat))
plogis(coef(mod10.throat))

# Logistic regression model (unadjusted)
# E: Education O: Pharyngeal STI testing
mod11.throat <- glm(prep.throat.always ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod11.throat)
exp(coef(mod11.throat))
exp(confint(mod11.throat))
plogis(coef(mod11.throat))

# Logistic regression model (unadjusted)
# E: Age category O: Pharyngeal STI testing
mod12.throat <- glm(prep.throat.always ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod12.throat)
exp(coef(mod12.throat))
exp(confint(mod12.throat))
plogis(coef(mod12.throat))

## Q17c: How Often Tested for STIs Rectal
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STIRECTFREQ, useNA = "always")

# Recoding to always vs. not always rectal STI testings
d2$prep.rect.always <- ifelse(d2$PREP_STIRECTFREQ %in% 2:4, 0, d2$PREP_STIRECTFREQ)
addmargins(table(d2$prep.rect.always))

# Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.rect.always))
addmargins(table(d2$region, d2$prep.rect.always))
addmargins(table(d2$deep_south, d2$prep.rect.always))
addmargins(table(d2$NCHS_2013, d2$prep.rect.always))
addmargins(table(d2$race.cat, d2$prep.rect.always))
addmargins(table(d2$age_cat, d2$prep.rect.always))
addmargins(table(d2$insurance, d2$prep.rect.always, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.rect.always, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.rect.always, useNA = "always"))

# Logistic regression model (unadjusted)
# E: Census division  O: Rectal STI testing
mod.rectal <- glm(prep.rect.always ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.rectal)
exp(coef(mod.rectal))
exp(confint(mod.rectal))
plogis(coef(mod.rectal))

# Logistic regression model (adjusted #1 / control for race/eth, age, income, insurance, education))
# E: Census division  O: Rectal STI testing
mod1.rectal <- glm(prep.rect.always ~ as.factor(DIVCODE) + age + race.cat,
                   data = d2, family = binomial())
summary(mod1.rectal)
exp(coef(mod1.rectal))
exp(confint(mod1.rectal))

# Logistic regression model (unadjusted)
# E: Census region O: Rectal STI testing
mod2.rectal <- glm(prep.rect.always ~ as.factor(region), data = d2, family = binomial())
summary(mod2.rectal)
exp(coef(mod2.rectal))
exp(confint(mod2.rectal))
plogis(coef(mod2.rectal))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region  O: Rectal STI testing
mod3.rectal <- glm(prep.rect.always ~ as.factor(region) + age + race.cat,
                   data = d2, family = binomial())
summary(mod3.rectal)
exp(coef(mod3.rectal))
exp(confint(mod3.rectal))

# Logistic regression model (unadjusted)
# E: Deep south O: Rectal STI testing
mod4.rectal <- glm(prep.rect.always ~ deep_south, data = d2, family = binomial())
summary(mod4.rectal)
exp(coef(mod4.rectal))
exp(confint(mod4.rectal))
plogis(coef(mod4.rectal))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Rectal STI testing
mod5.rectal <- glm(prep.rect.always ~ deep_south + age + race.cat,
                   data = d2, family = binomial())
summary(mod5.rectal)
exp(coef(mod5.rectal))
exp(confint(mod5.rectal))

# Logistic regression model (unadjusted)
# E: Urbanicity O: Rectal STI testing
mod6.rectal <- glm(prep.rect.always ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod6.rectal)
exp(coef(mod6.rectal))
exp(confint(mod6.rectal))
plogis(coef(mod6.rectal))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Urbanicity  O: Rectal STI testing
mod7.rectal <- glm(prep.rect.always ~ as.factor(NCHS_2013) + age + race.cat,
                   data = d2, family = binomial())
summary(mod7.rectal)
exp(coef(mod7.rectal))
exp(confint(mod7.rectal))

# Logistic regression model (unadjusted)
# E: Race/Ethnicity O: Rectal STI testing
mod8.rectal <- glm(prep.rect.always ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod8.rectal)
exp(coef(mod8.rectal))
exp(confint(mod8.rectal))
plogis(coef(mod8.rectal))

# Logistic regression model (unadjusted)
# E: Insurance O: Rectal STI testing
mod9.rectal <- glm(prep.rect.always ~ as.factor(insurance), data = d2, family = binomial())
summary(mod9.rectal)
exp(coef(mod9.rectal))
exp(confint(mod9.rectal))
plogis(coef(mod9.rectal))

# Logistic regression model (unadjusted)
# E: Houshold income  O: Rectal STI testing
mod10.rectal <- glm(prep.rect.always ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod10.rectal)
exp(coef(mod10.rectal))
exp(confint(mod10.rectal))
plogis(coef(mod10.rectal))

# Logistic regression model (unadjusted)
# E:Education  O: Rectal STI testing
mod11.rectal <- glm(prep.rect.always ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod11.rectal)
exp(coef(mod11.rectal))
exp(confint(mod11.rectal))
plogis(coef(mod11.rectal))

# Logistic regression model (unadjusted)
# E:Age category  O: Rectal STI testing
mod12.rectal <- glm(prep.rect.always ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod12.rectal)
exp(coef(mod12.rectal))
exp(confint(mod12.rectal))
plogis(coef(mod12.rectal))


## Q17c: How Often Tested for STIs Urethral
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STIURETHFREQ, useNA = "always")

# Recoding to always vs. not always urethral STI testings
d2$prep.ureth.always <- ifelse(d2$PREP_STIURETHFREQ %in% 2:4, 0, d2$PREP_STIURETHFREQ)
addmargins(table(d2$prep.ureth.always))

# Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.ureth.always))
addmargins(table(d2$region, d2$prep.ureth.always))
addmargins(table(d2$deep_south, d2$prep.ureth.always))
addmargins(table(d2$NCHS_2013, d2$prep.ureth.always))
addmargins(table(d2$race.cat, d2$prep.ureth.always))
addmargins(table(d2$age_cat, d2$prep.ureth.always))
addmargins(table(d2$insurance, d2$prep.ureth.always, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.ureth.always, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.ureth.always, useNA = "always"))

# Logistic regression model (unadjusted)
# E: Census division O: Urethral STI testing
mod.ureth <- glm(prep.ureth.always ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.ureth)
exp(coef(mod.ureth))
exp(confint(mod.ureth))
plogis(coef(mod.ureth))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: Urethral STI testing
mod1.ureth <- glm(prep.ureth.always ~ as.factor(DIVCODE) + age + race.cat,
                   data = d2, family = binomial())
summary(mod1.ureth)
exp(coef(mod1.ureth))
exp(confint(mod1.ureth))

# Logistic regression model (unadjusted)
# E: Census region O: Urethral STI testing
mod2.ureth <- glm(prep.ureth.always ~ as.factor(region), data = d2, family = binomial())
summary(mod2.ureth)
exp(coef(mod2.ureth))
exp(confint(mod2.ureth))
plogis(coef(mod2.ureth))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region  O: Urethral STI testing
mod3.ureth <- glm(prep.ureth.always ~ as.factor(region) + age + race.cat,
                  data = d2, family = binomial())
summary(mod3.ureth)
exp(coef(mod3.ureth))
exp(confint(mod3.ureth))

# Logistic regression model (unadjusted)
# E: Deep south O: Urethral STI testing
mod4.ureth <- glm(prep.ureth.always ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod4.ureth)
exp(coef(mod4.ureth))
exp(confint(mod4.ureth))
plogis(coef(mod4.ureth))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Urethral STI testing
mod5.ureth <- glm(prep.ureth.always ~ as.factor(deep_south) + age + race.cat,
                  data = d2, family = binomial())
summary(mod5.ureth)
exp(coef(mod5.ureth))
exp(confint(mod5.ureth))

# Logistic regression model (unadjusted)
# E: Urbanicity O: Urethral STI testing
mod6.ureth <- glm(prep.ureth.always ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod6.ureth)
exp(coef(mod6.ureth))
exp(confint(mod6.ureth))
plogis(coef(mod6.ureth))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Urbanicity  O: Urethral STI testing
mod7.ureth <- glm(prep.ureth.always ~ as.factor(NCHS_2013) + age + race.cat,
                  data = d2, family = binomial())
summary(mod7.ureth)
exp(coef(mod7.ureth))
exp(confint(mod7.ureth))

# Logistic regression model (unadjusted)
# E: Race/ethnicity O: Urethral STI testing
mod8.ureth <- glm(prep.ureth.always ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod8.ureth)
exp(coef(mod8.ureth))
exp(confint(mod8.ureth))
plogis(coef(mod8.ureth))

# Logistic regression model (unadjusted)
# E: Insurance O: Urethral STI testing
mod9.ureth <- glm(prep.ureth.always ~ as.factor(insurance), data = d2, family = binomial())
summary(mod9.ureth)
exp(coef(mod9.ureth))
exp(confint(mod9.ureth))
plogis(coef(mod9.ureth))

# Logistic regression model (unadjusted)
# E: Annual HH Income O: Urethral STI testing
mod10.ureth <- glm(prep.ureth.always ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod10.ureth)
exp(coef(mod10.ureth))
exp(confint(mod10.ureth))
plogis(coef(mod10.ureth))

# Logistic regression model (unadjusted)
# E: Highest level of education O: Urethral STI testing
mod11.ureth <- glm(prep.ureth.always ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod11.ureth)
exp(coef(mod11.ureth))
exp(confint(mod11.ureth))
plogis(coef(mod11.ureth))

# Logistic regression model (unadjusted)
# E: Age category O: Urethral STI testing
mod12.ureth <- glm(prep.ureth.always ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod12.ureth)
exp(coef(mod12.ureth))
exp(confint(mod12.ureth))
plogis(coef(mod12.ureth))


## Q17d: How Often Tested for STIs Blood
##       1:4: always, sometimes, rarely, never
table(d2$PREP_BLOODFREQ, useNA = "always")

# Recoding to always vs. not always urethral STI testings
d2$prep.blood.always <- ifelse(d2$PREP_BLOODFREQ %in% 2:4, 0, d2$PREP_BLOODFREQ)
addmargins(table(d2$prep.blood.always))

# Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.blood.always))
addmargins(table(d2$region, d2$prep.blood.always))
addmargins(table(d2$deep_south, d2$prep.blood.always))
addmargins(table(d2$NCHS_2013, d2$prep.blood.always))
addmargins(table(d2$race.cat, d2$prep.blood.always))
addmargins(table(d2$age_cat, d2$prep.blood.always))
addmargins(table(d2$insurance, d2$prep.blood.always, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.blood.always, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.blood.always, useNA = "always"))

# Logistic regression model (unadjusted)
# E: Census division  O: Blood STI testing
mod.blood <- glm(prep.blood.always ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.blood)
exp(coef(mod.blood))
exp(confint(mod.blood))
plogis(coef(mod.blood))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: Blood STI testing
mod1.blood <- glm(prep.blood.always ~ as.factor(DIVCODE) + age + race.cat,
                  data = d2, family = binomial())
summary(mod1.blood)
exp(coef(mod1.blood))
exp(confint(mod1.blood))

# Logistic regression model (unadjusted)
# E: Census region  O: Blood STI testing
mod2.blood <- glm(prep.blood.always ~ as.factor(region), data = d2, family = binomial())
summary(mod2.blood)
exp(coef(mod2.blood))
exp(confint(mod2.blood))
plogis(coef(mod2.blood))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region  O: Blood STI testing
mod3.blood <- glm(prep.blood.always ~ as.factor(region) + age + race.cat,
                  data = d2, family = binomial())
summary(mod3.blood)
exp(coef(mod3.blood))
exp(confint(mod3.blood))

# Logistic regression model (unadjusted)
# E: Deep south  O: Blood STI testing
mod4.blood <- glm(prep.blood.always ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod4.blood)
exp(coef(mod4.blood))
exp(confint(mod4.blood))
plogis(coef(mod4.blood))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: Blood STI testing
mod5.blood <- glm(prep.blood.always ~ as.factor(deep_south) + age + race.cat,
                  data = d2, family = binomial())
summary(mod5.blood)
exp(coef(mod5.blood))
exp(confint(mod5.blood))

# Logistic regression model (unadjusted)
# E: Urbanicity  O: Blood STI testing
mod6.blood <- glm(prep.blood.always ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod6.blood)
exp(coef(mod6.blood))
exp(confint(mod6.blood))
plogis(coef(mod6.blood))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Urbanicity   O: Blood STI testing
mod7.blood <- glm(prep.blood.always ~ as.factor(NCHS_2013) + age + race.cat,
                  data = d2, family = binomial())
summary(mod7.blood)
exp(coef(mod7.blood))
exp(confint(mod7.blood))

# Logistic regression model (unadjusted)
# E: Race/Ethnicity  O: Blood STI testing
mod8.blood <- glm(prep.blood.always ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod8.blood)
exp(coef(mod8.blood))
exp(confint(mod8.blood))
plogis(coef(mod8.blood))

# Logistic regression model (unadjusted)
# E: Insurance  O: Blood STI testing
mod9.blood <- glm(prep.blood.always ~ as.factor(insurance), data = d2, family = binomial())
summary(mod9.blood)
exp(coef(mod9.blood))
exp(confint(mod9.blood))
plogis(coef(mod9.blood))

# Logistic regression model (unadjusted)
# E: Annual household income  O: Blood STI testing
mod10.blood <- glm(prep.blood.always ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod10.blood)
exp(coef(mod10.blood))
exp(confint(mod10.blood))
plogis(coef(mod10.blood))

# Logistic regression model (unadjusted)
# E: Education  O: Blood STI testing
mod11.blood <- glm(prep.blood.always ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod11.blood)
exp(coef(mod11.blood))
exp(confint(mod11.blood))
plogis(coef(mod11.blood))

# Logistic regression model (unadjusted)
# E: Age category  O: Blood STI testing
mod12.blood <- glm(prep.blood.always ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod12.blood)
exp(coef(mod12.blood))
exp(confint(mod12.blood))
plogis(coef(mod12.blood))


## Q17d: How Often Tested for STIs: Blood, Urethra, Throat, Urine
##       1:4: always, sometimes, rarely, never
# Coding to always vs. not always all-site STI testings
d2$prep.allsite.always <- ifelse(d2$PREP_BLOODFREQ == 1 &
                                  d2$PREP_STIRECTFREQ == 1 &
                                  d2$PREP_STIURETHFREQ == 1 &
                                  d2$PREP_STITHROATFREQ == 1, 1, 
                                ifelse(d2$PREP_BLOODFREQ %in% 2:4 |
                                         d2$PREP_STIRECTFREQ %in% 2:4 |
                                         d2$PREP_STIURETHFREQ %in% 2:4 |
                                         d2$PREP_STITHROATFREQ %in% 2:4, 0, NA)
                                )

addmargins(table(d2$prep.allsite.always))

# Descriptive tables by exposure
addmargins(table(d2$DIVCODE, d2$prep.allsite.always))
addmargins(table(d2$region, d2$prep.allsite.always))
addmargins(table(d2$deep_south, d2$prep.allsite.always))
addmargins(table(d2$NCHS_2013, d2$prep.allsite.always))
addmargins(table(d2$race.cat, d2$prep.allsite.always))
addmargins(table(d2$age_cat, d2$prep.allsite.always))
addmargins(table(d2$insurance, d2$prep.allsite.always, useNA = "always"))
addmargins(table(d2$HHINCOME, d2$prep.allsite.always, useNA = "always"))
addmargins(table(d2$HLEDUCAT_2, d2$prep.allsite.always, useNA = "always"))

# Logistic regression model (unadjusted)
# E: Census division  O: All-site STI testing
mod.allsite <- glm(prep.allsite.always ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod.allsite)
exp(coef(mod.allsite))
exp(confint(mod.allsite))
plogis(coef(mod.allsite))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census division  O: All-site STI testing
mod1.allsite <- glm(prep.allsite.always ~ as.factor(DIVCODE) + age + race.cat,
                  data = d2, family = binomial())
summary(mod1.allsite)
exp(coef(mod1.allsite))
exp(confint(mod1.allsite))

# Logistic regression model (unadjusted)
# E: Census region O: All-site STI testing
mod2.allsite <- glm(prep.allsite.always ~ as.factor(region), data = d2, family = binomial())
summary(mod2.allsite)
exp(coef(mod2.allsite))
exp(confint(mod2.allsite))
plogis(coef(mod2.allsite))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Census region  O: All-site STI testing
mod3.allsite <- glm(prep.allsite.always ~ as.factor(region) + age + race.cat,
                    data = d2, family = binomial())
summary(mod3.allsite)
exp(coef(mod3.allsite))
exp(confint(mod3.allsite))

# Logistic regression model (unadjusted)
# E: Deep south O: All-site STI testing
mod4.allsite <- glm(prep.allsite.always ~ as.factor(deep_south), data = d2, family = binomial())
summary(mod4.allsite)
exp(coef(mod4.allsite))
exp(confint(mod4.allsite))
plogis(coef(mod4.allsite))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Deep south  O: All-site STI testing
mod5.allsite <- glm(prep.allsite.always ~ as.factor(deep_south) + age + race.cat,
                    data = d2, family = binomial())
summary(mod5.allsite)
exp(coef(mod5.allsite))
exp(confint(mod5.allsite))

# Logistic regression model (unadjusted)
# E: Urbanicity O: All-site STI testing
mod6.allsite <- glm(prep.allsite.always ~ as.factor(NCHS_2013), data = d2, family = binomial())
summary(mod6.allsite)
exp(coef(mod6.allsite))
exp(confint(mod6.allsite))
plogis(coef(mod6.allsite))

# Logistic regression model (adjusted #1 / control for race/eth, age)
# E: Urbanicity  O: All-site STI testing
mod7.allsite <- glm(prep.allsite.always ~ as.factor(NCHS_2013) + age + race.cat,
                    data = d2, family = binomial())
summary(mod7.allsite)
exp(coef(mod7.allsite))
exp(confint(mod7.allsite))

# Logistic regression model (unadjusted)
# E: Race/ethnicity O: All-site STI testing
mod8.allsite <- glm(prep.allsite.always ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod8.allsite)
exp(coef(mod8.allsite))
exp(confint(mod8.allsite))
plogis(coef(mod8.allsite))

# Logistic regression model (unadjusted)
# E: Insurance O: All-site STI testing
mod9.allsite <- glm(prep.allsite.always ~ as.factor(insurance), data = d2, family = binomial())
summary(mod9.allsite)
exp(coef(mod9.allsite))
exp(confint(mod9.allsite))
plogis(coef(mod9.allsite))

# Logistic regression model (unadjusted)
# E: Annual household income  O: All-site STI testing
mod10.allsite <- glm(prep.allsite.always ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod10.allsite)
exp(coef(mod10.allsite))
exp(confint(mod10.allsite))
plogis(coef(mod10.allsite))

# Logistic regression model (unadjusted)
# E: Education  O: All-site STI testing
mod11.allsite <- glm(prep.allsite.always ~ as.factor(HLEDUCAT_2), data = d2, family = binomial())
summary(mod11.allsite)
exp(coef(mod11.allsite))
exp(confint(mod11.allsite))
plogis(coef(mod11.allsite))

# Logistic regression model (unadjusted)
# E: Age category O: All-site STI testing
mod12.allsite <- glm(prep.allsite.always ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod12.allsite)
exp(coef(mod12.allsite))
exp(confint(mod12.allsite))
plogis(coef(mod12.allsite))


# Considering exposure / any receptive anal intercourse in past year
d2$RAI <- ifelse(d2$PART1RAI == 1 |
                  d2$PART1RAI_ONCE ==1 |
                  d2$PART2RAI == 1 |
                  d2$PART2RAI_ONCE ==1 |
                  d2$PART3RAI == 1 |
                  d2$PART3RAI_ONCE ==1 |
                  d2$PART4RAI == 1 |
                  d2$PART4RAI_ONCE ==1 |
                  d2$PART5RAI == 1 |
                  d2$PART5RAI_ONCE ==1,
                1, 
                ifelse(is.na(d2$PART1RAI) &
                         is.na(d2$PART1RAI_ONCE) &
                         is.na(d2$PART2RAI) &
                         is.na(d2$PART2RAI_ONCE) &
                         is.na(d2$PART3RAI) &
                         is.na(d2$PART3RAI_ONCE) &
                         is.na(d2$PART4RAI) &
                         is.na(d2$PART4RAI_ONCE) &
                         is.na(d2$PART5RAI) &
                         is.na(d2$PART5RAI_ONCE), 
                             NA, 0))

table(d2$RAI, useNA = "always")



# Figure 1 ----------------------------------------------------------------

# Boxplot type figure related to Table 2



# Figure 2 ----------------------------------------------------------------

# Map related to Table 3
