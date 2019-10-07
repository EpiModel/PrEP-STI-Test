
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
addmargins(table(d$race, d$prep_cat, useNA = "always"))
addmargins(table(d$race.cat, d$prep_cat, useNA = "always"))

### Re-categorize race to Asian, Black, Hispanic, Other, White
d$race.cat2 <- ifelse(d$race == "asian" & 
                        d$race.cat != "hispanic", 
                        "asian", 
                        d$race.cat)

table(d$race.cat2, useNA = "always")
table(d$race.cat2, useNA = "always")/3259*100

race <- table(d$race.cat2, d$prep_cat, useNA = "always")
race_perc <- t(t(race)/colSums(race)*100)
colSums(race_perc)


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

## Census region and division

table(d$DIVCODE, useNA = "always")
table(d$DIVCODE)/3259*100

census <- table(d$DIVCODE, d$prep_cat, useNA = 'always')
census
census_perc <- t(t(census)/colSums(census)*100)
census_perc

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
# 2: Health insurance, private

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

## Visited HCP in last 12 mo

table(d$SEEHCP, useNA = "always")
table(d$SEEHCP)/3259*100

hcp <- table(d$SEEHCP, d$prep_cat, useNA = 'always')
hcp
hcp_perc <- t(t(hcp)/colSums(hcp)*100)
hcp_perc


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

### Combine data for NEVER and EVER PrEP users / STI testing due to symptoms
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
table(d$STITESTFREQ, d$prep_cat, useNA = "always")/2450*100

# Frequency of PrEP-related check-ups among non-current PrEP users
table(d$PREPCHKFREQ, d$prep_cat, useNA = "always")
table(d$PREPCHKFREQ, d$prep_cat, useNA = "always")/178*100

# Frequency of PrEP-related check-ups among current PrEP users
table(d$PREPCHKFREQ_CURR, d$prep_cat, useNA = "always")
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

### HIV Testing ------------------------------

table(d$HIVFREQ_PREP, useNA = "always")




# Table 3 -----------------------------------------------------------------

# What are the predictors of recommended HIV and all-site STI screening in ever PrEP 
# users -- is there variation by demographics, risk behavior, or geogrpahy?


## Q15. While you were using PrEP (i.e. Truvada), how often did you return to the 
##      clinic or doctor’s office who prescribed you PrEP for check-ups related to PrEP?
##      [For Non Current PrEP Users]
##      1: 1 Month; 2: 3 Months; 3: 6 Months; 4: 9 Months; 6: Only Once; 
##      7: Some other interval; 8: Did not return regularly
table(d$PREPCHKFREQ, useNA = "always")

## Q16. Same as Q15, for Current PrEP Users
table(d$PREPCHKFREQ_CURR, useNA = "always")

## Q17a: How Often Tested for HIV
##       1:4: always, sometimes, rarely, never
table(d$PREP_HIVTESTFREQ, useNA = "always")

# Recoding for always/not
d$prep.hiv.always <- ifelse(d$PREP_HIVTESTFREQ %in% 2:4, 0, d$PREP_HIVTESTFREQ)
table(d$prep.hiv.always, useNA = "always")

## Q17b: How Often Tested for STIs in Throat
##       1:4: always, sometimes, rarely, never
table(d$PREP_STITHROATFREQ, useNA = "always")

## Q17c: How Often Tested for STIs Rectal
##       1:4: always, sometimes, rarely, never
table(d$PREP_STIRECTFREQ, useNA = "always")
d$prep.rect.always <- ifelse(d$PREP_STIRECTFREQ %in% 2:4, 0, d$PREP_STIRECTFREQ)
table(d$prep.rect.always, useNA = "always")

mod <- glm(prep.rect.always ~ as.factor(DIVCODE), data = d, family = binomial())
summary(mod)
plogis(coef(mod))

## Q17c: How Often Tested for STIs Urethral
##       1:4: always, sometimes, rarely, never
table(d$PREP_STIURETHFREQ, useNA = "always")


## Q17d: How Often Tested for STIs Blood
##       1:4: always, sometimes, rarely, never
table(d$PREP_BLOODFREQ, useNA = "always")



# Figure 1 ----------------------------------------------------------------

# Boxplot type figure related to Table 2



# Figure 2 ----------------------------------------------------------------

# Map related to Table 3
