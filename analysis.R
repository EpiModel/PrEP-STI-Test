
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


## Race
addmargins(table(d$race, d$prep_cat, useNA = "always"))
addmargins(table(d$race.cat, d$prep_cat, useNA = "always"))

### Re-categorize race to Asian, Black, Hispanic, Other, White
d$race.cat2 <- ifelse(d$race == "asian" & 
                        d$race.cat != "hispanic", 
                        "asian", 
                        d$race.cat)

table(d$race.cat2, useNA = "always")
table(d$race.cat2, useNA = "always")/3262*100

race <- table(d$race.cat2, d$prep_cat, useNA = "always")
race_perc <- t(t(race)/colSums(race)*100)
colSums(race_perc)

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
table(d$age_cat)/3262*100

age <- table(d$age_cat, d$prep_cat, useNA = 'always')
age
colSums(age)

age_perc <- t(t(age)/colSums(age)*100)
age_perc

## Census region and division

table(d$DIVCODE, useNA = "always")
table(d$DIVCODE)/3262*100

census <- table(d$DIVCODE, d$prep_cat, useNA = 'always')
census
census_perc <- t(t(census)/colSums(census)*100)
census_perc

## Urbanicity

table(d$NCHS_2013, useNA = "always")
table(d$NCHS_2013)/3262*100

urban <- table(d$NCHS_2013, d$prep_cat, useNA = 'always')
urban
urban_perc <- t(t(urban)/colSums(urban)*100)
urban_perc

## Highest level of education

table(d$HLEDUCAT, useNA = "always")
table(d$HLEDUCAT, useNA = "always")/3262*100

education <- table(d$HLEDUCAT, d$prep_cat, useNA = 'always')
education
education_perc <- t(t(education)/colSums(education)*100)
education_perc

## Annual income

table(d$HHINCOME, useNA = "always")
table(d$HHINCOME, useNA = "always")/3262*100

income <- table(d$HHINCOME, d$prep_cat, useNA = 'always')
income
income_perc <- t(t(income)/colSums(income)*100)
income_perc

## Health insurance

d_ins <- d %>%
  select(AMIS_ID, TYP_INSA:TYP_INSC) %>%
  gather(TYPE, INSURANCE, TYP_INSA:TYP_INSC)

table(d_ins$TYPE, d_ins$INSURANCE, useNA = "always")
table(d_ins$TYPE, d_ins$INSURANCE, useNA = "always")/3262*100

# private health plan through employer
typ_insa <- table(d$TYP_INSA, d$prep_cat, useNA = 'always')
typ_insa
insa_perc <- t(t(typ_insa)/colSums(typ_insa)*100)
insa_perc

# private health plan through exchange
typ_insa2 <- table(d$TYP_INSA2, d$prep_cat, useNA = 'always')
typ_insa2
insa2_perc <- t(t(typ_insa2)/colSums(typ_insa2)*100)
insa2_perc

# Medicaid or Medicare
typ_insg <- table(d$TYP_INSG, d$prep_cat, useNA = 'always')
typ_insg
insg_perc <- t(t(typ_insg)/colSums(typ_insg)*100)
insg_perc

# some other medical assistance program
typ_insh <- table(d$TYP_INSH, d$prep_cat, useNA = 'always')
typ_insh
insh_perc <- t(t(typ_insh)/colSums(typ_insh)*100)
insh_perc

# TRICARE
typ_insd <- table(d$TYP_INSD, d$prep_cat, useNA = 'always')
typ_insd
insd_perc <- t(t(typ_insd)/colSums(typ_insd)*100)
insd_perc

# VA
typ_inse <- table(d$TYP_INSE, d$prep_cat, useNA = 'always')
typ_inse
inse_perc <- t(t(typ_inse)/colSums(typ_inse)*100)
inse_perc

# some other health care plan
typ_insf <- table(d$TYP_INSF, d$prep_cat, useNA = 'always')
typ_insf
insf_perc <- t(t(typ_insf)/colSums(typ_insf)*100)
insf_perc

# don't currently have health insurance
typ_insi <- table(d$TYP_INSI, d$prep_cat, useNA = 'always')
typ_insi
insi_perc <- t(t(typ_insi)/colSums(typ_insi)*100)
insi_perc

# prefer not to answer
typ_insb <- table(d$TYP_INSB, d$prep_cat, useNA = 'always')
typ_insb
insb_perc <- t(t(typ_insb)/colSums(typ_insb)*100)
insb_perc

# don't know
typ_insc <- table(d$TYP_INSC, d$prep_cat, useNA = 'always')
typ_insc
insc_perc <- t(t(typ_insc)/colSums(typ_insc)*100)
insc_perc

## Visited HCP in last 12 mo

table(d$SEEHCP, useNA = "always")
table(d$SEEHCP)/3262*100

hcp <- table(d$SEEHCP, d$prep_cat, useNA = 'always')
hcp
hcp_perc <- t(t(hcp)/colSums(hcp)*100)
hcp_perc


# Table 2 -----------------------------------------------------------------

# causal question: is HIV screening rate and STI screening rate higher
# in current PrEP users compared to non-current PrEP users and never PrEP users?

# STI testing among NEVER PrEP users
# Number of times tested for an STI within the past 2 years
# could include syphilis, gonorrhea, and chlamydia (but not HIV)
table(d$STITEST_2YR, useNA = "always")
d$STITEST_2YR <- ifelse(d$STITEST_2YR == 2015, NA, d$STITEST_2YR) 
summary(d$STITEST_2YR)

addmargins(table(d$prep_cat, d$STITEST_2YR, useNA = "always"))

# STI testing among EVER PrEP Users (Current and Non-Current)
# Number of times testing for an STI within the past 2 years
# NOT during a PrEP visit
# could include syphilis, gonorrhea, and chlamydia (but not HIV)
table(d$STITEST_2YR_PREP, useNA = "always")
d$STITEST_2YR_PREP <- ifelse(d$STITEST_2YR_PREP == 2000, NA, d$STITEST_2YR_PREP)
summary(d$STITEST_2YR_PREP)

addmargins(table(d$prep_cat, d$STITEST_2YR_PREP, useNA = "always"))

# Of the tests in past 2 years (above) not from PrEP visit
# how many were due to STI symptoms
table(d$STITEST_2YR_SYMPT, useNA = "always")

# Identify any logic discrepancies between 
# None found 
table(d$STITEST_2YR, d$STITEST_2YR_SYMPT, useNA = "always") 

table(d$STITEST_2YR_NOTIF, useNA = "always")

table(d$STITEST_2YR_SYMPT_PREP, useNA = "always")
table(d$STITEST_2YR_NOTIF_PREP, useNA = "always")

table(d$STIREG, useNA = "always")
table(d$STITESTFREQ, useNA = "always")

table(d$HIVFREQ_PREP, useNA = "always")
table(d$STIFREQ_PREP, useNA = "always")

# Create summary table of continuous outcomes above by PrEP use category
a <- d %>%
  group_by(prep_cat) %>%
  summarise(
            # Never PrEP users
            mean_2yr = mean(STITEST_2YR, na.rm = T),
            sd_2yr = sd(STITEST_2YR, na.rm = T),
            mean_2yr_sym = mean(STITEST_2YR_SYMPT, na.rm = T),
            sd_2yr_sym = sd(STITEST_2YR_SYMPT, na.rm = T),
            mean_2yr_not = mean(STITEST_2YR_NOTIF, na.rm = T),
            sd_2yr_not = sd(STITEST_2YR_NOTIF, na.rm = T),
            
            # Ever PrEP users
            mean_2yr_prep = mean(STITEST_2YR_PREP, na.rm = T),
            sd_2yr_prep = sd(STITEST_2YR_PREP, na.rm = T),
            mean_2yr_sym_prep = mean(STITEST_2YR_SYMPT_PREP, na.rm = T),
            sd_2yr_sym_prep = sd(STITEST_2YR_SYMPT_PREP, na.rm = T),
            mean_2yr_not_prep = mean(STITEST_2YR_NOTIF_PREP, na.rm = T),
            sd_2yr_not_prep = sd(STITEST_2YR_NOTIF_PREP, na.rm = T),
            )


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
