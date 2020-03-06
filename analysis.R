
# PrEP STI Screening Analysis Script

library("ARTnetData")
sessioninfo::session_info()
# ARTnetData  * 1.0     2019-04-25 [1] Github (EpiModel/ARTnetData@f07ba02)

library("tidyverse") 
library("lubridate") # used to manipulate dates
library("sandwich") #used for Poisson regression with robust variance


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
nrow(d)

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

## Southeast states
table(is.na(d$State))

### Create "Southeast" indicator variable
  # 0: Not in Southeast
  # 1: In Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, and WV)

d$southeast <- ifelse(d$State %in% c("AL", "AR", "FL", "GA", "KY", "LA",
                                     "MS", "NC", "SC", "TN", "VA", "WV"),
                      1, 0)

table(d$State, d$southeast)
addmargins(table(d$southeast, d$prep_cat, useNA = "always"))

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


# Table 2 -----------------------------------------------------------------

# is HIV screening rate and STI screening rate higher
# in current PrEP users compared to non-current PrEP users and never PrEP users?

## Descriptive Table 2

# Frequency of PrEP-related check-ups among ever PrEP users
## Combine data for NEVER and EVER PrEP users / frequency of PrEP checkups
d$PREPCHKFREQ_COMB <- ifelse(is.na(d$PREPCHKFREQ),
                               d$PREPCHKFREQ_CURR, 
                               d$PREPCHKFREQ)

addmargins(table(d$PREPCHKFREQ_COMB, d$prep_cat))

# Frequency of HIV screening at PrEP visits
addmargins(table(d$PREP_HIVTESTFREQ, d$prep_cat))

# Frequency of throat swab/STI screening at PrEP visits
addmargins(table(d$PREP_STITHROATFREQ, d$prep_cat))

# Frequency of rectal swab/STI screening at PrEP visits
addmargins(table(d$PREP_STIRECTFREQ, d$prep_cat))

# Frequency of urine sample/STI screening at PrEP visits
addmargins(table(d$PREP_STIURETHFREQ, d$prep_cat))

# Frequency of blood sample/STI screening at PrEP visits
addmargins(table(d$PREP_BLOODFREQ, d$prep_cat))


# Table 3 -----------------------------------------------------------------

# What are the predictors of recommended blood and urogenital STI screening in ever PrEP 
# users -- is there variation by demographics, risk behavior, or geogrpahy?

# Create subset of ever PrEP users 
# Excludes 3 PrEP users with missing data on current use
d2 <- filter(d, d$PREP_REVISED == 1)
nrow(d2)

## Q17d: How Often Tested for STIs Blood
##       1:4: always, sometimes, rarely, never
table(d2$PREP_BLOODFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never blood STI testings
d2$prep.blood.sometimes <- ifelse(d2$PREP_BLOODFREQ %in% 1:2, 1, 
                                  ifelse(d2$PREP_BLOODFREQ %in% 3:4, 0, NA))
table(d2$prep.blood.sometimes, d2$PREP_BLOODFREQ)
addmargins(table(d2$prep.blood.sometimes))

# Descriptive tables by exposure
addmargins(table(d2$age_cat, d2$prep.blood.sometimes))
addmargins(table(d2$race.cat, d2$prep.blood.sometimes))
addmargins(table(d2$insurance, d2$prep.blood.sometimes))
addmargins(table(d2$HHINCOME, d2$prep.blood.sometimes))
addmargins(table(d2$DIVCODE, d2$prep.blood.sometimes))
addmargins(table(d2$southeast, d2$prep.blood.sometimes))

# Poisson regression with robust error variance function (unadjusted models)
poisson_reg <- function(outcome, exp) {
  poisson <- glm (outcome ~ as.factor(exp), data = d2, family = "poisson")
  pois.se <- diag(vcovHC(poisson, type = "HC"))^0.5
  pois.ll <- coef(poisson) - 1.96*pois.se
  pois.ul <- coef(poisson) + 1.96*pois.se
  pois.rr <- coef(poisson)
  round(cbind(exp(pois.rr), exp(pois.ll), exp(pois.ul)),2)
}

poisson_reg(d2$prep.blood.sometimes, d2$age_cat)
poisson_reg(d2$prep.blood.sometimes, d2$race.cat)
poisson_reg(d2$prep.blood.sometimes, d2$insurance)
poisson_reg(d2$prep.blood.sometimes, d2$HHINCOME)
poisson_reg(d2$prep.blood.sometimes, d2$DIVCODE)
poisson_reg(d2$prep.blood.sometimes, d2$southeast)

## Q17c: How Often Tested for STIs Urethral
##       1:4: always, sometimes, rarely, never
table(d2$PREP_STIURETHFREQ, useNA = "always")

# Recoding to always/sometimes vs. rarely/never urethral STI testings
d2$prep.ureth.sometimes <- ifelse(d2$PREP_STIURETHFREQ %in% 1:2, 1, 
                                  ifelse(d2$PREP_STIURETHFREQ %in% 3:4, 0, NA))
table(d2$prep.ureth.sometimes, d2$PREP_STIURETHFREQ)
addmargins(table(d2$prep.ureth.sometimes))

# Descriptive tables by exposure
addmargins(table(d2$age_cat, d2$prep.ureth.sometimes))
addmargins(table(d2$race.cat, d2$prep.ureth.sometimes))
addmargins(table(d2$insurance, d2$prep.ureth.sometimes))
addmargins(table(d2$HHINCOME, d2$prep.ureth.sometimes))
addmargins(table(d2$DIVCODE, d2$prep.ureth.sometimes))
addmargins(table(d2$southeast, d2$prep.ureth.sometimes))

# Create indicator for exposure at site
d2$exp.ureth <- ifelse(d2$anyIAI == 1, 1,
                       ifelse(d2$anyIOI ==1, 1, 0))
addmargins(table(d2$exp.ureth, d2$prep.ureth.sometimes))

# Unadjusted Poisson regression models 
poisson_reg(d2$prep.ureth.sometimes, d2$age_cat)
poisson_reg(d2$prep.ureth.sometimes, d2$race.cat)
poisson_reg(d2$prep.ureth.sometimes, d2$insurance)
poisson_reg(d2$prep.ureth.sometimes, d2$HHINCOME)
poisson_reg(d2$prep.ureth.sometimes, d2$DIVCODE)
poisson_reg(d2$prep.ureth.sometimes, d2$southeast)
poisson_reg(d2$prep.ureth.sometimes, d2$exp.ureth)

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
addmargins(table(d2$DIVCODE, d2$prep.throat.sometimes))
addmargins(table(d2$southeast, d2$prep.throat.sometimes))
addmargins(table(d2$race.cat, d2$prep.throat.sometimes))
addmargins(table(d2$age_cat, d2$prep.throat.sometimes))
addmargins(table(d2$insurance, d2$prep.throat.sometimes))
addmargins(table(d2$anyROI, d2$prep.throat.sometimes))
addmargins(table(d2$HHINCOME, d2$prep.throat.sometimes))

# Unadjusted Poisson regression models
poisson_reg(d2$prep.throat.sometimes, d2$age_cat)
poisson_reg(d2$prep.throat.sometimes, d2$race.cat)
poisson_reg(d2$prep.throat.sometimes, d2$insurance)
poisson_reg(d2$prep.throat.sometimes, d2$HHINCOME)
poisson_reg(d2$prep.throat.sometimes, d2$DIVCODE)
poisson_reg(d2$prep.throat.sometimes, d2$southeast)
poisson_reg(d2$prep.throat.sometimes, d2$anyROI)

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
addmargins(table(d2$southeast, d2$prep.rect.sometimes))
addmargins(table(d2$race.cat, d2$prep.rect.sometimes))
addmargins(table(d2$age_cat, d2$prep.rect.sometimes))
addmargins(table(d2$insurance, d2$prep.rect.sometimes))
addmargins(table(d2$anyRAI, d2$prep.rect.sometimes))
addmargins(table(d2$HHINCOME, d2$prep.rect.sometimes))

# Unadjsuted Poisson regression models
poisson_reg(d2$prep.rect.sometimes, d2$age_cat)
poisson_reg(d2$prep.rect.sometimes, d2$race.cat)
poisson_reg(d2$prep.rect.sometimes, d2$insurance)
poisson_reg(d2$prep.rect.sometimes, d2$HHINCOME)
poisson_reg(d2$prep.rect.sometimes, d2$DIVCODE)
poisson_reg(d2$prep.rect.sometimes, d2$southeast)
poisson_reg(d2$prep.rect.sometimes, d2$anyRAI)


# Table 5 -----------------------------------------------------------------

# Main predictor: Southeast
# Outcomes: blood, urogenital, rectal, & pharyngeal STI testing
# Multivariable associations
## Adjusted for age, race/ethnicity, annual household income, and exposure at site (where applicable)

# Poisson regression model (adjusted / control for race/eth, age, income)
# E: Southeast  O: Syphilis testing
poisson.adj1 <- glm (prep.blood.sometimes ~ southeast + age + race.cat + as.factor(HHINCOME), data = d2, family = "poisson")
pois.se <- diag(vcovHC(poisson.adj1, type = "HC"))^0.5
pois.ll <- coef(poisson.adj1) - 1.96*pois.se
pois.ul <- coef(poisson.adj1) + 1.96*pois.se
pois.rr <- coef(poisson.adj1)
round(cbind(exp(pois.rr), exp(pois.ll), exp(pois.ul)),2)

# Poisson regression model (adjusted / control for race/eth, age, income, exposure at site)
# E: Southeast  O: Urethral STI testing
poisson.adj2 <- glm (prep.ureth.sometimes ~ southeast + age + race.cat + exp.ureth + as.factor(HHINCOME), data = d2, family = "poisson")
pois.se <- diag(vcovHC(poisson.adj2, type = "HC"))^0.5
pois.ll <- coef(poisson.adj2) - 1.96*pois.se
pois.ul <- coef(poisson.adj2) + 1.96*pois.se
pois.rr <- coef(poisson.adj2)
round(cbind(exp(pois.rr), exp(pois.ll), exp(pois.ul)),2)

# Poisson regression model (adjusted / control for race/eth, age, income, exposure at site)
# E: Southeast  O: Rectal STI testing
poisson.adj3 <- glm (prep.rect.sometimes ~ southeast + age + race.cat + anyRAI + as.factor(HHINCOME), data = d2, family = "poisson")
pois.se <- diag(vcovHC(poisson.adj3, type = "HC"))^0.5
pois.ll <- coef(poisson.adj3) - 1.96*pois.se
pois.ul <- coef(poisson.adj3) + 1.96*pois.se
pois.rr <- coef(poisson.adj3)
round(cbind(exp(pois.rr), exp(pois.ll), exp(pois.ul)),2)

# Poisson regression model (adjusted / control for race/eth, age, income, exposure at site)
# E: Southeast  O: Pharyngeal STI testing
poisson.adj4 <- glm (prep.throat.sometimes ~ southeast + age + race.cat + anyROI + as.factor(HHINCOME), data = d2, family = "poisson")
pois.se <- diag(vcovHC(poisson.adj4, type = "HC"))^0.5
pois.ll <- coef(poisson.adj4) - 1.96*pois.se
pois.ul <- coef(poisson.adj4) + 1.96*pois.se
pois.rr <- coef(poisson.adj4)
round(cbind(exp(pois.rr), exp(pois.ll), exp(pois.ul)),2)
