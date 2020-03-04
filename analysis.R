
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

# Logistic regression model (unadjusted)
# E: Census division  O: Blood STI testing
mod1.blood <- glm(prep.blood.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod1.blood)
round(cbind(exp(coef(mod1.blood)),exp(confint(mod1.blood))),2)

y <- as.data.frame(coef(mod1.blood))
y$alpha <- mod1.blood$coefficients[1]
y$plogis <- plogis(y[,1] + y[,2])
round(y,3)

# Logistic regression model (unadjusted)
# E: Southeast  O: Syphilis testing
mod2.blood <- glm(prep.blood.sometimes ~ as.factor(southeast), data = d2, family = binomial())
summary(mod2.blood)
round(cbind(exp(coef(mod2.blood)),exp(confint(mod2.blood))),2)

round(plogis(sum(coef(mod2.blood))),3)

# Logistic regression model (unadjusted)
# E: Race/Ethnicity  O: Syphilis testing
mod3.blood <- glm(prep.blood.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod3.blood)
round(cbind(exp(coef(mod3.blood)),exp(confint(mod3.blood))),2)

y3 <- as.data.frame(coef(mod3.blood))
y3$alpha <- mod3.blood$coefficients[1]
y3$plogis <- plogis(y3[,1] + y3[,2])
round(y3,3)

# Logistic regression model (unadjusted)
# E: Age category  O: Syphilis testing
mod4.blood <- glm(prep.blood.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod4.blood)
round(cbind(exp(coef(mod4.blood)),exp(confint(mod4.blood))),2)

y4 <- as.data.frame(coef(mod4.blood))
y4$alpha <- mod4.blood$coefficients[1]
y4$plogis <- plogis(y4[,1] + y4[,2])
round(y4,3)

# Logistic regression model (unadjusted)
# E: Insurance  O: Syphilis testing
mod5.blood <- glm(prep.blood.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod5.blood)
round(cbind(exp(coef(mod5.blood)),exp(confint(mod5.blood))),2)

y5 <- as.data.frame(coef(mod5.blood))
y5$alpha <- mod5.blood$coefficients[1]
y5$plogis <- plogis(y5[,1] + y5[,2])
round(y5,3)

# Logistic regression model (unadjusted)
# E: Income  O: Syphilis testing
mod6.blood <- glm(prep.blood.sometimes ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod6.blood)
round(cbind(exp(coef(mod6.blood)),exp(confint(mod6.blood))),2)

y6 <- as.data.frame(coef(mod6.blood))
y6$alpha <- mod6.blood$coefficients[1]
y6$plogis <- plogis(y6[,1] + y6[,2])
round(y6,3)

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

# Logistic regression model (unadjusted)
# E: Census division O: Urethral STI testing
mod1.ureth <- glm(prep.ureth.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod1.ureth)
round(cbind(exp(coef(mod1.ureth)),exp(confint(mod1.ureth))),2)

x <- as.data.frame(coef(mod1.ureth))
x$alpha <- mod1.ureth$coefficients[1]
x$plogis <- plogis(x[,1] + x[,2])
round(x,3)

# Logistic regression model (unadjusted)
# E: Southeast O: Urethral STI testing
mod2.ureth <- glm(prep.ureth.sometimes ~ southeast, data = d2, family = binomial())
summary(mod2.ureth)
round(cbind(exp(coef(mod2.ureth)),exp(confint(mod2.ureth))),2)

plogis(sum(coef(mod2.ureth)))

# Logistic regression model (unadjusted)
# E: Race/ethnicity O: Urethral STI testing
mod3.ureth <- glm(prep.ureth.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod3.ureth)
round(cbind(exp(coef(mod3.ureth)),exp(confint(mod3.ureth))),2)

x3 <- as.data.frame(coef(mod3.ureth))
x3$alpha <- mod3.ureth$coefficients[1]
x3$plogis <- plogis(x3[,1] + x3[,2])
round(x3,3)

# Logistic regression model (unadjusted)
# E: Age category O: Urethral STI testing
mod4.ureth <- glm(prep.ureth.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod4.ureth)
round(cbind(exp(coef(mod4.ureth)),exp(confint(mod4.ureth))),2)

x4 <- as.data.frame(coef(mod4.ureth))
x4$alpha <- mod4.ureth$coefficients[1]
x4$plogis <- plogis(x4[,1] + x4[,2])
round(x4,3)

# Logistic regression model (unadjusted)
# E: Insurance O: Urethral STI testing
mod5.ureth <- glm(prep.ureth.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod5.ureth)
round(cbind(exp(coef(mod5.ureth)),exp(confint(mod5.ureth))),2)

x5 <- as.data.frame(coef(mod5.ureth))
x5$alpha <- mod5.ureth$coefficients[1]
x5$plogis <- plogis(x5[,1] + x5[,2])
round(x5,3)

# Logistic regression model (unadjusted)
# E: Exposure at site O: Urethral STI testing
mod6.ureth <- glm(prep.ureth.sometimes ~ as.factor(exp.ureth), data = d2, family = binomial())
summary(mod6.ureth)
round(cbind(exp(coef(mod6.ureth)),exp(confint(mod6.ureth))),2)

plogis(sum(coef(mod6.ureth)))

# Logistic regression model (unadjusted)
# E: Income O: Urethral STI testing
mod7.ureth <- glm(prep.ureth.sometimes ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod7.ureth)
round(cbind(exp(coef(mod7.ureth)),exp(confint(mod7.ureth))),2)

x7 <- as.data.frame(coef(mod7.ureth))
x7$alpha <- mod7.ureth$coefficients[1]
x7$plogis <- plogis(x7[,1] + x7[,2])
round(x7,3)


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

# Logistic regression model (unadjusted)
# E: Census division  O: Pharyngeal STI testing
mod1.throat <- glm(prep.throat.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod1.throat)
round(cbind(exp(coef(mod1.throat)),exp(confint(mod1.throat))),2)

b <- as.data.frame(coef(mod1.throat))
b$alpha <- mod1.throat$coefficients[1]
b$plogis <- plogis(b[,1] + b[,2])
round(b,3)

# Logistic regression model (unadjusted)
# E: Southeast O: Pharyngeal STI testing
mod2.throat <- glm(prep.throat.sometimes ~ southeast, data = d2, family = binomial())
summary(mod2.throat)
round(cbind(exp(coef(mod2.throat)),exp(confint(mod2.throat))),2)

plogis(sum(coef(mod2.throat)))

# Logistic regression model (unadjusted)
# E: Race/ethnicity O: Pharyngeal STI testing
mod3.throat <- glm(prep.throat.sometimes ~ race.cat, data = d2, family = binomial())
summary(mod3.throat)
round(cbind(exp(coef(mod3.throat)),exp(confint(mod3.throat))),2)

b3 <- as.data.frame(coef(mod3.throat))
b3$alpha <- mod3.throat$coefficients[1]
b3$plogis <- plogis(b3[,1] + b3[,2])
round(b3,3)

# Logistic regression model (unadjusted)
# E: Age category O: Pharyngeal STI testing
mod4.throat <- glm(prep.throat.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod4.throat)
round(cbind(exp(coef(mod4.throat)),exp(confint(mod4.throat))),2)

b4 <- as.data.frame(coef(mod4.throat))
b4$alpha <- mod4.throat$coefficients[1]
b4$plogis <- plogis(b4[,1] + b4[,2])
round(b4,3)

# Logistic regression model (unadjusted)
# E: Insurance O: Pharyngeal STI testing
mod5.throat <- glm(prep.throat.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod5.throat)
round(cbind(exp(coef(mod5.throat)),exp(confint(mod5.throat))),2)

b5 <- as.data.frame(coef(mod5.throat))
b5$alpha <- mod5.throat$coefficients[1]
b5$plogis <- plogis(b5[,1] + b5[,2])
round(b5,3)

# Logistic regression model (unadjusted)
# E: Exposure at pharyngeal site O: Pharyngeal STI testing
mod6.throat <- glm(prep.throat.sometimes ~ as.factor(anyROI),
                    data = d2, family = binomial())
summary(mod6.throat)
round(cbind(exp(coef(mod6.throat)),exp(confint(mod6.throat))),2)

plogis(sum(coef(mod6.throat)))

# Logistic regression model (unadjusted)
# E: Income O: Pharyngeal STI testing
mod7.throat <- glm(prep.throat.sometimes ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod7.throat)
round(cbind(exp(coef(mod7.throat)),exp(confint(mod7.throat))),2)

b7 <- as.data.frame(coef(mod7.throat))
b7$alpha <- mod7.throat$coefficients[1]
b7$plogis <- plogis(b7[,1] + b7[,2])
round(b7,3)

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

# Logistic regression model (unadjusted)
# E: Census division  O: Rectal STI testing
mod1.rectal <- glm(prep.rect.sometimes ~ as.factor(DIVCODE), data = d2, family = binomial())
summary(mod1.rectal)
round(cbind(exp(coef(mod1.rectal)),exp(confint(mod1.rectal))),2)

c <- as.data.frame(coef(mod1.rectal))
c$alpha <- mod1.rectal$coefficients[1]
c$plogis <- plogis(c[,1] + c[,2])
round(c,3)

# Logistic regression model (unadjusted)
# E: Southeast O: Rectal STI testing
mod2.rectal <- glm(prep.rect.sometimes ~ southeast, data = d2, family = binomial())
summary(mod2.rectal)
round(cbind(exp(coef(mod2.rectal)),exp(confint(mod2.rectal))),2)

plogis(sum(coef(mod2.rectal)))


# Logistic regression model (unadjusted)
# E: Race/Ethnicity O: Rectal STI testing
mod3.rectal <- glm(prep.rect.sometimes ~ as.factor(race.cat), data = d2, family = binomial())
summary(mod3.rectal)
round(cbind(exp(coef(mod3.rectal)),exp(confint(mod3.rectal))),2)

c3 <- as.data.frame(coef(mod3.rectal))
c3$alpha <- mod3.rectal$coefficients[1]
c3$plogis <- plogis(c3[,1] + c3[,2])
round(c3,3)

# Logistic regression model (unadjusted)
# E:Age category  O: Rectal STI testing
mod4.rectal <- glm(prep.rect.sometimes ~ as.factor(age_cat), data = d2, family = binomial())
summary(mod4.rectal)
round(cbind(exp(coef(mod4.rectal)),exp(confint(mod4.rectal))),2)

c4 <- as.data.frame(coef(mod4.rectal))
c4$alpha <- mod4.rectal$coefficients[1]
c4$plogis <- plogis(c4[,1] + c4[,2])
round(c4,3)

# Logistic regression model (unadjusted)
# E: Insurance O: Rectal STI testing
mod5.rectal <- glm(prep.rect.sometimes ~ as.factor(insurance), data = d2, family = binomial())
summary(mod5.rectal)
round(cbind(exp(coef(mod5.rectal)),exp(confint(mod5.rectal))),2)

c5 <- as.data.frame(coef(mod5.rectal))
c5$alpha <- mod5.rectal$coefficients[1]
c5$plogis <- plogis(c5[,1] + c5[,2])
round(c5,3)

# Logistic regression model (unadjusted)
# E: Exposure at site  O: Rectal STI testing
mod6.rectal <- glm(prep.rect.sometimes ~ anyRAI, data = d2, family = binomial())
summary(mod6.rectal)
round(cbind(exp(coef(mod6.rectal)),exp(confint(mod6.rectal))),2)

plogis(sum(coef(mod6.rectal)))

# Logistic regression model (unadjusted)
# E: Income O: Rectal STI testing
mod7.rectal <- glm(prep.rect.sometimes ~ as.factor(HHINCOME), data = d2, family = binomial())
summary(mod7.rectal)
round(cbind(exp(coef(mod7.rectal)), exp(confint(mod7.rectal))),2)

c7 <- as.data.frame(coef(mod7.rectal))
c7$alpha <- mod7.rectal$coefficients[1]
c7$plogis <- plogis(c7[,1] + c7[,2])
round(c7,3)


# Table 5 -----------------------------------------------------------------

# Main predictor: Southeast
# Outcomes: blood, urogenital, rectal, & pharyngeal STI testing
# Multivariable associations
## Adjusted for age, race/ethnicity, annual household income, and exposure at site (where applicable)

# Logistic regression model (adjusted / control for race/eth, age, income)
# E: Southeast  O: Blood STI testing
mod.blood.adj <- glm(prep.blood.sometimes ~ southeast + age + race.cat + as.factor(HHINCOME),
                  data = d2, family = binomial())
summary(mod.blood.adj)
round(cbind(exp(coef(mod.blood.adj)),exp(confint(mod.blood.adj))),2)

# Logistic regression model (adjusted / control for race/eth, age, income, and exposure at site)
# E: Southeast  O: Urethral STI testing
mod.ureth.adj <- glm(prep.ureth.sometimes ~ southeast + age + race.cat + exp.ureth + as.factor(HHINCOME),
                  data = d2, family = binomial())
summary(mod.ureth.adj)
round(cbind(exp(coef(mod.ureth.adj)),exp(confint(mod.ureth.adj))),2)

# Logistic regression model (adjusted / control for race/eth, age, income and exposure at site)
# E: Southeast  O: Rectal STI testing
mod.rectal.adj <- glm(prep.rect.sometimes ~ southeast + age + race.cat + anyRAI + as.factor(HHINCOME),
                   data = d2, family = binomial())
summary(mod.rectal.adj)
round(cbind(exp(coef(mod.rectal.adj)),exp(confint(mod.rectal.adj))),2)

# Logistic regression model (adjusted / control for race/eth, age, exposure at site)
# E: Southeast  O: Pharyngeal STI testing
mod.throat.adj <- glm(prep.throat.sometimes ~ southeast + age + race.cat + anyROI + as.factor(HHINCOME),
                   data = d2, family = binomial())
summary(mod.throat.adj)
round(cbind(exp(coef(mod.throat.adj)), exp(confint(mod.throat.adj))),2)






