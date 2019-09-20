
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





# Table 2 -----------------------------------------------------------------

# causal question: is HIV screening rate and STI screening rate higher
# in current PrEP users compared to non-current PrEP users and never PrEP users?

table(d$STITEST_2YR, useNA = "always")
d$STITEST_2YR <- ifelse(d$STITEST_2YR == 2015, NA, d$STITEST_2YR)
summary(d$STITEST_2YR)

table(d$STITEST_2YR, d$PREP_REVISED)

table(d$STITEST_2YR_PREP, useNA = "always")
d$STITEST_2YR_PREP <- ifelse(d$STITEST_2YR_PREP == 2000, NA, d$STITEST_2YR_PREP)
summary(d$STITEST_2YR_PREP)

table(d$STITEST_2YR_PREP, d$PREP_REVISED)

table(d$STITEST_2YR_SYMPT, useNA = "always")
table(d$STITEST_2YR, d$STITEST_2YR_SYMPT, useNA = "always")

table(d$STITEST_2YR_NOTIF, useNA = "always")

table(d$STITEST_2YR_SYMPT_PREP, useNA = "always")
table(d$STITEST_2YR_NOTIF_PREP, useNA = "always")

table(d$STIREG, useNA = "always")
table(d$STITESTFREQ, useNA = "always")

table(d$HIVFREQ_PREP, useNA = "always")
table(d$STIFREQ_PREP, useNA = "always")



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
