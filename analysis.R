
# PrEP STI Screening Analysis Script

library("ARTnetData")

d <- ARTnet.wide
l <- ARTnet.long

table(d$PREP_REVISED, useNA = "always")
table(d$PREP_CURRENT, useNA = "always")

# from AMIS, ever, PY prep
table(d$PREP_EVER)
table(d$PREP_USED)

table(d$hiv)

table(d$PREP_REVISED, d$survey.year, useNA = "always")

prep.elig <- ifelse((d$RCNTRSLT == 1 & d$EVRPOS == 0) | 
                    (d$RCNTRSLT == 1 & d$EVRPOS == 0 & d$STATUS == 1), 1, 0)
table(prep.elig)

# 1565 NA, but only 483 inelig
table(d$PREP_REVISED, useNA = "always")

table(d$PREP_CURRENT, useNA = "always")
131+659+1+1

table(d$PREPCHKFREQ, useNA = "always")

table(d$PREPCHKFREQ_CURR, useNA = "always")

# 1:4: always, sometimes, rarely, never
table(d$PREP_HIVTESTFREQ, useNA = "always")
d$prep.hiv.always <- ifelse(d$PREP_HIVTESTFREQ %in% 2:4, 0, d$PREP_HIVTESTFREQ)
table(d$prep.hiv.always, useNA = "always")

table(d$race.cat)
mod <- glm(prep.hiv.always ~ age, data = d, family = binomial())
summary(mod)

table(d$PREP_STITHROATFREQ, useNA = "always")
table(d$PREP_STIRECTFREQ, useNA = "always")
d$prep.rect.always <- ifelse(d$PREP_STIRECTFREQ %in% 2:4, 0, d$PREP_STIRECTFREQ)
table(d$prep.rect.always, useNA = "always")

mod <- glm(prep.rect.always ~ as.factor(DIVCODE), data = d, family = binomial())
summary(mod)
plogis(coef(mod))

d$DIVCODE

table(d$PREP_STIURETHFREQ, useNA = "always")
table(d$PREP_BLOODFREQ, useNA = "always")

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

