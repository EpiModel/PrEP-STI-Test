
names(d)
d$EVERTEST
d$TEST2YRS

table(d$EVERTEST)
table(d$EVERTEST, d$TEST2YRS, useNA = "always")

summary(d$TEST2YRS/2)
(1/(mean(d$TEST2YRS/2, na.rm = TRUE)))*52

evertest2 <- ifelse(d$EVERTEST %in% c(7, 9), NA, d$EVERTEST)
mod <- glm(evertest2 ~ age, data = d, family = binomial())
summary(mod)

dat <- data.frame(age = 15:65)
pred <- predict(mod, newdata = dat, type = "response")
cbind(dat, pred)

table(d$TSTP12M)
d$RCNTSTMONTH
d$RCNTSTYEAR

anytest.2y <- ifelse(d$TEST2YRS > 0, 1, 0)
anytest.2y[d$EVERTEST == 0] <- 0
table(anytest.2y)
table(anytest.2y, d$EVERTEST)

summary(anytest.2y)

d$race.cat3 <- rep(NA, nrow(d))
d$race.cat3[d$race.cat == "black"] <- 1
d$race.cat3[d$race.cat == "hispanic"] <- 2
d$race.cat3[d$race.cat %in% c("white", "other")] <- 3

mod <- glm(anytest.2y ~ as.factor(race.cat3), data = d, family = binomial())
summary(mod)

dat <- data.frame(race.cat3 = 1:3)
pred <- predict(mod, newdata = dat, type = "response")
cbind(dat, pred)

# solve for weekly testing rate

1-(1-0.0124)^104

# black: 0.01325
# hisp: 0.0125
# white: 0.0124

# plan:
# input above as base testing rates
# these apply to late.tester = 0
# also allow late.tester = 1, status = 0 people to test at rate 1/12 years
# then late.tester = 1, status = 1 people test at rate 1/aids.test.int
# calibrate base testing rates for everyone to match above predictions per 2 years given influence of late.testers
# add diag.stage attr for hiv test mod
# calibrate late tester actually to match prop diagnosed in AIDS stage

