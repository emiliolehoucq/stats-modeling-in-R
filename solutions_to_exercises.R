library(foreign)
library(pastecs)
library(lmtest)
library(lattice)
library(lme4)
library(nlme)

# 1

gss_2006 <- read.dta("GSS2006.dta")

# Descriptive statistics

# 2

summary(gss_2006$socsci)
gss_2006$socsci <- ifelse(gss_2006$socsci == "very scientific", 3, 
                          ifelse(gss_2006$socsci == "pretty scientific", 2, 
                                 ifelse(gss_2006$socsci == "not too scientific", 1, 
                                        ifelse(gss_2006$socsci == "not scientific at all", 0, NA))))
gss_2006 <- gss_2006[!is.na(gss_2006$socsci), ]

summary(gss_2006$partyid)
gss_2006$partyid <- factor(gss_2006$partyid, levels = c("strong democrat", "not str democrat", "ind,near dem", "independent", "ind,near rep", "not str republican", "strong republican"))
gss_2006 <- gss_2006[!is.na(gss_2006$partyid), ]

summary(gss_2006$age)
gss_2006 <- gss_2006[!is.na(gss_2006$age), ]

summary(gss_2006$sex)
gss_2006$sex <- factor(gss_2006$sex, levels = c("male", "female"))

stat.desc(gss_2006$socsci)
stat.desc(gss_2006$age)

# 3

mean <- tapply(gss_2006$socsci, gss_2006$partyid, mean)
standard_deviation <- tapply(gss_2006$socsci, gss_2006$partyid, sd)
number_of_observations <- tapply(gss_2006$socsci, gss_2006$partyid, length)
round(cbind(mean, standard_deviation, number_of_observations), digits = 6)

mean <- tapply(gss_2006$socsci, gss_2006$sex, mean)
standard_deviation <- tapply(gss_2006$socsci, gss_2006$sex, sd)
number_of_observations <- tapply(gss_2006$socsci, gss_2006$sex, length)
round(cbind(mean, standard_deviation, number_of_observations), digits = 6)

# 4

hist(gss_2006$socsci)

# 5

boxplot(socsci~partyid,data=gss_2006)
boxplot(socsci~sex,data=gss_2006)
plot(socsci~age, data = gss_2006)
boxplot(socsci~age,data=gss_2006)

# 6

cor(gss_2006$age, gss_2006$socsci)

# Modeling and extracting information in linear regression

# 8

ols <- lm(socsci~age,data=gss_2006)
summary(ols)
ols_2 <- lm(socsci~age+relevel(partyid, ref = "independent"),data=gss_2006)
summary(ols_2)

# 9

plot(gss_2006[gss_2006$sex == "male", ]$socsci, gss_2006[gss_2006$sex == "male", ]$age)
par(new = TRUE, col = "red")
plot(gss_2006[gss_2006$sex == "female", ]$socsci, gss_2006[gss_2006$sex == "female", ]$age)

ols_3 <- lm(socsci~age*as.factor(sex)+relevel(partyid, ref = "independent"),data=gss_2006)
summary(ols_3)

# 10

res <- ols_2$residuals
pred <- ols_2$fitted.values

# 11

confint(ols_2, level = 0.95)
predict(lm(socsci~age+relevel(partyid, ref = "strong republican"),data=gss_2006), data.frame(age=40, partyid="strong republican"), interval="confidence", level = 0.95, se.fit=TRUE)
predict(ols_2, data.frame(age=40, partyid="independent"),  interval="prediction", level = 0.95, se.fit=TRUE)

# 12

alpha = 0.05
n = dim(gss_2006)[1]
ci <- predict(ols_2, data.frame(age=40, partyid="independent"), interval="confidence", level= 1-alpha, se.fit=TRUE)
yh.hat <- ci$fit[1]
se.yh.hat <- ci$se.fit
w <- sqrt(2*qf(1-alpha, 2, n-2))
lower_bound <- yh.hat - w*se.yh.hat
upper_bound <- yh.hat + w*se.yh.hat
band <- c(lower_bound, upper_bound)
band

# Diagnostics in linear regression

# 13

std_res <- rstandard(ols_2)
qqnorm(std_res, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(std_res)

# 14

plot(res ~ pred, xlab="Fitted", ylab="Residual", main="Residual plot against fitted values")
abline(h=0)
plot(res ~ gss_2006$age, xlab="X", ylab="Residual", main="Residual plot against age")
abline(h=0)

# 15

VIF <- 1/(1-summary(lm(age~relevel(partyid, ref = "independent"),data=gss_2006))$r.squared)
VIF

# 16

bptest(ols_2, studentize = FALSE)

# 17

p <- 3 # numer of parameters
case <- c(1:n) # n defined above
plot(case, rstudent(ols_2), type="l", xlab="Case Numbers", 
     ylab="Studentized Deleted Residuals", main="Test for Outlying Y Values")
text(case, rstudent(ols_2), case)
alpha <- 0.05
crit <- qt(1-alpha/(2*n), n-p-1)
which(abs(rstudent(ols_2)) >=crit ) # There's no evidence of outlying Y observations

# Logistic regression

# 18

rm(gss_2006)

gss_2006 <- read.dta("/Users/emiliolehoucqmazuera/Google Drive/Teaching/Statistical modeling in R/Workshop materials/GSS2006.dta")

gss_2006$socsci <- ifelse(gss_2006$socsci %in% c("very scientific", "pretty scientific"), 1, 0)
gss_2006 <- gss_2006[!is.na(gss_2006$socsci), ]

# 19

logit <- glm(socsci~age+relevel(partyid, ref = "independent"),data=gss_2006, family = "binomial")
summary(logit)

# 20

exp(coef(logit))

# 21

plot(socsci~age, data=gss_2006)
lines(gss_2006$age[order(gss_2006$age)], logit$fitted[order(gss_2006$age)], 
      type="l", col="red")
title(main="Data with Fitted Logistic Regression Line")

data.smooth <- predict(loess(socsci~age, data=gss_2006, span=0.75))
points(gss_2006$age[order(gss_2006$age)], data.smooth[order(gss_2006$age)], 
       type="b",lty=2, col="green")
legend(5,0.9, c("logistic","loess smooth"), col=c("red", "green"), lty=c(1:2))

# Hierarchical modeling

# 36

wvs <- read.dta("WVS6.dta")

table(wvs$V197)
typeof(wvs$V197)
wvs$V197 <- as.numeric(wvs$V197)
wvs$V197 <- ifelse(wvs$V197 < 11, wvs$V197, NA)
table(wvs$V197)

table(wvs$V248)
typeof(wvs$V248)
wvs$V248 <- as.numeric(wvs$V248)
wvs$V248 <- ifelse(wvs$V248 < 13, wvs$V248, NA)
table(wvs$V248)
wvs$educ <- ifelse(wvs$V248 == 1, 0, 
                   ifelse(wvs$V248 %in% c(2, 3), 1,
                          ifelse(wvs$V248 %in% c(4:9), 2, NA)))
table(wvs$educ)
wvs$educ <- factor(wvs$educ, levels = c(0, 1, 2))

table(wvs$V2)

# 37

xyplot(V197 ~ V248 | as.factor(V2), wvs,
       col.line = 'black',
       type = c("p", "r"),
       main = 'Variability in valuation of sci and tech ~ educ relationship')

# 38

rirs <- lmer(V197 ~ 1 + relevel(educ, ref = "2") + (1 + relevel(educ, ref = "2") | V2), data = wvs)
summary(rirs)
rirs_2 <- lme(V197 ~ 1 + relevel(educ, ref = "2"), random = ~ 1 + relevel(educ, ref = "2") | V2, data = wvs, na.action = na.exclude)
summary(rirs_2)

# 39

table(wvs$V9)
wvs$V9 <- as.numeric(wvs$V9)
wvs$V9 <- ifelse(wvs$V9 < 5, wvs$V9, NA)
table(wvs$V9)
wvs$rel_grand_mean_centered <- wvs$V9 - mean(wvs$V9, na.rm=TRUE)

fsltv <- lmer(V197 ~ 1 + relevel(educ, ref = "2") + rel_grand_mean_centered + (1 | V2), data = wvs)
summary(fsltv)
fsltv_2 <- lme(V197 ~ 1 + relevel(educ, ref = "2") + rel_grand_mean_centered, random = ~ 1 | V2, data = wvs, na.action = na.exclude)
summary(fsltv_2)

# 40

rscli <- lmer(V197 ~ 1 + relevel(educ, ref = "2")*rel_grand_mean_centered + (0 + relevel(educ, ref = "2") |V2), data = wvs)
summary(rscli)
rscli_2 <- lme(V197 ~ 1 + relevel(educ, ref = "2")*rel_grand_mean_centered, random = ~ 0 + relevel(educ, ref = "2") | V2, data = wvs, na.action = na.exclude)
summary(rscli_2)

# 41

wvs$sci_tech_val <- ifelse(wvs$V197 %in% c(1:5), 0, 1)
table(wvs$sci_tech_val)

logit <- glmer(sci_tech_val ~ relevel(educ, ref = "2") + (1 | V2), data = wvs, control = glmerControl(optimizer = "Nelder_Mead"),
               family = binomial(link="logit"))
summary(logit)
