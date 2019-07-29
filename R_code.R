library(pastecs)
library(lm.beta)
library(lmtest)
library(foreign)
library(lattice)
library(lme4)
library(nlme)
library(survival)
library(dplyr)
library(ggfortify)
library(survminer)
library(rms)

data <- iris

# Exploratory data analysis

## Univariate descriptive statistics

stat.desc(data$Sepal.Width)

## Descriptive statistics by groups 

mean <- tapply(data$Sepal.Length, data$Sepal.Width, mean)
standard_deviation <- tapply(data$Sepal.Length, data$Sepal.Width, sd)
number_of_observations <- tapply(data$Sepal.Length, data$Sepal.Width, length)
round(cbind(mean, standard_deviation, number_of_observations), digits = 6)

## Histogram

hist(data$Sepal.Width)

## Scatter plot

plot(data$Sepal.Length, data$Sepal.Width)

## Stacking plots (without ggplot2)

plot(data[data$Species == "setosa", ]$Sepal.Length, data[data$Species == "setosa", ]$Sepal.Width)
par(new = TRUE, col = "red")
plot(data[data$Species == "versicolor", ]$Sepal.Length, data[data$Species == "versicolor", ]$Sepal.Width)
par(new = TRUE, col = "green")
plot(data[data$Species == "virginica", ]$Sepal.Length, data[data$Species == "virginica", ]$Sepal.Width)

## Scatter plot matrix

pairs(data[,c(1:4)])

## correlation matrix

cor(data[,c(1:4)])

# Linear regression

## Linear regression syntax

ols <- lm(Sepal.Width ~ Sepal.Length + as.factor(Species), data = data)
summary(ols)

ols_quadratic <- lm(Sepal.Width ~ Sepal.Length + I(Sepal.Length^2) + as.factor(Species), data = data)
summary(ols_quadratic)

ols_interaction <- lm(Sepal.Width ~ Sepal.Length*as.factor(Species), data = data)
summary(ols_interaction)

ols_q_i <- lm(Sepal.Width ~ Sepal.Length*as.factor(Species) + I(Sepal.Length^2), data = data)
summary(ols_q_i)

## Plot the regression line

## Obtaining the residuals

res <- ols$residuals

## Obtaining the fitted values

pred <- ols$fitted.values

## Obtaining the ANOVA table

anova(ols)

## Obtaining the variance-covariance matrix

vcov(ols)

## Obtaining confidence intervals for the coefficients

confint(ols, level = 0.95)

## Obtaining confidence interval of the mean response

predict(ols, data.frame(Sepal.Length=4.0, Species="setosa"), interval="confidence", level = 0.95, se.fit=TRUE)

## Obtaining prediction limits for new observation

predict(ols, data.frame(Sepal.Length=4.0, Species="setosa"),  interval="prediction", level = 0.95, se.fit=TRUE)

## Obtaining confidence band for the entire regression line

alpha = 0.05
n = dim(iris)[1]
ci <- predict(ols, data.frame(Sepal.Length=4.0, Species="setosa"), interval="confidence", level= 1-alpha, se.fit=TRUE)
yh.hat <- ci$fit[1]
se.yh.hat <- ci$se.fit
w <- sqrt(2*qf(1-alpha, 2, n-2))
lower_bound <- yh.hat - w*se.yh.hat
upper_bound <- yh.hat + w*se.yh.hat
band <- c(lower_bound, upper_bound)
band

## Standardized regression

lm.beta(ols)

# Model selection
## General linear test approach

anova(ols_q_i, ols_interaction)

anova(ols_q_i, ols_quadratic)

anova(ols_quadratic, ols)

anova(ols_interaction, ols)

## AIC, BIC, and Adjusted R2

aic <- c(AIC(ols_quadratic), AIC(ols_interaction))
bic <- c(BIC(ols_quadratic), BIC(ols_interaction))
ar2 <- c(summary(ols_quadratic)$adj.r.squared, summary(ols_interaction)$adj.r.squared)
cbind(aic, bic, ar2)

## Step wise selection

base <- lm(Sepal.Width ~ 1, data=data)
retailer_quadratic <- lm(Sepal.Width ~ . + I(Sepal.Length^2), data=data)
retailer_interaction <- lm(Sepal.Width ~ . + Sepal.Length*Species, data=data)
step_1 <- step(base, scope = list(upper=retailer_quadratic, lower= ~1), direction = "both", trace=TRUE)
step_2 <- step(base, scope = list(upper=retailer_interaction, lower= ~1), direction = "both", trace=TRUE)

summary(step_1)

summary(step_2)

# Diagnostics

## Normal probability plot

std_res <- rstandard(ols_quadratic)
qqnorm(std_res, 
     ylab="Standardized Residuals", 
     xlab="Normal Scores") 
qqline(std_res)

## Residual plots

par(mfrow=c(2,2))
plot(res ~ pred, xlab="Fitted", ylab="Residual", main="Residual plot against fitted values")
abline(h=0)
plot(res ~ data$Sepal.Length, xlab="X", ylab="Residual", main="Residual plot against X")
abline(h=0)

## Test for multicollinearity

# See also function vif() in package "car"
VIF <- rep(0,2)
VIF[1] <- 1/(1-summary(lm(Species ~ Sepal.Length, data = data))$r.squared)
VIF[2] <- 1/(1-summary(lm(Sepal.Length ~ as.factor(Species), data = data))$r.squared)
VIF

## Test for heteroskedasticity

bptest(ols_quadratic, studentize = FALSE)

## Test for outlying Y observations--studentized deleted residual

p <- 5 # numer of parameters
case <- c(1:n) # n defined above
plot(case, rstudent(ols_quadratic), type="l", xlab="Case Numbers", 
     ylab="Studentized Deleted Residuals", main="Test for Outlying Y Values")
text(case, rstudent(ols_quadratic), case)
alpha <- 0.05
crit <- qt(1-alpha/2n, n-p-1)
which(abs(rstudent(ols_quadratic)) >=crit ) # Here there's no evidence of outlying Y observations

## Test for outlying X observations--hat matrix leverage values

leverage <- hatvalues(ols_quadratic)
plot(case, leverage, type="l", xlab="Case Numbers", 
     ylab="Leverage", main="Test for Outlying X Values")
text(case, leverage, case)
abline(h=0.5, col=2)
X_out <- which(leverage>0.5)
leverage[X_out] # Here there's no outlying X observations

## Tests for influential observations
## Cook's distance

plot(case, cooks.distance(ols_quadratic), type="l", xlab="Case Numbers", 
     ylab="Cook's Distance", main = "Test for Influential Values: Cook's Distance")
text(case, cooks.distance(ols_quadratic))

inf_obs <- which(cooks.distance(ols_quadratic)>0.5)
inf_obs

## DFFITS

plot(case, dffits(ols_quadratic), type="l", xlab="Case Numbers", 
     ylab="DFFITS", main = "Test for Influential Values: DFFITS")
text(case, dffits(ols_quadratic))

inf_obs2 <- which(abs(dffits(ols_quadratic))>2/sqrt(p/n))
inf_obs2

## DFBETAS

inf_obs3 <- which(abs(dfbeta(ols_quadratic))>2/sqrt(n))
inf_obs3

# Generalized linear models

## logistic regression

data$Sepal.Width_binary <- ifelse(data$Sepal.Width >= median(data$Sepal.Width), 1, 0)
logit <- glm(Sepal.Width_binary ~ Sepal.Length + as.factor(Species), data = data, family = "binomial")
summary(logit)

## Plot the logistic regression line and smoothing line

plot(Sepal.Width_binary~Sepal.Length, data=data)
lines(data$Sepal.Length[order(data$Sepal.Length)], logit$fitted[order(data$Sepal.Length)], 
       type="l", col="red")
title(main="Data with Fitted Logistic Regression Line")

data.smooth <- predict(loess(Sepal.Width_binary~Sepal.Length, data=data, span=0.75))
points(data$Sepal.Length[order(data$Sepal.Length)], data.smooth[order(data$Sepal.Length)], 
      type="b",lty=2, col="green")
legend(5,0.9, c("logistic","loess smooth"), col=c("red", "green"), lty=c(1:2))

## Exponentiated coefficients

exp(coef(logit))

# Poisson and Quasi-Poisson Regression
data(epil)
summary(epil)
df=epil[epil$V4 == 1,]
df <- subset(df, select = -c(V4,period))

sapply(df, class)
ggplot(df,aes(x=df$y))+ 
  geom_histogram(binwidth = 1, center = 0.5) +
  scale_x_continuous(breaks=seq(1,max(df$y), by = 5))+
  ylab("Count")+ xlab("data")+
  ggtitle("Histogram plot of the number of epileptic seizures after the last 2 weeks")

### get column names
print(colnames(df))

model = glm(y~ 1, family=poisson(link=log),data=df)
summary(model)

### get coefficients
print(coef(model))

### print fitted model values
print(data.frame(df$y,model$fitted))

### print model predictors 
model$linear.predictors
exp(model$linear.predictors)


### Hypothesis test for goodness of fit
print(1-pchisq(model$deviance,model$df.residual))

#plot data comparison
df_original = data.frame(data=df$y)
df_fitted = data.frame(data=model$fitted)
plot(df_original$data,df_fitted$data,ylabel='fit data',xlabel='original data')


### add a Covariate to the fit -- treatment
model = glm(y~ 1 +trt, family=poisson(link=log),df)
summary(model)

print(1-pchisq(model$deviance,model$df.residual))

df_fitted = data.frame(data=model$fitted)
df_fitted$name = "fitted"

plot(df_original$data,df_fitted$data)


### add a Covariate to the fit -- treatment, age
model = glm(y ~ 1 +trt*age, family=poisson(link=log),df)
summary(model)

print(1-pchisq(model$deviance,model$df.residual))

df_fitted = data.frame(data=model$fitted)
df_fitted$name = "fitted"

combined=rbind(df_original,df_fitted)

plot(df_original$data,df_fitted$data)


### Overdispersion might be present -- try Quasipossion  
model = glm(y ~ 1 +trt*age, family=quasipoisson(link=log),df)
summary(model)

print(1-pchisq(model$deviance,model$df.residual))

df_fitted = data.frame(data=model$fitted)
df_fitted$name = "fitted"

combined=rbind(df_original,df_fitted)

plot(df_original$data,df_fitted$data)


# Hierarchical modeling

student_data <- read.csv("~/Downloads/hsb1.csv")
school_data  <- read.csv("~/Downloads/hsb2.csv")
student_data$ses_grandmean <- student_data$ses - mean(student_data$ses) # Grand-mean centered student SES 
school_data$sm_ses_grandmean <- school_data$meanses - mean(school_data$meanses) # Grand-mean centered school SES

data <- merge(student_data, school_data, by = "id")

ses_group_mean <- aggregate(data$ses, list(data$id), FUN = mean, data = data) # Group-mean centered student SES
names(ses_group_mean)<- c('id','groupmeanSES')
data <- merge(data, ses_group_mean, by = "id")

groups <- unique(data$id)[sample(1:160,20)]
subset <- data[data$id%in%groups, ]

xyplot(mathach ~ ses | as.factor(id), subset,
       col.line = 'black',
       type = c("p", "r"),
       main = 'Variability in Math Achievement ~ SES Relationship')

xyplot(mathach ~ ses |as.factor(id), subset,
                        col.line = 'black',
                        type = c("p", "smooth"),
                        main = 'Variability in Math Achievement ~ SES Relationship')

xyplot(mathach ~ ses, subset,
       type = c("p", "smooth"),
       group = data$id,
       main = 'Variability in Math Achievement ~ SES Relationship')

unconditional <- lmer(mathach ~ 1 + (1|id), data = data)
summary(unconditional) # on p-values in nlme: https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html
confint(unconditional) # you can also just calculate an approximate 95% confidence interval yourself: estimate +/- 2(SE) (in this case: 12.64 +/- 2(0.24))
unconditional_2 <- lme(mathach ~ 1, random = ~ 1 | id, data = data)
summary(unconditional_2)

random_intercept_fixed_slope <- lmer(mathach ~ 1 + groupmeanSES + (1|id), data = data)
summary(random_intercept_fixed_slope)
confint(random_intercept_fixed_slope)
random_intercept_fixed_slope_2 <- lme(mathach ~ 1 + groupmeanSES, random = ~ 1 | id, data = data)
summary(random_intercept_fixed_slope_2)

random_intercept_random_slope <- lmer(mathach ~ 1 + groupmeanSES + (1 + groupmeanSES|id), data = data)
summary(random_intercept_random_slope)
random_intercept_random_slope_2 <- lme(mathach ~ 1 + groupmeanSES, random = ~ 1 + groupmeanSES | id, data = data)
summary(random_intercept_random_slope_2)

fixed_intercept_random_slope <- lmer(mathach ~ 1 + groupmeanSES + (0 + groupmeanSES|id), data = data)
summary(fixed_intercept_random_slope)
fixed_intercept_random_slope_2 <- lme(mathach ~ 1 + groupmeanSES, random = ~ 0 + groupmeanSES | id, data = data)
summary(fixed_intercept_random_slope_2)

fixed_slope_level_two_variable <- lmer(mathach ~ 1 + groupmeanSES + sm_ses_grandmean + (1|id), data = data)
summary(fixed_slope_level_two_variable)
fixed_slope_level_two_variable_2 <- lme(mathach ~ 1 + groupmeanSES + sm_ses_grandmean, random = ~ 1 | id, data = data)
summary(fixed_slope_level_two_variable_2)

random_slope_level_two_variable <- lmer(mathach ~ 1 + groupmeanSES + sm_ses_grandmean + (1 + groupmeanSES|id), data = data)
summary(random_slope_level_two_variable)
random_slope_level_two_variable_2 <- lme(mathach ~ 1 + groupmeanSES + sm_ses_grandmean, random = ~ 1 + groupmeanSES | id, data = data)
summary(random_slope_level_two_variable_2)  

fixed_slope_cl_interaction <- lmer(mathach ~ 1 + groupmeanSES*sm_ses_grandmean + (1|id), data = data)
summary(fixed_slope_cl_interaction)
fixed_slope_cl_interaction_2 <- lme(mathach ~ 1 + groupmeanSES*sm_ses_grandmean, random = ~ 1 | id, data = data)
summary(fixed_slope_cl_interaction_2)

random_slope_cl_interaction <- lmer(mathach ~ 1 + groupmeanSES*sm_ses_grandmean + (1 + groupmeanSES|id), data = data)
summary(random_slope_cl_interaction)
random_slope_cl_interaction_2 <- lme(mathach ~ 1 + groupmeanSES*sm_ses_grandmean, random = ~ 1 + groupmeanSES | id, data = data)
summary(random_slope_cl_interaction_2)

logit_random_intercept_and_slope <- glmer(minority ~ groupmeanSES + (1 + groupmeanSES | id), data = data, 
                                    family = binomial(link="logit"))
summary(logit_random_intercept_and_slope)

specified_variance_covariance_matrix_for_random_effects <- lme(mathach ~ 1 + groupmeanSES*sm_ses_grandmean, random = ~ 1 + groupmeanSES | id, 
                                                               correlation = corAR1(), data = data) # just an example, not needed in this case (useful for growth curve models)!
summary(specified_variance_covariance_matrix_for_random_effects)
 
# Survival Analysis
attach(colon)
head(colon)

sapply(colon,class)

###Dichotomize age and nodes. Change data labels to factors

colon_subset_recurrence = colon[colon$etype==1,]
colon_subset_recurrence$age.ds = sapply(colon_subset_recurrence$age, function(x) ifelse(x > 60, 1, 0))
colon_subset_recurrence$age.ds <- factor(colon_subset_recurrence$age.ds, levels= c("0","1"), labels=c("<60",">60"))
colon_subset_recurrence$nodes.ds = sapply(colon_subset_recurrence$nodes, function(x) ifelse(x > 3, 1, 0))
colon_subset_recurrence$nodes.ds <- factor(colon_subset_recurrence$nodes.ds, levels= c("0","1"), labels=c("<3",">3"))

colon_subset_recurrence$sex <- factor(colon_subset_recurrence$sex, levels= c("0","1"), labels=c("F","M"))
colon_subset_recurrence$obstruct <- factor(colon_subset_recurrence$obstruct,levels= c("0","1"), labels=c("no obstruct","obstruct"))
colon_subset_recurrence$adhere <- factor(colon_subset_recurrence$adhere,levels= c("0","1"), labels=c("no adhere","adhere"))

colon_subset_recurrence$perfor <- factor(colon_subset_recurrence$perfor, levels= c("0","1"), labels=c("no perfor","perfor"))
colon_subset_recurrence$differ <- factor(colon_subset_recurrence$differ, levels= c("1","2","3"), labels=c("well","mod","poor"))
colon_subset_recurrence$extent <- factor(colon_subset_recurrence$extent,  levels= c("1","2","3","4"),
                                         labels=c("submucosa", "muscle", "serosa", "contiguous"))
colon_subset_recurrence$surg <- factor(colon_subset_recurrence$surg,levels= c("0","1"), 
                                       labels=c("short","long"))
head(colon_subset_recurrence)

surv <-with(colon_subset_recurrence, Surv(time,status))

# Kalpan-Meier
km_fit <- survfit(surv~1, data=colon_subset_recurrence)
summary(km_fit)
autoplot(km_fit)
ggsurvplot(km_fit, data = colon_subset_recurrence, pval = TRUE)

km_fit <- survfit(surv~1 + obstruct, data=colon_subset_recurrence)
summary(km_fit)
ggsurvplot(km_fit, data = colon_subset_recurrence, pval = TRUE,conf.int = TRUE,
           risk.table = TRUE, ggtheme = theme_bw(),risk.table.col = "strata")

km_fit <- survfit(surv~1 + adhere, data=colon_subset_recurrence)
summary(km_fit)
ggsurvplot(km_fit, data = colon_subset_recurrence, pval = TRUE,conf.int = TRUE)

km_fit <- survfit(surv~1 + adhere + obstruct, data=colon_subset_recurrence)
summary(km_fit)
ggsurvplot(km_fit, data = colon_subset_recurrence, pval = TRUE,
           conf.int = TRUE)

km_fit <- survfit(surv~1 + nodes.ds + obstruct, data=colon_subset_recurrence)
summary(km_fit)
ggsurvplot(km_fit, data = colon_subset_recurrence, pval = TRUE,
           conf.int = TRUE)

km_fit <- survfit(surv~1 + nodes.ds + obstruct + adhere, data=colon_subset_recurrence)
summary(km_fit)
ggsurvplot(km_fit, data = colon_subset_recurrence, pval = TRUE,
           conf.int = TRUE)

#Cox Proportional Hazard
cox <- coxph(Surv(time,status) ~ 1 + obstruct, data=colon_subset_recurrence)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable="obstruct",conf.int = TRUE)
summary(cox)
coef(cox)
test.ph <- cox.zph(cox)
test.ph
ggforest(cox, data = colon_subset_recurrence)


cox <- coxph(Surv(time,status) ~ 1 + obstruct + adhere, data=colon_subset_recurrence)
summary(cox)
coef(cox)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable = "obstruct",conf.int = TRUE)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable = "adhere",conf.int = TRUE)
ggforest(cox, data = colon_subset_recurrence)
test.ph <- cox.zph(cox)
test.ph

cox <- coxph(Surv(time,status) ~ 1 + obstruct + adhere + nodes.ds, data=colon_subset_recurrence)
summary(cox)
coef(cox)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable = "obstruct",conf.int = TRUE)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable = "adhere",conf.int = TRUE)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable = "nodes.ds",conf.int = TRUE)
ggforest(cox, data = colon_subset_recurrence)
test.ph <- cox.zph(cox)
test.ph

cox <- coxph(Surv(time,status) ~ 1 + obstruct + adhere + nodes.ds + extent, data=colon_subset_recurrence)
summary(cox)
coef(cox)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable = "obstruct",conf.int = TRUE)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable = "adhere",conf.int = TRUE)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable = "nodes.ds",conf.int = TRUE)
ggadjustedcurves(cox,data=colon_subset_recurrence,variable = "extent",conf.int = TRUE)
ggforest(cox, data = colon_subset_recurrence)
test.ph <- cox.zph(cox)
test.ph

### create a new subject and see their survival curve

subject_one <- data.frame(obstruct = factor('no obstruct'), adhere = factor('adhere'), nodes.ds = factor('<3'), 
                          extent=factor('serosa'))
prediction_one <- survfit(cox, subject_one, data = colon_subset_recurrence)
ggsurvplot(prediction_one, ylab = "No recurrence probability")
ggsurvplot(prediction_one, fun="cumhaz")

td.coxph <- time.dep.coxph(burn, 'T1', 'D1', 2:4, 'Z1', verbose=F)

# Aalen's additive regression model

aa_fit <- aareg(surv ~1 + obstruct + adhere + nodes.ds + surg + rx + age, data = colon_subset_recurrence)
autoplot(aa_fit)
summary(aa_fit)

#Accelerated failure time models

sr_fit = survreg(surv ~ 1 + obstruct + adhere + nodes.ds, dist="weibull",data=colon_subset_recurrence)
summary(sr_fit)

### create a new subject and see their survival curve

subject_two = list(obstruct = factor('no obstruct'), adhere = factor('adhere'), nodes.ds = factor('<3'))
prediction_two = survfit(sr_fit, subject_two, data = colon_subset_recurrence)
plot(predict(sr_fit, newdata=subject_two,type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),
     col="red",type='l',xlab='time',ylab='Survival probability',main='Weibull')
detach(colon)
