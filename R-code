library(pastecs)
library(lm.beta)
library(lmtest)
library(foreign)
library(lattice)
library(lme4)
library(nlme)

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

## Test for lack of fit (?????????)

full <- lm(Sepal.Width ~ as.factor(Sepal.Length)*as.factor(I(Sepal.Length^2))*as.factor(Species), data=data)
anova(ols_quadratic, full)

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
VIF[1] <- 1/(1-summary(lm(Sepal.Width ~ Sepal.Length, data = data))$r.squared)
VIF[2] <- 1/(1-summary(lm(Sepal.Width ~ as.factor(Species), data = data))$r.squared)
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
crit <- qt(1-alpha/2/n, n-p-1)
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
```
```{r, echo=TRUE}
inf_obs <- which(cooks.distance(ols_quadratic)>0.5)
inf_obs

## DFFITS

plot(case, dffits(ols_quadratic), type="l", xlab="Case Numbers", 
     ylab="DFFITS", main = "Test for Influential Values: DFFITS")
text(case, dffits(ols_quadratic))
```
```{r, echo=TRUE}
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
