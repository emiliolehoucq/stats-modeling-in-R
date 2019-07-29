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

hist(data$Sepal.Width,
     xlab='Sepal Width',ylab='Count',
     main= 'Histogram plot of sepal length vs sepal width')
## Scatter plot

plot(data$Sepal.Length, data$Sepal.Width,
     xlab='Sepal length', ylab='Sepal Width',
     main= 'Scatter plot of sepal length vs sepal width')

## Stacking plots (without ggplot2)

plot(data[data$Species == "setosa", ]$Sepal.Length, 
     data[data$Species == "setosa", ]$Sepal.Width, 
     xlab = 'Sepal Length',  ylab= 'Sepal Width',
     xlim = range(as.matrix(data$Sepal.Length)), 
     ylim = range(as.matrix(data$Sepal.Width)),
     main= 'Scatter plot of sepal length vs sepal width')

points(data[data$Species == "versicolor", ]$Sepal.Length,
       data[data$Species == "versicolor", ]$Sepal.Width,
       col = 'yellow')

points(data[data$Species == "virginica", ]$Sepal.Length,
       data[data$Species == "virginica", ]$Sepal.Width, col = 'blue')

## Scatter plot matrix

pairs(data[,c(1:4)])

## correlation matrix

cor(data[,c(1:4)])

# Linear regression

## Linear regression syntax

### General Syntax: Constant Term

ols <- lm(Sepal.Width ~ Sepal.Length + as.factor(Species), data = data)
summary(ols)
print(mean(data$Sepal.Width))

plot(data[data$Species == "setosa", ]$Sepal.Length, 
     data[data$Species == "setosa", ]$Sepal.Width, 
     xlab = 'Sepal Width',  ylab= 'Sepal Length',
     xlim = range(as.matrix(data$Sepal.Length)), 
     ylim = range(as.matrix(data$Sepal.Width)),
     main= 'Scatter plot of sepal length vs sepal width')

coefs = coef(ols)
abline(coefs[1],0)

### General Syntax: Explanatory Variable and Constant Term

ols_quadratic <- lm(Sepal.Width ~ Sepal.Length + I(Sepal.Length^2) + as.factor(Species), data = data)
summary(ols_quadratic)

plot(data[data$Species == "setosa", ]$Sepal.Length, 
     data[data$Species == "setosa", ]$Sepal.Width, 
     xlab = 'Sepal Width',  ylab= 'Sepal Length',
     xlim = range(as.matrix(data$Sepal.Length)), 
     ylim = range(as.matrix(data$Sepal.Width)),
     main= 'Scatter plot of sepal length vs sepal width')

coefs = coef(ols)
abline(coefs[1],coefs[2])

### General Syntax: Factors

ols <- lm(Sepal.Width ~ 1 + Sepal.Length + as.factor(Species), data = data)
summary(ols)

plot(data[data$Species == "setosa", ]$Sepal.Length, 
     data[data$Species == "setosa", ]$Sepal.Width, 
     xlab = 'Sepal Width',  ylab= 'Sepal Length',
     xlim = range(as.matrix(data$Sepal.Length)), 
     ylim = range(as.matrix(data$Sepal.Width)),
     main= 'Scatter plot of sepal length vs sepal width')

points(data[data$Species == "versicolor", ]$Sepal.Length,
       data[data$Species == "versicolor", ]$Sepal.Width,
       col = 'yellow')

points(data[data$Species == "virginica", ]$Sepal.Length,
       data[data$Species == "virginica", ]$Sepal.Width, col = 'blue')
coefs = coef(ols)
abline(coefs[1],coefs[2])
abline(coefs[3] + coefs[1],coefs[2],col='yellow')
abline(coefs[4] +coefs[1],coefs[2],col='blue')

### Advanced Syntax: Nonlinear Regression

ols_quadratic <- lm(Sepal.Width ~ Sepal.Length + I(Sepal.Length^2), data = data)
summary(ols_quadratic)


### Advanced Syntax: Interaction term

ols_interaction <- lm(Sepal.Width ~ Sepal.Length + Petal.Length + Sepal.Length*Petal.Length, data = data)
summary(ols_interaction)

### Advanced Syntax: Non-linear regression and Interaction term 

ols_q_i <- lm(Sepal.Width ~ Sepal.Length*as.factor(Species) + I(Sepal.Length^2), data = data)
summary(ols_q_i)


## Obtaining the residuals

res <- ols$residuals
head(res)


## Obtaining the fitted values

pred <- ols$fitted.values
head(data.frame(pred=pred,orig=data$Sepal.Width ))

## Obtaining the ANOVA table

anova(ols)

## Obtaining the variance-covariance matrix

vcov(ols)

## Obtaining confidence intervals for the coefficients

confint(ols, level = 0.95)

## Obtaining confidence interval of the mean response

predict(ols, data.frame(Sepal.Length=4.0, Species="setosa"),
        interval="confidence", 
        level = 0.95, se.fit=TRUE)

## Obtaining prediction limits for new observation

predict(ols, data.frame(Sepal.Length=4.0, Species="setosa"), 
        interval="prediction", level = 0.95, se.fit=TRUE)

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

plot(res ~ pred, xlab="Fitted", ylab="Residual", main="Residual plot against fitted values")
abline(h=0)
plot(res ~ data$Sepal.Length, xlab="X", ylab="Residual", main="Residual plot against X")
abline(h=0)

## Test for multicollinearity

# See also function vif() in package "car"
VIF <- 1/(1-summary(lm(Sepal.Length ~ as.factor(Species), data = data))$r.squared)
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
crit <- qt(1-alpha/(2*n), n-p-1)
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

# Logistic regression
data <- iris
data$Sepal.Width_binary <- ifelse(data$Sepal.Width >= median(data$Sepal.Width), 1, 0)

## Data 

## Logistic Regression with only the constant term

logit <- glm(Sepal.Width_binary ~ 1, data = data, family = "binomial")
summary(logit)


p_avg <- mean(data$Sepal.Width_binary)
log_odds_avg <- log(p_avg/(1-p_avg))
print(log_odds_avg)

## Logistic Regression with Species  

logit <- glm(Sepal.Width_binary ~ as.factor(Species), data = data, family = "binomial")
summary(logit)

log_odds_avg_fun <- function(data){
  p_avg <- mean(data)
  log_odds_avg <- log(p_avg/(1-p_avg))
  return(log_odds_avg)
}

tapply(data$Sepal.Width_binary,
       data$Species, log_odds_avg_fun)

coefficients<-unname(coef(logit))
print(c(coefficients[1],coefficients[1]+coefficients[2],
        coefficients[1]+coefficients[3]))

## Logistic Regression with continuous variable

## Logistic Regression with continuous variable, Sepal.Length

logit <- glm(Sepal.Width_binary ~ Sepal.Length, 
             data = data, family = "binomial")
summary(logit)

logit <- glm(Sepal.Width_binary ~ Species +Sepal.Length, 
             data = data, family = "binomial")
summary(logit)

plot(data[data$Species == "setosa", ]$Sepal.Length, 
     data[data$Species == "setosa", ]$Sepal.Width_binary, 
     xlim=as.matrix(range(data$Sepal.Length)),
     xlab = 'Sepal Length',  ylab= 'Sepal Width binary',
     main= 'Scatter plot of sepal length vs sepal width')

points(data$Sepal.Length[data$Species == "setosa"],
       logit$fitted[data$Species == "setosa"],  pch=15,
       col="red")

plot(data[data$Species == "versicolor", ]$Sepal.Length,
     data[data$Species == "versicolor", ]$Sepal.Width_binary,
     xlim=as.matrix(range(data$Sepal.Length)),
     xlab = 'Sepal Length',  ylab= 'Sepal Width binary',
     main= 'Scatter plot of sepal length vs sepal width')

points(data$Sepal.Length[data$Species == "versicolor"],
       logit$fitted[data$Species == "versicolor"],  pch=15,
       col="yellow")

plot(data[data$Species == "virginica", ]$Sepal.Length,
     data[data$Species == "virginica", ]$Sepal.Width_binary,
     xlim=as.matrix(range(data$Sepal.Length)),
     xlab = 'Sepal Length',  ylab= 'Sepal Width binary',
     main= 'Scatter plot of sepal length vs sepal width')

points(data$Sepal.Length[data$Species == "virginica"],
       logit$fitted[data$Species == "virginica"],  pch=15,
       col="blue")

## Goodness of Fit

### Deviance

### Saturated Model

p_value = pchisq(logit$deviance, 
                 logit$df.residual, lower.tail = F)
print(p_value)

p_value = pchisq(logit$null.deviance-logit$deviance,
                 logit$df.null-logit$df.residual, lower.tail = F)
print(p_value)

anova(logit,test="Chisq")

# Poisson General Linear Model

attach(bioChemists)
summary(bioChemists)

sapply(bioChemists, class)

bioChemists$kid5 <- factor(bioChemists$kid5, 
                           levels= unique(bioChemists$kid5),
                           labels= unique(bioChemists$kid5))

ggplot(bioChemists,aes(x=bioChemists$art))+ 
  geom_histogram(binwidth = 1, center = 1) +
  scale_x_continuous(breaks=seq(0,max(bioChemists$art), by = 1))+
  ylab("Frequency")+ xlab("data")+
  ggtitle("Histogram plot of the number of articles published by biochemist phd students in last 3 years")

mean(bioChemists$art)
var(bioChemists$art)

## Possion Regression with constant term

poisson_model = glm(art ~ 1, family=poisson(link=log),data=bioChemists)
summary(poisson_model)

print(coef(poisson_model))

print(log(mean(bioChemists$art)))

## Goodness of fit

### Saturated model

p_value = pchisq(poisson_model$deviance,
                 poisson_model$df.residual, lower.tail = F)
print(p_value)

## Possion Regression with martial status covariate

poisson_model = glm(art~1+mar , family=poisson(link=log),data=bioChemists)
summary(poisson_model)

plot(bioChemists$art,ylab='number of articles',xlab = 'Index')
points(poisson_model$fitted[bioChemists$mar=='Single'],col="red")
points(poisson_model$fitted[bioChemists$mar=='Married'],col="blue")

p_value = pchisq(poisson_model$deviance,
                 poisson_model$df.residual, lower.tail = F)
print(p_value)

p_value = pchisq(poisson_model$null.deviance-logit$deviance,
                 poisson_model$df.null-logit$df.residual, lower.tail = F)
print(p_value)

anova(poisson_model,test="Chisq")

##  Possion Regression with martial status and children covariate

poisson_model = glm(art ~ 1 + kid5 + mar,
                    family=poisson(link=log),data=bioChemists)
summary(poisson_model)

plot(bioChemists$art,ylab='number of articles',xlab = 'Index')
single_bioChemists = bioChemists[bioChemists$mar=='Single',]

points(poisson_model$fitted[single_bioChemists$kid5== 0],col="blue",pch=1)
points(poisson_model$fitted[single_bioChemists$kid5== 1],col="yellow",pch=1)
points(poisson_model$fitted[single_bioChemists$kid5== 2],col="red",pch=1)
points(poisson_model$fitted[single_bioChemists$kid5== 3],col="green",pch=1)


plot(bioChemists$art,ylab='number of articles',xlab = 'Index')
mar_bioChemists = bioChemists[bioChemists$mar=='Married',]

points(poisson_model$fitted[mar_bioChemists$kid5== 0],col="blue",pch=2)
points(poisson_model$fitted[mar_bioChemists$kid5== 1],col="yellow",pch=2)
points(poisson_model$fitted[mar_bioChemists$kid5== 2],col="red",pch=2)
points(poisson_model$fitted[mar_bioChemists$kid5== 3],col="green",pch=2)

## Goodness of Fit

### Saturated model 

p_value = pchisq(poisson_model$deviance,
                 poisson_model$df.residual, lower.tail = F)
print(p_value)

### Null model

p_value = pchisq(poisson_model$null.deviance-poisson_model$deviance,
                 poisson_model$df.null-poisson_model$df.residual, lower.tail = F)
print(p_value)

### Anova

anova(poisson_model,test="Chisq")


##  Possion Regression with continuous variables, mentor articles and martial status

poisson_model = glm(art ~ 1 + ment +mar,
                    family=poisson(link=log),data=bioChemists)
summary(poisson_model)

plot(bioChemists$art,ylab='number of articles',xlab = 'Index')

points(poisson_model$fitted,col="blue",pch=1)

## Goodness of Fit

### Saturated model 

p_value = pchisq(poisson_model$deviance,
                 poisson_model$df.residual, lower.tail = F)
print(p_value)

### Null model

p_value = pchisq(poisson_model$null.deviance-poisson_model$deviance,
                 poisson_model$df.null-poisson_model$df.residual, lower.tail = F)
print(p_value)


### Anova

anova(poisson_model,test="Chisq")


# Log-Linear Regression

## Contingency Table

bioChemists$art_binary <- sapply(bioChemists$art,function(x) ifelse(x > 1, 1, 0))
bioChemists$ment_binary <- sapply(bioChemists$ment,function(x) ifelse(x > median(bioChemists$ment), 1, 0))

### One-Way Contingency Table

table(art_relative=bioChemists$art_binary)

### Two-Way Contingency Table

table(art_relative=bioChemists$art_binary,ment=bioChemists$ment_binary)

### Three-Way Contingency Table

table(art_relative=bioChemists$art_binary,ment=bioChemists$ment_binary,
      kid5=bioChemists$kid5)

## Independent Model for two-way contigency table

contigency_table = table(art_relative=bioChemists$art_binary,ment=bioChemists$ment_binary)
contigency_table.df = as.data.frame(contigency_table)
print(contigency_table.df)

log_linear_model_int <- glm(Freq ~ art_relative + ment, 
                            data = contigency_table.df, family = poisson)
summary(log_linear_model_int)

### Goodness of fit

p_value = pchisq(log_linear_model_int$deviance,
                 log_linear_model_int$df.residual, lower.tail = F)
print(p_value)

## Saturated Model for the two-way contingency table 

log_linear_model_sat <- glm(Freq ~ art_relative*ment, 
                            data = contigency_table.df, family = poisson)
summary(log_linear_model_sat)


### Goodness of fit

p_value = pchisq(0,
                 log_linear_model_sat$df.residual, 
                 lower.tail = F)
print(p_value)


## Model Comparison

anova(log_linear_model_int,log_linear_model_sat,test='Chisq')

anova(log_linear_model_sat,test='Chisq')

## Independent Model for the three-way contingency table 

contigency_table = table(art_relative=bioChemists$art_binary,
                         ment=bioChemists$ment_binary,
                         kid5=bioChemists$kid5)
contigency_table.df = as.data.frame(contigency_table)

log_linear_model_int <- glm(Freq ~ art_relative + ment + kid5, 
                            data = contigency_table.df, family = poisson)
summary(log_linear_model_int)

### Goodness of fit

p_value = pchisq(log_linear_model_int$deviance,
                 log_linear_model_int$df.residual, lower.tail = F)
print(p_value)

## Saturated Model


log_linear_model_sat <- glm(Freq ~ art_relative*ment*kid5, 
                            data = contigency_table.df, family = poisson)
summary(log_linear_model_sat)

### Goodness of fit

p_value = pchisq(log_linear_model_sat$deviance,
                 log_linear_model_sat$df.residual, lower.tail = F)
print(p_value)

##  Model Comparison

anova(log_linear_model_int,log_linear_model_sat,test='Chisq')

anova(log_linear_model_sat,test='Chisq')

### Goodness of fit

## Hierarchical modeling

student_data <- read.csv("data/hsb1.csv")
school_data  <- read.csv("data/hsb2.csv")
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

## Subsetting data and converting data

colon_subset_recurrence = colon[colon$etype==1,]
colon_subset_recurrence$age.ds = sapply(colon_subset_recurrence$age,
                                        function(x) ifelse(x > 60, 1, 0))

sapply(colon,class)

colon_subset_recurrence$age.ds <- factor(colon_subset_recurrence$age.ds, 
                                         levels= c("0","1"),
                                         labels=c("<60",">60"))

colon_subset_recurrence$node4 <- factor(colon_subset_recurrence$node4, 
                                        levels= c("0","1"), 
                                        labels=c("<4",">4"))


colon_subset_recurrence$sex <- factor(colon_subset_recurrence$sex,
                                      levels= c("0","1"), labels=c("F","M"))

colon_subset_recurrence$obstruct <- factor(colon_subset_recurrence$obstruct,
                                           levels= c("0","1"),
                                           labels=c("no obstruct","obstruct"))
colon_subset_recurrence$adhere <- factor(colon_subset_recurrence$adhere,
                                         levels= c("0","1"),
                                         labels=c("no adhere","adhere"))
colon_subset_recurrence$perfor <- factor(colon_subset_recurrence$perfor, 
                                         levels= c("0","1"), 
                                         labels=c("no perfor","perfor"))

colon_subset_recurrence$differ <- factor(colon_subset_recurrence$differ,
                                         levels= c("1","2","3"),
                                         labels=c("well","mod","poor"))
colon_subset_recurrence$extent <- factor(colon_subset_recurrence$extent, 
                                         levels= c("1","2","3","4"),
                                         labels=c("submucosa", "muscle", "serosa", "contiguous"))
colon_subset_recurrence$surg <- factor(colon_subset_recurrence$surg,
                                       levels= c("0","1"), 
                                       labels=c("short","long"))


head(colon_subset_recurrence)


## Surv Object

surv <-with(colon_subset_recurrence, Surv(time,status))
head(surv)

# Kalpan-Meier Estimator

##  Kalpan-Meier Estimator for the entire data

km_fit <- survfit(surv~1, data=colon_subset_recurrence)

summary(km_fit,times=c(1,10,20,30,40,50))

ggsurvplot(km_fit, data = colon_subset_recurrence,
           conf.int = TRUE,risk.table = TRUE,
           ggtheme = theme_bw(),
           risk.table.col = "strata")

##  Kalpan-Meier Estimator for the data divided into obstruct and no obstruct

km_fit <- survfit(surv~obstruct, data=colon_subset_recurrence)

summary(km_fit,times=c(1,10,20,30,40,50))

ggsurvplot(km_fit, data = colon_subset_recurrence, 
           pval = TRUE,conf.int = TRUE,
           risk.table = TRUE, ggtheme = theme_bw(),
           risk.table.col = "strata")

p_value <- survdiff(surv~obstruct, data=colon_subset_recurrence)
print(p_value)

##  Kalpan-Meier Estimator for the data divided into adhere and no adhere

km_fit <- survfit(surv~adhere, data=colon_subset_recurrence)

summary(km_fit,times=c(1,10,20,30,40,50))

ggsurvplot(km_fit, data = colon_subset_recurrence, 
           pval = TRUE,conf.int = TRUE,
           risk.table = TRUE, ggtheme = theme_bw(),
           risk.table.col = "strata")

survdiff(surv~adhere,data=colon_subset_recurrence)


##  Kalpan-Meier Estimator for the data divided into (adhere, obstruct), (adhere, no obstruct), (no adhere, obstruct) and (no adhere, no obstruct) 

km_fit <- survfit(surv~adhere + obstruct, data=colon_subset_recurrence)

summary(km_fit,times=c(1,10,20,30,40,50))

ggsurvplot(km_fit, data = colon_subset_recurrence, 
           pval = TRUE,conf.int = TRUE,
           risk.table = TRUE, ggtheme = theme_bw(),
           risk.table.col = "strata")

survdiff(surv~adhere + obstruct,data=colon_subset_recurrence)

# Cox Proportional Hazard

## Cox Proportional Hazard for $X_1 = \text{surg}$

cox <- coxph(surv ~  surg,
             data=colon_subset_recurrence)

summary(cox)
coef(cox)
ggforest(cox, data = colon_subset_recurrence)

### Testing Proportionality Assumption

test.ph <- cox.zph(cox)
print(test.ph)
plot(test.ph)

### Model Selection

anova(cox)

## Cox Proportional Hazard for $X_1 = \text{surg}$, $X_2 = \text{adher}$


cox <- coxph(surv ~  surg + adhere, 
             data=colon_subset_recurrence)
summary(cox)
coef(cox)
ggforest(cox, data = colon_subset_recurrence)

### Testing Proportionality Assumption

test.ph <- cox.zph(cox)
print(test.ph)
par(mfrow=c(1,2))   
plot(test.ph)

### Model Selection

anova(cox)

## Cox Proportional Hazard for $X_1 = \text{surg}$, $X_2 = \text{adher}$, $X_3 = \text{nodes}$

cox <- coxph(surv ~ surg + adhere + nodes, 
             data=colon_subset_recurrence)
summary(cox)
coef(cox)
ggforest(cox, data = colon_subset_recurrence)

### Testing Proportionality Assumption

test.ph <- cox.zph(cox)
test.ph
par(mfrow=c(1,3))   
plot(test.ph)

### Model Selection

anova(cox)

## Estimating Survival Curve

subject_one <- data.frame(surg = factor('short'),
                          adhere = factor('adhere'), nodes = 5)

prediction_one <- survfit(cox, subject_one, 
                          data = colon_subset_recurrence)

ggsurvplot(prediction_one, ylab = "Probability of no recurrence ",
           conf.int = TRUE,
           ggtheme = theme_bw())

ggsurvplot(prediction_one, fun="cumhaz",
           conf.int = TRUE,risk.table = TRUE,
           ggtheme = theme_bw(),
           risk.table.col = "strata")

# Accelerated failure time models

## Exponential models

###  Learning Exponential models

survregExp <- survreg(surv ~ 1 + surg + adhere + nodes,
                      dist="exponential",data=colon_subset_recurrence)
summary(survregExp)

### Estimating Survival Curve

subject_two = list(surg = factor('short'), adhere = factor('no adhere'), nodes = 5)

plot(predict(survregExp, newdata=subject_two,
             type="quantile",p=seq(.01,.99,by=.01)),
     seq(.99,.01,by=-.01), col="red",type='l',xlab='time',
     ylab='Survival probability',main='Exponential AFT Model')


###  Learning Weibull models

survregWeibull = survreg(surv ~ 1 + surg + adhere + nodes,
                         dist="weibull",data=colon_subset_recurrence)
summary(survregWeibull)

subject_two = list(surg = factor('short'), 
                   adhere = factor('no adhere'), 
                   nodes = 5)


plot(predict(survregWeibull, newdata=subject_two,
             type="quantile",p=seq(.01,.99,by=.01)),
     seq(.99,.01,by=-.01), col="red",type='l',xlab='time',
     ylab='Survival probability',main='Weibull AFT Model')

### Log-normal models

###  Learning Log-normal models

survregLogNormal = survreg(surv ~ 1 + surg + adhere + nodes,
                           dist="lognormal",data=colon_subset_recurrence)
summary(survregLogNormal)

### Estimating Survival Curve

subject_two = list(surg = factor('short'), 
                   adhere = factor('no adhere'), 
                   nodes = 5)

plot(predict(survregLogNormal, newdata=subject_two,
             type="quantile",p=seq(.01,.99,by=.01)),
     seq(.99,.01,by=-.01), col="red",type='l',xlab='time',
     ylab='Survival probability',main='Log Normal AFT Model')
