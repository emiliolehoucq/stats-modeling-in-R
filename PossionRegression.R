library(ggplot2)
library(magrittr)

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

#get column names
print(colnames(df))

model = glm(y~ 1, family=poisson(link=log),data=df)
summary(model)

# get coefficients
print(coef(model))

# print fitted model values
print(data.frame(df$y,model$fitted))

# print model predictors 
model$linear.predictors
exp(model$linear.predictors)


# Hypothesis test for goodness of fit
print(1-pchisq(model$deviance,model$df.residual))

#plot data comparison
df_original = data.frame(data=df$y)
df_fitted = data.frame(data=model$fitted)
plot(df_original$data,df_fitted$data,ylabel='fit data',xlabel='original data')


#add a Covariate to the fit -- treatment
model = glm(y~ 1 +trt, family=poisson(link=log),df)
summary(model)

print(1-pchisq(model$deviance,model$df.residual))

df_fitted = data.frame(data=model$fitted)
df_fitted$name = "fitted"

plot(df_original$data,df_fitted$data)


#add a Covariate to the fit -- treatment, age
model = glm(y ~ 1 +trt*age, family=poisson(link=log),df)
summary(model)

print(1-pchisq(model$deviance,model$df.residual))

df_fitted = data.frame(data=model$fitted)
df_fitted$name = "fitted"

combined=rbind(df_original,df_fitted)

plot(df_original$data,df_fitted$data)


# Overdispersion might be present -- try Quasipossion  
model = glm(y ~ 1 +trt*age, family=quasipoisson(link=log),df)
summary(model)

print(1-pchisq(model$deviance,model$df.residual))

df_fitted = data.frame(data=model$fitted)
df_fitted$name = "fitted"

combined=rbind(df_original,df_fitted)

plot(df_original$data,df_fitted$data)
