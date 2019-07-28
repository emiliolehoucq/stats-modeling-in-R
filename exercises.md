# Exercises

1. Read the GSS2006 data in R.

# Part I

## Descriptive statistics

2. Provide univariate descriptive statistics for socsci, age, partyid, and sex.

3. Provide descriptive statistics for socsci by partyid and by sex.

4. Make a histogram of socsci.

5. Make informative graphs of socsci against age, socsci against sex, and socsci against partyid.

6. Correlate socsci and age.

## Modeling and extracting information in linear regression

8. Model socsi as a function of age and partyid. Treat partyid as a categorical variable and independent as the base category for estimating the model. 

9. Make a plot to explore a possible interaction between age and sex. Test such interaction in a model.

10. Extract the residual and predicted values for a model with age and partyid.

11. Obtain confidence interval for the coefficients and a confidence interval for the mean response for a person of 40 years old who is strong republican and prediction limits for a new observation for a person of 40 years old and who is independent.

12. Obtain a confidence band for the entire regression line.

## Diagnostics in linear regression

13. Provide a normal probability plot.

14. Provide plots of the residuals against the fitted values and against age.

15. Provide the variance inflation factors of the coefficients.

16. Provide the p-value for the Breusch-Pagan test for heteroskedasticity.

17. Provide graphical evidence and conduct a test for the presence of outlying Y observations.

# Part II

## Logistic regression

18. Read the GSS 2006 data again. Recode socsci so that "very scientific" and "pretty scientific" are coded as "1" and "not too scientific" and "not scientific at all" are coded as 0.

19. Model socsci (binary) as a function of age and partyid with a logistic regression.

20. Provide the exponentiated coefficients.

21.  Plot the logistic regression line and a smoothing line. 

## Poisson Regression

22. Load the `epil` data set from the `MASS` package

23. Ensure that all the covariates are of the right class

24. Using Poisson regression, model the number of siezures against the treatment, age and period

25. Using Poisson regression, model the number of siezures against the interaction of all three covariates, treatment, age and period

26. Using `anova`, what can you say about the iteration factors?

27. How do these models compare to the null model?

28. How do these models compare to the saturate model?

29. Can this data be accurately model as Poisson process? What assumptions does it violate?

## Log-Linear Regression

30. Load the colon data set from the `survival` package. Subset the data set for those whose cancer recurred. Store this subset in `colon_subset_recurrence`.


31. Add a new column to `colon_subset_recurrence` called `time.dichotomize` which classifies if the observed time was greater or less than the median

32. Build a two-contingency way table with the covariates, `time.dichotomize` and `node4`

33. Considering the two-contingency way table, are there any significant interaction effects between the two covariates that determine the mean count of a cell?

34. Build a three-contingency way table with the covariates, `time.dichotomize`, `node4` and `rx`

35. Considering the three-contingency way table, are there any significant interaction effects between the three covariates that determine the mean count of a cell?


## Hierarchical modeling

36. Read the 6th wave of the World Values Survey in R.

37. In the same plot, graph the relation between people’s valuation of science and technology (V197) and their education (V248) for all countries (V2) in the survey. Treat education as a continuous variable.

Note: For the next exercises, recode education as a three-level factor (0 for no formal education, 1 for incomplete or complete primary education, and 1 for complete or incomplete secondary education of any type)—this will make the estimation faster. Also, consider that individual observations are nested within countries. Finally, model intercepts as random effects unless otherwise indicated.

38. Model people’s valuation of science and technology as a function of their education. Treat education as a random effect.

39. Model people’s valuation of science and technology as a function of their education and the grand mean of how important religion is in people’s lives (so, before doing this, you need to grand-mean-center V9). Treat education as a fixed effect.

40. Model people’s valuation of science and technology as a function of their education interacted with the grand mean of how important religion is in people’s lives. Treat education as a random effect and the intercept as a fixed effect.

41. Recode people’s valuation of science as a binary variable (0 for 1 to 5 and 1 for 6 to 10). Model it as a function of their education with a logit link function. Treat education as a fixed effect.

# Part III

42. Load the myeloma data set from `survminer` package

43. The `event` tell us  whether someone has died at the last known time the patient seen. This is our censored colomn. Create a Surv object, `surv`, with the censor status and time of event.

## Kaplan-Meier

44. Create and plot a Kaplan-Meier estimator the entire dataset.

45. Create and plot a Kaplan-Meier estimator as function of `molecular_group` the entire dataset. What does p-value tell after adding `molecular_group`?

## Cox Proportional Hazard

46. Create and plot the coefficients of Cox Proportional Hazard function with all gene expression covariates. Which covariates are significant?

