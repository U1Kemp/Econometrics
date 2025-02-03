
# Estimating an Ordinal Model


# For the ordered probit/logit model
library(MASS)
# For the brant test
library(brant)
# For the generalized ordered logit
library(VGAM)
# For marginal effects
library(erer)

# Fair	R Documentation
# Extramarital Affairs Data

# Description
# A cross-section data. Number of observations: 601. Observation: individuals
# Country : United States
# Variables:
# sex:    a factor with levels (male,female)
# age:    age in years
# ym:     number of years married
# child:  yes or no, a factor
# religious:  how religious, from 1 (anti) to 5 (very)
# education:  years of education
# occupation: occupation, from 1 to 7, according to Hollingshead's classification (reverse numbering)
# rate:       self rating of marriage, from 1 (very unhappy) to 5 (very happy)
# nbaffairs:  number of affairs in past year
# Source: Fair, R. (1977) “A note on the computation of the tobit estimator”, Econometrica, 45, 1723-1727.
# https://fairmodel.econ.yale.edu/rayfair/pdf/1978A200.PDF.

##############################################################################
# Data on marital happiness and affairs
# Documentation: https://vincentarelbundock.github.io/Rdatasets/doc/Ecdat/Fair.html
mar <- read.csv('/Users/arshadrahman/Library/CloudStorage/Dropbox/1-WorkFiles/1 CMI/Econometrics/R Codes/Ordinal Model/Fair.csv')

# See how various factors predict marital happiness
OrdLogistic <- polr(factor(rate) ~ age + child + religious + education + nbaffairs,
          data = mar, 
          method = 'logistic' # change to 'probit' for ordered probit
)
summary(OrdLogistic)

# (1) Age has a negative (positive) effect on marriage being very happy (very unhappy)
# (2) Having a child negatively (positively) impacts marriage being very happy (very unhappy)
# relative to someone not having a child
# (3) Number of affairs has a negative (positive) effect on marriage being 
# very happy (very unhappy)
# (4) Being more religious has a positive (negative) effect on marriage being 
# very happy (very unhappy) 
# (5) Education has a positive effect on marriage being very happy and 
# negative effect on marriage being very unhappy

# Output explanation
# Coefficients: This section provides information about the estimated coefficients (β values) for the predictor variables.
# Value: This column displays the estimated coefficients. For age, the estimated coefficient is approximately -0.01957.
# Std. Error: This column shows the standard errors associated with each coefficient estimate. 
# t value: This column displays the t-values for the coefficients. 
# Intercepts: This section provides information about the estimated threshold parameters for the ordinal categories. 
# In ordinal logistic regression, the model estimates threshold parameters for each category.
# Value: This column displays the estimated threshold values. In this example, 1|2 represents the threshold between 
# categories 1 and 2, and 2|3 represents the threshold between categories 2 and 3, and so on.
# Std. Error: This column shows the standard errors associated with each threshold estimate.
# t value: This column displays the t-values for the threshold parameters. As with coefficients, 
# t-values help assess the significance of threshold estimates.
# Residual Deviance: The residual deviance measures how well the model fits the data. A smaller residual deviance 
# indicates a better fit. In this case, the residual deviance is approximately 1551.503
# AIC (Akaike Information Criterion): The AIC is a measure of model quality that considers both goodness of 
# fit and model complexity. Lower AIC values indicate better models. Here, the AIC is approximately 1569.503
