

#-----------------------------------------------------------------------------
#                 Binary Model - Boston Housing
#-----------------------------------------------------------------------------

# We start by loading the data set HMDA which provides data that relate to mortgage
# applications filed in Boston in the year of 1990.

# We are interested in modelling the binary variable "deny", an 
# indicator for whether an applicant’s mortgage application has been accepted 
# (deny = no) or denied (deny = yes). A regressor that ought to have
# power in explaining whether a mortgage application has been denied is "pirat", 
# the size of the anticipated total monthly loan payments relative to the 
# applicants income.

rm(list=ls()) 

# load `AER` package and attach the HMDA data
library(AER)
library(ggplot2)
library("writexl")  

data(HMDA)

# inspect the data
head(HMDA)
summary(HMDA)

# write_xlsx(HMDA,"/Users/arshadrahman/Library/CloudStorage/Dropbox/1-WorkFiles/1 IIT-Kanpur/e-Masters/R Codes/Binary Model/hmda.xlsx")


# deny:    Factor. Was the mortgage denied?
# pirat:   Payments to income ratio.
# hirat:   Housing expense to income ratio.
# lvrat:   Loan to value ratio.
# chist:   Factor. Credit history: consumer payments.
# mhist:   Factor. Credit history: mortgage payments.
# phist:   Factor. Public bad credit record?
# unemp:   1989 Massachusetts unemployment rate in applicant's industry.
# selfemp:   Factor. Is the individual self-employed?
# insurance: Factor. Was the individual denied mortgage insurance?
# condomin:  Factor. Is the unit a condominium?
# afam:      Factor. Is the individual African-American?
# single:    Factor. Is the individual single?
# hschool:   Factor. Does the individual have a high-school diploma?




#-----------------------------------------------------------------------------
#                         Linear Regression 
#-----------------------------------------------------------------------------
# CLRM:   deny = beta1 + beta2 * (P/I) ratio + epsilon

# We estimate this model as a linear regression model using lm(). Before we 
# do so, the variable deny must be converted to a numeric variable using 
# as.numeric() as lm() does not accept the dependent variable to be of 
# class factor. Note that as.numeric(HMDA$deny) will turn deny = no into 
# deny = 1 and deny = yes into deny = 2, so using as.numeric(HMDA$deny)-1 
# we obtain the values 0 and 1.

# convert 'deny' to numeric
HMDA$deny <- as.numeric(HMDA$deny) - 1

# estimate a simple linear regression, aka, linear probabilty model
denymod1 <- lm(deny ~ pirat, data = HMDA)
summary(denymod1)

# plot the data
plot(x = HMDA$pirat,
     y = HMDA$deny,
     main = "Scatterplot and Estimated Regression Line",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.8)
# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")
# add the estimated regression line
abline(denymod1, lwd = 1.8, col = "steelblue")
text(1.5, 0.5, cex= 0.8, "deny.hat = -0.07991 + 0.60353*(P/I)")


# According to the estimated model, a payment-to-income ratio of 1 is 
# associated with an expected probability of mortgage application denial of 
# roughly 50%. The model indicates that there is a positive relation between 
# the payment-to-income ratio and the probability of a denied mortgage 
# application so individuals with a high ratio of loan payments to income 
# are more likely to be rejected.

# We may use coeftest() to obtain robust standard errors for both 
# coefficient estimates.
# print robust coefficient summary
coeftest(denymod1, vcov. = vcovHC, type = "HC1")

# The true coefficient on P/I ratio is statistically different from 0 at the 
# 1% level. Interpretation: a 1 percentage point increase in P/I ratio leads 
# to an increase in the probability of a loan denial 
# by 0.604 · 0.01 = 0.00604 ≈ 0.6%.

#-----------------------------------------------------------------------------
# Is there racial bias? Lets see via Multiple Linear Regression
#-----------------------------------------------------------------------------

# We augment our previous model by an additional regressor black which equals
# 1 if the applicant is an African American and equals 0 otherwise. Such a 
# specification is the baseline for investigating if there is racial 
# discrimination in the mortgage market: if being black has a significant
# (positive) influence on the probability of a loan denial when we control 
# for factors that allow for an objective assessment of an applicant’s 
# creditworthiness, this is an indicator for discrimination.

# rename the variable 'afam' for consistency
colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"
# estimate the model
denymod2 <- lm(deny ~ pirat + black, data = HMDA)
coeftest(denymod2, vcov. = vcovHC)

# The coefficient on black is positive and significantly different from zero 
# at the 0.01% level. The interpretation is that, holding constant the 
# P/I ratio, being black increases the probability of a mortgage application 
# denial by about 17.7%. This finding is compatible with racial discrimination.
# However, it might be distorted by omitted variable bias so discrimination 
# could be a premature conclusion.


#-----------------------------------------------------------------------------
#                 Binary Model: Probit Regression
#-----------------------------------------------------------------------------

# The LPM has a major flaw, it predicts P(y=1|x_1, ...,x_k) to lie outside 0 
# and 1. However probabilities cannot lie between 0 and 1.

# We now estimate a simple Probit model of the probability of a mortgage denial.
# estimate the simple probit model
denyprobit <- glm(deny ~ pirat,
                  family = binomial(link = "probit"),
                  data = HMDA)
coeftest(denyprobit, vcov. = vcovHC, type = "HC1")

# Estimated model: Est P(deny=1| P/I ratio) = \Phi(-2.19 + 2.97*(P/I) ratio)

# plot data
plot(x = HMDA$pirat, y = HMDA$deny,
     main = "Probit Model of the Probability of Denial, given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4), cex.main = 0.85)
# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred") 
abline(h = 0, lty = 2, col = "darkred") 
text(2.5, 0.9, cex = 0.8, "Mortgage denied") 
text(2.5, -0.1, cex= 0.8, "Mortgage approved")
# add estimated regression line
x <- seq(0, 3, 0.01)
y <- predict(denyprobit, list(pirat = x), type = "response")
lines(x, y, lwd = 1.5, col = "steelblue")

# The estimated regression function has a stretched “S”-shape which is typical
# for the CDF of a continuous random variable with symmetric PDF like that of 
# a normal random variable. The function is nonlinear and flattens 
# out for large and small values of P/I ratio. The functional form thus also 
# ensures that the predicted conditional probabilities of a denial lie 
# between 0 and 1.

# We use predict() to compute the predicted change in the denial probability 
# when P/I ratio is increased from 0.3 to 0.4.

# 1. compute predictions for P/I ratio = 0.3, 0.4
predictions1 <- predict(denyprobit,
                       newdata = data.frame("pirat" = c(0.3, 0.4)),
                       type = "response")

# 2. Compute difference in probabilities
diff(predictions1)

# We find that an increase in the payment-to-income ratio from 0.3 to 0.4 
# is predicted to increase the probability of denial by approximately 6.1%.

# We continue by using an augmented Probit model to estimate the effect of 
# race on the probability of a mortgage application denial.

denyprobit2 <- glm(deny ~ pirat + black,
                   family = binomial(link = "probit"), data = HMDA)
coeftest(denyprobit2, vcov. = vcovHC, type = "HC1")

# Estimated model: Est P(deny=1|x's) = \Phi(-2.26 + 2.74*(P/I) ratio + 0.71*black)

# All coefficients are statistically significant. The coefficient for African 
# Americans is positive and indicates that African Americans have a higher 
# probability of denial than White Americans, ceteris paribus. Also, applicants
# with a high P/I ratio face a higher risk of being rejected.

# Question: How big is the estimated difference in denial probabilities 
# between two hypothetical applicants with the same payments-to-income ratio? 
# As before, we may use predict() to compute this difference.

predictions2 <- predict(denyprobit2, 
                       newdata = data.frame("black" = c("no", "yes"),
                       "pirat" = c(0.3,0.3)), type = "response")
# 2. compute difference in probabilities
diff(predictions2)
#> 0.1578117

#-----------------------------------------------------------------------------
#                 Binary Model: Logit Regression
#-----------------------------------------------------------------------------

# As for Probit regression, there is no simple interpretation of the model 
# coefficients and it is best to consider predicted probabilities or 
# differences in predicted probabilities. However, for the logit model the 
# coefficients represent the log-odds (or logarithm of odds ratio) of success.

# Estimating a logit model

denylogit <- glm(deny ~ pirat,
                 family = binomial(link = "logit"),
                 data = HMDA)
coeftest(denylogit, vcov. = vcovHC, type = "HC1")


plot(x = HMDA$pirat, y = HMDA$deny,
     #main = "Probit and Logit Models of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4), cex.main = 0.9)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred") 
abline(h = 0, lty = 2, col = "darkred") 
text(2.5, 0.9, cex = 0.8, "Mortgage denied") 
text(2.5, -0.1, cex= 0.8, "Mortgage approved")
# add estimated regression line of Probit and Logit models

x <- seq(0, 3, 0.01)
y_probit <- predict(denyprobit, list(pirat = x), type = "response") 
y_logit <- predict(denylogit, list(pirat = x), type = "response")
lines(x, y_probit, lwd = 1.5, col = "steelblue") 
lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)
# add a legend
legend("topleft", horiz = TRUE,
  legend = c("Probit", "Logit"), col = c("steelblue", "black"), lty = c(1, 2))

# Both models produce very similar estimates of the probability that a 
# mortgage application will be denied depending on the applicants 
# payment-to-income ratio.

# We now extend the Logit model of mortgage denial with the additional 
# regressor black.

# estimate a Logit regression with multiple regressors
denylogit2 <- glm(deny ~ pirat + black,
                  family = binomial(link = "logit"),
                  data = HMDA)
coeftest(denylogit2, vcov. = vcovHC, type = "HC1") 

# Estimated model: Est. P(deny=1|x's) = F(-4.13 + 5.37*(P/I) ratio + 1.27*black)

# Similar to the Probit model, all model coefficients are highly significant 
# and we obtain positive estimates for the coefficients on P/I ratio and black.

predictions3 <- predict(denylogit2,
                       newdata = data.frame("black" = c("no", "yes"),
                                "pirat"=c(0.3,0.3)), type = "response")
predictions3
# 2. Compute difference in probabilities
diff(predictions3) 
#> 0.1492945

# We find that the white applicant faces a denial probability of only 7.5%, 
# while the African American is rejected with a probability of 22.4%, 
# a difference of 14.9 percentage points.

#-----------------------------------------------------------------------------
#                   Model Fit: Probit vs Logit
#-----------------------------------------------------------------------------

# compute the null Probit model
denyprobit_null <- glm(formula = deny ~ 1,
                       family = binomial(link = "probit"),
                       data = HMDA)
# compute the pseudo-R2 using 'logLik'
1 - logLik(denyprobit2)[1]/logLik(denyprobit_null)[1] 
#> [1] 0.08594259

# compute the null Logit model
denylogit_null <- glm(formula = deny ~ 1,
                       family = binomial(link = "logit"),
                       data = HMDA)
# compute the pseudo-R2 using 'logLik'
1 - logLik(denylogit2)[1]/logLik(denylogit_null)[1] 
#> [1] 0.08759475
