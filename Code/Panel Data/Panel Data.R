
# Session on Panel Data Models

library(ggplot2)
library(dplyr)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(geepack)


#-----------------------------------------------------------------------------
#  In R, the plm package is commonly used for panel data analysis.

rm(list = ls())

data("Grunfeld", package = "plm")

# Number of observations - 200 (20 years for 10 firms)

# Variables name definitions::
  
# invest  - Gross investment in 1947 dollars
# value   - Market value as of Dec. 31 in 1947 dollars
# capital - Stock of plant and equipment in 1947 dollars
# firm    - General Motors, US Steel, General Electric, Chrysler,
# Atlantic Refining, IBM, Union Oil, Westinghouse, Goodyear,
# Diamond Match, (11: American Steel, only presented in AER package)
# year    - 1935 - 1954


Grunfeld %>%
  select(year, firm) %>%
  table()
# The infix operator %>% is not part of base R, but is in fact defined by the 
# package magrittr (CRAN) and is heavily used by dplyr (CRAN).

# Checking the data is balanced or unbalanced
Grunfeld %>%
  is.pbalanced()

# Visualization Techniques
# When doing graphical analysis with panel data, the entity and time dimension 
# has to be taken into account in order to get meaningful results.

# Entire Heterogeneity
# Heterogeneity across firms can be shown with a line plot. The blue line 
# connects the mean values of inv, using all available years across firms (entities).

Grunfeld %>%
  group_by(firm) %>%
  summarise(inv_mean = mean(inv)) %>%
  left_join(Grunfeld) %>%
  ggplot(data = ., 
         aes(x = reorder(as.character(firm), firm), y = inv)) +
  geom_point() +
  geom_line(aes(x = firm, y = inv_mean), col = "blue") +
  labs(x = "Firm", y = "Gross Investment")

# Time Heterogeneity
# Here the blue line connects the mean values of inv, using all available 
# firms across years (time).

Grunfeld %>%
  group_by(year) %>%
  summarise(inv_mean = mean(inv)) %>%
  left_join(Grunfeld) %>%
  ggplot(data = ., 
         aes(x = year, y = inv)) +
  geom_point() +
  geom_line(aes(x = year, y = inv_mean), col = "blue") +
  scale_x_continuous(labels = as.character(Grunfeld$year), 
                     breaks = Grunfeld$year) +
  labs(x = "Year", y = "Gross Investment") +
  theme(axis.text.x = element_text(angle = 90))

#-----------------------------------------------------------------------------
#                       Pooled Model
#-----------------------------------------------------------------------------

# With function lm() it is straightforward to estimate the pooled OLS model. 
# I regress the firms’ gross investment (inv) on their stock of plant and 
# equipment (capital).

pooled_ols_lm <- lm(inv ~ capital + value, data = Grunfeld )
summary(pooled_ols_lm)

# We achieve the same coefficient estimates by using function plm() from 
# package plm. First, an index has to be supplied, corresponding to the entity 
# and/or time dimension of the panel. The argument model= is set to "pooling". 

pooled_ols_plm <- plm(inv ~ capital, data = Grunfeld, 
                      index = c("firm", "year"), 
                      effect = "individual", model = "pooling")
summary(pooled_ols_plm)

# Scatter plot

ggplot(data = Grunfeld,
       aes(x = capital, y = inv)) +
  geom_point(aes(shape = factor(firm, 
                                levels = c(1:10)))) +
  geom_smooth(method = "lm", se = F) +
  scale_shape_manual(values = 1:10) +
  labs(x = "Stock of Plant and Equipment",
       y = "Gross Investment",
       shape = "Firm")

# With a scatterplot it is easy to see that, although firms could be 
# distinguished by the variable firm, OLS estimation treats all observations 
# as if they come from the same entity and fits the regression line 
# accordingly.

#-----------------------------------------------------------------------------
#                       Fixed-Effects Model
#-----------------------------------------------------------------------------

# The fixed effects (FE) model, also called within estimator or least squares 
# dummy variable (LSDV) model, is commonly applied to remove omitted variable 
# bias. By estimating changes within a specific group (over time) all 
# time-invariant differences between entities (individuals, firms, …) are 
#controlled for. For example: (1) the unobserved ability of the management 
# influencing the firm’s revenue or, (2)  the skills influencing an employee’s wage.

# The assumption behind the FE model is that something influences the 
# independent variables and one needs to control for it (the error term and 
# independent variables are correlated). Hence, the FE model removes 
# characteristics that do not change over time, leading to unbiased estimates 
# of the remaining regressors on the dependent variable. If unobserved 
# characteristics do not change over time, each change in the dependent 
# variable must be due to influences not related to the fixed effects, 
# which are controlled for. The FE model is hence suited for investigating 
# causal relationships.

# Note that the influence of time-invariant regressors on the dependent 
# variable cannot be examined with a FE model. Also they do not work well 
# with data with low within-variance or variables which only change slowly 
# over time.

#-----------------------------------------------------------------------------
#              (1) Least Squares Dummy Variable Estimation
#-----------------------------------------------------------------------------


# (a) Fixed Effect Model using the lm() function
#-----------------------------------------------------------------------------
# With function lm() a FE model can be estimated by including dummy variables 
# for all firms. This is the so called least squares dummy variable (LSDV) 
# approach. I have shown before that the factor variable firm uniquely 
# identifies each firm in the dataset. Similarly to the pooled OLS model, 
# I am regressing inv on capital. If there is a large number of individuals, 
# the LSDV method is expensive from a computational point of view.

fe_model_lm <- lm(inv ~ capital + factor(firm), data = Grunfeld)
summary(fe_model_lm)

# NOTE: Remember that one firm dummy variable is dropped to avoid the dummy 
# variable trap.

# (b) Fixed Effect Model using the lm() function, excluding the intercept.
#-----------------------------------------------------------------------------
# Next up, I calculate the same model but drop the constant (intercept) 
# by adding -1 to the formula, so that no coefficient (level) of firm is 
# excluded. Note that this does not alter the coefficient estimate of capital!

fe_model_lm_nocons <- lm(inv ~ capital + factor(firm) -1, data = Grunfeld)
summary(fe_model_lm_nocons)

# Scatter plot

ggplot(data = broom::augment(fe_model_lm),
       aes(x = capital, y = .fitted)) +
  geom_point(aes(color = `factor(firm)`)) +
  geom_line(aes(color = `factor(firm)`)) +
  geom_line(data=broom::augment(pooled_ols_lm), 
            aes(x = capital, y =.fitted), 
            color = "blue", lty="dashed", linewidth = 1) +
  labs(x = "Stock of Plant and Equipment", y = "Fitted Values (inv ~ capital)",
       color = "Firm") 

# Due to the introduction of firm dummy variables each firm has its own 
# intercept with the y axis! For comparison, I plotted the fitted values from 
# the pooled OLS model (blue dashed line). Its slope is more steep compared 
# to the LSDV approach as influential observations of firm 1 lead to an upward bias.


#-----------------------------------------------------------------------------
#                 (2) Within Groups Estimator
#-----------------------------------------------------------------------------

# (a) Fixed Effects Model using plm()
#-----------------------------------------------------------------------------
# The same coefficient estimates as with the LSDV approach can be computed 
# with function plm(). The argument model= is now set to "within". This is 
# the within estimator with n entity-specific intercepts.

fe_model_plm <- plm(inv ~ capital, data = Grunfeld, 
                    index = c("firm", "year"), 
                    effect = "individual", model = "within")
summary(fe_model_plm)

# The coefficient of capital indicates how much inv changes over time, on 
# average per country, when capital increases by one unit.

fixef(fe_model_plm)

# With function fixef() the fixed effects, i.e. the constants for each firm, 
# can be extracted. Compare them with the coefficients of the LSDV approach 
# (w/o the constant) - they must be identical.


# (b) Fixed Effects Model using felm()
#-----------------------------------------------------------------------------

# The function felm() from package lfe does the same as lm() and plm(). 
# However, it displays the F-statistic and R2 of a full and projected model. 
# The full model is the first model with dummy variables for each firm 
# (LSDV approach) and the projected model is the within estimator. In general, 
# you want to provide the R2 from the full model. Note that the values of the 
# F-statistic and R2 of the full model displayed here are only the same when 
# the constant is included in the LSDV model.

fe_model_felm <- lfe::felm(inv ~ capital | factor(firm), 
                           data = Grunfeld)
summary(fe_model_felm)

# Testing for FE
# With function pFtest() one can test for fixed effects with the null 
# hypothesis that pooled OLS is better than FE. Alternatively, this test can 
# be carried out by jointly assessing the significance of the dummy variables 
# in the LSDV approach. The results are identical.

# Within estimator vs. Pooled OLS
pFtest(fe_model_plm, pooled_ols_plm)


# Joint significane test with LSDV approach
car::linearHypothesis(fe_model_lm,
                      hypothesis.matrix = matchCoefs(fe_model_lm, "firm"))

# In both cases the null hypothesis is rejected in favor of the alternative 
# that there are significant fixed effects.

#-----------------------------------------------------------------------------
#                 (3) First Difference Estimator
#-----------------------------------------------------------------------------

# There is another way of estimating a FE model by specifying model = "fd" in 
# function plm().

fe_model_fd<- plm(inv ~ capital -1, data = Grunfeld,
                  index = c("firm", "year"), 
                  effect = "individual", model = "fd")
summary(fe_model_fd)

# The coefficient of capital is now different compared to the LSDV approach 
# and within-groups estimator. This is because the coefficients and standard 
# errors of the first-differenced model are only identical to the previously 
# obtained results when there are two time periods. For longer time series, 
# both the coefficients and the standard errors will be different.

# With Two periods
# Let’s verify the former assumption by dropping all years except 1935 and 
# 1936 from the Grunfeld dataset and estimate the model again (also for 
# the within model).

# Within estimation (two periods)
fe_model_plm_check <- plm(inv ~ capital, 
                          data = Grunfeld, 
                          subset = year %in% c(1935, 1936), 
                          index = c("firm", "year"), 
                          effect = "individual", model = "within")

lmtest::coeftest(fe_model_plm_check)

# FD estimation (two periods)
fe_model_fd_check<- plm(inv ~ capital -1,
                        data = Grunfeld, 
                        subset = year %in% c(1935, 1936), 
                        index = c("firm", "year"), 
                        effect = "individual", model = "fd")

lmtest::coeftest(fe_model_fd_check)

#-----------------------------------------------------------------------------
#                       Random-Effects Model
#-----------------------------------------------------------------------------

# The RE model (also called Partial Pooling Model) assumes, in contrast to 
# the FE model, that any variation between entities is random and not 
# correlated with the regressors used in the estimation model. If there are 
# reasons to believe that differences between entities influence the dependent 
# variable, a RE model should be preferred. This also means that time-invariant 
# variables (like a person’s gender) can be taken into account as regressors. 
# The entity’s error term (unobserved heterogeneity) is hence not correlated 
# with the regressors.

#To break down the difference between FE and RE:
# (1) the FE model assumes that an individual (entity) specific effect is 
# correlated with the independent variables,
# (2) while the RE model assumes that an individual (entity) specific effect 
# is not correlated with the independent variables.

# With function plm() the RE model can be estimated. The argument model= is set to value "random".

re_model_plm <- plm(inv ~ capital, data = Grunfeld, 
                    index = c("firm", "year"), 
                    effect = "individual", model = "random")

summary(re_model_plm)

# The coefficients in the RE model include both the within-entity and 
# between-entity effects. When having data with multiple entities and time 
# periods the coefficient of capital represents the average effect on inv 
# when capital changes across years and between firms by one unit.


#-----------------------------------------------------------------------------
#                    Fixed-Effects or Random-Effects?
#-----------------------------------------------------------------------------

# A decision between a fixed and random effects model can be made with the 
# Hausman test, which checks whether the individual error terms are correlated 
# with the regressors. The null hypothesis states that there is no such 
# correlation (RE). The alternative hypothesis is that a correlation 
# exists (FE). The test is implemented in function phtest()

phtest(fe_model_plm, re_model_plm)