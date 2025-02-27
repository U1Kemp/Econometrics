#-----------------------------------------------------------------------------
# Coding part of Econometrics Homework 2
# Utpalraj Kemprai
#-----------------------------------------------------------------------------

# Load libraries
# For reading Excel file
library(readxl)
# For the ordered probit/logit model
library(MASS)
# For the brant test
library(brant)
# For the generalized ordered logit
library(VGAM)
# For marginal effects
library(erer)
# For Panel Data Model
library(plm)

#-----------------------------------------------------------------------------
# Question 1.e
#-----------------------------------------------------------------------------
# read the data from file
file_path <- "C:/Users/utpal/Documents/Notes/Projects/Econometrics/Assignments/Assignment 2/Feb14Data.xlsx"
Feb14Data <- read_excel(file_path, 
                        col_types = c("text", "text", "text", 
                                      "text", "numeric", "text", "text", 
                                      "text", "text", "numeric", "text", 
                                      "text", "skip", "skip"))
View(Feb14Data)
#-----------------------------------------------------------------------------
# check the columns
#-----------------------------------------------------------------------------
colnames(Feb14Data)

# [1] "q46"     "q57"     "relig"   "sex"     "age"     "educ2"   "race3m1" "income"  "party"  
# [10] "hh1"     "state"   "q55" 

#-----------------------------------------------------------------------------
# Column Description
#-----------------------------------------------------------------------------
# q46 -> answer to question about legalization: medicinal, notlegal, personal
# q57 -> answer to question about past use: No, Yes
# relig -> religion of respondent: 13 unique values
# sex -> sex of respondent: Female, Male
# age -> age of the respondent
# educ2 -> education level of the respondent
# race3m1 -> race of the respondent
# income -> income of the respondent
# party -> political party affiliation of respondent
# hh1 -> Household size of the respondent
# state -> state of the respondent
# q55 -> answer to question: if marijuana will be legalized

#-----------------------------------------------------------------------------
# view the unique entries/number of unique entries in a column
#-----------------------------------------------------------------------------
for(i in colnames(Feb14Data)){
  # if less than 7, print all unique entries
  if(length(unique(Feb14Data[[i]])) <= 8){
    print(paste("Column",i))
    cat("Unique entries: ")
    cat(unique(Feb14Data[[i]]),sep = '; ')
    cat("",sep = '\n')
    cat("",sep = '\n')
  }
  # else print only the number of unique entries
  else{
    print(paste("Column",i))
    print(paste('Number of unique entries:', length(unique(Feb14Data[[i]]))))
    cat("",sep = '\n')
  }
}
#-----------------------------------------------------------------------------
# (i) Descriptive Summary of the data
#-----------------------------------------------------------------------------
# Take log of age
dummy_copy = Feb14Data

dummy_copy$age = log(Feb14Data$age)
names(dummy_copy)[names(dummy_copy) == "age"] <- "log_age"

# Convert income to numbers and take log
quantify_income <- function(text){
  if(text == "50 to under $75,000"){
    return(62500)
  }
  else if(text == "20 to under $30,000"){
    return(25000)
  }
  else if(text == "10 to under $20,000"){
    return(15000)
  }
  else if(text == "30 to under $40,000"){
    return(35000)
  }
  else if(text == "Less than $10,000"){
    return(5000)
  }
  else if(text == "$150,000 or more"){
    return(150000)
  }
  else if(text == "100 to under $150,000"){
    return(125000)
  }
  else if(text == "40 to under $50,000"){
    return(45000)
  }
  else if(text == "75 to under $100,000"){
    return(87500)
  }
}

dummy_copy$income = sapply(dummy_copy$income,quantify_income) 
dummy_copy$income = log(dummy_copy$income)
names(dummy_copy)[names(dummy_copy) == "income"] <- "log_income"

#-----------------------------------------------------------------------------
# Descriptive Summary of Continuous variables
#-----------------------------------------------------------------------------
means <- rep(0,3)
stdev <- rep(0,3)
ct = 1
for(col in c('log_age','log_income','hh1')){
  means[ct] = round(mean(dummy_copy[[col]]),2)
  stdev[ct] = round(sd(dummy_copy[[col]]),2)
  ct = ct + 1
}

# Create a tables for Descriptive Summary of Continuous Variables
table_cont <- data.frame(
  Variable = c('log_age','log_income','hh1'),
  Mean = means,
  StdDev = stdev
)

table_cont
#     Variable  Mean StdDev
# 1    log_age  3.72   0.44
# 2 log_income 10.63   0.98
# 3        hh1  2.74   1.42

#-----------------------------------------------------------------------------
# Descriptive Summary of Categorical variables
#-----------------------------------------------------------------------------
# past use and male
#-----------------------------------------------------------------------------
table_use_and_male <- data.frame(
  Variable = c("Past Use", "Male"),
  Counts = round(c(sum(dummy_copy$q57=='Yes'),sum(dummy_copy$sex=="Male")),2),
  Percentage = round(c(mean(dummy_copy$q57=='Yes')*100,
                       mean(dummy_copy$sex=="Male")*100),2)
)

table_use_and_male
#   Variable Counts Percentage
# 1 Past Use    719      48.19
# 2     Male    791      53.02
#-----------------------------------------------------------------------------
# Education
#-----------------------------------------------------------------------------
educ_counts = rep(0,3)
educ_counts[1] = sum(dummy_copy$educ2 == "Postgraduate Degree" 
                     | dummy_copy$educ2 == "Four year college"
                     | dummy_copy$educ2 == "Some Postgraduate")
educ_counts[2] = sum(dummy_copy$educ2 == 'Some College' 
                     | dummy_copy$educ2 == "Associate Degree")
educ_counts[3] = sum(dummy_copy$educ2 == 'HS' 
                     | dummy_copy$educ2 == 'Less than HS' 
                     | dummy_copy$educ2 == 'HS Incomplete')

educ_per = 100*educ_counts/length(dummy_copy$educ2)

table_educ = data.frame(
  Category = c("Bachelors & Above", "Below Bachelors", "High School & Below"),
  Counts = educ_counts,
  Percentage = round(educ_per,2)
)

table_educ
#              Category Counts Percentage
# 1   Bachelors & Above    551      36.93
# 2     Below Bachelors    434      29.09
# 3 High School & Below    507      33.98
#-----------------------------------------------------------------------------
# Eventually legal
#-----------------------------------------------------------------------------
sum(dummy_copy$q55 == "Yes, it will")
mean(dummy_copy$q55 == "Yes, it will")*100
table_even_legal <- data.frame(
  Variable = 'Eventually legal',
  Counts = sum(dummy_copy$q55 == "Yes, it will"),
  Percentage = round(mean(dummy_copy$q55 == "Yes, it will")*100, digits = 2)
)
table_even_legal
#          Variable Counts Percentage
#1 Eventually legal   1154   77.35

#-----------------------------------------------------------------------------
# Race
#-----------------------------------------------------------------------------
table_race = data.frame(
  Category = c("White","African American","Others"),
  Counts = c(sum(dummy_copy$race3m1 == 'White'),
             sum(dummy_copy$race3m1 == "Black or African-American"),
             sum(dummy_copy$race3m1 != "White" 
                 & dummy_copy$race3m1 != "Black or African-American")),
  Percentage = round(c(100*mean(dummy_copy$race3m1 == 'White'),
                 100*mean(dummy_copy$race3m1 == "Black or African-American"),
                 100*mean(dummy_copy$race3m1 != "White" 
                     & dummy_copy$race3m1 != "Black or African-American")),2)
)
table_race
#          Category Counts Percentage
#1            White   1149  77.01
#2 African American    202  13.54
#3           Others    141   9.45

#-----------------------------------------------------------------------------
# Party Affiliation
#-----------------------------------------------------------------------------
table_party = data.frame(
  Category = c("Republican","Democrat","Independent & Others"),
  Counts = c(sum(dummy_copy$party == "Republican"),
             sum(dummy_copy$party == "Democrat"),
             sum(dummy_copy$party != "Republican" 
                 & dummy_copy$party != "Democrat")),
  Percentage = round(c(100*mean(dummy_copy$party == "Republican"),
                 100*mean(dummy_copy$party == "Democrat"),
                 100*mean(dummy_copy$party != "Republican" 
                     & dummy_copy$party != "Democrat")),2)
)
table_party

#              Category Counts Percentage
#1           Republican    333   22.32
#2             Democrat    511   34.25
#3 Independent & Others    648   43.43

#-----------------------------------------------------------------------------
# Religion
#-----------------------------------------------------------------------------
## Christian
christian_count = sum(dummy_copy$relig == "Christian (VOL.)")
christian_per = christian_count*100/length(dummy_copy$relig)

## Roman Catholic
rom_cat_count = sum(dummy_copy$relig == "Roman Catholic (Catholic)")
rom_cat_per = rom_cat_count*100/length(dummy_copy$relig)

## Protestant
pro_count = sum(dummy_copy$relig == "Protestant (Baptist, Methodist, Non-denominational, Lutheran, Presbyterian, Pentecostal, Episcopalian, Reformed, etc.)")
pro_per = pro_count*100/length(dummy_copy$relig)

## Liberal
liberal_count = sum(dummy_copy$relig == "Agnostic (not sure if there is a God)"
    | dummy_copy$relig == "Nothing in particular" 
    | dummy_copy$relig == "Atheist (do not believe in God)"
    | dummy_copy$relig == "Unitarian (Universalist) (VOL.)")
liberal_per = liberal_count*100/length(dummy_copy$relig)
## Conservative
con_count = sum(dummy_copy$relig == "Mormon (Church of Jesus Christ of Latter-day Saints/LDS)"
    | dummy_copy$relig == "Jewish (Judaism)"
    | dummy_copy$relig == "Hindu"
    | dummy_copy$relig == "Buddhist"
    | dummy_copy$relig == "Muslim (Islam)"
    | dummy_copy$relig == "Orthodox (Greek, Russian, or some other orthodox church)")
con_per = con_count*100/length(dummy_copy$relig)

table_rel <- data.frame(
  Category = c("Christain","Roman Catholic","Protestant","Liberal","Conservative"),
  Counts = c(christian_count, rom_cat_count, pro_count, liberal_count, con_count),
  Percentage = round(c(christian_per, rom_cat_per, pro_per, liberal_per, con_per),2)
)

table_rel
#        Category Counts Percentage
#1      Christain    182  12.20
#2 Roman Catholic    290  19.44
#3     Protestant    550  36.86
#4        Liberal    348  23.32
#5   Conservative    122   8.18
#-----------------------------------------------------------------------------
# Public Opinion
#-----------------------------------------------------------------------------
table_opinion <- data.frame(
  Category = c("Medicinal","Not Legal","Personal"),
  Counts = c(sum(dummy_copy$q46 == "medicinal"),
             sum(dummy_copy$q46 == "notlegal"),
             sum(dummy_copy$q46 == "personal")),
  Percentage = round(c(100*mean(dummy_copy$q46 == "medicinal"),
                 100*mean(dummy_copy$q46 == "notlegal"),
                 100*mean(dummy_copy$q46 == "personal")),2)
)
table_opinion
#   Category Counts Percentage
#1 Medicinal    640   42.90
#2 Not Legal    218   14.61
#3  Personal    634   42.49
#-----------------------------------------------------------------------------
# Tolerant States
#-----------------------------------------------------------------------------
tolerant_states <- c("Alaska", "Arizona", "California", "Colorado", "Connecticut", 
                     "Delaware", "Hawaii", "Illinois", "Maine", "Maryland", 
                     "Massachusetts", "Michigan", "Montana", "Nevada", "New Hampshire", 
                     "New Jersey", "New Mexico", "Oregon", "Rhode Island", "Vermont", 
                     "Washington", "Washington DC", "District of Columbia")

table_tol <- data.frame(
  Variable = "Tolerant States",
  Counts = sum(dummy_copy$state %in% tolerant_states),
  Percentage = round(100*mean(dummy_copy$state %in% tolerant_states),2)
)
table_tol
#         Variable Counts Percentage
#1 Tolerant States    556   37.27

#-----------------------------------------------------------------------------
# (ii) Analyze public opinion on extent marijuana legalization in the US i.e., estimate Model 8
#-----------------------------------------------------------------------------
# Model 8
# Prepare the variables for the model
#-----------------------------------------------------------------------------

# Past Use
dummy_copy$pastuse = (dummy_copy$q57 == "Yes") + 0

# Male
dummy_copy$male = (dummy_copy$sex == "Male") + 0

# Education
dummy_copy$bachelor_above = ((dummy_copy$educ2 == "Postgraduate Degree") 
                             + (dummy_copy$educ2 == "Four year college") 
                             + (dummy_copy$educ2 == "Some Postgraduate"))
dummy_copy$below_bachelor = ((dummy_copy$educ2 == 'Some College') 
                             + (dummy_copy$educ2 == "Associate Degree"))
dummy_copy$below_hs = ((dummy_copy$educ2 == 'HS') 
                       + (dummy_copy$educ2 == 'Less than HS') 
                       + (dummy_copy$educ2 == 'HS Incomplete'))


# Tolerant state
## function to check if a state is tolerant
is_tolerant <- function(state){
  if(state %in% tolerant_states){
    return(1)
  }
  else{
    return(0)
  }
}
## apply the function
dummy_copy$tolerant_state = sapply(dummy_copy$state, is_tolerant)

# Eventually legal
dummy_copy$expected_legal = (dummy_copy$q55 == "Yes, it will") + 0

# Race
dummy_copy$black = (dummy_copy$race3m1 == "Black or African-American") + 0

dummy_copy$white = (dummy_copy$race3m1 == "White") + 0
dummy_copy$other_race = rep(1,length(dummy_copy$race3m1)) - dummy_copy$black - dummy_copy$white

# Party Affiliation
dummy_copy$democrat = (dummy_copy$party == "Democrat") + 0 
dummy_copy$republican = (dummy_copy$party == "Republican") + 0
dummy_copy$other_party = rep(1,length(dummy_copy$party)) - dummy_copy$democrat - dummy_copy$republican

# Religion
dummy_copy$christian = (dummy_copy$relig == "Christian (VOL.)") + 0
dummy_copy$roman_catholic = (dummy_copy$relig == "Roman Catholic (Catholic)") + 0
dummy_copy$protestant = (dummy_copy$relig == "Protestant (Baptist, Methodist, Non-denominational, 
                         Lutheran, Presbyterian, Pentecostal, Episcopalian, Reformed, etc.)")
dummy_copy$liberal = ((dummy_copy$relig == "Agnostic (not sure if there is a God)") 
                      + (dummy_copy$relig == "Nothing in particular") 
                      + (dummy_copy$relig == "Atheist (do not believe in God)") 
                      + (dummy_copy$relig == "Unitarian (Universalist) (VOL.)"))
dummy_copy$conservative = ((dummy_copy$relig == "Mormon (Church of Jesus Christ of Latter-day Saints/LDS)")
                           + (dummy_copy$relig == "Jewish (Judaism)")
                           + (dummy_copy$relig == "Hindu")
                           + (dummy_copy$relig == "Buddhist")
                           + (dummy_copy$relig == "Muslim (Islam)")
                           + (dummy_copy$relig == "Orthodox (Greek, Russian, or some other orthodox church)"))

# Opinion on Marijuana Legalization
order_opinion <- function(opinion){
  if(opinion == "notlegal"){
    return(1)
  }
  else if(opinion == "medicinal"){
    return(2)
  }
  else if(opinion == "personal"){
    return(3)
  }
}
## Apply the function
dummy_copy$opinion = sapply(dummy_copy$q46,order_opinion)
#-----------------------------------------------------------------------------
# Estimate Model 8
#-----------------------------------------------------------------------------
OrdProbit <- polr(factor(opinion) ~ log_age + log_income + hh1 + pastuse + bachelor_above +
                    below_bachelor + tolerant_state + expected_legal +
                    black + other_race + democrat + other_party + male +
                    christian + roman_catholic + liberal + conservative,
                    data = dummy_copy, Hess = TRUE,
                    method = 'probit' # set to 'probit' for ordered probit
)

# get the coefficients (except intercept)
round(coeftest(OrdProbit),2) # upto two places after decimal

# t test of coefficients:
#   
#                  Estimate Std. Error t value Pr(>|t|)    
#   log_age           -0.35       0.08   -4.51   <2e-16 ***
#   log_income         0.09       0.04    2.47     0.01 ** 
#   hh1               -0.02       0.02   -0.87     0.38    
#   pastuse            0.69       0.06   10.67   <2e-16 ***
#   bachelor_above     0.24       0.08    3.01   <2e-16 ***
#   below_bachelor     0.05       0.08    0.62     0.54    
#   tolerant_state     0.07       0.07    1.04     0.30    
#   expected_legal     0.57       0.07    7.76   <2e-16 ***
#   black              0.03       0.10    0.26     0.79    
#   other_race        -0.28       0.11   -2.56     0.01 ** 
#   democrat           0.44       0.09    5.03   <2e-16 ***
#   other_party        0.36       0.08    4.56   <2e-16 ***
#   male               0.06       0.06    1.00     0.32    
#   christian          0.16       0.10    1.60     0.11    
#   roman_catholic     0.10       0.09    1.19     0.23    
#   liberal            0.39       0.09    4.39   <2e-16 ***
#   conservative       0.09       0.12    0.76     0.45    
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#-----------------------------------------------------------------------------
# Estimate and Standard error of intercept and cut-point of Model 8
#-----------------------------------------------------------------------------
# The polr function in R, does not contain the intercept term by design
# So \gamma_1 = 0 convention is not followed either
# The choice of intercept which sets \gamma_1 = 0, is the negative of 1|2
# given by the polr function, i.e. 
#-----------------------------------------------------------------------------
# Estimates
#-----------------------------------------------------------------------------
intercept = -OrdProbit$zeta[1]
names(intercept) = NULL
round(intercept,2)
# 0.35

#-----------------------------------------------------------------------------
# This choice of intercept will make the other cutoff point (\gamma_2) to be
#-----------------------------------------------------------------------------
cut_point = OrdProbit$zeta[2] - OrdProbit$zeta[1]
names(cut_point)= NULL
round(cut_point,2) # upto two places after decimal
# 1.46
#-----------------------------------------------------------------------------
# Standard Error
#-----------------------------------------------------------------------------
se <- sqrt(diag(vcov(OrdProbit)))

# se_thresholds has standard errors of 1|2 and 2|3
se_thresholds <- se[names(se) %in% names(OrdProbit$zeta)]
se_thresholds
# 1|2       2|3 
# 0.4801557 0.4807061 

names(se_thresholds) = NULL

# covariance of the original thresholds 1|2 and 2|3 of polr
cov12 = vcov(OrdProbit)[18,19] # as 1|2 and 2|3 are the 18th and 19th variable
cov12
# 0.2295884

#-----------------------------------------------------------------------------
# the standard error of intercept will be the same as the standard error of
# OrdProbit$zeta[1]

# standard error of the intercept
round(se_thresholds[1],2)
# 0.48

# standard error of cut-point(\gamma_2) = se(2|3 - 1|2) = \sqrt(var(2|3) + var(1|2) - 2cov(1|2,2|3))
# where 1|2 and 2|3 are the default cutpoints given by polr

# standard error of the new cut-point(\gamma_2):
se_cut_point = sqrt(se_thresholds[2]^2 + se_thresholds[1]^2 - 2 * cov12)
round(se_cut_point,2)
# 0.05

#-----------------------------------------------------------------------------
# t-value of the intercept and cut-point
#-----------------------------------------------------------------------------
# intercept t-value
round(intercept/se_thresholds[1],2) # intercept/se(intercept)
# 0.72 

# intercept p-value
round(2 * pt(-abs(intercept/se_thresholds[1]), 1491),2)
# 0.47

# cut-point t-value
round(cut_point/se_cut_point,2) # (cut_point)/se(cut_point)
# 29.58

# cut-point p-value
round(2 * pt(-abs(cut_point/se_cut_point), 1491),2)
# 0
# we get p-value as 0, as it is less than 0.01
#-----------------------------------------------------------------------------
# LR Statistic, McFadden R^2 and Hit Rate

#-----------------------------------------------------------------------------
# LR Statistic
#----------------------------------------------------------------------------- 
model_reduced = polr(factor(opinion)~1,
                     data = dummy_copy, Hess = TRUE,
                     method = 'probit')
lr = -2*(logLik(model_reduced) - logLik(OrdProbit))
lr = as.double(lr)
round(lr,2)
# 377

#-----------------------------------------------------------------------------
# McFadden R^2 
#-----------------------------------------------------------------------------
Rm = 1 - logLik(OrdProbit)/logLik(model_reduced)
Rm = as.double(Rm)
round(Rm,2)
# 0.13

#-----------------------------------------------------------------------------
# Hit Rate
#-----------------------------------------------------------------------------
predicted_classes = predict(OrdProbit,type = "class")
hit_rate = mean(predicted_classes == dummy_copy$opinion)
round(hit_rate*100,2)
# 58.91

#-----------------------------------------------------------------------------
# (iii) Covariate Effects for Variables
#-----------------------------------------------------------------------------

# Coefficients
betas = OrdProbit$coefficients

# X and cut points
X = dummy_copy[names(betas)]
opinions = dummy_copy$opinion
cut_points = c(-Inf,OrdProbit$zeta[1],OrdProbit$zeta[2],Inf)


#-----------------------------------------------------------------------------
# Covariate Effect of Age, 10 years
#-----------------------------------------------------------------------------
x_age1 = X
# increment age by 10 years
x_age1[1:dim(X)[1],1] = log(exp(x_age1[1:dim(X)[1],1]) + 10) 
x_age0 = X

ce_age = rep(0,3)
for(i in 1:3){
  ce_age[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_age1)%*%as.matrix(betas))-
                     pnorm(cut_points[i] - as.matrix(x_age1)%*%as.matrix(betas)))- 
    mean(pnorm(cut_points[i+1] - as.matrix(x_age0)%*%as.matrix(betas))-
                     pnorm(cut_points[i] - as.matrix(x_age0)%*%as.matrix(betas)))
}
names(ce_age) <- c("not legal","medicinal use","personal use")
round(ce_age,3)
# not legal medicinal use  personal use 
#     0.015         0.012        -0.028

#-----------------------------------------------------------------------------
# Covariate Effect of Income, $10000 
#-----------------------------------------------------------------------------
x_income1 = X
x_income1[1:dim(X)[1],2] = log(exp(x_income1[1:dim(X)[1],2]) + 10000)
x_income0 = X
# x_past_use0[1:dim(X)[1],1] = 0

ce_income = rep(0,3)
for(i in 1:3){
  ce_income[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_income1)%*%as.matrix(betas))-
                    pnorm(cut_points[i] - as.matrix(x_income1)%*%as.matrix(betas)))- 
    mean(pnorm(cut_points[i+1] - as.matrix(x_income0)%*%as.matrix(betas))-
                    pnorm(cut_points[i] - as.matrix(x_income0)%*%as.matrix(betas)))
}
names(ce_income) <- c("not legal","medicinal use","personal use")
round(ce_income,3)
# not legal medicinal use  personal use 
#    -0.005        -0.003         0.008 

#-----------------------------------------------------------------------------
# Covariate Effect of Past Use
#-----------------------------------------------------------------------------
x_past_use1 = X
x_past_use1[1:dim(X)[1],4] = 1
x_past_use0 = X
x_past_use0[1:dim(X)[1],4] = 0

ce_pastuse = rep(0,3)
for(i in 1:3){
  ce_pastuse[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_past_use1)%*%as.matrix(betas))-
                        pnorm(cut_points[i] - as.matrix(x_past_use1)%*%as.matrix(betas)))-
                  mean(pnorm(cut_points[i+1] - as.matrix(x_past_use0)%*%as.matrix(betas))-
                        pnorm(cut_points[i] - as.matrix(x_past_use0)%*%as.matrix(betas)))
}
names(ce_pastuse) <- c("not legal","medicinal use","personal use")
round(ce_pastuse,3)
# not legal medicinal use  personal use 
#    -0.129        -0.113         0.243 

#-----------------------------------------------------------------------------
# Covariate Effect of Bachelor and Above
#-----------------------------------------------------------------------------
x_bachelorabove1 = X
x_bachelorabove1[1:dim(X)[1],5] = 1
x_bachelorabove0 = X
x_bachelorabove0[1:dim(X)[1],5] = 0

ce_bachelorabove = rep(0,3)
for(i in 1:3){
  ce_bachelorabove[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_bachelorabove1)%*%as.matrix(betas))-
                         pnorm(cut_points[i] - as.matrix(x_bachelorabove1)%*%as.matrix(betas)))- 
                        mean(pnorm(cut_points[i+1] - as.matrix(x_bachelorabove0)%*%as.matrix(betas))-
                         pnorm(cut_points[i] - as.matrix(x_bachelorabove0)%*%as.matrix(betas)))
}
names(ce_bachelorabove) <- c("not legal","medicinal use","personal use")
round(ce_bachelorabove,3)
# not legal medicinal use  personal use 
#    -0.045        -0.035         0.080

#-----------------------------------------------------------------------------
# Covariate Effect of Eventually Legal
#-----------------------------------------------------------------------------
x_event_legal1 = X
x_event_legal1[1:dim(X)[1],8] = 1
x_event_legal0 = X
x_event_legal0[1:dim(X)[1],8] = 0

ce_event_legal = rep(0,3)
for(i in 1:3){
  ce_event_legal[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_event_legal1)%*%as.matrix(betas))-
                               pnorm(cut_points[i] - as.matrix(x_event_legal1)%*%as.matrix(betas)))- 
                      mean(pnorm(cut_points[i+1] - as.matrix(x_event_legal0)%*%as.matrix(betas))-
                               pnorm(cut_points[i] - as.matrix(x_event_legal0)%*%as.matrix(betas)))
}
names(ce_event_legal) <- c("not legal","medicinal use","personal use")
round(ce_event_legal,3)
# not legal medicinal use  personal use 
#    -0.126        -0.060         0.186 

#-----------------------------------------------------------------------------
# Covariate Effect of Other Races
#-----------------------------------------------------------------------------
x_other_race1 = X
x_other_race1[1:dim(X)[1],10] = 1
x_other_race0 = X
x_other_race0[1:dim(X)[1],10] = 0

ce_other_race = rep(0,3)
for(i in 1:3){
  ce_other_race[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_other_race1)%*%as.matrix(betas))-
                             pnorm(cut_points[i] - as.matrix(x_other_race1)%*%as.matrix(betas)))-
                     mean(pnorm(cut_points[i+1] - as.matrix(x_other_race0)%*%as.matrix(betas))-
                             pnorm(cut_points[i] - as.matrix(x_other_race0)%*%as.matrix(betas)))
}
names(ce_other_race) <- c("not legal","medicinal use","personal use")
round(ce_other_race,3)
# not legal medicinal use  personal use 
#     0.059         0.031        -0.089 

#-----------------------------------------------------------------------------
# Covariate Effect of Democrat
#-----------------------------------------------------------------------------
x_democrat1 = X
x_democrat1[1:dim(X)[1],11] = 1
x_democrat0 = X
x_democrat0[1:dim(X)[1],11] = 0

ce_democrat = rep(0,3)
for(i in 1:3){
  ce_democrat[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_democrat1)%*%as.matrix(betas))-
                            pnorm(cut_points[i] - as.matrix(x_democrat1)%*%as.matrix(betas)))- 
                   mean(pnorm(cut_points[i+1] - as.matrix(x_democrat0)%*%as.matrix(betas))-
                            pnorm(cut_points[i] - as.matrix(x_democrat0)%*%as.matrix(betas)))
}
names(ce_democrat) <- c("not legal","medicinal use","personal use")
round(ce_democrat,3)

# not legal medicinal use  personal use 
#    -0.080        -0.066         0.147
#-----------------------------------------------------------------------------
# Covariate Effect of Other Party
#-----------------------------------------------------------------------------
x_other_party1 = X
x_other_party1[1:dim(X)[1],12] = 1
x_other_party0 = X
x_other_party0[1:dim(X)[1],12] = 0

ce_other_party = rep(0,3)
for(i in 1:3){
  ce_other_party[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_other_party1)%*%as.matrix(betas))-
                          pnorm(cut_points[i] - as.matrix(x_other_party1)%*%as.matrix(betas)))- 
                      mean(pnorm(cut_points[i+1] - as.matrix(x_other_party0)%*%as.matrix(betas))-
                          pnorm(cut_points[i] - as.matrix(x_other_party0)%*%as.matrix(betas)))
}
names(ce_other_party) <- c("not legal","medicinal use","personal use")
round(ce_other_party,3)

# not legal medicinal use  personal use 
#    -0.070        -0.051         0.121 
#-----------------------------------------------------------------------------
# Covariate Effect of Liberal
#-----------------------------------------------------------------------------
x_liberal1 = X
x_liberal1[1:dim(X)[1],16] = 1
x_liberal0 = X
x_liberal0[1:dim(X)[1],16] = 0

ce_liberal = rep(0,3)
for(i in 1:3){
  ce_liberal[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_liberal1)%*%as.matrix(betas))-
                             pnorm(cut_points[i] - as.matrix(x_liberal1)%*%as.matrix(betas)))- 
                  mean(pnorm(cut_points[i+1] - as.matrix(x_liberal0)%*%as.matrix(betas))-
                             pnorm(cut_points[i] - as.matrix(x_liberal0)%*%as.matrix(betas)))
}
names(ce_liberal) <- c("not legal","medicinal use","personal use")
round(ce_liberal,3)

# not legal medicinal use  personal use 
#    -0.068        -0.066         0.134 
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------





#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Question 2
#-----------------------------------------------------------------------------
# Load the data
Grunfeld220obs <- read_excel("C:/Users/utpal/Documents/Notes/Projects/Econometrics/Assignments/Assignment 2/Grunfeld220obs.xlsx", 
  sheet = "Sheet1")

# List of firms in the data
unique(Grunfeld220obs$firm)
# [1] "General Motors"    "US Steel"          "General Electric"  "Chrysler"         
# [5] "Atlantic Refining" "IBM"               "Union Oil"         "Westinghouse"     
# [9] "Goodyear"          "Diamond Match"     "American Steel"   

# Exclude "American Steel"
exclude = unique(Grunfeld220obs$firm)[11]
Grunfeld <- Grunfeld220obs[Grunfeld220obs$firm != exclude,]

#-----------------------------------------------------------------------------
# 2.a
#-----------------------------------------------------------------------------
# Pooled Effects Model
#-----------------------------------------------------------------------------
pooled_effect_plm <- plm(invest ~ capital + value, data = Grunfeld,
                      index = c("firm", "year"),
                      effect = "individual", model = "pooling")
summary(pooled_effect_plm)
# Pooling Model
# 
# Call:
#   plm(formula = invest ~ capital + value, data = Grunfeld, effect = "individual", 
#       model = "pooling", index = c("firm", "year"))
# 
# Balanced Panel: n = 10, T = 20, N = 200
# 
# Residuals:
#       Min.   1st Qu.    Median   3rd Qu.      Max. 
# -291.6757  -30.0137    5.3033   34.8293  369.4464 
# 
# Coefficients:
#                  Estimate  Std. Error t-value  Pr(>|t|)    
# (Intercept)   -42.7143694   9.5116760 -4.4907 1.207e-05 ***
#   capital       0.2306785   0.0254758  9.0548 < 2.2e-16 ***
#   value         0.1155622   0.0058357 19.8026 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    9359900
# Residual Sum of Squares: 1755900
# R-Squared:      0.81241
# Adj. R-Squared: 0.8105
# F-statistic: 426.576 on 2 and 197 DF, p-value: < 2.22e-16

#-----------------------------------------------------------------------------
# 2.b
#-----------------------------------------------------------------------------
# Fixed Effects Model
#-----------------------------------------------------------------------------
fe_model_plm <- plm(invest ~ capital + value, data = Grunfeld,
                      index = c("firm", "year"),
                      effect = "individual", model = "within")
summary(fe_model_plm)
# Oneway (individual) effect Within Model
# 
# Call:
#   plm(formula = invest ~ capital + value, data = Grunfeld, effect = "individual", 
#       model = "within", index = c("firm", "year"))
# 
# Balanced Panel: n = 10, T = 20, N = 200
# 
# Residuals:
#   Min.    1st Qu.     Median    3rd Qu.       Max. 
# -184.00857  -17.64316    0.56337   19.19222  250.70974 
# 
# Coefficients:
#         Estimate Std. Error t-value  Pr(>|t|)    
# capital 0.310065   0.017355 17.8666 < 2.2e-16 ***
#   value 0.110124   0.011857  9.2879 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    2244400
# Residual Sum of Squares: 523480
# R-Squared:      0.76676
# Adj. R-Squared: 0.75311
# F-statistic: 309.014 on 2 and 188 DF, p-value: < 2.22e-16

#-----------------------------------------------------------------------------
# Firm specific intercepts
#-----------------------------------------------------------------------------
round(summary(fixef(fe_model_plm)),2) # upto two decimal places

#                     Estimate Std. Error t-value Pr(>|t|)    
# Atlantic Refining    -114.62      14.17   -8.09   <2e-16 ***
# Chrysler              -27.81      14.08   -1.98     0.05 *  
# Diamond Match          -6.57      11.83   -0.56     0.58    
# General Electric     -235.57      24.43   -9.64   <2e-16 ***
# General Motors        -70.30      49.71   -1.41     0.16    
# Goodyear              -87.22      12.89   -6.77   <2e-16 ***
# IBM                   -23.16      12.67   -1.83     0.07 .  
# Union Oil             -66.55      12.84   -5.18   <2e-16 ***
# US Steel              101.91      24.94    4.09   <2e-16 ***
# Westinghouse          -57.55      13.99   -4.11   <2e-16 ***
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# 2.c
#-----------------------------------------------------------------------------
# Random Effects Model
#-----------------------------------------------------------------------------
re_model_plm <- plm(invest ~ capital + value, data = Grunfeld,
                      index = c("firm", "year"),
                      effect = "individual", model = "random")
summary(re_model_plm)
# Oneway (individual) effect Random Effect Model 
# (Swamy-Arora's transformation)
# 
# Call:
# plm(formula = invest ~ capital + value, data = Grunfeld, effect = "individual", 
#     model = "random", index = c("firm", "year"))
# 
# Balanced Panel: n = 10, T = 20, N = 200
# 
# Effects:
#                   var std.dev share
# idiosyncratic 2784.46   52.77 0.282
# individual    7089.80   84.20 0.718
# theta: 0.8612
# 
# Residuals:
#      Min.   1st Qu.    Median   3rd Qu.      Max. 
# -177.6063  -19.7350    4.6851   19.5105  252.8743 
# 
# Coefficients:
#               Estimate Std. Error z-value Pr(>|z|)    
# (Intercept) -57.834415  28.898935 -2.0013  0.04536 *  
# capital       0.308113   0.017180 17.9339  < 2e-16 ***
# value         0.109781   0.010493 10.4627  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    2381400
# Residual Sum of Squares: 548900
# R-Squared:      0.7695
# Adj. R-Squared: 0.76716
# Chisq: 657.674 on 2 DF, p-value: < 2.22e-16

#-----------------------------------------------------------------------------
# Additional Analysis
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#Test for Individual Effects
#-----------------------------------------------------------------------------
pFtest(fe_model_plm, pooled_effect_plm)

# OUTPUT:
# F test for individual effects
# 
# data:  invest ~ capital + value
# F = 49.177, df1 = 9, df2 = 188, p-value < 2.2e-16
# alternative hypothesis: significant effects

# As the p-value is very small(\(<2.2\times10^{-16}\)), the null
# hypothesis is rejected in favor of the alternative that there are 
# significant individual effects. Hence, a Fixed Effect model will be 
# a better choice than a Pooled Effect model, in this case.
#-----------------------------------------------------------------------------
# Hausman Test
#-----------------------------------------------------------------------------
phtest(fe_model_plm,re_model_plm)

# OUTPUT:
# Hausman Test
# 
# data:  invest ~ capital + value
# chisq = 2.3304, df = 2, p-value = 0.3119
# alternative hypothesis: one model is inconsistent

# The null hypothesis cannot be rejected here, as (\(\text{p-value} = 0.3119\)). 
# Hence it makes sense to use a Random Effects model instead of a Fixed Effects model.
#-----------------------------------------------------------------------------