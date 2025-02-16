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

dummy_copy

#-----------------------------------------------------------------------------
# Descriptive Summary of Continuous variables
#-----------------------------------------------------------------------------
means <- rep(0,3)
stdev <- rep(0,3)
ct = 1
for(col in c('log_age','log_income','hh1')){
  means[ct] = mean(dummy_copy[[col]])
  stdev[ct] = sd(dummy_copy[[col]])
  ct = ct + 1
}

# Create a tables for Descriptive Summary of Continuous Variables
table_cont <- data.frame(
  Variable = c('log_age','log_income','hh1'),
  Mean = means,
  StdDev = stdev
)

table_cont
#    Variable      Mean    StdDev
#1    log_age  3.724690 0.4442719
#2 log_income 10.628089 0.9812756
#3        hh1  2.737936 1.4209633

#-----------------------------------------------------------------------------
# Descriptive Summary of Categorical variables
#-----------------------------------------------------------------------------
# past use and male
#-----------------------------------------------------------------------------
table_use_and_male <- data.frame(
  Variable = c("Past Use", "Male"),
  Counts = c(sum(dummy_copy$q57=='Yes'),sum(dummy_copy$sex=="Male")),
  Percentage = c(mean(dummy_copy$q57=='Yes')*100, mean(dummy_copy$sex=="Male")*100)
)

table_use_and_male
#  Variable Counts Percentage
#1 Past Use    719   48.19035
#2     Male    791   53.01609
#-----------------------------------------------------------------------------
# Education
#-----------------------------------------------------------------------------
educ_counts = rep(0,3)
educ_counts[1] = sum(dummy_copy$educ2 == "Postgraduate Degree" | dummy_copy$educ2 == "Four year college"
                     | dummy_copy$educ2 == "Some Postgraduate")
educ_counts[2] = sum(dummy_copy$educ2 == 'Some College' | dummy_copy$educ2 == "Associate Degree")
educ_counts[3] = sum(dummy_copy$educ2 == 'HS' | dummy_copy$educ2 == 'Less than HS' 
                     | dummy_copy$educ2 == 'HS Incomplete')

educ_per = rep(0,3)
educ_per[1] = 100*mean(dummy_copy$educ2 == "Postgraduate Degree" | dummy_copy$educ2 == "Four year college"
                     | dummy_copy$educ2 == "Some Postgraduate")
educ_per[2] = 100*mean(dummy_copy$educ2 == 'Some College' | dummy_copy$educ2 == "Associate Degree")
educ_per[3] = 100*mean(dummy_copy$educ2 == 'HS' | dummy_copy$educ2 == 'Less than HS' 
                     | dummy_copy$educ2 == 'HS Incomplete')

table_educ = data.frame(
  Category = c("Bachelors & Above", "Below Bachelors", "High School & Below"),
  Counts = educ_counts,
  Percentage = educ_per
)

table_educ
#             Category Counts Percentage
#1   Bachelors & Above    551   36.93029
#2     Below Bachelors    434   29.08847
#3 High School & Below    507   33.98123

#-----------------------------------------------------------------------------
# Eventually legal
#-----------------------------------------------------------------------------
sum(dummy_copy$q55 == "Yes, it will")
mean(dummy_copy$q55 == "Yes, it will")*100
table_even_legal <- data.frame(
  Variable = 'Eventually legal',
  Counts = sum(dummy_copy$q55 == "Yes, it will"),
  Percentage = mean(dummy_copy$q55 == "Yes, it will")*100
)
table_even_legal
#          Variable Counts Percentage
#1 Eventually legal   1154   77.34584

#-----------------------------------------------------------------------------
# Race
#-----------------------------------------------------------------------------
## White
sum(dummy_copy$race3m1 == 'White')
mean(dummy_copy$race3m1 == "White")*100

## African American
sum(dummy_copy$race3m1 == "Black or African-American")
mean(dummy_copy$race3m1 == "Black or African-American")*100

## Others
length(dummy_copy$educ2) - sum(dummy_copy$race3m1 == 'White') -
  sum(dummy_copy$race3m1 == "Black or African-American")
(1 - mean(dummy_copy$race3m1 == 'White') -
  mean(dummy_copy$race3m1 == "Black or African-American"))*100

table_race = data.frame(
  Category = c("White","African American","Others"),
  Counts = c(sum(dummy_copy$race3m1 == 'White'),
             sum(dummy_copy$race3m1 == "Black or African-American"),
             sum(dummy_copy$race3m1 != "White" 
                 & dummy_copy$race3m1 != "Black or African-American")),
  Percentage = c(100*mean(dummy_copy$race3m1 == 'White'),
                 100*mean(dummy_copy$race3m1 == "Black or African-American"),
                 100*mean(dummy_copy$race3m1 != "White" 
                     & dummy_copy$race3m1 != "Black or African-American"))
)
table_race
#          Category Counts Percentage
#1            White   1149  77.010724
#2 African American    202  13.538874
#3           Others    141   9.450402

#-----------------------------------------------------------------------------
# Party Affiliation
#-----------------------------------------------------------------------------
table_party = data.frame(
  Category = c("Republican","Democrat","Independent & Others"),
  Counts = c(sum(dummy_copy$party == "Republican"),
             sum(dummy_copy$party == "Democrat"),
             sum(dummy_copy$party != "Republican" 
                 & dummy_copy$party != "Democrat")),
  Percentage = c(100*mean(dummy_copy$party == "Republican"),
                 100*mean(dummy_copy$party == "Democrat"),
                 100*mean(dummy_copy$party != "Republican" 
                     & dummy_copy$party != "Democrat"))
)
table_party

#              Category Counts Percentage
#1           Republican    333   22.31903
#2             Democrat    511   34.24933
#3 Independent & Others    648   43.43164

#-----------------------------------------------------------------------------
# Religion
#-----------------------------------------------------------------------------
## Christian
christian_count = sum(dummy_copy$relig == "Christian (VOL.)")
christian_per = mean(dummy_copy$relig == "Christian (VOL.)")*100

## Roman Catholic
rom_cat_count = sum(dummy_copy$relig == "Roman Catholic (Catholic)")
rom_cat_per = mean(dummy_copy$relig == "Roman Catholic (Catholic)")*100

## Protestant
pro_count = sum(dummy_copy$relig == "Protestant (Baptist, Methodist, Non-denominational, Lutheran, Presbyterian, Pentecostal, Episcopalian, Reformed, etc.)")
pro_per = mean(dummy_copy$relig == "Protestant (Baptist, Methodist, Non-denominational, Lutheran, Presbyterian, Pentecostal, Episcopalian, Reformed, etc.)")*100

## Liberal
liberal_count = sum(dummy_copy$relig == "Agnostic (not sure if there is a God)"
    | dummy_copy$relig == "Nothing in particular" 
    | dummy_copy$relig == "Atheist (do not believe in God)"
    | dummy_copy$relig == "Unitarian (Universalist) (VOL.)")
liberal_per = mean(dummy_copy$relig == "Agnostic (not sure if there is a God)"
    | dummy_copy$relig == "Nothing in particular" 
    | dummy_copy$relig == "Atheist (do not believe in God)"
    | dummy_copy$relig == "Unitarian (Universalist) (VOL.)")*100

## Conservative
con_count = sum(dummy_copy$relig == "Mormon (Church of Jesus Christ of Latter-day Saints/LDS)"
    | dummy_copy$relig == "Jewish (Judaism)"
    | dummy_copy$relig == "Hindu"
    | dummy_copy$relig == "Buddhist"
    | dummy_copy$relig == "Muslim (Islam)"
    | dummy_copy$relig == "Orthodox (Greek, Russian, or some other orthodox church)")
con_per = mean(dummy_copy$relig == "Mormon (Church of Jesus Christ of Latter-day Saints/LDS)"
    | dummy_copy$relig == "Jewish (Judaism)"
    | dummy_copy$relig == "Hindu"
    | dummy_copy$relig == "Buddhist"
    | dummy_copy$relig == "Muslim (Islam)"
    | dummy_copy$relig == "Orthodox (Greek, Russian, or some other orthodox church)")*100

table_rel <- data.frame(
  Category = c("Christain","Roman Catholic","Protestant","Liberal","Conservative"),
  Counts = c(christian_count, rom_cat_count, pro_count, liberal_count, con_count),
  Percentage = c(christian_per, rom_cat_per, pro_per, liberal_per, con_per)
)

table_rel
#        Category Counts Percentage
#1      Christain    182  12.198391
#2 Roman Catholic    290  19.436997
#3     Protestant    550  36.863271
#4        Liberal    348  23.324397
#5   Conservative    122   8.176944
#-----------------------------------------------------------------------------
# Public Opinion
#-----------------------------------------------------------------------------
table_opinion <- data.frame(
  Category = c("Medicinal","Not Legal","Personal"),
  Counts = c(sum(dummy_copy$q46 == "medicinal"),sum(dummy_copy$q46 == "notlegal"),
             sum(dummy_copy$q46 == "personal")),
  Percentage = c(100*mean(dummy_copy$q46 == "medicinal"),100*mean(dummy_copy$q46 == "notlegal"),
                 100*mean(dummy_copy$q46 == "personal"))
)
table_opinion
#   Category Counts Percentage
#1 Medicinal    640   42.89544
#2 Not Legal    218   14.61126
#3  Personal    634   42.49330
#-----------------------------------------------------------------------------
# Tolerant States
#-----------------------------------------------------------------------------
tolerant_states <- c("Alaska", "Arizona", "California", "Colorado", "Connecticut", 
                     "Delaware", "Hawaii", "Illinois", "Maine", "Maryland", 
                     "Massachusetts", "Michigan", "Montana", "Nevada", "New Hampshire", 
                     "New Jersey", "New Mexico", "Oregon", "Rhode Island", "Vermont", 
                     "Washington", "Washington DC", "District of Columbia")

count_tol = 0
for(state in tolerant_states){
  count_tol = count_tol + sum(dummy_copy$state == state)
}

table_tol <- data.frame(
  Variable = "Tolerant States",
  Counts = count_tol,
  Percentage = 100*count_tol/length(dummy_copy$state)
)
table_tol
#         Variable Counts Percentage
#1 Tolerant States    556   37.26542

#-----------------------------------------------------------------------------
# (ii) Analyze public opinion on extent marijuana legalization in the US i.e., estimate Model 8
#-----------------------------------------------------------------------------
# Model 8
# Prepare the variables for the model
#-----------------------------------------------------------------------------
# Intercept
dummy_copy$intercept = rep(1,length(dummy_copy$q46))

# Past Use
dummy_copy$pastuse = (dummy_copy$q57 == "Yes") + 0

# Male
dummy_copy$male = (dummy_copy$sex == "Male") + 0

# Education
dummy_copy$bachelor_above = (dummy_copy$educ2 == "Postgraduate Degree") + (dummy_copy$educ2 == "Four year college") + (dummy_copy$educ2 == "Some Postgraduate")
dummy_copy$below_bachelor = (dummy_copy$educ2 == 'Some College') + (dummy_copy$educ2 == "Associate Degree")
dummy_copy$below_hs = (dummy_copy$educ2 == 'HS') + (dummy_copy$educ2 == 'Less than HS') + (dummy_copy$educ2 == 'HS Incomplete')

# Tolerant state
## function to check if a state is tolerant
is_tolerant <- function(state){
  if(sum(state == tolerant_states) == 1){
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
dummy_copy$protestant = (dummy_copy$relig == "Protestant (Baptist, Methodist, Non-denominational, Lutheran, Presbyterian, Pentecostal, Episcopalian, Reformed, etc.)")
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

# get the coefficients
coeftest(OrdProbit)

# t test of coefficients:
#   
#                       Estimate Std. Error t value  Pr(>|t|)    
#   log_age            -0.354255   0.078488 -4.5135 6.884e-06 ***
#   log_income          0.086380   0.035015  2.4670   0.01374 *  
#   hh1                -0.020272   0.023174 -0.8748   0.38185    
#   pastuseTRUE         0.685678   0.064285 10.6662 < 2.2e-16 ***
#   bachelor_above      0.240526   0.079949  3.0085   0.00267 ** 
#   below_bachelor      0.048159   0.078118  0.6165   0.53767    
#   tolerant_state      0.068051   0.065381  1.0408   0.29812    
#   expected_legalTRUE  0.566664   0.073017  7.7607 1.569e-14 ***
#   blackTRUE           0.025615   0.098506  0.2600   0.79488    
#   other_race         -0.275511   0.107530 -2.5622   0.01050 *  
#   democratTRUE        0.440368   0.087511  5.0321 5.447e-07 ***
#   other_party         0.364407   0.079980  4.5562 5.638e-06 ***
#   maleTRUE            0.062537   0.062727  0.9970   0.31894    
#   christianTRUE       0.164217   0.102767  1.5979   0.11027    
#   roman_catholucTRUE  0.104086   0.087470  1.1900   0.23426    
#   liberal             0.390372   0.088889  4.3917 1.205e-05 ***
#   conservative        0.091912   0.120986  0.7597   0.44756    
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#-----------------------------------------------------------------------------
# Standard error of intercept and cut-point of Model 8
#-----------------------------------------------------------------------------

# The polr function in R, does not contain the intercept term by design
# Also \gamma_1 = 0, convention is not followed either
# The choice of intercept which sets \gamma_1 = 0, is the negative of 1|2
# given by the polr function, i.e. 
#-----------------------------------------------------------------------------
intercept = -OrdProbit$zeta[1]
names(intercept) = NULL
intercept
# 0.3453124

#-----------------------------------------------------------------------------
# the standard error of intercept will be the same as the standard error of
# OrdProbit$zeta[1]

# This choice of intercept will make the other cutoff point (\gamma_2) to be
#-----------------------------------------------------------------------------
cut_point = OrdProbit$zeta[2] - OrdProbit$zeta[1]
cut_point
# 2|3 
# 1.464429 
#-----------------------------------------------------------------------------
# standard errors of the intercept and threshold
se <- sqrt(diag(vcov(OrdProbit)))

# se_thresholds has standard errors of 1|2 and 2|3
se_thresholds <- se[names(se) %in% names(OrdProbit$zeta)]
se_thresholds
# 1|2       2|3 
# 0.4801557 0.4807061 

#-----------------------------------------------------------------------------
# covariance of the original thresholds 1|2 and 2|3 of polr
cov12 = vcov(OrdProbit)[18,19] # as 1|2 and 2|3 are the 18th and 19th variable
cov12
# 0.2295884
#-----------------------------------------------------------------------------
# standard error of the intercept
se_thresholds[1]
# 0.4801557 

# standard error of cut-point = se(2|3 - 1|2) = \sqrt(var(2|3) + var(1|2) - 2 cov(1|2,2|3))
se_cut_point = sqrt(se_thresholds[2]^2 + se_thresholds[1]^2 - 2 * cov12)
names(se_cut_point) = NULL
se_cut_point
# 0.04950927 

#-----------------------------------------------------------------------------
# LR Statistic
#----------------------------------------------------------------------------- 
model_reduced = polr(factor(opinion)~1,
                     data = dummy_copy, Hess = TRUE,
                     method = 'probit')
lr = -2*(logLik(model_reduced) - logLik(OrdProbit))
lr
# 'log Lik.' 377.0007 (df=2)

#-----------------------------------------------------------------------------
# McFadden R^2 
#-----------------------------------------------------------------------------
Rm = 1 - logLik(OrdProbit)/logLik(model_reduced)
Rm
# 'log Lik.' 0.1253671 (df=19)

#-----------------------------------------------------------------------------
# Hit Rate
#-----------------------------------------------------------------------------
predicted_classes = predict(OrdProbit,type = "class")
hit_rate = mean(predicted_classes == dummy_copy$opinion)
hit_rate*100
# 58.91421

#-----------------------------------------------------------------------------
# (iii) Covariate Effects for Variables
#-----------------------------------------------------------------------------
# Age, 10 years
# Income $ 10,000
#-----------------------------------------------------------------------------
betas = OrdProbit$coefficients
betas
# log_age     log_income            hh1        pastuse bachelor_above 
# -0.35425503     0.08637971    -0.02027156     0.68567777     0.24052640 
# below_bachelor tolerant_state expected_legal          black     other_race 
# 0.04815898     0.06805104     0.56666363     0.02561470    -0.27551116 
# democrat    other_party           male      christian roman_catholic 
# 0.44036840     0.36440738     0.06253713     0.16421691     0.10408574 
# liberal   conservative 
# 0.39037183     0.09191198 
X = dummy_copy[names(betas)]
opinions = dummy_copy$opinion
cut_points = c(-Inf,OrdProbit$zeta[1],OrdProbit$zeta[2],Inf)

#-----------------------------------------------------------------------------
# Covariate Effect of Age, 10 years
#-----------------------------------------------------------------------------
x_age1 = X
x_age1[1:dim(X)[1],1] = log(exp(x_age1[1:dim(X)[1],1]) + 10)
x_age0 = X
# x_past_use0[1:dim(X)[1],1] = 0

ce_age = rep(0,3)
for(i in 1:3){
  ce_age[i] = mean(pnorm(cut_points[i+1] - as.matrix(x_age1)%*%as.matrix(betas))-
                     pnorm(cut_points[i] - as.matrix(x_age1)%*%as.matrix(betas)))- mean(pnorm(cut_points[i+1] - as.matrix(x_age0)%*%as.matrix(betas))-
                                                                                          pnorm(cut_points[i] - as.matrix(x_age0)%*%as.matrix(betas)))
}

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
                     pnorm(cut_points[i] - as.matrix(x_income1)%*%as.matrix(betas)))- mean(pnorm(cut_points[i+1] - as.matrix(x_income0)%*%as.matrix(betas))-
                                                                                          pnorm(cut_points[i] - as.matrix(x_income0)%*%as.matrix(betas)))
}

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
                         pnorm(cut_points[i] - as.matrix(x_past_use1)%*%as.matrix(betas)))- mean(pnorm(cut_points[i+1] - as.matrix(x_past_use0)%*%as.matrix(betas))-
           pnorm(cut_points[i] - as.matrix(x_past_use0)%*%as.matrix(betas)))
}

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
                         pnorm(cut_points[i] - as.matrix(x_bachelorabove1)%*%as.matrix(betas)))- mean(pnorm(cut_points[i+1] - as.matrix(x_bachelorabove0)%*%as.matrix(betas))-
                                                                                                   pnorm(cut_points[i] - as.matrix(x_bachelorabove0)%*%as.matrix(betas)))
}

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
                               pnorm(cut_points[i] - as.matrix(x_event_legal1)%*%as.matrix(betas)))- mean(pnorm(cut_points[i+1] - as.matrix(x_event_legal0)%*%as.matrix(betas))-
                                                                                                              pnorm(cut_points[i] - as.matrix(x_event_legal0)%*%as.matrix(betas)))
}

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
                             pnorm(cut_points[i] - as.matrix(x_other_race1)%*%as.matrix(betas)))- mean(pnorm(cut_points[i+1] - as.matrix(x_other_race0)%*%as.matrix(betas))-
                                                                                                          pnorm(cut_points[i] - as.matrix(x_other_race0)%*%as.matrix(betas)))
}
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
                            pnorm(cut_points[i] - as.matrix(x_democrat1)%*%as.matrix(betas)))- mean(pnorm(cut_points[i+1] - as.matrix(x_democrat0)%*%as.matrix(betas))-
                                                                                                        pnorm(cut_points[i] - as.matrix(x_democrat0)%*%as.matrix(betas)))
}
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
                          pnorm(cut_points[i] - as.matrix(x_other_party1)%*%as.matrix(betas)))- mean(pnorm(cut_points[i+1] - as.matrix(x_other_party0)%*%as.matrix(betas))-
                                                                                                    pnorm(cut_points[i] - as.matrix(x_other_party0)%*%as.matrix(betas)))
}
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
                             pnorm(cut_points[i] - as.matrix(x_liberal1)%*%as.matrix(betas)))- mean(pnorm(cut_points[i+1] - as.matrix(x_liberal0)%*%as.matrix(betas))-
                                                                                                          pnorm(cut_points[i] - as.matrix(x_liberal0)%*%as.matrix(betas)))
}
#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
# Question 2.a

#-----------------------------------------------------------------------------
