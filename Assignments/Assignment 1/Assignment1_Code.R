#-----------------------------------------------------------------------------
# Coding part of Econometrics Homework 1
# Utpalraj Kemprai
#-----------------------------------------------------------------------------

# Load libraries
library(readxl)
library(AER)

#-----------------------------------------------------------------------------
# Question (2) 
#-----------------------------------------------------------------------------

# read the data from file
file_path = "C:/Users/utpal/Documents/Notes/Econometrics/Assignments/Assignment 1/TransportChoiceDataset.xlsx"
TransportChoiceDataset <- read_excel(file_path, col_types = c("numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "skip", "skip", "skip", "skip", "skip", 
                                                   "skip", "skip"))

#-----------------------------------------------------------------------------
# part(a)
#-----------------------------------------------------------------------------

# View the data
View(TransportChoiceDataset)

# Data Types
# "DCOST :Continuous"
# "CARS :Discrete"
# "DOVTT :Continuous"
# "DIVTT :Continuous"
# "INTCPT :Discrete" 
# "DEPEND :Discrete"


# Create a tables for descriptive summary of continuous variables
table_cont <- data.frame(
  Name = c("DCOST", "DOVTT", "DIVTT"),
  Mean = c(mean(TransportChoiceDataset[['DCOST']]), mean(TransportChoiceDataset[['DOVTT']]), mean(TransportChoiceDataset[['DIVTT']])),
  StdDev = c(sd(TransportChoiceDataset[['DCOST']]), sd(TransportChoiceDataset[['DOVTT']]), sd(TransportChoiceDataset[['DIVTT']]))
)

options(digits = 4)
# view table
table_cont

#    Name   Mean StdDev
# 1 DCOST -12.94  37.97
# 2 DOVTT  12.85  10.06
# 3 DIVTT  17.05  17.96

#-----------------------------------------------------------------------------

# INTCPT
unique(TransportChoiceDataset$INTCPT)
# [1] 1

# Descriptive Summary for Discrete variables
options(digits = 3) # calculate upto two digit after decimal

#-----------------------------------------------------------------------------
# Descriptive summary for CARS
#-----------------------------------------------------------------------------
count_table = table(TransportChoiceDataset$CARS)
percentage_table = prop.table(count_table) * 100
table_cars <- data.frame(
  Count = count_table,
  Percentage = percentage_table
)
table_cars <- table_cars[c("Count.Var1","Count.Freq", "Percentage.Freq")]
colnames(table_cars) = c('CARS','count','percentage' )
table_cars  

#   CARS count percentage
# 1    0    81       9.620
# 2    1   359      42.637
# 3    2   322      38.242
# 4    3    64       7.601
# 5    4    12       1.425
# 6    5     3       0.356
# 7    7     1       0.119

#-----------------------------------------------------------------------------

options(digits = 4)
#-----------------------------------------------------------------------------
# Descriptive summary for DEPEND
#-----------------------------------------------------------------------------
count_table = table(TransportChoiceDataset$DEPEND)
percentage_table = prop.table(count_table) * 100
table_depend <- data.frame(
  Count = count_table,
  Percentage = percentage_table
)
table_depend <- table_depend[c("Count.Var1","Count.Freq", "Percentage.Freq")]
colnames(table_depend) = c('DEPEND','count','percentage' )
table_depend

#   DEPEND count percentage
# 1      0   135   16.03
# 2      1   707   83.97

#-----------------------------------------------------------------------------
# Part (b)
#-----------------------------------------------------------------------------
#
# Probit Model
#-----------------------------------------------------------------------------
probit = glm(DEPEND ~ DCOST + CARS + DOVTT + DIVTT,
             data = TransportChoiceDataset,
             family = binomial(link = "probit"))

coeftest(probit)

# z test of coefficients:
#   
#                Estimate Std. Error   z value Pr(>|z|)    
#  (Intercept)   -0.60077    0.16555   -3.63  0.00028 ***
#    DCOST        0.00957    0.00205    4.67    3e-06 ***
#    CARS         1.22519    0.11397   10.75  < 2e-16 ***
#    DOVTT        0.03237    0.00974    3.32  0.00090 ***
#    DIVTT        0.00535    0.00498    1.07  0.28326    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#-----------------------------------------------------------------------------

# Logit Model
#-----------------------------------------------------------------------------
logit = glm(DEPEND ~ DCOST + CARS + DOVTT + DIVTT,
             data = TransportChoiceDataset,
             family = binomial(link = "logit"))

coeftest(logit)

# z test of coefficients:
#   
#             Estimate Std. Error   z value Pr(>|z|)    
# (Intercept) -1.22177    0.30351     -4.03  5.7e-05 ***
#   DCOST      0.01694    0.00382      4.44  9.0e-06 ***
#   CARS       2.30828    0.22612     10.21  < 2e-16 ***
#   DOVTT      0.06223    0.01874      3.32   0.0009 ***
#   DIVTT      0.00925    0.00946      0.98   0.3282    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#-----------------------------------------------------------------------------
# Part (c)
# AIC,BIC,logLik and hit-rate for the models
#-----------------------------------------------------------------------------

# Function for finding hit-rate of a model
find_hitrate <- function(model){
  predictions <- fitted(model)
  predictions <- predictions > 0.5
  return(sum(predictions == TransportChoiceDataset$DEPEND)/length(predictions))
}

#-----------------------------------------------------------------------------
options(digits = 5)
# Probit Model
#-----------------------------------------------------------------------------
AIC(probit)
# [1] 470.33

BIC(probit)
# [1] 494

logLik(probit)
# 'log Lik.' -230.16 (df=5)

find_hitrate(probit)
# [1] 0.9038

#-----------------------------------------------------------------------------
# Logit Model
#-----------------------------------------------------------------------------

AIC(logit)
# [1] 465.74

BIC(logit)
# [1] 489.42

logLik(logit)
#'log Lik.' -227.87 (df=5)

find_hitrate(logit)
# [1] 0.9038

#-----------------------------------------------------------------------------
# Question (3) 
#-----------------------------------------------------------------------------

# read the data
file_path = "C:/Users/utpal/Documents/Notes/Econometrics/Assignments/Assignment 1/Mroz Data.xlsx"
Mroz_Data <- read_excel(file_path, sheet = "Data")

# Mroz_Data <- data.frame(Mroz_Data)
View(Mroz_Data)

# column names
colnames(Mroz_Data)

# [1] "LFP"    "WHRS"   "KL6"    "K618"   "WA"     "WE"     "WW"     "RPWG"   "HHRS"   "HA"    
# [11] "HE"     "HW"     "FAMINC" "MTR"    "WMED"   "WFED"   "UN"     "CIT"    "AX"  

# Variables of interest
# WHRS ~ WomenEduc + WomenExp + WomenAge + childl6

# Filter out variables which are not of interest
interested_columns = c("WHRS","KL6","WE","WA","AX")
Mroz_Data <- Mroz_Data[interested_columns]
View(Mroz_Data)

#-----------------------------------------------------------------------------
# part (a)
#-----------------------------------------------------------------------------

# Create a tables for descriptive summary of variables
means <- rep(0,length(colnames(Mroz_Data)))
std <- rep(0,length(means))
names <- colnames(Mroz_Data)

options(digits = 2)
for(i in 1:length(means)){
  means[i] = mean(Mroz_Data[[names[i]]])
  std[i] = sd(Mroz_Data[[names[i]]])
}
table_cont <- data.frame(
  Name = names,
  Mean = means,
  StdDev = std
)
table_cont

  # Name   Mean StdDev
# 1 WHRS 740.58 871.31
# 2  KL6   0.24   0.52
# 3   WE  12.29   2.28
# 4   WA  42.54   8.07
# 5   AX  10.63   8.07

#-----------------------------------------------------------------------------
# part (b)
#-----------------------------------------------------------------------------

# filter to only have positive work hours
filtered_by_WHRS = Mroz_Data[Mroz_Data$WHRS > 0, ]

# Fit a Linear regression model to only on positive values of WHRS
lr_model <- lm(WHRS ~ WE + WA + AX + KL6,
               data = filtered_by_WHRS)

coeftest(lr_model)

# t test of coefficients:
#   
#               Estimate  Std. Error    t value Pr(>|t|)    
#   (Intercept)  1829.75     292.54    6.25  9.8e-10 ***
#   WE            -16.46      15.58   -1.06   0.2913    
#   WA            -17.11       5.46   -3.13   0.0018 ** 
#   AX             33.94       5.01    6.77  4.2e-11 ***
#   KL6          -305.31      96.45   -3.17   0.0017 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#-----------------------------------------------------------------------------
# part (d)
#-----------------------------------------------------------------------------
options(digits = 4)
# Fit a tobit model
Tobit = tobit(WHRS ~ WE + WA + AX + KL6,
              data = Mroz_Data, left = 0, right = Inf,
              dist = "gaussian")

coeftest(Tobit)

# z test of coefficients:
#   
#                Estimate Std. Error   z value Pr(>|z|)    
#   (Intercept) 1349.8763   386.2991    3.49  0.00048 ***
#   WE            73.2910    20.4746    3.58  0.00034 ***
#   WA           -60.7678     6.8882   -8.82  < 2e-16 ***
#   AX            80.5353     6.2878   12.81  < 2e-16 ***
#   KL6         -918.9181   111.6607   -8.23  < 2e-16 ***
#   Log(scale)     7.0332     0.0371  189.57  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#-----------------------------------------------------------------------------
# part (e)
#-----------------------------------------------------------------------------

# Means of all the regressor variables
Tobit$means

# (Intercept)          WE          WA          AX         KL6
#      1.0000     12.2869     42.5378     10.6308      0.2377 

# set x_p to means of all regressor variables
x_p = Tobit$means

# set index corresponding to KL6(childl6) to 1
x_p[5] = 1

x_p
# (Intercept)          WE          WA          AX         KL6 
#       1.00       12.29       42.54       10.63        1.00 

# coefficients of the Tobit Model
Tobit$coefficients

# (Intercept)          WE          WA          AX         KL6 
#     1349.88       73.29      -60.77       80.54     -918.92 

# coefficient for WE (WomenEduc)
beta_j = Tobit$coefficients[2]

error_sigma = Tobit$scale

# Marginal Effect of another year of education on observed hours of work
# \Phi(\frac{x'_p \beta}{\sigma})
marginal_effect = beta_j*pnorm(x_p%*%Tobit$coefficients/error_sigma)
marginal_effect

#      [,1]
# [1,] 26.6

#-----------------------------------------------------------------------------

