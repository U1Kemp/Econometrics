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
# ii) Analyze public opinion on extent marijuana legalization in the US i.e., estimate Model 8
#-----------------------------------------------------------------------------

OrdLogistic <- polr(factor(q46) ~ log_age + log_income + factor(relig) + q57 + factor(educ2) +
                      race3m1 + party + hh1 + q55 ,
                    data = dummy_copy, 
                    method = 'logistic' # change to 'probit' for ordered probit
)
summary(OrdLogistic)
