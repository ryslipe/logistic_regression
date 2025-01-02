library(ggplot2)
library(cowplot)

data <- read.csv('processed.cleveland.data', header = FALSE)

# first 6 rows - no column names listed
head(data)

# "age",
# "sex",# 0 = female, 1 = male
# "cp", # chest pain
# 1 = typical angina,
# 2 = atypical angina,
# 3 = non-anginal pain,
# 4 = asymptomatic
# "trestbps", # resting blood pressure (in mm Hg)
# "chol", # serum cholestoral in mg/dl
# "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
# "restecg", # resting electrocardiographic results
# 1 = normal
# 2 = having ST-T wave abnormality
# 3 = showing probable or definite left ventricular hypertrophy
# "thalach", # maximum heart rate achieved
# "exang",   # exercise induced angina, 1 = yes, 0 = no
# "oldpeak", # ST depression induced by exercise relative to rest
# "slope", # the slope of the peak exercise ST segment
# 1 = upsloping
# 2 = flat
# 3 = downsloping
# "ca", # number of major vessels (0-3) colored by fluoroscopy
# "thal", # this is short of thalium heart scan
# 3 = normal (no cold spots)
# 6 = fixed defect (cold spots during rest and exercise)
# 7 = reversible defect (when cold spots only appear during exercise)
# "hd" # (the predicted attribute) - diagnosis of heart disease
# 0 if less than or equal to 50% diameter narrowing
# 1 if greater than 50% diameter narrowing

# add column names listed on website
colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps", 
  "chol",
  "fbs",  
  "restecg", 
  "thalach", 
  "exang",   
  "oldpeak",
  "slope", 
  "ca",
  "thal",
  "hd"
)

# view data structure
str(data)

# change sex category to 0 - Female and 1 - Male
data[data$sex == 0,]$sex <- 'F'
data[data$sex == 1,]$sex <- 'M'

# change sex column to categorical
data$sex <- as.factor(data$sex)

# cp, rbs, restecg, exang, and slope are all supposed to be factors
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

# ca is also categorical but has question marks so change this - remember NA values exist here 
data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

# thal has NA values but should be categorical
data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

# change hd column to 0 for healthy and 1 for unhealthy with ifelse
data$hd <- ifelse(test = data$hd == 0, yes = 'Healthy', no = 'Unhealthy')

# number of missing rows in ca and thal - only 6 so they can be removed
nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]

# we want all columns but we do not want the rows where the values are NA
data <- data[!(is.na(data$ca) | is.na(data$thal)),]

# create a tables for heart disease and each variable
xtabs(~ hd + sex, data = data)
xtabs(~ hd + cp, data = data)
xtabs(~ hd + fbs, data = data)
xtabs(~ hd + restecg, data = data)
xtabs(~ hd + exang, data = data)
xtabs(~ hd + slope, data = data)
xtabs(~ hd + ca, data = data)
xtabs(~ hd + thal, data = data)



data$hd <- as.factor(data$hd)

################################################################################
# Modeling
################################################################################

# only using sex as the predictor - family = binomial means logistic regression
logistic <- glm(hd ~ sex, data = data, family = "binomial")
summary(logistic)

# compute R2
# log likelihood of null deviance
ll_null <- logistic$null.deviance / -2

# log likelihood of proposed
ll_proposed <- logistic$deviance / -2

# compute R squared
r_squared <- (ll_null - ll_proposed) / ll_null

# model that uses all variables
logistic <- glm(hd ~ ., data = data, family = 'binomial')
summary(logistic)

# log likelihood of null deviance
ll_null <- logistic$null.deviance / -2

# log likelihood of proposed
ll_proposed <- logistic$deviance / -2

# compute R squared
r_squared <- (ll_null - ll_proposed) / ll_null

# use equation for p-value - we want upper tail so 1 - pchisq
# pchisq gives the area up til our test value but we want the rest so we use 1 - pchisq
p_val = 1 - pchisq(2*(ll_proposed - ll_null), df = (length(logistic$coefficients)- 1))

