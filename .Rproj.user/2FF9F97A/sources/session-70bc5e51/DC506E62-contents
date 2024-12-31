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
