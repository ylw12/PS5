############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 5
## Author: Luwei Ying
############################################

rm(list=ls())
setwd("/Users/luweiying/Desktop/ASP/PS5/PS5_Luwei")

# Read in the data and subset the data frame by variable, leaving only the 
# variables we are interested in.
library(foreign)
ANES <- read.dta("anes_timeseries_2012_stata12.dta")
ANESslected <- ANES[ , c("ft_dpc", "gender_respondent_x", "interest_attention",
                         "prmedia_atinews", "prmedia_attvnews", "prmedia_atpprnews",
                         "prmedia_atrdnews", "presapp_track", "libcpre_self", "pid_self")]

# Name the columns in a better way.
colnames(ANESslected) <- c("Obama", "Gender", "Attention", "Internet", "TV", 
                           "Newspaper", "Radio", "Direction", "Selfplacement", "PartyIdentity")

# Clean the data
# Recoded gender, man is coded as 1 and female 0.
ANESslected[ ,2] <- ifelse(substr(ANESslected[ , 2], 1, 1) == '1', 1, 0)

# Recode the varible about political interest and media.
for(i in c(3,4,5,6,7)){
ANESslected[ , i] <- ifelse(substr(ANESslected[ , i], 1, 1) == '1', 1,
                          ifelse(substr(ANESslected[ , i], 1, 1) == '2', 2,
                                 ifelse(substr(ANESslected[ , i], 1, 1) == '3', 3,
                                        ifelse(substr(ANESslected[ , i], 1, 1) == '4', 4, 
                                               ifelse(substr(ANESslected[ , i], 1, 1) == '5', 5, NA)))))
}

# Recode the varible about whether the overall direction is right or wrong.
ANESslected[ , 8] <- ifelse(substr(ANESslected[ , 8], 1, 1) == '1', 1,
                              ifelse(substr(ANESslected[ , 8], 1, 1) == '2', 0, NA))

# Recode self-placement on the left-right dimention.
ANESslected[ , 9] <- ifelse(substr(ANESslected[ , 9], 1, 1) == '1', 1,
                            ifelse(substr(ANESslected[ , 9], 1, 1) == '2', 2,
                                   ifelse(substr(ANESslected[ , 9], 1, 1) == '3', 3,
                                          ifelse(substr(ANESslected[ , 9], 1, 1) == '4', 4, 
                                                 ifelse(substr(ANESslected[ , 9], 1, 1) == '5', 5, 
                                                        ifelse(substr(ANESslected[ , 9], 1, 1) == '6', 6, 
                                                               ifelse(substr(ANESslected[ , 9], 1, 1) == '7', 7, NA)))))))

# Recode the party idendity, collapse the levels into three.
ANESslected[ , 10] <- ifelse(substr(ANESslected[ , 10], 1, 1) == '1', 1,
                             ifelse(substr(ANESslected[ , 10], 1, 1) == '2', 2, NA))

# Randomly subset the data into two partitions.
set.seed(662)
training <- ANESslected[sample(1:nrow(ANESslected), nrow(ANESslected)/2, replace = FALSE),]
testing <- ANESslected[!(row.names(ANESslected) %in% row.names(training)),]

# Build the models
# Because I got too many NAs in the four variables, "Internet", "TV", "Newspaper", "Radio",
# I leave them out of the model to get more predictions later.

# OLS
model_OLS <- lm(Obama ~ Gender + Attention + Direction + Selfplacement + PartyIdentity, 
                data = training)
summary(model_OLS)

# Tobit
library(AER)
model_Tobit <- tobit(Obama ~ Gender + Attention + Direction + Selfplacement + PartyIdentity, 
                     data = training, 
                     left = 0, right = 100)
summary(model_Tobit)

# Random Forest
library(randomForest)
model_RF <- randomForest(Obama ~ Gender + Attention + Direction + Selfplacement + PartyIdentity, 
                         data = na.omit(training), trees = 1000)
summary(model_RF)

# Now use the above three models to predict the value of Obama thermometer the "testing" 
# group.
PredOLS <- predict(model_OLS, newdata = testing, type="response")
PredTobit <- predict(model_Tobit, newdata = testing, type="response")
PredRF <- predict(model_RF, newdata = testing, type="response")

# Save the predicted values into a matrix.
Allpredictions <- as.matrix(cbind(PredOLS, PredTobit, PredRF))

head(Allpredictions, 100)

# Test (We can later go back here and test the functions with these data)
r <- rep(median(y), nrow(y))
p <- Allpredictions
y <- as.matrix(testing[ ,1])
FitStats(y=y, p=p, r=r, fits=c("rmse", "mad", "mrae"))
FitStats(y=y, p=p, fits=c("rmse", "mad", "mrae")) #This would throw an error.