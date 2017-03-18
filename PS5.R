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
ANESslected <- ANES[ , c("ft_dpc", "gender_respondent_x", "dem_raceeth_x", "interest_attention",
                         "prmedia_atinews", "prmedia_attvnews", "prmedia_atpprnews",
                         "prmedia_atrdnews", "presapp_track", "libcpre_self", "pid_self")]

# Name the columns in a better way.
colnames(ANESslected) <- c("Obama", "Gender", "Ethnicty", "Attention", "Internet", "TV", 
                           "Newspaper", "Radio", "Direction", "Selfplacement", "PartyIdentity")

# Clean the data
# Recode the varible about political interest and media.
for(i in c(4,5,6,7,8)){
ANESslected[ , i] <- ifelse(substr(ANESslected[ , i], 1, 1) == '1', 1,
                          ifelse(substr(ANESslected[ , i], 1, 1) == '2', 2,
                                 ifelse(substr(ANESslected[ , i], 1, 1) == '3', 3,
                                        ifelse(substr(ANESslected[ , i], 1, 1) == '4', 4, 
                                               ifelse(substr(ANESslected[ , i], 1, 1) == '5', 5, NA)))))
}

# Recode the varible about political interest and media.
for(i in c(4,5,6,7,8)){
  ANESslected[ , i] <- ifelse(substr(ANESslected[ , i], 1, 1) == '1', 1,
                              ifelse(substr(ANESslected[ , i], 1, 1) == '2', 2,
                                     ifelse(substr(ANESslected[ , i], 1, 1) == '3', 3,
                                            ifelse(substr(ANESslected[ , i], 1, 1) == '4', 4, 
                                                   ifelse(substr(ANESslected[ , i], 1, 1) == '5', 5, NA)))))
}

# Recode self-placement on the left-right dimention.
ANESslected[ , 10] <- ifelse(substr(ANESslected[ , 10], 1, 1) == '1', 1,
                            ifelse(substr(ANESslected[ , 10], 1, 1) == '2', 2,
                                   ifelse(substr(ANESslected[ , 10], 1, 1) == '3', 3,
                                          ifelse(substr(ANESslected[ , 10], 1, 1) == '4', 4, 
                                                 ifelse(substr(ANESslected[ , 10], 1, 1) == '5', 5, 
                                                        ifelse(substr(ANESslected[ , 10], 1, 1) == '6', 6, 
                                                               ifelse(substr(ANESslected[ , 10], 1, 1) == '7', 7, NA)))))))

# Randomly subset the data into two partitions.
