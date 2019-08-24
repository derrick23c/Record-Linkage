

##########################################################
#             INSTALL THE PROPER PACKAGES 
##########################################################
install.packages("RecordLinkage")
library(RecordLinkage)

install.packages("readr")
library(readr)rm(list=ls())

##########################################################
#                       PREP
##########################################################

#Set workind directory to the folder that contains the csv data file for deduplication
setwd("C:/Users/kythomson/Desktop/Initiative_Patient Matching Data Challenge") 

#Read in the CSV file
mydata <- read_csv("PatientRecords_7.18clean_FOR USE.csv")

#Choose which columns to include for use in the deduplication algorithm
cols <- c(1:14,19:21,23)
cleaned <- mydata[,cols]

#This increases the available memory for R to use. Helps prevent error messages when working with large data sets
memory.limit(size=20000)
gc()

##########################################################
#                 RUNNING THE ALGORITHM
##########################################################

#This is the deduplication step. You can choose which fields to block on and elect to apply particular functions on  fields of choice. 
rpairs <- compare.dedup(cleaned, blockfld = c(3,4), strcmp = c(3,4), strcmpfun = jarowinkler, exclude = c(1,2,5,6,8,9,10,14))
summary(rpairs)  # [Optional] If you would like to see a summary

#This step is the probabilistic step, where the algorithm will run and assign weights to each possible record pair. Higher weights are better.
rpairs <- emWeights(rpairs)
hist(rpairs$Wdata, plot = FALSE) # [Optional] If you would like to see a histogram of the weights


#This step is technically unnecessary, it simply classifies the record pairs as "links", "possible links", or "nonlinks" according to the thresholds that the user defines manually.
rpairsClassified <- emClassify(rpairs, threshold.upper = 24, threshold.lower = -7)

##########################################################
#                     Export to CSV
##########################################################

#Writes the complete list of possible record pairs, regardless of matching classification, to a CSV file. Writes them in single row format, with the matching weight listed as the last column. 
write.csv(getPairs(rpairsClassified, show="all", sort=TRUE, single.rows=TRUE), file = "08102017_Probabilistic(SingleRow)_Run15.csv",row.names=FALSE)

##########################################################
#                     See also
##########################################################

#These are different ways of viewing the record pairs

#See just the "links"
Links <- getPairs(rpairsClassified, show="links", sort=TRUE)

#See just the "possible links"
Possibles <- getPairs(rpairsClassified, show="possible", sort=TRUE)

#See just the "non-links"
Non-links <- getPairs(rpairsClassified, show="nonlinks", sort=TRUE)

#See record pairs with assigned weights between 30 and 15
getPairs(rpairs, 30, 15)

#This will clear your work environment to help free up space, run at the beginning of the code
rm(list=ls())
