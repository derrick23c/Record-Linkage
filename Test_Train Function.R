###############################################################
# TRAINING USING RESULTS OF UNSUPERVISED RUNS BLOCKING ON SSN #
###############################################################
rm(list=ls())

setwd("C:/Users/kythomson/Desktop/Initiative_Patient Matching Data Challenge/Machine Learning")
library('RecordLinkage')
library('readr')
train9_11 <- read_csv("9.11train1.csv")
test9_11 <- read_csv("9.11test1.csv")

#Define identity vector for the training dataset
train_identity <- as.vector(train9_11$identity)

#Keep only Enterprise ID, DOB, newZIP, newLast, newFirst, Gender, newSSN, and concatAddress
keep_cols <-c(1,3,4,5,6,7,8,12) 
train_col <- train9_11[,keep_cols]
test_col <- test9_11[,keep_cols]

full <- rbind(train_col, test_col)

train9_11 <- 0
test9_11 <- 0 

#Use only records with data for SSN
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
###########################
#TRAINING DATASET: "KNOWN"#
#BLOCKING: ON SSN         #
#METHOD: RPART            #
###########################

memory.limit(size=50000)

test_train <- function(block, num_non, Unsup_method, Sup_method) {
  #Use the above defined "completeFun" to remove NAs from the blkField in the testing and training datasets#
  complete_test <<- completeFun(full, block)
  
  #create record pairs on selected block variable from all of the data#
  overall_pairs <<- compare.dedup(full, blockfld = c(block), strcmp= c(5,6,8), strcmpfun = jarowinkler, exclude=c(1),
                                  n_match = 61885)
  
  #Run classifyUnsup on paired data#
  
  #Count number of record pairs#
  count_pairs <<- nrow(getPairs(overall_pairs, single.row=TRUE))
                       
  #Define expected proporation of links#
  prop_links = 61885/(count_pairs)
  
  #Run classifyUnsup#
  Unsup_pairs <<- classifyUnsup(overall_pairs, Unsup_method, iter.max = 300)
  
  #Sample from the unsupervised classification
  samp_valid <<- genSamples(Unsup_pairs, num_non, prop_links)
  
  #Define testing and training datasets#
  train <<- samp_valid$train
  test <<- samp_valid$valid
  
  #Train model
  model <<- trainSupv(train, Sup_method, use.pred = TRUE, include.data = FALSE)
  
  #Fit pairs using model
  modelClassify <<- classifySupv(model, test)
  prediction <<- as.vector(modelClassify$prediction)

  #Pull out prediction k
  ClassifiedPairs <<- getPairs(modelClassify, single.row = TRUE)
  result_pairs <<- cbind(ClassifiedPairs, prediction)
  
}





#-------------------------------------*****************************************------------------------------
test_train(block= c(4,5), num_non=20000, Unsup_method="bclust", Sup_method="svm")
#-------------------------------------*****************************************------------------------------




#Keep only links from trained classification dataset#
#Also rename to reflect block, Unsupervised method (kmeans or bclust), supverised method, & # of samples
#so for example: SSN_kmeans_rpart_20000
result_pairs2 <<- subset(result_pairs, prediction=="L",drop=TRUE)

#just output the Enterprise IDs#
Ent_IDcols <- c(2,11)
SSN_kmeans_rpart_20000 <<- result_pairs2[,Ent_IDcols]  



#-------------------------------------*****************************************------------------------------
#Write to CSV#
write.csv(SSN_kmeans_rpart_20000, file = "FirstLast_bclust_svm_20000.csv" )
#-------------------------------------*****************************************------------------------------