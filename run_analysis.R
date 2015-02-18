run_analysis <- function() {
  library(dplyr)
  
  setwd("UCI HAR Dataset")
  #universal activity linkage and var names#
  head <- read.table("features.txt", header = FALSE)
  activity <- read.table("activity_labels.txt", header = FALSE)
  
  setwd("test")
  #read testing data#
  xtest <- read.table("x_test.txt", header = FALSE)
  ytest <- read.table("y_test.txt", header = FALSE)
  subtest <- read.table("subject_test.txt", header = FALSE)
  
  setwd("../train")
  #read train data#
  x <- read.table("x_train.txt", header = FALSE)
  y <- read.table("y_train.txt", header = FALSE)
  sub <- read.table("subject_train.txt", header = FALSE)
  
  #name columns#
    names(x) <- head[,2]
    names(xtest) <- head[,2]
    names(activity) <- c("link","activity")
  #combine sub and activity colums training#
    xy <- cbind(x,y)
    xytest <- cbind(xtest,ytest)
  #combine sub and activity colums test#
    xysub <-cbind(xy,sub)
    xysubtest <- cbind(xytest,subtest)
  #Rename appended colums#
  colnames(xysub)[562:563] <- c("link","subject")
  colnames(xysubtest)[562:563] <- c("link","subject")
  #merge activities to identifier#
  finaltrain <- merge(xysub,activity,by ="link")
  finaltest  <- merge(xysubtest,activity, by ="link")
  #set the two datasets together#
  finalcombined <- rbind(finaltrain,finaltest)
  #select only variables which contain the strings "subject", "activity", "mean", or "std"#
  store2<-finalcombined[,grepl("subject|activity|mean|std",names(finalcombined))]
  #summarize average by strata, subject - activity# 
  finalsummary <- store2 %>% group_by(subject,activity) %>% summarise_each(funs(mean))
  
}