run_analysis<- function(){
  library(plyr)
  library(reshape2)
  #Place the following files in the working directory -
  #1. activity_labels.txt
  #2. features.txt
  #3. subject_test.txt
  #4. subject_train.txt
  #5. x_train.txt
  #6. y_train.txt
  #7. x_test.txt
  #8. y_test.txt
  
  subject_test<- read.table("subject_test.txt", col.names=c("Subject"))
  subject_train<- read.table("subject_train.txt", col.names=c("Subject"))
  subject_all<- rbind(subject_train, subject_test)
  
  feat_test<- read.table("X_test.txt")
  feat_train<- read.table("X_train.txt")
  feat_all<- rbind(feat_train, feat_test)
  feat_list<- read.table("features.txt", col.names=c("index", "feature_labels"))
  feature_labels<- feat_list$feature_labels
  features_subset<- grepl('mean\\(\\)|std\\(\\)', feature_labels)
  feature_list <- as.character(feature_labels[features_subset])
  colnames(feat_all) <- feature_labels
  feat_all<- feat_all[,features_subset]
  
  act_test<- read.table("Y_test.txt")
  act_train<- read.table("Y_train.txt")
  act_all<- rbind(act_train, act_test)
  colnames(act_all)<- "activityLabel"
  act_labels<- read.table("activity_labels.txt", sep=" ", col.names=c("activityLabel", "Activity"))
  act_all<-join(act_all, act_labels, by="activityLabel", type="left")
  act_all$activityLabel <- NULL
  
  all_df <- cbind(feat_all, act_all, subject_all)
  
  tdf <- melt(all_df, id=c("Subject", "Activity"), measure.vars=feature_list)
  tdf <- dcast(tdf, Activity + Subject ~ variable, mean)
  tdf <- tdf[order(tdf$Subject, tdf$Activity), ]
  
  rownames(tdf) <- 1:nrow(tdf)
  tdf <- tdf[,c(2,1,3:68)]
  write.table(tdf, file="tidy_dataset.txt")
}