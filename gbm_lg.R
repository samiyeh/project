setwd("/Users/samiyehmahmoudian/Desktop/Kaggle")
training<-read.csv("train.csv")
sum(is.na(training))
test<- read.csv ("test.csv")
library(xgboost)
library(Matrix)
library(caret)
str(training)
summary (training)
dim(training)
head(training)
training$TARGET<-ifelse(training$TARGET==0,"YES","NO")
training$TARGET<-as.factor(training$TARGET)
training<-training[,-1] #remove the ID column
testID <- test$ID
test<-test[,-1]
# Doing routine process!

#processing zero varance  
p<-preProcess(training,method="zv")
train1<-predict(p,training)
test1<-predict(p,test)

library(digest)
train_NoDuplicate<-train1[!duplicated(lapply(train1, digest))]
test_NoDuplicate<-test1[!duplicated(lapply(test1, digest))]
head (train_NoDuplicate)
if (ncol(train_NoDuplicate)!=ncol(train1)){
    print("we remove Duplicated")
    print(ncol(train_NoDuplicate))
}


fitControl <- trainControl(method = "cv",
                           number = 5,
                           verboseIter=TRUE,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)
# Set a seed for reproducibility
set.seed(1234)

gbmGrid=expand.grid(
    .n.trees = c(200),
    .interaction.depth = c(4),
    .shrinkage = c(0.05),
    .n.minobsinnode = c(10)
)
gbmFit1 <- train(TARGET ~ ., data = train_NoDuplicate, method = "gbm",
                 # preProcess="scale",
                 trControl = fitControl,
                 tuneGrid =gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")

model <- train(TARGET ~ .,  data=train_NoDuplicate, method="glm", family="binomial", trControl = fitControl, metric = "ROC")
