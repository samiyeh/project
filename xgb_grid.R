library(xgboost)
library(Matrix)

set.seed(1234)

train <- read.csv("/Users/samiyehmahmoudian/Desktop/Kaggle/train.csv")
test  <- read.csv("/Users/samiyehmahmoudian/Desktop/Kaggle/test.csv")

##### Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

##### Extracting TARGET
train.y <- train$TARGET
train$TARGET <- NULL

##### 0 count per line
count0 <- function(x) {
    return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
    if (length(unique(train[[f]])) == 1) {
        cat(f, "is constant in train. We delete it.\n")
        train[[f]] <- NULL
        test[[f]] <- NULL
    }
}

##### Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
    f1 <- pair[1]
    f2 <- pair[2]
    
    if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
        if (all(train[[f1]] == train[[f2]])) {
            cat(f1, "and", f2, "are equals.\n")
            toRemove <- c(toRemove, f2)
        }
    }
}

feature.names <- setdiff(names(train), toRemove)

train$var38 <- log(train$var38)
test$var38 <- log(test$var38)

train <- train[, feature.names]
test <- test[, feature.names]
tc <- test

#---limit vars in test based on min and max vals of train
print('Setting min-max lims on test data')
for(f in colnames(train)){
    lim <- min(train[,f])
    test[test[,f]<lim,f] <- lim
    
    lim <- max(train[,f])
    test[test[,f]>lim,f] <- lim  
}
#---

train$TARGET <- train.y

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
    nrounds = c(300,560,1000),
    eta = c(0.1,0.02, 0.002),
    max_depth = c(2, 4, 6, 8),
    subsample= c(0.6815,0.8),
    colsample_bytre =c(0.701,0.8), 
    gamma = c(0.8,10000)
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    returnData = FALSE,
    returnResamp = "all",                                                        # save losses across all models
    classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
    summaryFunction = twoClassSummary,
    allowParallel = TRUE
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb_train_1 = train(
    x = as.matrix(train %>%
                      select(-TARGET)),
    y = as.factor(train$TARGET),
    trControl = xgb_trcontrol_1,
    tuneGrid = xgb_grid_1,
    method = "xgbTree"
)


#######actual variables

feature.names

test$TARGET <- -1

{
    library(pROC)
    #auc<-auc(as.numeric(actual),as.numeric(predicted))
    #auc 
}

# BAD
# num_var35 = tc['num_var35']
# saldo_var30 = tc['saldo_var30']
# No improvement
# num_var1 = tc['num_var1']

