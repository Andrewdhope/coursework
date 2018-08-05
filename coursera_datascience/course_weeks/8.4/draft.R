
# course_week: 8.4
# description: Full classification model training and evaluation for a large dataset.
# make generic: no


draft <- function(df) 
{
    df.orig <- df
    
    # EXPLORATORY ANALYSIS
    
    table(sapply(df, class))
    
    # make my own validation set by randomly selecting ~10% of all num_windows. I'll need this to check initial predictions.
    
    # 1) generate length(unique(df2$num_window))/10 uniform random numbers
    
    m <- length(unique(df2$num_window))
    n <- round(m / 10)
    
    set.seed(8)
    valset <- round(runif(n, min = 1, max = m))
    valset <- unique(valset)
    
    df.val <- subset(df2, df2$num_window %in% valset)
    df.train <- subset(df2, !(df2$num_window %in% valset))
    
    # repeat the same procedure to break df.val into subsets 1:10
    # createDataPartition
    
    # FEATURE SELECTION
    
    # remove columns that are totally na
    df <- df.orig[, colSums(is.na(df.orig)) == 0] # df now only includes complete columns
    # a table will show that all columns are either complete, or missing 19216 values
    table(colSums(is.na(df.orig)))
    # are those columns with only 406 values relevant at all?
    # save them off
    df.removed <- df.orig[, colSums(is.na(df.orig)) > 0]
    # these all correspond to new_window = yes
    
    # continue with some feature selection. 93 is too many.
    # can get rid of another ~35 by doing the same thing with a null check
    df2 <- df[, colSums(df == "") == 0]
    
    # model should remove any variables that align with the index, such as num_window.
    # these are the first 7 variables that record test meta-data.
    # actually don't remove them from the data frame, rather remove them from the training formula.
    # df3 <- df2[, -c(1:7)]
    
    set.seed(42)
    
    # ALGORITHM SELECTION
    
    # choice of models should start with classification models
    # random forest, g-bootstrapped-m, regularlized logistic regression
    
    # naive bayes
    # fit.nb <- train(classe ~ ., data = df.xyz, method = "nb", verbose = TRUE, trControl = trainControl(verboseIter = TRUE) # ~30 minutes
    # pred.nb <- predict(fit.nb, df.val)
    # sum(pred.nb == df.val$classe)/length(df.val$classe)
    # 0.674 (training set had 0.75)
    
    # random forest
    # fit.rf <- train(classe ~ ., data = df.xyz, method = "rf", verbose = TRUE, trControl = trainControl(verboseIter = TRUE) # ~90 minutes
    # pred.rf <- predict(fit.rf, df.val)
    # sum(pred.rf == df.val$classe)/length(df.val$classe)
    # 0.944 (training set had 0.98)
    
    # random forest 2
    # fit.rf2 <- train(classe ~ ., data = df.train2, method = "rf", verbose = TRUE, trControl = trainControl(verboseIter = TRUE, method = "cv", number = 10))
    # pred.rf2 <- predict(fit.rf2, df.val)
    # sum(pred.rf2 == df.val$classe)/length(df.val$classe)
    # 0.948 (training set had 0.99)
    
    # random forest 3 (tuning mtry)
    # fit.rf3 <- train(classe ~ ., data = df.train2, method = "rf", verbose = TRUE, trControl = trainControl(verboseIter = TRUE, method = "cv", number = 10, search = "grid"), tuneGrid = expand.grid(.mtry=c(20, 25, 30, 35)))
    # pred.rf3 <- predict(fit.rf3, df.val)
    # sum(pred.rf3 == df.val$classe)/length(df.val$classe)
    # 0.947 (training set had 0.996)
    
    # gradient boosted model
    # fit.gbm <- train(classe ~ ., data = df.xyz, method = "gbm", verbose = TRUE, trControl = trainControl(verboseIter = TRUE) # ~60 minutes
    # pred.gbm <- predict(fit.gbm, df.val)
    # sum(pred.gbm == df.val$classe)/length(df.val$classe)
    # 0.96 (training set had 0.885)
    
    # (regularized) logistic regression - regLogistic
    # fit.rl <- train(classe ~ ., data = df.xyz, method = "rl", verbose = TRUE, trControl = trainControl(verboseIter = TRUE) # ~60 minutes
    # pred.rl <- predict(fit.rl, df.val)
    # sum(pred.rl == df.val$classe)/length(df.val$classe)
    #  (training set had )
    
    # TUNING PARAMETERS
    
    # cross validation is built into a lot of the methods in train (trControl). confirm this with my acutal methods.
    # it seems like they're bootstrapping with 25 samples... or something...
    # http://topepo.github.io/caret/train-models-by-tag.html
    # yep, re-did with cv and 10 tries. different than bootstrapping.
    
    # read up on the tuning parameters and figure out which cross validation method to use.
    # - selectionFunction
    # - "cv", number = 10, repeat = 25
    # one more time with a different mtry values
    # - tunegrid <- expand.grid(.mtry = c(20, 25, 30, 35))
    
    # EVALUATION
    
    # read up on evaluation metrics for classification algorithms.
    # Gini error, Deviance
    # accuracy. kappa
    # confusionMatrix(pred, df.val$classe)
    
    # Load test data and evaluate. Predict what the accuracy will be (high-end of the confidence interval).
}